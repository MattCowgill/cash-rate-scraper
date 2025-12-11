# ============================================================================
# RBA CASH RATE FUTURE MEETING LINE CHARTS
# ============================================================================
# Creates static line charts for every future RBA meeting where at least one
# rate move exceeds a 10% probability at any point in the historical scrapes.
# A caption reminds readers that probabilities may not sum to 100% because
# smaller moves are omitted.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(glue)
  library(ggplot2)
  library(here)
  library(lubridate)
  library(purrr)
  library(readrba)
  library(scales)
  library(tibble)
})

# ----------------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------------
spread <- 0.00
override <- 3.60
hours_tz <- 11

# ----------------------------------------------------------------------------
# Data loading
# ----------------------------------------------------------------------------
cash_rate <- readRDS(here("combined_data", "all_data.Rds"))
load(here("combined_data", "rmse_new.RData"))

cash_rate$cash_rate <- cash_rate$cash_rate + spread

# Output directory for multi-meeting line charts
line_dir <- here("docs", "meeting_lines")
if (!dir.exists(line_dir)) dir.create(line_dir, recursive = TRUE)

# ----------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------
blend_weight <- function(days_to_meeting) {
  pmax(0, pmin(1, 1 - days_to_meeting / 30))
}

# ----------------------------------------------------------------------------
# RBA meeting schedule
# ----------------------------------------------------------------------------
meeting_schedule <- tibble(
  meeting_date = as.Date(c(
    "2025-02-18", "2025-04-01", "2025-05-20", "2025-07-08",
    "2025-08-12", "2025-09-30", "2025-11-04", "2025-12-09",
    "2026-02-03", "2026-03-17", "2026-05-05", "2026-06-16",
    "2026-08-11", "2026-09-29", "2026-11-03", "2026-12-08",
    "2027-02-03", "2027-03-17"
  ))
) %>%
  mutate(
    expiry = if_else(
      day(meeting_date) >= days_in_month(meeting_date) - 1,
      ceiling_date(meeting_date, "month"),
      floor_date(meeting_date, "month")
    )
  ) %>%
  select(expiry, meeting_date)

# ----------------------------------------------------------------------------
# Current rate and scrape filtering
# ----------------------------------------------------------------------------
last_meeting <- max(meeting_schedule$meeting_date[
  meeting_schedule$meeting_date <= Sys.Date()
])

use_override <- !is.null(override) && (Sys.Date() - last_meeting <= 1)

if (use_override) {
  initial_rt <- override
  current_rate <- override
} else {
  latest_rt <- read_rba(series_id = "FIRMMCRTD") %>%
    slice_max(date, n = 1, with_ties = FALSE) %>%
    pull(value)
  initial_rt <- latest_rt
  current_rate <- latest_rt
}

cutoff_time <- ymd_hm(paste0(Sys.Date(), " 14:30"), tz = "Australia/Melbourne")
now_melb <- now(tzone = "Australia/Melbourne")
cutoff_date <- if (now_melb < cutoff_time) Sys.Date() - 1 else Sys.Date()

all_times <- sort(unique(cash_rate$scrape_time))

# Use every available scrape to maximize history in the charts instead of
# starting at the most recent meeting date (which can omit data when a meeting
# happens today).
scrapes <- all_times

# ----------------------------------------------------------------------------
# Implied rate calculations for every scrape × meeting
# ----------------------------------------------------------------------------
all_list <- map(scrapes, function(scr) {
  scr_date <- as.Date(scr)

  df_rates <- cash_rate %>%
    filter(scrape_time == scr) %>%
    select(expiry = date, forecast_rate = cash_rate, scrape_time)

  df <- meeting_schedule %>%
    distinct(expiry, meeting_date) %>%
    mutate(scrape_time = scr) %>%
    left_join(df_rates, by = "expiry") %>%
    arrange(expiry)

  df <- df %>% filter(!is.na(forecast_rate))
  if (nrow(df) == 0) return(NULL)

  prev_implied <- NA_real_
  out <- vector("list", nrow(df))

  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    rt_in <- if (is.na(prev_implied)) initial_rt else prev_implied

    r_tp1 <- if (row$meeting_date < row$expiry) {
      row$forecast_rate
    } else {
      nb <- (day(row$meeting_date) - 1) / days_in_month(row$expiry)
      na <- 1 - nb
      (row$forecast_rate - rt_in * nb) / na
    }

    out[[i]] <- tibble(
      scrape_time = scr,
      meeting_date = row$meeting_date,
      implied_mean = r_tp1,
      days_to_meeting = as.integer(row$meeting_date - scr_date),
      previous_rate = rt_in
    )

    prev_implied <- r_tp1
  }

  bind_rows(out)
})

all_estimates <- all_list %>%
  compact() %>%
  bind_rows() %>%
  filter(days_to_meeting >= 0) %>%
  left_join(rmse_days, by = "days_to_meeting") %>%
  rename(stdev = finalrmse)

max_rmse <- suppressWarnings(max(rmse_days$finalrmse, na.rm = TRUE))
if (!is.finite(max_rmse)) {
  stop("No finite RMSE values found in rmse_days$finalrmse")
}

bad_sd <- !is.finite(all_estimates$stdev) |
          is.na(all_estimates$stdev) |
          all_estimates$stdev <= 0
if (any(bad_sd, na.rm = TRUE)) {
  all_estimates$stdev[bad_sd] <- max_rmse
}

# ----------------------------------------------------------------------------
# Probability buckets
# ----------------------------------------------------------------------------
bucket_centers <- seq(0.10, 6.10, by = 0.25)
half_width <- 0.125
current_center <- bucket_centers[which.min(abs(bucket_centers - current_rate))]

bucket_list <- vector("list", nrow(all_estimates))

for (i in seq_len(nrow(all_estimates))) {
  mu_i <- all_estimates$implied_mean[i]
  sigma_i <- all_estimates$stdev[i]
  d_i <- all_estimates$days_to_meeting[i]

  p_vec <- sapply(bucket_centers, function(b) {
    lower <- b - half_width
    upper <- b + half_width
    pnorm(upper, mean = mu_i, sd = sigma_i) - pnorm(lower, mean = mu_i, sd = sigma_i)
  })
  p_vec[p_vec < 0] <- 0
  p_vec[p_vec < 0.01] <- 0

  # Guard against zero-probability vectors to keep downstream calculations finite
  p_total <- sum(p_vec)
  if (p_total > 0) {
    p_vec <- p_vec / p_total
  } else {
    p_vec <- rep(0, length(p_vec))
  }

  nearest <- order(abs(bucket_centers - mu_i))[1:2]
  b1 <- min(bucket_centers[nearest])
  b2 <- max(bucket_centers[nearest])
  w2 <- (mu_i - b1) / (b2 - b1)

  l_vec <- numeric(length(bucket_centers))
  l_vec[bucket_centers == b1] <- 1 - w2
  l_vec[bucket_centers == b2] <- w2

  blend <- blend_weight(d_i)
  v <- blend * l_vec + (1 - blend) * p_vec

  v_total <- sum(v)
  if (v_total > 0) v <- v / v_total

  bucket_list[[i]] <- tibble(
    scrape_time = all_estimates$scrape_time[i],
    meeting_date = all_estimates$meeting_date[i],
    implied_mean = mu_i,
    stdev = sigma_i,
    days_to_meeting = d_i,
    bucket = bucket_centers,
    probability_linear = l_vec,
    probability_prob = p_vec,
    probability = v,
    diff = bucket_centers - current_rate
  )
}

all_estimates_buckets <- bind_rows(bucket_list)
future_meetings <- meeting_schedule$meeting_date[meeting_schedule$meeting_date > Sys.Date()]
as_of_time <- with_tz(as.POSIXct(max(all_estimates_buckets$scrape_time)) + hours(hours_tz), tz = "Australia/Sydney")

# ----------------------------------------------------------------------------
# Line charts for every future meeting (thresholded at 10% peak probability)
# ----------------------------------------------------------------------------
move_levels <- c("-75 bp cut", "-50 bp cut", "-25 bp cut", "No change",
                "+25 bp hike", "+50 bp hike", "+75 bp hike")

move_palette <- c(
  "-75 bp cut" = "#08306b",
  "-50 bp cut" = "#2171b5",
  "-25 bp cut" = "#6baed6",
  "No change" = "#666666",
  "+25 bp hike" = "#ef3b2c",
  "+50 bp hike" = "#cb181d",
  "+75 bp hike" = "#99000d"
)

for (mt in future_meetings) {
  meeting_df <- all_estimates_buckets %>%
    filter(meeting_date == mt) %>%
    mutate(
      diff_center = bucket - current_center,
      move = case_when(
        near(diff_center, -0.75, tol = 0.01) ~ "-75 bp cut",
        near(diff_center, -0.50, tol = 0.01) ~ "-50 bp cut",
        near(diff_center, -0.25, tol = 0.01) ~ "-25 bp cut",
        near(diff_center, 0.00, tol = 0.01) ~ "No change",
        near(diff_center, 0.25, tol = 0.01) ~ "+25 bp hike",
        near(diff_center, 0.50, tol = 0.01) ~ "+50 bp hike",
        near(diff_center, 0.75, tol = 0.01) ~ "+75 bp hike",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(move)) %>%
    group_by(move) %>%
    mutate(peak_probability = max(probability, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(peak_probability >= 0.05) %>%
    mutate(move = factor(move, levels = move_levels)) %>%
    droplevels()

  if (nrow(meeting_df) == 0) {
    message(glue("Skipping {mt}: no moves exceed 5% probability."))
    next
  }

  end_time <- max(meeting_df$scrape_time)
  start_time <- min(meeting_df$scrape_time)
  six_months_back <- as.POSIXct(Sys.Date() %m-% months(3), tz = "Australia/Melbourne")
  axis_start <- max(start_time + hours(hours_tz), six_months_back)

  line_plot <- ggplot(meeting_df, aes(x = scrape_time + hours(hours_tz), y = probability, color = move)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_color_manual(values = move_palette, drop = TRUE) +
    labs(
      title = glue("Cash rate move probabilities — {format(as.Date(mt), '%d %B %Y')}"),
      subtitle = glue("As of {format(as_of_time, '%d %B %Y, %I:%M %p AEST')}"),
      x = "Scrape time (AEST)",
      y = "Probability",
      color = "Move",
      caption = "Probabilities may not add up to 100% because moves with small probabilities are not included."
    ) +
    coord_cartesian(xlim = c(axis_start, end_time + hours(hours_tz))) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  meeting_label <- format(as.Date(mt), "%Y-%m-%d")
  output_path <- here("docs", "meeting_lines", glue("line_probabilities_meeting_{meeting_label}.png"))
  ggsave(filename = output_path, plot = line_plot, width = 8, height = 5, dpi = 300, device = "png")
}
