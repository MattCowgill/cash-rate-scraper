# =============================================
# 1) Setup & libraries
# =============================================
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(tidyr)
  library(plotly)
  library(readrba)
  library(scales)
  library(ggpattern)
  library(ggtext)
  library(zoo)  # Added for na.locf
})

# =============================================
# 1b) Create required directories
# =============================================
required_dirs <- c(
  "docs/meetings",
  "docs/meetings/csv",
  "combined_data"
)

for (dir_path in required_dirs) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
    cat("Created directory:", dir_path, "\n")
  }
}

# =============================================
# 2) Load data & RMSE lookup
# =============================================
cash_rate <- readRDS("combined_data/all_data.Rds")
load("combined_data/rmse_days.RData")

# Consolidate to daily level
cash_rate_daily <- cash_rate %>%
  mutate(scrape_date = as.Date(scrape_time)) %>%
  group_by(scrape_date, date) %>%
  slice_max(scrape_time, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(scrape_date, date, cash_rate)

cat("Daily consolidation complete:\n")
cat("  Original rows:", nrow(cash_rate), "\n")
cat("  Daily rows:", nrow(cash_rate_daily), "\n")
cat("  Unique dates:", length(unique(cash_rate_daily$scrape_date)), "\n")

blend_weight <- function(days_to_meeting) {
  pmax(0, pmin(1, 1 - days_to_meeting / 30))
}

latest_rt <- read_rba(series_id = "FIRMMCRTD") |>
             slice_max(date, n = 1, with_ties = FALSE) |>
             pull(value)

spread <- 0.00
cash_rate_daily$cash_rate <- cash_rate_daily$cash_rate + spread

# =============================================
# 3) Define RBA meeting schedule
# =============================================
meeting_schedule <- tibble(
  meeting_date = as.Date(c(
    "2022-02-01","2022-03-01","2022-04-05","2022-05-03",
    "2022-06-07","2022-07-05","2022-08-02","2022-09-06",
    "2022-10-04","2022-11-01","2022-12-06",
    "2023-02-07","2023-03-07","2023-04-04","2023-05-02",
    "2023-06-06","2023-07-04","2023-08-01","2023-09-05",
    "2023-10-03","2023-11-07","2023-12-05",
    "2024-02-06","2024-03-19","2024-05-07","2024-06-18",
    "2024-08-06","2024-09-24","2024-11-05","2024-12-10",
    "2025-02-18","2025-04-01","2025-05-20",
    "2025-07-08","2025-08-12","2025-09-30","2025-11-04","2025-12-09",
    "2026-02-03","2026-03-17","2026-05-05","2026-06-16",
    "2026-08-11","2026-09-29","2026-11-03","2026-12-08",
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

# =============================================
# 3b) Define ABS data release schedule
# =============================================
abs_releases <- tribble(
  ~dataset, ~datetime,
  "CPI", ymd_hm("2025-01-29 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-04-30 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-07-30 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-10-29 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-01-29 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-02-26 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2025-02-19 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-01-16 11:30", tz = "Australia/Melbourne")
)

# =============================================
# 4) Process data and create visualizations
# =============================================
last_meeting <- max(meeting_schedule$meeting_date[meeting_schedule$meeting_date < Sys.Date()])
print(last_meeting)

current_rate <- read_rba(series_id = "FIRMMCRTD") %>%
  filter(date == max(date)) %>%
  pull(value)

initial_rt <- latest_rt
all_dates <- sort(unique(cash_rate_daily$scrape_date))

cat("Total available daily scrapes:", length(all_dates), "\n")
cat("Date range:", min(all_dates), "to", max(all_dates), "\n")

scrapes_daily <- all_dates

# Create area data
rba_historical <- read_rba(series_id = "FIRMMCRTD") %>%
  arrange(date)

all_list_area <- map(all_dates, function(scr_date) {
  historical_rate <- rba_historical %>%
    filter(date <= scr_date) %>%
    slice_max(date, n = 1, with_ties = FALSE) %>%
    pull(value)
  
  initial_rt_at_scrape <- if(length(historical_rate) > 0) historical_rate else latest_rt
  
  df_rates <- cash_rate_daily %>% 
    filter(scrape_date == scr_date) %>%
    select(expiry = date, forecast_rate = cash_rate, scrape_date)
  
  df <- meeting_schedule %>%
    distinct(expiry, meeting_date) %>%
    mutate(scrape_date = scr_date) %>%
    left_join(df_rates, by = "expiry") %>%
    arrange(expiry) %>%
    filter(!is.na(forecast_rate))
  
  if (nrow(df) == 0) return(NULL)
  
  prev_implied <- NA_real_
  out <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    rt_in <- if (is.na(prev_implied)) initial_rt_at_scrape else prev_implied
    
    r_tp1 <- if (row$meeting_date < row$expiry) {
      row$forecast_rate
    } else {
      nb <- (day(row$meeting_date)-1) / days_in_month(row$expiry)
      na <- 1 - nb
      (row$forecast_rate - rt_in * nb) / na
    }
    
    out[[i]] <- tibble(
      scrape_date = scr_date,
      meeting_date = row$meeting_date,
      implied_mean = r_tp1,
      days_to_meeting = as.integer(row$meeting_date - scr_date),
      previous_rate = rt_in
    )
    prev_implied <- r_tp1
  }
  bind_rows(out)
})

all_estimates_area <- all_list_area %>%
  compact() %>%
  bind_rows() %>%
  filter(days_to_meeting >= 0) %>%
  left_join(rmse_days, by = "days_to_meeting") %>%
  rename(stdev = finalrmse)

max_rmse <- suppressWarnings(max(rmse_days$finalrmse, na.rm = TRUE))
if (!is.finite(max_rmse)) stop("No finite RMSE values found")

bad_sd <- !is.finite(all_estimates_area$stdev) | is.na(all_estimates_area$stdev) | all_estimates_area$stdev <= 0
n_bad <- sum(bad_sd, na.rm = TRUE)

if (n_bad > 0) {
  message(sprintf("Replacing %d missing/invalid stdev(s) with max RMSE = %.4f", n_bad, max_rmse))
  all_estimates_area$stdev[bad_sd] <- max_rmse
}

# Extended range bucketing
bp_span <- 300L
step_bp <- 25L
sd_fallback <- suppressWarnings(stats::median(all_estimates_area$stdev[is.finite(all_estimates_area$stdev)], na.rm = TRUE))
if (!is.finite(sd_fallback) || sd_fallback <= 0) sd_fallback <- 0.01

current_center_ext <- current_rate
bucket_min <- 0.1
bucket_max <- 6.1
bucket_centers_ext <- seq(bucket_min, bucket_max, by = 0.25)
half_width_ext <- 0.125

cat("Creating extended buckets for", nrow(all_estimates_area), "estimate rows\n")

bucket_list_ext <- vector("list", nrow(all_estimates_area))
for (i in seq_len(nrow(all_estimates_area))) {
  mu_i <- all_estimates_area$implied_mean[i]
  sigma_i <- all_estimates_area$stdev[i]
  d_i <- all_estimates_area$days_to_meeting[i]

  if (!is.finite(mu_i)) next
  if (!is.finite(sigma_i) || sigma_i <= 0) sigma_i <- sd_fallback

  p_vec <- sapply(bucket_centers_ext, function(b) {
    lower <- b - half_width_ext
    upper <- b + half_width_ext
    pnorm(upper, mean = mu_i, sd = sigma_i) - pnorm(lower, mean = mu_i, sd = sigma_i)
  })

  p_vec[!is.finite(p_vec) | p_vec < 0] <- 0
  p_vec[p_vec < 0.01] <- 0
  s <- sum(p_vec, na.rm = TRUE)
  if (is.finite(s) && s > 0) {
    p_vec <- p_vec / s
  } else {
    p_vec[] <- 0
  }

  nearest <- order(abs(bucket_centers_ext - mu_i))[1:2]
  b1 <- min(bucket_centers_ext[nearest])
  b2 <- max(bucket_centers_ext[nearest])
  denom <- (b2 - b1)
  w2 <- if (denom > 0) (mu_i - b1) / denom else 0
  w2 <- min(max(w2, 0), 1)
  l_vec <- numeric(length(bucket_centers_ext))
  l_vec[which(bucket_centers_ext == b1)] <- 1 - w2
  l_vec[which(bucket_centers_ext == b2)] <- w2

  blend <- blend_weight(d_i)
  v <- blend * l_vec + (1 - blend) * p_vec

  bucket_list_ext[[i]] <- tibble::tibble(
    scrape_date = all_estimates_area$scrape_date[i],
    meeting_date = all_estimates_area$meeting_date[i],
    implied_mean = mu_i,
    stdev = sigma_i,
    days_to_meeting = d_i,
    bucket = bucket_centers_ext,
    probability = v
  )
}

all_estimates_buckets_ext <- dplyr::bind_rows(bucket_list_ext) %>%
  dplyr::mutate(
    diff_bps = as.integer(round((bucket - current_center_ext) * 100L)),
    diff_bps = pmax(pmin(diff_bps, bp_span), -bp_span),
    move = sprintf("%.2f%%", bucket)
  )

rate_levels <- sort(unique(all_estimates_buckets_ext$bucket))
rate_labels <- sprintf("%.2f%%", rate_levels)

all_estimates_buckets_ext <- all_estimates_buckets_ext %>%
  dplyr::mutate(move = factor(move, levels = rate_labels))

future_meetings_all <- meeting_schedule %>%
  dplyr::mutate(meeting_date = as.Date(meeting_date)) %>%
  dplyr::pull(meeting_date)

cat("Future meetings found:", length(future_meetings_all), "\n")

fmt_date <- function(x) format(as.Date(x), "%d %B %Y")
fmt_file <- function(x) format(as.Date(x), "%Y-%m-%d")

# =============================================
# AREA PLOTS WITH DYNAMIC CENTER RATE
# =============================================

for (mt in future_meetings_all) {
  cat("\n=== Processing meeting:", as.character(as.Date(mt)), "===\n")
  
  df_mt <- all_estimates_buckets_ext %>%
    dplyr::filter(as.Date(meeting_date) == as.Date(mt)) %>%
    dplyr::group_by(scrape_date, move) %>%
    dplyr::summarise(probability = sum(probability, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(scrape_date, move, fill = list(probability = 0)) %>%
    dplyr::arrange(scrape_date, move)
  
  if (nrow(df_mt) == 0) {
    cat("Skipping - no data for meeting\n")
    next 
  }
  
  df_mt <- df_mt %>%
    dplyr::filter(
      !is.na(scrape_date), !is.na(probability),
      is.finite(probability), probability >= 0, !is.na(move)
    ) %>%
    dplyr::mutate(
      probability = pmin(probability, 1.0),
      probability = pmax(probability, 0.0)
    )
  
  if (nrow(df_mt) == 0) next
  
  meeting_date_proper <- as.Date(mt) - days(1)
  start_xlim_mt <- min(df_mt$scrape_date, na.rm = TRUE)
  end_xlim_mt <- meeting_date_proper
  
  available_moves <- unique(df_mt$move[!is.na(df_mt$move)])
  valid_move_levels <- rate_labels[rate_labels %in% available_moves]
  
  df_mt <- df_mt %>%
    dplyr::filter(move %in% valid_move_levels) %>%
    dplyr::mutate(move = factor(move, levels = rev(valid_move_levels))) %>%
    dplyr::filter(!is.na(move))
  
  if (nrow(df_mt) == 0) next
  
  actual_outcome <- NULL
  is_past_meeting <- meeting_date_proper < Sys.Date()
  
  if (is_past_meeting) {
    actual_outcome_data <- rba_historical %>%
      dplyr::filter(date > meeting_date_proper+2) %>%
      dplyr::slice_min(date, n = 1, with_ties = FALSE)
    
    if (nrow(actual_outcome_data) > 0) {
      actual_outcome <- actual_outcome_data$value
      cat("Actual outcome:", actual_outcome, "%\n")
    }
  }
  
  center_rate <- if (!is.null(actual_outcome)) actual_outcome else current_rate
  cat("Center rate for this meeting:", center_rate, "%\n")
  
  # Create fill map dynamically
  all_rates <- sort(unique(all_estimates_buckets_ext$bucket))
  fill_map <- setNames(character(length(all_rates)), sprintf("%.2f%%", all_rates))
  
  for (i in seq_along(all_rates)) {
    rate <- all_rates[i]
    diff_from_center <- rate - center_rate
    
    if (abs(diff_from_center) < 0.01) {
      fill_map[i] <- "#BFBFBF"
    } else if (diff_from_center < 0) {
      bp_below <- abs(diff_from_center) * 100
      fill_map[i] <- if (bp_below >= 125) "#000080" else
                     if (bp_below >= 100) "#0033A0" else
                     if (bp_below >= 75) "#004B8E" else
                     if (bp_below >= 50) "#1A5CB0" else "#5FA4D4"
    } else {
      bp_above <- diff_from_center * 100
      fill_map[i] <- if (bp_above >= 125) "#800000" else
                     if (bp_above >= 100) "#A00000" else
                     if (bp_above >= 75) "#B50000" else
                     if (bp_above >= 49) "#C71010" else "#E07C7C"
    }
  }
  
  legend_min <- center_rate - 1.50
  legend_max <- center_rate + 1.50
  legend_rates <- all_rates[all_rates >= legend_min & all_rates <= legend_max]
  if (center_rate %in% all_rates && !(center_rate %in% legend_rates)) {
    legend_rates <- sort(c(legend_rates, center_rate))
  }
  legend_breaks <- sprintf("%.2f%%", legend_rates)
  
  df_mt_highlight <- NULL
  if (!is.null(actual_outcome)) {
    actual_outcome_label <- sprintf("%.2f%%", actual_outcome)
    outcome_exists <- actual_outcome_label %in% df_mt$move
    
    if (outcome_exists) {
      df_mt_highlight <- df_mt %>%
        dplyr::arrange(scrape_date, desc(move)) %>%
        dplyr::group_by(scrape_date) %>%
        dplyr::mutate(
          cumulative_prob = cumsum(probability),
          lower_bound = cumulative_prob - probability,
          is_actual = (move == actual_outcome_label)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(is_actual, probability > 0)
    }
  }
  
  rba_meetings_in_range <- meeting_schedule %>%
    dplyr::filter(meeting_date > start_xlim_mt, meeting_date < meeting_date_proper)
  
  abs_releases_in_range <- abs_releases %>%
    dplyr::mutate(release_date = as.Date(datetime)) %>%
    dplyr::filter(release_date > start_xlim_mt, release_date < meeting_date_proper)
  
  filename <- paste0("docs/meetings/daily_area_", fmt_file(meeting_date_proper), ".png")
  
  tryCatch({
    start_month <- lubridate::floor_date(start_xlim_mt, "month")
    end_month <- lubridate::ceiling_date(end_xlim_mt, "month")
    breaks_vec <- seq.Date(from = start_month, to = end_month, by = "month")
    date_labels <- function(x) format(x, "%b-%Y")
    
    area_mt <- ggplot2::ggplot(df_mt, ggplot2::aes(x = scrape_date, y = probability, fill = move)) +
      ggplot2::geom_area(position = "stack", alpha = 0.95, colour = NA)
    
    # Add highlight pattern for actual outcome if it exists
    if (!is.null(actual_outcome) && !is.null(df_mt_highlight) && nrow(df_mt_highlight) > 0) {
      area_mt <- area_mt +
        ggpattern::geom_ribbon_pattern(
          data = df_mt_highlight,
          aes(x = scrape_date, ymin = lower_bound, ymax = cumulative_prob),
          pattern = "stripe",
          pattern_fill = "gold",
          pattern_color = "gold",
          pattern_density = 0.15,
          pattern_spacing = 0.015,
          pattern_angle = 45,
          fill = "gold",
          alpha = 0.3,
          color = NA,
          inherit.aes = FALSE
        )
    }
    
    area_mt <- area_mt +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", 
                         color = "grey40", linewidth = 0.7, alpha = 0.8)
    
    # Add RBA meeting lines if they exist
    if (nrow(rba_meetings_in_range) > 0) {
      area_mt <- area_mt +
        ggplot2::geom_vline(
          data = rba_meetings_in_range,
          aes(xintercept = as.numeric(meeting_date)),
          linetype = "solid", color = "white", 
          linewidth = 0.6, alpha = 0.6
        )
    }
    
    area_mt <- area_mt +
      ggplot2::scale_fill_manual(
        values = fill_map,
        breaks = legend_breaks,
        drop = FALSE,
        name = "Cash Rate",
        guide = ggplot2::guide_legend(override.aes = list(alpha = 1))
      ) +
      ggplot2::scale_x_date(
        limits = c(start_xlim_mt, end_xlim_mt),
        breaks = breaks_vec,
        labels = date_labels,
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent_format(accuracy = 1),
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        title = paste("Cash Rate Scenarios for Meeting on", fmt_date(meeting_date_proper)),
        subtitle = if(!is.null(actual_outcome)) {
          paste0("Daily probability distribution | Actual outcome: <span style='color:gold;'>**", 
                 sprintf("%.2f%%", actual_outcome), "**</span>")
        } else {
          "Daily probability distribution"
        },
        x = "Date", 
        y = "Probability"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.title.x = ggplot2::element_text(size = 14),
        axis.title.y = ggplot2::element_text(size = 14),
        plot.subtitle = ggtext::element_markdown(),
        legend.position = "right",
        legend.title = ggplot2::element_blank()
      )
    
    temp_filename <- paste0(filename, ".tmp")
    ggplot2::ggsave(filename = temp_filename, plot = area_mt, width = 12, height = 5, dpi = 300, device = "png")

    if (file.exists(temp_filename)) {
      file.rename(temp_filename, filename)
      cat("✓ Saved from temp file:", filename, "\n")
    } else {
      cat("✗ Temp file was not created\n")
    }

  }, error = function(e) {
    cat("✗ Error:", e$message, "\n")
    cat("↪ Retrying with simplified plot (geom_area only) to bypass rendering issue...\n")

    safe_plot <- ggplot2::ggplot(
      df_mt,
      ggplot2::aes(x = scrape_date, y = probability, fill = move)
    ) +
      ggplot2::geom_area(position = "stack", colour = NA) +
      ggplot2::scale_fill_manual(
        values = fill_map,
        breaks = legend_breaks,
        drop = FALSE,
        name = "Cash Rate",
        guide = ggplot2::guide_legend(override.aes = list(alpha = 1))
      ) +
      ggplot2::scale_x_date(
        limits = c(start_xlim_mt, end_xlim_mt),
        breaks = breaks_vec,
        labels = date_labels,
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent_format(accuracy = 1),
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        title = paste("Cash Rate Scenarios for Meeting on", fmt_date(meeting_date_proper)),
        subtitle = "Daily probability distribution (simplified rendering)",
        x = "Date",
        y = "Probability"
      ) +
      ggplot2::theme_bw()

    tryCatch({
      ggplot2::ggsave(filename = temp_filename, plot = safe_plot, width = 12, height = 5, dpi = 300, device = "png")

      if (file.exists(temp_filename)) {
        file.rename(temp_filename, filename)
        cat("✓ Simplified plot saved:", filename, "\n")
      } else {
        cat("✗ Simplified temp file was not created\n")
      }
    }, error = function(e2) {
      cat("✗ Simplified plot also failed:", e2$message, "\n")
    })
  })
}

# CSV exports
for (mt in future_meetings_all) {
  df_mt_csv <- all_estimates_buckets_ext %>%
    dplyr::filter(as.Date(meeting_date) == as.Date(mt)) %>%
    dplyr::group_by(scrape_date, move, diff_bps, bucket) %>%
    dplyr::summarise(probability = sum(probability, na.rm = TRUE), .groups = "drop") %>%
    tidyr::complete(scrape_date, move, fill = list(probability = 0)) %>%
    dplyr::arrange(scrape_date, diff_bps) %>%
    dplyr::mutate(meeting_date = as.Date(mt), bucket_rate = bucket) %>%
    dplyr::select(meeting_date, scrape_date, move, diff_bps, bucket_rate, probability)
  
  if (nrow(df_mt_csv) == 0) next
  
  meeting_date_proper <- as.Date(mt)
  csv_filename <- paste0("docs/meetings/csv/daily_area_data_", fmt_file(meeting_date_proper), ".csv")
  
  tryCatch({
    write.csv(df_mt_csv, csv_filename, row.names = FALSE)
    cat("CSV exported:", csv_filename, "\n")
  }, error = function(e) {
    cat("Error exporting CSV:", e$message, "\n")
  })
}

# Combined CSV export
combined_csv <- all_estimates_buckets_ext %>%
  dplyr::filter(as.Date(meeting_date) %in% future_meetings_all) %>%
  dplyr::group_by(meeting_date, scrape_date, move, diff_bps, bucket) %>%
  dplyr::summarise(probability = sum(probability, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(meeting_date, scrape_date, diff_bps) %>%
  dplyr::mutate(meeting_date = as.Date(meeting_date), bucket_rate = bucket) %>%
  dplyr::select(meeting_date, scrape_date, move, diff_bps, bucket_rate, probability)

if (nrow(combined_csv) > 0) {
  tryCatch({
    write.csv(combined_csv, "docs/meetings/csv/daily_all_meetings_area_data.csv", row.names = FALSE)
    cat("Combined CSV exported: docs/meetings/csv/daily_all_meetings_area_data.csv\n")
  }, error = function(e) {
    cat("Error exporting combined CSV:", e$message, "\n")
  })
}

