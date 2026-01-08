# ==============================================================================
# RBA CASH RATE PROBABILITY ANALYSIS
# ==============================================================================
# Purpose: Analyze and visualize RBA cash rate futures data to calculate
#          probabilities of different rate outcomes for upcoming meetings
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARY LOADING
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(tidyr)
  library(plotly)
  library(readrba)   # For RBA data access
  library(scales)
  library(glue)
})

# ------------------------------------------------------------------------------
# 2. DATA LOADING & CONFIGURATION
# ------------------------------------------------------------------------------

# Load cash rate futures data (columns: date, cash_rate, scrape_time)
cash_rate <- readRDS("combined_data/all_data.Rds")

# Load RMSE lookup table (days_to_meeting → forecast error)
load("combined_data/rmse_new.RData")

print(head(rmse_days$finalrmse, 30))

# Configuration parameters
spread <- 0.00  # Spread adjustment for cash rate
override <- 3.60  # Manual override for current rate (if needed)

# Apply spread adjustment
cash_rate$cash_rate <- cash_rate$cash_rate + spread

# Create output directory structure
if (!dir.exists("docs/meetings")) dir.create("docs/meetings", recursive = TRUE)

hours_tz <- 11


# ------------------------------------------------------------------------------
# 3. HELPER FUNCTIONS
# ------------------------------------------------------------------------------

# Linear blend weight: transitions from discrete (0) to probabilistic (1)
# over the last 30 days before a meeting
blend_weight <- function(days_to_meeting) {
  pmax(0, pmin(1, 1 - days_to_meeting / 30))
}

# ------------------------------------------------------------------------------
# 4. DEFINE RBA MEETING SCHEDULE
# ------------------------------------------------------------------------------

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
    # Determine futures contract expiry month
    # If meeting is in last 1-2 days of month, contract expires next month
    expiry = if_else(
      day(meeting_date) >= days_in_month(meeting_date) - 1,
      ceiling_date(meeting_date, "month"),
      floor_date(meeting_date, "month")
    )
  ) %>% 
  select(expiry, meeting_date)

now_melb <- now(tzone = "Australia/Melbourne")
today_melb <- as.Date(now_melb)
cutoff <- ymd_hm(paste0(today_melb, " 14:30"), tz = "Australia/Melbourne")

next_meeting <- if (today_melb %in% meeting_schedule$meeting_date) {
  if (now_melb < cutoff) {
    today_melb
  } else {
    meeting_schedule %>%
      filter(meeting_date > today_melb) %>%
      slice_min(meeting_date) %>%
      pull(meeting_date)
  }
} else {
  meeting_schedule %>%
    filter(meeting_date > today_melb) %>%
    slice_min(meeting_date) %>%
    pull(meeting_date)
}

print(paste("Next meeting:", next_meeting))

# ------------------------------------------------------------------------------
# 5. DEFINE ABS DATA RELEASE SCHEDULE
# ------------------------------------------------------------------------------

abs_releases <- tribble(
  ~dataset, ~datetime,
  
  # CPI (quarterly releases at 11:30 AM AEST)
  "CPI", ymd_hm("2025-01-29 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-04-30 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-07-30 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-10-29 11:30", tz = "Australia/Melbourne"),

  
  # CPI (monthly releases)
  "CPI Indicator", ymd_hm("2025-01-29 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-02-26 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-03-26 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-04-30 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-05-28 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-06-25 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-07-30 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-08-27 11:30", tz = "Australia/Melbourne"),
  "CPI Indicator", ymd_hm("2025-09-24 11:30", tz = "Australia/Melbourne"),
  
"CPI", ymd_hm("2025-11-26 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-01-07 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-01-28 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-02-25 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-03-25 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-04-29 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-05-27 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-06-24 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-07-29 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-08-26 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-09-30 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-10-28 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-11-25 11:30", tz = "Australia/Melbourne"),
"CPI", ymd_hm("2026-12-30 11:30", tz = "Australia/Melbourne"),
  
  # WPI (quarterly releases)
  "WPI", ymd_hm("2025-02-19 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2025-05-14 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2025-08-13 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2025-11-19 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2026-02-18 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2026-05-13 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2026-08-12 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2026-11-17 11:30", tz = "Australia/Melbourne"),
  "WPI", ymd_hm("2027-02-17 11:30", tz = "Australia/Melbourne"),
  
  # National Accounts (quarterly releases)
  "National Accounts", ymd_hm("2025-03-05 11:30", tz = "Australia/Melbourne"),
  "National Accounts", ymd_hm("2025-06-04 11:30", tz = "Australia/Melbourne"),
  "National Accounts", ymd_hm("2025-09-03 11:30", tz = "Australia/Melbourne"),
  "National Accounts", ymd_hm("2025-12-03 11:30", tz = "Australia/Melbourne"),
  "National Accounts", ymd_hm("2026-03-04 11:30", tz = "Australia/Melbourne"),
  "National Accounts", ymd_hm("2026-06-03 11:30", tz = "Australia/Melbourne"),
  "National Accounts", ymd_hm("2026-09-02 11:30", tz = "Australia/Melbourne"),
  "National Accounts", ymd_hm("2026-12-02 11:30", tz = "Australia/Melbourne"),
  
  # Labour Force (monthly releases)
  "Labour Force", ymd_hm("2025-01-16 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-02-20 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-03-20 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-04-17 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-05-15 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-06-19 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-07-17 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-08-14 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-09-18 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-10-16 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-11-13 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2025-12-11 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-01-22 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-02-19 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-03-19 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-04-23 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-05-14 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-06-18 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-07-16 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-08-20 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-09-17 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-10-15 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-11-19 11:30", tz = "Australia/Melbourne"),
  "Labour Force", ymd_hm("2026-12-17 11:30", tz = "Australia/Melbourne")
)

# ------------------------------------------------------------------------------
# 6. DETERMINE CURRENT RATE & MEETING CONTEXT
# ------------------------------------------------------------------------------

# Find the most recent past meeting
last_meeting <- max(meeting_schedule$meeting_date[
  meeting_schedule$meeting_date <= Sys.Date()
])

# Determine whether to use manual override or live RBA data
use_override <- !is.null(override) && (Sys.Date() - last_meeting <= 1)

# Get current rate (either from override or latest RBA publication)
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

# Print diagnostics
print(paste("Last meeting:", last_meeting))
print(paste("Using override:", use_override))
print(paste("Initial rate:", initial_rt))

# ------------------------------------------------------------------------------
# 7. FILTER SCRAPES BASED ON TIME CUTOFF
# ------------------------------------------------------------------------------

# Determine which scrapes to include based on 2:30 PM AEST cutoff
now_melb <- now(tzone = "Australia/Melbourne")
cutoff_time <- ymd_hm(paste0(Sys.Date(), " 14:30"), tz = "Australia/Melbourne")

# If before 2:30 PM, include yesterday's data; otherwise only today's
cutoff_date <- if (now_melb < cutoff_time) {
  Sys.Date() - 1
} else {
  Sys.Date()
}

print(paste("Current time (Melbourne):", now_melb))
print(paste("Cutoff time:", cutoff_time))

# Get all scrape times and filter based on cutoff
all_times <- sort(unique(cash_rate$scrape_time))
scrapes <- all_times[all_times >= cutoff_date | all_times > last_meeting]

print("Latest scrapes:")
print(tail(scrapes))

# ------------------------------------------------------------------------------
# 8. CALCULATE IMPLIED MEAN RATES FOR EACH SCRAPE × MEETING
# ------------------------------------------------------------------------------

# For each scrape time, calculate the implied rate for each future meeting
all_list <- map(scrapes, function(scr) {
  
  scr_date <- as.Date(scr)
  
  # Get the most recent futures price for each contract expiry at this scrape
  df_rates <- cash_rate %>% 
    filter(scrape_time == scr) %>%
    select(
      expiry = date,
      forecast_rate = cash_rate,
      scrape_time
    )
  
  # Join futures prices to meeting schedule
  df <- meeting_schedule %>%
    distinct(expiry, meeting_date) %>%
    mutate(scrape_time = scr) %>%
    left_join(df_rates, by = "expiry") %>%
    arrange(expiry)
  
  # Remove contracts with no price data yet
  df <- df %>% filter(!is.na(forecast_rate))
  if (nrow(df) == 0) return(NULL)
  
  # Calculate implied rates iteratively
  prev_implied <- NA_real_
  out <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    # Starting rate is either initial rate or previous meeting's implied rate
    rt_in <- if (is.na(prev_implied)) initial_rt else prev_implied
    
    # Calculate implied rate for this meeting
    # If meeting is before contract expiry, use contract rate directly
    # Otherwise, decompose the weighted average
    r_tp1 <- if (row$meeting_date < row$expiry) {
      row$forecast_rate
    } else {
      # Days before meeting / total days in month
      nb <- (day(row$meeting_date) - 1) / days_in_month(row$expiry)
      na <- 1 - nb
      # Solve for rate after meeting: (contract_rate - rt_in * nb) / na
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

# Combine all scrapes and add forecast uncertainty (RMSE)
all_estimates <- all_list %>%
  compact() %>%
  bind_rows() %>%
  filter(days_to_meeting >= 0) %>%
  left_join(rmse_days, by = "days_to_meeting") %>%
  rename(stdev = finalrmse)

# Handle missing or invalid standard deviations
max_rmse <- suppressWarnings(max(rmse_days$finalrmse, na.rm = TRUE))
if (!is.finite(max_rmse)) {
  stop("No finite RMSE values found in rmse_days$finalrmse")
}

bad_sd <- !is.finite(all_estimates$stdev) | 
          is.na(all_estimates$stdev) | 
          all_estimates$stdev <= 0
n_bad <- sum(bad_sd, na.rm = TRUE)

if (n_bad > 0) {
  message(sprintf(
    "Replacing %d missing/invalid stdev(s) with max RMSE = %.4f",
    n_bad, max_rmse
  ))
  all_estimates$stdev[bad_sd] <- max_rmse
}

# Display sample of estimates
all_estimates %>% filter(meeting_date == next_meeting) %>% tail(100) %>% print(n = Inf, width = Inf)

# ------------------------------------------------------------------------------
# 9. CALCULATE PROBABILITIES FOR EACH RATE BUCKET
# ------------------------------------------------------------------------------

# Define rate buckets (0.10% to 6.10% in 0.25% increments)
bucket_centers <- seq(0.10, 6.10, by = 0.25)
half_width <- 0.125  # Each bucket spans ±0.125% around center

# Find the bucket closest to current rate (for "no change")
current_center <- bucket_centers[which.min(abs(bucket_centers - current_rate))]

# Calculate probabilities for each estimate × bucket combination
bucket_list <- vector("list", nrow(all_estimates))

for (i in seq_len(nrow(all_estimates))) {
  mu_i <- all_estimates$implied_mean[i]
  sigma_i <- all_estimates$stdev[i]
  d_i <- all_estimates$days_to_meeting[i]
  
  # METHOD 1: Probabilistic (uses normal distribution)
  p_vec <- sapply(bucket_centers, function(b) {
    lower <- b - half_width
    upper <- b + half_width
    pnorm(upper, mean = mu_i, sd = sigma_i) - 
      pnorm(lower, mean = mu_i, sd = sigma_i)
  })
  p_vec[p_vec < 0] <- 0
  p_vec[p_vec < 0.01] <- 0  # Remove noise
  p_vec <- p_vec / sum(p_vec)  # Normalize
  
  # METHOD 2: Linear interpolation (for near-term precision)
  nearest <- order(abs(bucket_centers - mu_i))[1:2]
  b1 <- min(bucket_centers[nearest])
  b2 <- max(bucket_centers[nearest])
  w2 <- (mu_i - b1) / (b2 - b1)  # Interpolation weight
  
  l_vec <- numeric(length(bucket_centers))
  l_vec[bucket_centers == b1] <- 1 - w2
  l_vec[bucket_centers == b2] <- w2
  
  # BLEND: Linear method near meeting, probabilistic method far out
  blend <- blend_weight(d_i)
  v <- blend * l_vec + (1 - blend) * p_vec
  
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
    diff = bucket_centers - current_rate,
    diff_s = sign(bucket_centers - current_rate) * 
             abs(bucket_centers - current_rate)^(1/4)  # For color scaling
  )
}

all_estimates_buckets <- bind_rows(bucket_list)

# ------------------------------------------------------------------------------
# 10. CREATE BAR CHARTS FOR EACH FUTURE MEETING
# ------------------------------------------------------------------------------

# Identify future meetings and latest scrape
future_meetings <- meeting_schedule$meeting_date[
  meeting_schedule$meeting_date > Sys.Date()
]
latest_scrape <- max(all_estimates_buckets$scrape_time) + hours(hours_tz)

print(paste("Latest scrape:", latest_scrape))

# Generate a bar chart for each upcoming meeting
for (mt in future_meetings) {
  
  # Filter data for this specific meeting and latest scrape
  bar_df <- all_estimates_buckets %>%
    filter(
      scrape_time == latest_scrape,
      meeting_date == mt
    )
  
  # Create bar chart with color gradient (blue=cut, gray=hold, red=hike)
  p <- ggplot(bar_df, aes(x = factor(bucket), y = probability, fill = diff_s)) +
    geom_col(show.legend = FALSE) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_gradient2(
      midpoint = 0,
      low = "#0022FF",      # Blue for cuts
      mid = "#B3B3B3",      # Gray for no change
      high = "#FF2200",     # Red for hikes
      limits = range(bar_df$diff_s, na.rm = TRUE)
    ) +
    labs(
      title = paste("Cash Rate Outcome Probabilities —", 
                   format(as.Date(mt), "%d %B %Y")),
      subtitle = paste(
        "As of", 
        format(
          with_tz(as.POSIXct(latest_scrape) + hours(hours_tz), 
                 tzone = "Australia/Sydney"),
          "%d %B %Y, %I:%M %p AEST"
        )
      ),
      x = "Target Rate (%)",
      y = "Probability (%)"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  meeting_plot_dir <- file.path("docs", "plots", "meeting_probabilities")
  if (!dir.exists(meeting_plot_dir)) {
    dir.create(meeting_plot_dir, recursive = TRUE)
  }

  # Save chart
  ggsave(
    filename = file.path(
      meeting_plot_dir,
      paste0("rate_probabilities_", gsub(" ", "_", mt), ".png")
    ),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300,
    device = "png"
  )
}



# ------------------------------------------------------------------------------
# 12. PREPARE DATA FOR LINE CHART (TOP 3-4 OUTCOMES)
# ------------------------------------------------------------------------------

print(all_estimates_buckets %>% filter(meeting_date == next_meeting))

# First, create the move labels for ALL buckets
buckets_with_moves <- all_estimates_buckets %>%
  filter(meeting_date == next_meeting) %>%
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
      TRUE ~ sprintf("%+.0f bp", round(diff_center * 100))
    )
  ) %>%
  filter(!is.na(move))  # Remove any buckets that couldn't be labeled

# NOW identify the top 3-4 most probable outcomes (from labeled buckets only)
top3_buckets <- buckets_with_moves %>% 
  group_by(bucket, move) %>%
  summarise(probability = mean(probability, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = probability, n = 4, with_ties = FALSE) %>% 
  pull(bucket)

# Filter to top outcomes
top3_df <- buckets_with_moves %>%
  filter(bucket %in% top3_buckets) %>%
  mutate(
    move = factor(
      move,
      levels = c("-75 bp cut", "-50 bp cut", "-25 bp cut", "No change",
                 "+25 bp hike", "+50 bp hike", "+75 bp hike")
    )
  ) %>%
  select(-diff_center)

# Verify we have data
if (nrow(top3_df) == 0) {
  stop("No valid data in top3_df. Check bucket matching logic.")
}

# Set x-axis limits for line chart
start_xlim <- max(
  min(top3_df$scrape_time) + hours(hours_tz),
  as.POSIXct(today_melb %m-% months(12), tz = "Australia/Melbourne") + hours(hours_tz)
)
end_xlim <- as.POSIXct(next_meeting, tz = "Australia/Melbourne") + hours(hours_tz+7)

# ==============================================================================
# Save summary data instead of HTML
# ==============================================================================
# Get top 3 probabilities for latest scrape
top3_summary <- top3_df %>%
  filter(scrape_time == latest_scrape) %>%
  group_by(move) %>%
  summarise(probability = mean(probability, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(probability)) %>%
  slice_head(n = 3)

# Create summary data object
rba_summary_data <- list(
  scrape_date = as.Date(latest_scrape + hours(hours_tz)),
  next_meeting = next_meeting,
  top3_moves = top3_summary$move,
  top3_probabilities = top3_summary$probability
)

# Save as RDS file
saveRDS(rba_summary_data, "docs/rba_summary_data.rds")
cat("\nProbability summary data saved to: docs/rba_summary_data.rds\n")


# ------------------------------------------------------------------------------
# 13. CREATE LINE CHART SHOWING PROBABILITY EVOLUTION
# ------------------------------------------------------------------------------

# Define colour palettes for moves and ABS releases
move_colors <- c(
  "-75 bp cut" = "#000080",
  "-50 bp cut" = "#004B8E",
  "-25 bp cut" = "#5FA4D4",
  "No change" = "#BFBFBF",
  "+25 bp hike" = "#E07C7C",
  "+50 bp hike" = "#B50000",
  "+75 bp hike" = "#800000"
)

abs_colors <- c(
  "CPI" = "#FF6B6B",
  "CPI Indicator" = "#4ECDC4",
  "WPI" = "#45B7D1",
  "National Accounts" = "#FFA726",
  "Labour Force" = "#AB47BC"
)

combined_colors <- c(move_colors, abs_colors)

# Prepare data for alternate model comparison
top3_df_long <- top3_df %>%
  select(scrape_time, move, probability, probability_linear) %>%
  pivot_longer(
    cols = c(probability, probability_linear),
    names_to = "model",
    values_to = "probability_value"
  ) %>%
  drop_na(probability_value) %>%
  mutate(
    model = recode(
      model,
      probability = "Standard probability model",
      probability_linear = "Two-outcome linear model"
    ),
    model = factor(
      model,
      levels = c("Standard probability model", "Two-outcome linear model")
    )
  )


std_df <- top3_df_long %>%
  filter(model == "Standard probability model")

lin_df <- top3_df_long %>%
  filter(model == "Two-outcome linear model")


# Create static line plot
line <- ggplot(top3_df, aes(x = scrape_time + hours(hours_tz),
                            y = probability,
                            colour = move,
                            group = move)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(
    values = combined_colors,
    name = ""
  ) +
  scale_x_datetime(
    limits = c(start_xlim, end_xlim),
    date_breaks = "2 day",
    date_labels = "%d %b",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = glue("Cash-Rate Moves for the Next Meeting on {format(next_meeting, '%d %b %Y')}"),
    subtitle = glue("as of {format(as.Date(latest_scrape + hours(hours_tz)), '%d %b %Y')}"),
    x = "Forecast date",
    y = "Probability"
  ) +
  # Add vertical lines for ABS data releases
  geom_vline(
    data = abs_releases,
    aes(xintercept = datetime, colour = dataset),
    linetype = "dashed",
    alpha = 0.8
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "right",
    legend.title = element_blank()
  )

line_plot_dir <- file.path("docs", "plots", "line_charts")
if (!dir.exists(line_plot_dir)) {
  dir.create(line_plot_dir, recursive = TRUE)
}

# Save static plot (two versions)
ggsave(file.path(line_plot_dir, "line.png"), line, width = 10, height = 5, dpi = 300)
ggsave(
  file.path(
    line_plot_dir,
    glue("line_{format(next_meeting, '%d %b %Y')}.png")
  ), 
  line, 
  width = 10, 
  height = 5, 
  dpi = 300
)

print(top3_df, n = 50)


line_dual <- ggplot() +
  # Standard probability model (solid)
  geom_line(
    data = std_df,
    aes(
      x = scrape_time + hours(hours_tz),
      y = probability_value,
      colour = move,
      group = move,
      linetype = "Standard probabilities"
    ),
    linewidth = 1.1
  ) +
  # Two-outcome linear model (dotted)
  geom_line(
    data = lin_df,
    aes(
      x = scrape_time + hours(hours_tz),
      y = probability_value,
      colour = move,
      group = move,
      linetype = "Linear probability model"
    ),
    linewidth = 1.1
  ) +
  scale_colour_manual(
    values = combined_colors,
    name = ""
  ) +
  scale_linetype_manual(
    values = c(
      "Standard probabilities" = "solid",
      "Linear probability model" = "dotted"
    ),
    name = "Model"
  ) +
  scale_x_datetime(
    limits = c(start_xlim, end_xlim),
    date_breaks = "2 day",
    date_labels = "%d %b",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = glue("Cash-Rate Moves for the Next Meeting on {format(next_meeting, '%d %b %Y')}"),
    subtitle = glue(
      "Comparing standard probabilities with two-outcome linear model as of {format(as.Date(latest_scrape + hours(hours_tz)), '%d %b %Y')}"
    ),
    x = "Forecast date",
    y = "Probability"
  ) +
  geom_vline(
    data = abs_releases,
    aes(xintercept = datetime, colour = dataset),
    linetype = "dashed",
    alpha = 0.8
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "right"
  )

ggsave(file.path(line_plot_dir, "line_dual.png"), line_dual, width = 10, height = 5, dpi = 300)
ggsave(
  file.path(
    line_plot_dir,
    glue("line_dual_{format(next_meeting, '%d %b %Y')}.png")
  ),
  line_dual,
  width = 10,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 14. CREATE INTERACTIVE VERSION WITH PROPER VERTICAL LINES
# ------------------------------------------------------------------------------

# Create base plot WITHOUT any vertical lines
meeting_label <- format(next_meeting, "%d %b %Y")

line_int_base <- ggplot(top3_df, aes(x = scrape_time + hours(hours_tz),
                                      y = probability,
                                      colour = move,
                                      group = move)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(
    values = combined_colors,
    name = ""
  ) +
  scale_x_datetime(
    limits = c(start_xlim, end_xlim),
    date_breaks = "2 day",
    date_labels = "%d %b",
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  labs(
    title = glue("Cash-Rate Moves for the Next Meeting on {format(next_meeting, '%d %b %Y')}"),
    subtitle = glue("as of {format(as.Date(latest_scrape + hours(hours_tz)), '%d %b %Y')}"),
    x = "Forecast date",
    y = "Probability"
  ) +
  aes(text = paste0(
    "Meeting: ", meeting_label, "<br>",
    "Time: ", format(scrape_time + hours(hours_tz), "%d %b %H:%M"), "<br>",
    "Move: ", move, "<br>",
    "Probability: ", percent(probability, accuracy = 1)
  )) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "right",
    legend.title = element_blank()
  )

# Filter releases to only show those within the chart's date range
relevant_releases <- abs_releases %>%
  filter(datetime >= start_xlim & datetime <= end_xlim)

# Build release segment data for interactive plot
# Use explicit x/xend/y/yend columns so ggplotly can reliably
# evaluate the aesthetics during conversion to plotly.
release_segments <- relevant_releases %>%
  transmute(
    dataset,
    x = datetime,
    xend = datetime,
    y = 0,
    yend = 1,
    text = paste0(
      "<b>", dataset, "</b><br>",
      "Meeting: ", meeting_label, "<br>",
      format(datetime, "%d %b %Y<br>%H:%M AEST")
    )
  )

# Combine probability lines with release segments
line_int_complete <- line_int_base +
  geom_segment(
    data = release_segments,
    aes(
      x = x,
      xend = xend,
      y = y,
      yend = yend,
      colour = dataset,
      text = text
    ),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 1
  )

# Convert to Plotly
interactive_line <- ggplotly(line_int_complete, tooltip = "text") %>%
  layout(
    hovermode = "x unified",
    legend = list(x = 1.02, y = 0.5, xanchor = "left"),
    showlegend = TRUE
  )

# Final layout adjustments
interactive_line <- interactive_line %>%
  layout(
    xaxis = list(
      title = "Forecast date",
      tickangle = 45
    ),
    yaxis = list(
      title = "Probability",
      tickformat = ".0%"
    ),
    title = list(
      text = paste0(
        glue("Cash-Rate Moves for the Next Meeting on {format(next_meeting, '%d %b %Y')}"),
        "<br>",
        "<sub>", glue("as of {format(as.Date(latest_scrape + hours(hours_tz)), '%d %b %Y')}"), "</sub>"
      )
    )
  )

# Save interactive HTML
htmlwidgets::saveWidget(
  interactive_line,
  file = "docs/line_interactive.html",
  selfcontained = TRUE
)

cat("\nInteractive chart saved to: docs/line_interactive.html\n")
