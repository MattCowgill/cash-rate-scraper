# ==============================================================================
# Actual vs Forecast Implied Cash Rate
# ==============================================================================
# Purpose: Create a simple time-series chart comparing the historical RBA cash
#          rate with the front-month futures-implied cash rate level.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(lubridate)
  library(readrba)
  library(scales)
})

# ------------------------------------------------------------------------------
# 1) Load and prepare data
# ------------------------------------------------------------------------------

# Ensure output directory exists
if (!dir.exists(here("docs"))) {
  dir.create(here("docs"), recursive = TRUE)
}

# Load consolidated futures data (columns: date, cash_rate, scrape_time)
cash_rate <- readRDS(here("combined_data", "all_data.Rds"))

# Latest observation per contract per scrape date
cash_rate_daily <- cash_rate %>%
  mutate(
    scrape_date = as.Date(scrape_time),
    expiry = as.Date(date)
  ) %>%
  group_by(scrape_date, expiry) %>%
  slice_max(scrape_time, n = 1, with_ties = FALSE) %>%
  ungroup()

# Front-month implied rate per day (earliest available contract)
front_month <- cash_rate_daily %>%
  group_by(scrape_date) %>%
  arrange(expiry, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    date = scrape_date,
    implied_rate = cash_rate
  )

# Historical actual cash rate
actual_cash_rate <- read_rba(series_id = "FIRMMCRTD") %>%
  transmute(
    date,
    actual_rate = value
  )

# Limit actual series to overlap with futures-derived data
plot_actual <- actual_cash_rate %>%
  filter(date >= min(front_month$date, na.rm = TRUE))

# ------------------------------------------------------------------------------
# 2) Build plot
# ------------------------------------------------------------------------------

latest_scrape <- max(front_month$date, na.rm = TRUE)

actual_vs_implied_plot <- ggplot() +
  geom_line(
    data = plot_actual,
    aes(x = date, y = actual_rate, color = "Actual cash rate"),
    linewidth = 1.1
  ) +
  geom_line(
    data = front_month,
    aes(x = date, y = implied_rate, color = "Front-month implied level"),
    linewidth = 1,
    alpha = 0.85
  ) +
  scale_color_manual(
    values = c(
      "Actual cash rate" = "#1b9e77",
      "Front-month implied level" = "#7570b3"
    ),
    name = NULL
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.05)) +
  labs(
    title = "RBA Cash Rate vs Front-Month Futures-Implied Level",
    subtitle = paste0(
      "Latest futures scrape: ",
      format(latest_scrape, "%d %B %Y")
    ),
    x = "Date",
    y = "Rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )

# ------------------------------------------------------------------------------
# 3) Save output
# ------------------------------------------------------------------------------

ggsave(
  filename = here("docs", "cash_rate_vs_implied.png"),
  plot = actual_vs_implied_plot,
  width = 10,
  height = 6,
  dpi = 300
)

cat("Saved chart to", here("docs", "cash_rate_vs_implied.png"), "\n")
