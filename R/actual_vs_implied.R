# ==============================================================================
# Actual vs Forecast Implied Cash Rate
# ==============================================================================
# Purpose: Create charts comparing the historical RBA cash rate with the
#          front-month futures-implied cash rate level, and visualise full
#          forecast paths captured on key event dates (RBA meetings, CPI
#          releases, and Labour Force releases). Static and interactive
#          outputs are written to the docs/ directory.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(here)
  library(htmlwidgets)
  library(lubridate)
  library(purrr)
  library(plotly)
  library(readrba)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)
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
# 2) Build actual vs implied plot
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
# 3) Build historical forecast paths on key event dates
# ------------------------------------------------------------------------------

# Event schedules ---------------------------------------------------------------
rba_meetings <- tibble(
  event_type = "RBA meeting",
  event_time = ymd_hm(c(
    "2025-02-18 14:30", "2025-04-01 14:30", "2025-05-20 14:30",
    "2025-07-08 14:30", "2025-08-12 14:30", "2025-09-30 14:30",
    "2025-11-04 14:30", "2025-12-09 14:30", "2026-02-03 14:30",
    "2026-03-17 14:30", "2026-05-05 14:30", "2026-06-16 14:30",
    "2026-08-11 14:30", "2026-09-29 14:30", "2026-11-03 14:30",
    "2026-12-08 14:30", "2027-02-03 14:30", "2027-03-17 14:30"
  ), tz = "Australia/Melbourne")
)

cpi_releases <- tribble(
  ~event_type, ~event_time,
  "CPI", ymd_hm("2025-01-29 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-04-30 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-07-30 11:30", tz = "Australia/Melbourne"),
  "CPI", ymd_hm("2025-10-29 11:30", tz = "Australia/Melbourne"),
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
  "CPI", ymd_hm("2026-12-30 11:30", tz = "Australia/Melbourne")
)

labour_force <- tribble(
  ~event_type, ~event_time,
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

events <- bind_rows(rba_meetings, cpi_releases, labour_force) %>%
  mutate(event_date = as.Date(event_time))

# Helper: choose the first scrape on/after the event, otherwise the latest before
target_scrapes <- sort(unique(cash_rate$scrape_time))

select_scrape <- function(event_time) {
  later <- target_scrapes[target_scrapes >= event_time]
  if (length(later) > 0) {
    return(min(later))
  }
  max(target_scrapes[target_scrapes <= event_time])
}

max_scrape_time <- max(target_scrapes)

forecast_snapshots <- events %>%
  filter(event_time <= max_scrape_time) %>%
  mutate(
    scrape_time = map(event_time, select_scrape) %>% reduce(c),
    event_label = str_c(event_type, ": ", format(event_date, "%d %b %Y"))
  ) %>%
  distinct(event_type, event_label, scrape_time, event_time)

forecast_paths <- forecast_snapshots %>%
  inner_join(cash_rate, by = "scrape_time") %>%
  mutate(expiry = as.Date(date)) %>%
  select(event_type, event_label, event_time, scrape_time, expiry, cash_rate)

# Only keep expiries within two years of the scrape to avoid very distant tails
forecast_paths <- forecast_paths %>%
  filter(expiry <= as.Date(scrape_time) + years(2))

latest_event_date <- forecast_snapshots %>%
  slice_max(event_time, n = 1, with_ties = FALSE) %>%
  pull(event_time) %>%
  as.Date()

forecast_plot <- ggplot(forecast_paths, aes(x = expiry, y = cash_rate)) +
  geom_line(aes(group = event_label, color = event_type), alpha = 0.5) +
  geom_point(aes(color = event_type), size = 0.7, alpha = 0.7) +
  scale_color_manual(
    values = c(
      "RBA meeting" = "#1b9e77",
      "CPI" = "#d95f02",
      "Labour Force" = "#7570b3"
    ),
    name = "Event type"
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.05)) +
  labs(
    title = "Forecast Paths Captured on Key Event Dates",
    subtitle = paste0(
      "Forecast curves from RBA meetings, CPI releases, and Labour Force releases up to ",
      format(latest_event_date, "%d %B %Y")
    ),
    x = "Futures contract expiry",
    y = "Implied cash rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 15)
  )

forecast_plot_interactive <- ggplotly(forecast_plot, tooltip = c("x", "y", "colour"))

# ------------------------------------------------------------------------------
# 4) Save outputs
# ------------------------------------------------------------------------------

# Static actual vs implied comparison
actual_vs_implied_path <- here("docs", "cash_rate_vs_implied.png")
ggsave(
  filename = actual_vs_implied_path,
  plot = actual_vs_implied_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Static forecast path overlay
forecast_path_png <- here("docs", "cash_rate_forecast_paths.png")
ggsave(
  filename = forecast_path_png,
  plot = forecast_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Interactive widget
forecast_path_html <- here("docs", "cash_rate_forecast_paths.html")
saveWidget(
  widget = forecast_plot_interactive,
  file = forecast_path_html,
  selfcontained = TRUE
)

cat("Saved charts to", actual_vs_implied_path, "and", forecast_path_png, "\n")
cat("Saved interactive chart to", forecast_path_html, "\n")
