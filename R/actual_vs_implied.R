# ==============================================================================
# Actual vs Forecast Implied Cash Rate
# ==============================================================================
# Purpose: Create charts comparing the historical RBA cash rate with the
#          front-month futures-implied cash rate level, and visualise full
#          forecast paths captured on key event dates (RBA meetings, CPI
#          releases, and Labour Force releases). Static and interactive
#          outputs are written to the docs/ directory.
# ==============================================================================

ensure_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
  }
}

ensure_packages(c(
  "dplyr", "ggplot2", "here", "htmlwidgets", "lubridate", "purrr",
  "plotly", "readrba", "scales", "viridis", "stringr", "tibble",
  "tidyr", "evaluate"
))

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
  library(viridis)
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
# 2) Build historical forecast paths on key event dates
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
) %>%
  filter(month(event_time) %in% c(2, 5, 8, 11))

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

select_same_day_last <- function(event_time) {
  event_date <- as.Date(event_time)
  same_day_scrapes <- target_scrapes[as.Date(target_scrapes) == event_date]

  if (length(same_day_scrapes) > 0) {
    return(max(same_day_scrapes))
  }

  select_scrape(event_time)
}

max_scrape_time <- max(target_scrapes)

forecast_snapshots <- events %>%
  filter(event_time <= max_scrape_time) %>%
  mutate(
    scrape_time = map2(
      event_time,
      event_type,
      ~ if (.y == "RBA meeting") select_same_day_last(.x) else select_scrape(.x)
    ) %>% reduce(c),
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

latest_scrape_date <- forecast_paths %>%
  summarise(latest = max(as.Date(scrape_time), na.rm = TRUE)) %>%
  pull(latest)

x_start <- latest_scrape_date - years(1)
x_end <- latest_scrape_date + months(18)

plot_actual_filtered <- plot_actual %>%
  filter(date >= x_start)

forecast_paths_window <- forecast_paths %>%
  mutate(scrape_date = as.Date(scrape_time)) %>%
  mutate(
    event_reason = case_when(
      event_type == "RBA meeting" ~ "RBA meeting (policy decision)",
      event_type == "CPI" ~ "Inflation print (CPI release)",
      event_type == "Labour Force" ~ "Labour Force / unemployment release",
      TRUE ~ event_type
    ),
    tooltip_text = str_glue(
      "Reason: {event_reason} on {format(event_time, '%d %b %Y')}<br>",
      "Scrape: {format(scrape_time, '%d %b %Y %H:%M %Z')}<br>",
      "Expiry: {format(expiry, '%b %Y')}<br>",
      "Implied rate: {scales::number(cash_rate, accuracy = 0.001)}%"
    )
  ) %>%
  filter(expiry >= x_start, expiry <= x_end)

forecast_paths_grouped <- forecast_paths_window %>%
  arrange(scrape_time, expiry) %>%
  group_split(scrape_time)

path_changes <- map_dfr(seq_along(forecast_paths_grouped), function(i) {
  path_df <- forecast_paths_grouped[[i]]
  current_scrape <- unique(path_df$scrape_time)

  if (length(current_scrape) != 1) {
    return(tibble())
  }

  if (i == 1) {
    return(tibble(scrape_time = current_scrape, mean_change_bp = NA_real_))
  }

  previous_df <- forecast_paths_grouped[[i - 1]]
  overlap <- inner_join(
    path_df %>% select(expiry, cash_rate),
    previous_df %>% select(expiry, cash_rate),
    by = "expiry",
    suffix = c("", "_prev")
  )

  mean_change_bp <- overlap %>%
    summarise(mean_bp = mean(abs(cash_rate - cash_rate_prev) * 100, na.rm = TRUE)) %>%
    pull(mean_bp)

  tibble(scrape_time = current_scrape, mean_change_bp = mean_change_bp)
})

top_changes <- path_changes %>%
  filter(!is.na(mean_change_bp)) %>%
  slice_max(mean_change_bp, n = 8, with_ties = FALSE)

kept_scrapes <- if (nrow(top_changes) > 0) {
  top_changes %>% pull(scrape_time)
} else {
  unique(forecast_paths_window$scrape_time)
}

forecast_paths_window <- forecast_paths_window %>%
  filter(scrape_time %in% kept_scrapes) %>%
  arrange(scrape_time, expiry)

latest_path <- forecast_paths_window %>%
  filter(scrape_time == max(scrape_time))

latest_event_date <- forecast_snapshots %>%
  filter(scrape_time %in% kept_scrapes) %>%
  slice_max(event_time, n = 1, with_ties = FALSE) %>%
  pull(event_time) %>%
  as.Date()

  forecast_plot <- ggplot() +
    geom_line(
      data = plot_actual_filtered,
      aes(x = date, y = actual_rate, text = "Actual cash rate"),
      linewidth = 1,
      color = "#2b2b2b"
    ) +
    geom_line(
      data = forecast_paths_window,
      aes(
        x = expiry,
        y = cash_rate,
        group = scrape_time,
        color = scrape_date,
        text = tooltip_text
      ),
      alpha = 0.5
    ) +
    scale_color_gradientn(
      colors = viridis(9),
      name = "Scrape date",
      guide = guide_colorbar(barheight = unit(5, "cm"), barwidth = unit(0.4, "cm"))
    ) +
  scale_y_continuous(labels = number_format(accuracy = 0.05)) +
  scale_x_date(
    limits = c(x_start, x_end),
    date_labels = "%b %Y",
    date_breaks = "2 months",
    expand = c(0.01, 0)
  ) +
  labs(
    title = "Cash Rate Forecast Paths",
    subtitle = paste0(
      "Top 8 daily shifts up to ",
      format(latest_event_date, "%d %B %Y")
    ),
    x = "Futures contract expiry",
    y = "Implied cash rate (%)",
    caption = "Forecasts include a mix of previous settlement and last trade data which can create a non-linearity along the forecast path"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 15),
    plot.caption = element_text(margin = margin(t = 10))
  )

forecast_plot_interactive <- ggplotly(forecast_plot, tooltip = "text") %>%
  layout(showlegend = FALSE,
         annotations = list(
           list(
             text = "Forecasts include a mix of previous settlement and last trade data which can create a non-linearity along the forecast path",
             x = 0,
             xref = "paper",
             xanchor = "left",
             y = -0.15,
             yref = "paper",
             yanchor = "top",
             align = "left",
             showarrow = FALSE,
             font = list(size = 10)
           )
         ))

# ------------------------------------------------------------------------------
# 3) Save outputs
# ------------------------------------------------------------------------------

# Static forecast path overlay
forecast_plot_dir <- here("docs", "plots", "forecast_paths")
if (!dir.exists(forecast_plot_dir)) {
  dir.create(forecast_plot_dir, recursive = TRUE)
}

forecast_path_png <- file.path(forecast_plot_dir, "cash_rate_forecast_paths.png")
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

cat("Saved chart to", forecast_path_png, "\n")
cat("Saved interactive chart to", forecast_path_html, "\n")
