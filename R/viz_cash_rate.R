suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(ggrepel)
          })

cash_rate <- readRDS(file.path("combined_data", "all_data.Rds"))

viz_1 <- cash_rate |>
  ggplot(aes(x = date, y = cash_rate, col = scrape_date, group = scrape_date)) +
  geom_line() +
  geom_line(data = ~filter(., scrape_date == max(scrape_date)),
            colour = "red",
            linewidth = 1.5) +
  geom_text(data = ~filter(.,
                           scrape_date == max(scrape_date)) |>
              filter(date == max(date)),
            colour = "red",
            hjust = 0,
            lineheight = 0.8,
            aes(label = format(scrape_date, "%e %b\n%Y"))) +
  scale_colour_date(date_labels = "%b '%y") +
  scale_x_date(expand = expansion(c(0, 0.1)),
               date_labels = "%b\n%Y",
               breaks = seq(max(cash_rate$date),
                            min(cash_rate$date),
                            by = "-1 year")) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  theme_minimal() +
  labs(subtitle = "Expected future cash rate",
       colour = "Expected\nas at:",
       x = "Date") +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank())

max_scrape_date <- max(cash_rate$scrape_date)
week_ago <- max_scrape_date - weeks(1)
year_ago <- max_scrape_date - years(1)
month_ago <- max_scrape_date - months(1)
yesterday <- max(cash_rate$scrape_date[cash_rate$scrape_date < max_scrape_date])

viz_2 <- cash_rate |>
  filter(scrape_date %in% c(max_scrape_date,
                            min(scrape_date[scrape_date >= week_ago]),
                            min(scrape_date[scrape_date >= month_ago]),
                            max(scrape_date[scrape_date != max(scrape_date)]))) |>
  distinct() |>
  ggplot(aes(x = date, y = cash_rate,
             col = factor(scrape_date),
             group = scrape_date)) +
  geom_line() +
  geom_text_repel(data = ~group_by(., scrape_date) |>
                    filter(date == max(date)),
                  hjust = 0,
                  nudge_x = 10,
                  min.segment.length = 10000,
                  aes(label = format(scrape_date, "%d %b %Y"))) +
  scale_x_date(expand = expansion(c(0.05, 0.15)),
               date_labels = "%b\n%Y",
               breaks = seq(max(cash_rate$date),
                            min(cash_rate$date),
                            by = "-6 months")) +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(subtitle = "Expected future cash rate",
       x = "Date") +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank())

viz_3 <- cash_rate |>
  group_by(scrape_date) |>
  filter(cash_rate == max(cash_rate)) |>
  ggplot(aes(x = scrape_date, y = cash_rate)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(date_labels = "%b\n%Y",
               date_breaks = "3 months") +
  labs(x = "Expected as at",
       subtitle = "Expected peak cash rate") +
  scale_y_continuous(labels = \(x) paste0(x, "%"),
                     breaks = seq(0, 100, 0.25)) +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_blank())


viz_4 <- cash_rate |>
  group_by(scrape_date) |>
  summarise(peak_date = min(date[cash_rate == max(cash_rate)])) |>
  ggplot(aes(x = scrape_date, y = peak_date)) +
  geom_point() +
  geom_smooth(method = "loess",
              formula = y ~ x,
              span = 0.1,
              se = FALSE) +
  theme_minimal() +
  scale_x_date(date_labels = "%b\n%Y",
               date_breaks = "3 months") +
  scale_y_date(date_labels = "%b\n%Y",
               date_breaks = "3 months") +
  labs(x = "Expected as at",
       y = "Expected to peak in") +
  theme(panel.grid.minor = element_blank())

viz_5 <- cash_rate |>
  group_by(scrape_date) |>
  mutate(next_month = floor_date(scrape_date + months(1), "month"),
         current_rate = mean(cash_rate[date == next_month])) |>
  filter(date >= scrape_date) |>
  mutate(diff = cash_rate - current_rate) |>
  filter(diff <= -0.25) |>
  filter(date == min(date)) |>
  ggplot(aes(x = scrape_date,
             y = date)) +
  geom_point() +
  geom_line() +
  scale_x_date("Expected as at",
               limits = ymd("2024-01-01",
                            max(cash_rate$scrape_date)),
               date_labels = "%b\n%Y") +
  scale_y_date("Expected date of first cut",
               date_labels = "%b\n%Y",
               date_breaks = "3 months",
               limits = \(x) ymd("2024-01-01",
                                 x[2])) +
  theme_minimal() +
  labs(subtitle = "Expected date of first rate cut",
       caption = "Refers to the first date at which futures pricing imply a 100% or greater chance of a 25bp cut relative to the then-current rate.")
