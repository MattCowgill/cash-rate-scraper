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
               date_labels = "%b\n%Y") +
  theme_minimal() +
  labs(subtitle = "Expected future cash rate",
       colour = "Expected\nas at:",
       x = "Date")

cash_rate |>
  filter(scrape_date == max(scrape_date[scrape_date != max(scrape_date)]))

viz_2 <- cash_rate |>
  filter(scrape_date %in% c(max(scrape_date),
                            max(scrape_date) - weeks(1),
                            max(scrape_date) - months(1),
                            max(scrape_date) - years(1),
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
  scale_x_date(expand = expansion(c(0.05, 0.15))) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(subtitle = "Expected future cash rate",
       x = "Date")



viz_3 <- cash_rate |>
  group_by(scrape_date) |>
  filter(cash_rate == max(cash_rate)) |>
  ggplot(aes(x = scrape_date, y = cash_rate)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(date_labels = "%b\n%Y",
               date_breaks = "1 months") +
  labs(x = "Expected as at",
       y = "Expected peak cash rate")


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
               date_breaks = "1 months") +
  labs(x = "Expected as at",
       y = "Cash rate expected to peak in")
