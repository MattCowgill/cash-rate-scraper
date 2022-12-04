suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
          })

cash_rate <- readRDS(file.path("combined_data", "all_data.Rds"))

viz_1 <- cash_rate |>
  ggplot(aes(x = date, y = cash_rate, col = scrape_date, group = scrape_date)) +
  geom_line() +
  theme_minimal() +
  labs(subtitle = "Expected future cash rate",
       colour = "Expected\nas at:",
       x = "Date")


viz_2 <- cash_rate |>
  group_by(scrape_date) |>
  filter(cash_rate == max(cash_rate)) |>
  ggplot(aes(x = scrape_date, y = cash_rate)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(date_labels = "%b\n%Y",
               date_breaks = "1 months") +
  labs(x = "Expected as at",
       y = "Expected peak cash rate")


viz_3 <- cash_rate |>
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
