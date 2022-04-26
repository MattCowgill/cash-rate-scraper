library(tidyverse)

cash_rate <- readRDS(file.path("combined_data", "all_data.Rds"))

cash_rate |>
  mutate(scrape_date = factor(scrape_date)) |>
  ggplot(aes(x = date, y = cash_rate, col = scrape_date)) +
  geom_line()
