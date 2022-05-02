suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
          })

cash_rate <- readRDS(file.path("combined_data", "all_data.Rds"))

viz <- cash_rate |>
  ggplot(aes(x = date, y = cash_rate, col = scrape_date, group = scrape_date)) +
  geom_line()