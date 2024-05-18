library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(jsonlite)

json_url <- "https://asx.api.markitdigital.com/asx-research/1.0/derivatives/interest-rate/IB/futures?days=1&height=179&width=179"

json_file <- tempfile()

download.file(json_url, json_file)

new_data <- jsonlite::fromJSON(json_file) |>
  pluck("data", "items") |>
  as_tibble() |>
  mutate(dateExpiry = ymd(dateExpiry),
         dateExpiry = floor_date(dateExpiry, "month")) |>
  filter(pricePreviousSettlement != 0) |>
  mutate(cash_rate = 100 - pricePreviousSettlement) |>
  select(date = dateExpiry,
         cash_rate,
         scrape_date = datePreviousSettlement)

# Write a CSV of today's data
write_csv(new_data, file.path("daily_data",
                              paste0("scraped_cash_rate_", Sys.Date() - 1, ".csv")))

# Load all existing data, combine with latest data
all_data <- file.path("daily_data") |>
  list.files(pattern = ".csv",
             full.names = TRUE) |>
  read_csv(col_types = "DdD") |>
  filter(!scrape_date %in% ymd("2022-08-06",
                               "2022-08-07",
                               "2022-08-08",
                               "2023-01-18",
                               "2023-01-24",
                               "2023-01-31",
                               "2023-02-02",
                               "2022-12-30",
                               "2022-12-29")) |>
  filter(!is.na(date),
         !is.na(cash_rate))

saveRDS(all_data,
        file = file.path("combined_data",
                         "all_data.Rds"))


