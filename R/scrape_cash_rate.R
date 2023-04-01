# Scrape ASX 30 Day Interbank Cash Rate Futures Implied Yield Target from PDF
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)

# Download PDF from the ASX
pdf_url <- "https://www.asx.com.au/data/trt/ib_expectation_curve_graph.pdf"
pdf_path <- tempfile(fileext = ".pdf")
download.file(pdf_url, pdf_path, mode = "wb")

# Call a web service to extract the page_tables in the PDF. We use the web service called by
# https://docs2info.com/demo/ which is free.

# Upload the PDF as a multipart form.
r <- POST("https://pdftables-2uqbalu5ya-uw.a.run.app/upload",
  body = list(`uploaded-file` = upload_file(pdf_path, type="application/pdf")),
  encode = "multipart"
)

# The web service returns a JSON dict like this
# {
#   "NumberPages":1,
#   "FirstPageProcessed":1,
#   "LastPageProcessed":1,
#   "BadPages":null,
#   "PageTables":{
#     "1":[
#       {"Width":19,
#       "Height":2,
#       "Data":[
#         ["","Mar-23","Apr-23","May-23","Jun-23","Jul-23","Aug-23","Sep-23","Oct-23","Nov-23","Dec-23","Jan-24","Feb-24","Mar-24","Apr-24","May-24","Jun-24","Jul-24","Aug-24"],
#         ["Implied Yield","3.510","3.600","3.660","3.635","3.590","3.530","3.515","3.475","3.440","3.400","3.390","3.365","3.340","3.315","3.290","3.270","3.240","3.225"]]}]}}"
# }
# The ASX PDF has one 2 x 19 table on page 1. We could check that the returned JSON dict has exactly
# one 2 x 19 table on page 1 and report an error if it doesn't.
# Instead we assume we have the correct table.
dict <- content(r, "parsed")
cash_rate_table <- dict$PageTables$`1`[[1]]$Data

# Trim the row headers in column 1.
date <- tail(cash_rate_table[[1]], -1)
cash_rate <- tail(cash_rate_table[[2]], -1)

# Convert row 1 to dates and row 2 to numbers.
date <- lubridate::my(date)
cash_rate <- as.numeric(cash_rate)

# Create a tibble with our newly-scraped data
new_data <- tibble(date = date,
      cash_rate = cash_rate,
      scrape_date = Sys.Date())

# Write a CSV of today's data
write_csv(new_data, file.path("daily_data",
                              paste0("scraped_cash_rate_", Sys.Date(), ".csv")))

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


