# Scrape ASX 30 Day Interbank Cash Rate Futures Implied Yield Target from PDF
library(tidyverse)
library(tesseract)
library(magick)

# Download PDF from the ASX
pdf_url <- "https://www.asx.com.au/data/trt/ib_expectation_curve_graph.pdf"
pdf_path <- tempfile(fileext = ".pdf")
download.file(pdf_url, pdf_path, mode = "wb")

# Note that the table in the PDF appears to be part of an image, rather than a
# normal PDF table. This means we need to use optical character recognition to
# extract it

# Read the PDF as an image, crop it, and remove gridlines
full_image <- image_read_pdf(pdf_path)

table_image <- full_image |>
  magick::image_crop(geometry = geometry_area(width = 875 * 3.1,
                                              height = 40 * 3.1,
                                              x_off = 170 * 3.1,
                                              y_off = 640 * 3.1)) |>
  image_quantize(colorspace = "gray") |>
  image_transparent(color = "white", fuzz = 60)

# Extract the characters from the image
strings <- table_image |>
  ocr() |>
  str_split(pattern = "\n") |>
  unlist()

strings <- strings[strings != ""]

string_list <- map(strings, ~(str_split(.x, " ")[[1]]))

string_list <- map(string_list, ~.x[.x != ""])

# Create a tibble with our newly-scraped data
new_data <- tibble(date = string_list[[1]],
       cash_rate = string_list[[2]],
       scrape_date = Sys.Date()) |>
  mutate(date = lubridate::my(date))

# The decimal point is not always picked up; add it in
# Note we are assuming all future cash rates are <10%

new_data <- new_data |>
  mutate(cash_rate = if_else(str_sub(cash_rate, 2, 2) == ".",
                             cash_rate,
                             paste0(str_sub(cash_rate, 1, 1),
                                    ".",
                                    str_sub(cash_rate, 2L, -1L))),
         cash_rate = as.numeric(cash_rate))


# Write a CSV of today's data
write_csv(new_data, file.path("daily_data",
                              paste0("scraped_cash_rate_", Sys.Date(), ".csv")))

# Load all existing data, combine with latest data
all_data <- file.path("daily_data") |>
  list.files(pattern = ".csv",
             full.names = TRUE) |>
  read_csv(col_types = "DdD") |>
  filter(!scrape_date %in% c(as.Date("2022-08-06"),
                             as.Date("2022-08-07"),
                             as.Date("2022-08-08"))) |>
  filter(!is.na(date),
         !is.na(cash_rate))

saveRDS(all_data,
        file = file.path("combined_data",
                         "all_data.Rds"))

