library(readr)
library(ggplot2)

source(here::here("R", "load-data.R"))
source(here::here("R", "plot-daily-cases.R"))

start_date <- as.Date("2021-11-15")

daily <- load_data(open_data = FALSE, data_type = "onset", start_date = start_date, clean = FALSE)

date <- max(daily$date)
download_date <- Sys.Date()
readr::write_csv(
  daily,
  here::here("data", "public", paste0(download_date, "-cases-by-sgtf.csv"))
)
