library(readr)
library(ggplot2)

source(here::here("R", "load-data.R"))
source(here::here("R", "plot-daily-cases.R"))

daily <- load_data()

date <- max(daily$date)
download_date <- Sys.Date()
readr::write_csv(
  daily,
  here::here("data", "public", paste0(download_date, "-cases-by-sgtf.csv"))
)