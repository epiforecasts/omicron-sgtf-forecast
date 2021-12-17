
source(here::here("R", "load-data.R"))
source(here::here("R", "plot-daily-cases.R"))

daily <- load_data()

date <- max(daily$date)
readr::write_csv(
  daily, here::here("data", "public", paste0(date, "-cases-by-sgtf.csv"))
)

plot <- plot_daily_cases(
  daily, caption = "Data are sourced from the UKHSA and are by specimen dat"
)

ggsave(here("figures", "daily-cases.png"),
       plot, dpi = "print", width = 7, height = 7)
