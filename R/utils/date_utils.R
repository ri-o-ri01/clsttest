make_weekdays <- function(start_date, end_date) {
  seq.Date(as.Date(start_date), as.Date(end_date), by = "day") %>%
    keep(~ wday(.x) %in% 2:6)  # 平日のみをフィルタリング
}