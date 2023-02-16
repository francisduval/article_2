get_nb_days_since_monday <- function(datetime) {
  wday(datetime, week_start = 1) + hour(datetime) / 24 + minute(datetime) / 1440 + second(datetime) / 86400 - 1
}