compute_extra_trip_vars <- function(data) {
  data %>% 
    mutate(
      time_start_norm = (as.numeric(time_start) * 2 * pi) / 86400,
      cos_time_start = cos(time_start_norm),
      sin_time_start = sin(time_start_norm),
      nb_days_since_monday = get_nb_days_since_monday(datetime_start),
      nb_days_since_monday_norm = (nb_days_since_monday * 2 * pi) / 7,
      cos_nb_days_since_monday = cos(nb_days_since_monday_norm),
      sin_nb_days_since_monday = sin(nb_days_since_monday_norm),
      weekday = weekdays(date_start)
    ) %>% 
    select(
      -time_start_norm,
      -nb_days_since_monday_norm
    )
}