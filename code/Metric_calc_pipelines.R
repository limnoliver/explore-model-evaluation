## Creating function to use the metrics function for future input into the delaware project pipeline

calc_max_metric <- function(dat_in, grouping, date_range = 170:245){
  dat_mod <- dat_in %>%
      group_by(seg_id_nat, lubridate::year(date)) 
  
  max_metric <- 
    calc_max_timing(data_in = dat_mod, 
                    observe_col = temp_c, predict_col = sntemp_temp_c, 
                    date_col = date)
  return(max_metric)
}




calc_all_metrics <- function(dat_in, grouping) {
  
  dat_in <- dat_in %>%
    mutate(year = lubridate::year(date),
           month = lubridate::month(date))
  
  if (is.na(grouping)) {
    dat_mod <- dat_in
  } else {
    dat_mod <- dat_in %>%
      group_by(grouping)
  }
  
  metrics <- dat_mod %>%
    # calls the metric functions you already created
    summarize(sntemp_rmse = calc_rmse(temp_c, sntemp_temp_c))
  # do all of the RMSE calcs
  # then to the NSE calcs...and so on
  
}