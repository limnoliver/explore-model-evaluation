## Creating function to use the metrics function for future input into the delaware project pipeline

calc_max_metric <- function(dat_in, date_range = 170:245){
  dat_mod <- dat_in %>%
      group_by(model, seg_id_nat, lubridate::year(date)) 
  
  max_metric <- 
    calc_timing_max(data_in = dat_mod, 
                    observe_col = obs_temp_c, predict_col = pred_temp_c, 
                    date_col = date)
  return(max_metric)
}

calc_all_metrics <- function(dat_in, grouping) {
  dat_in <- dat_in %>%
    group_by(model)
    
  if (is.na(grouping)) {
    dat_mod <- dat_in
  } else {
    dat_mod <- dat_in %>%
      group_by(grouping)
  }
  metrics <- dat_mod %>%
    summarize(mae = calc_mae(observe_data = obs_temp_c, 
                    predict_data = pred_temp_c),
              rmse =  calc_rmse(observe_data = obs_temp_c, 
                      predict_data = pred_temp_c),
              mare = calc_mare(observe_data = obs_temp_c, 
                      predict_data = pred_temp_c),
              nse = calc_nash(observe_col = obs_temp_c, 
                     predict_col =  pred_temp_c)) 
}


