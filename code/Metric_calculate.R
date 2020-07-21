## Creating function to use the metrics function for future input into the Delaware project pipeline:

# Calls the four metrics functions (mae, rmse, mare, and nash). This function accepts 2 arguments data-in and grouping, the inputted data will be grouped by model to suit the data we're working with. The grouping arguments accepts 'NA' for no grouping, or any other grouping such as by segment, month, year, etc. We call the four metrics function to produce a dataframe with metrics of interest.  
calc_all_metric <- function(dat_in, grouping) {
  dat_in <- dat_in %>%
    #mutate(year = lubridate::year(date),
     #      month = lubridate::month(date)) %>%
    group_by(model)

  group_test <- deparse(substitute({{grouping}}))
  if (group_test == 'NA') {
    dat_mod <- dat_in
  } else {
    dat_mod <- dat_in %>%
      group_by({{grouping}})
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
  return(metrics)
}

# Calls exceedance metric to find when the exceedance of certain temperature was predicted correctly by the models. 
calc_exc_metric <- function(dat_in, grouping) {
  dat_in <- dat_in %>%
    group_by(model)
  
  group_test <- deparse(substitute({{grouping}}))
  if (group_test == 'NA') {
    dat_mod <- dat_in
  } else {
    dat_mod <- dat_in %>%
      group_by({{grouping}})
  }
  exceedance_metric <- dat_mod %>%
  summarize(n = n(),
            true_pos = calc_exceedance(observe_col = obs_temp_c, 
                            predict_col = pred_temp_c,
                            metric = 'prop_true_pos', 
                            threshold = 25.5),
            true_neg = calc_exceedance(observe_col = obs_temp_c, 
                            predict_col = pred_temp_c,
                            metric = 'prop_true_neg', 
                            threshold = 25.5),
            false_pos = calc_exceedance(observe_col = obs_temp_c, 
                            predict_col = pred_temp_c,
                            metric = 'prop_false_pos', 
                            threshold = 25.5),
            false_neg = calc_exceedance(observe_col = obs_temp_c, 
                            predict_col = pred_temp_c,
                            metric = 'prop_false_neg'))
  return(exceedance_metric)
}

## Calls the calc_timing_max that returns the max temperature and time of max temps. This function accepts 2 arguments data_in and date-range. date_range in set to summer days. It groups data by the model, segment_id, and year.
calc_max_metric <- function(dat_in, date_range = 170:245){
  dat_mod <- dat_in %>%
    group_by(model, seg_id_nat, lubridate::year(date)) 
  
  max_metric <- 
    calc_timing_max(data_in = dat_mod, 
                    observe_col = obs_temp_c, predict_col = pred_temp_c, 
                    date_col = date)
  return(max_metric)
}
