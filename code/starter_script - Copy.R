# an example of paired predictions and observations
library(dplyr)
dat <- readRDS('data/predicted_observed_temps.rds')
# note that SNTemp predictions (uncalibrated process model)
# have predictions for all places/times
# here you just have the test period from the hybrid model (rgcn2_full_temp_c)
# and sparse observations (some reaches measured, some not)
# removing rows with na values. 

rgnc_dat_filter <- dat %>% filter(!is.na(temp_c), !is.na(rgcn2_full_temp_c))
str(rgnc_dat_filter)
summary(rgnc_dat_filter)
#grouping data by seg_id only.
rgnc_by_seg <- rgnc_dat_filter %>% group_by(seg_id_nat) 
summary(rgnc_by_seg)
#grouping data by seg_id and date.
rgnc_by_seg_date <- rgnc_dat_filter %>% group_by(seg_id_nat, year = lubridate::year(date)) 
summary(rgnc_by_seg_date)
## Absolute Residual Error: calculating Mean Absolute Error (MAE) metric to compare both models prediction data vs observed data. We found the mae by finding the sum of the absolute value difference in predicted and observed temperature. Then divided the sum by the n row in grouped data.

cal_mae <- function(observe_data, predict_data, n_digits = 2) {
    mae = round((mean(abs(observe_data - predict_data), na.rm = TRUE )), digits = n_digits)
    return(mae)
}
mae <- rgnc_by_seg %>%  
  summarize(MAE_Process_Model = cal_mae(temp_c, sntemp_temp_c),
            MAE_Hybrid_Model = cal_mae(temp_c, rgcn2_full_temp_c))

# Absolute Residual Error: Root Mean Square Error (RMSE) square the difference in predicted and observed temperature.  sum the squared values, then divided by n of row. Finally,took the square root
cal_rmse <- function(observe_data, predict_data, n_digits = 2 ){
  rmse <- round(sqrt(mean((observe_data - predict_data) ^2, na.rm = TRUE )), digits = n_digits)
  return(rmse)
}
rmse <- rgnc_by_seg %>%
  summarize(RMSE_Process_Model = cal_rmse(temp_c, sntemp_temp_c),
            RMSE_Hybrid_Model = cal_rmse(temp_c, rgcn2_full_temp_c))

# Relative Parameter: Mean Absolute Relative Error (MARE) metric to compare both models prediction data vs observed data. We found MARE by: 1) dividing the difference between the predicted and observed data by the observed measurements. 2) sum the division answer. 3) divide by number of rows in grouped data.  
## mare metric when removing inf terms from ratio of temp diff and observed temp. 
cal_mare <- function(observe_data, predict_data, n_digits = 2){
  mare <- round(mean((abs(observe_data - predict_data) / observe_data), na.rm = TRUE), digits = n_digits)
  return(mare)
}  
mare <- rgnc_by_seg %>%
  mutate(temp_c = ifelse( temp_c %in% 0, 0.1, temp_c)) %>%
  summarize( MARE_Process_Model = cal_mare(temp_c, sntemp_temp_c),
             MARE_Hybrid_Model = cal_mare(temp_c, rgcn2_full_temp_c))

compare_metric <- plyr:: join_all(list(mae, rmse, mare),  by = 'seg_id_nat', type = 'left')
summary(compare_metric) 

#Finding the max temperature for each segment. Also, finding the year associated with it. 
cal_max_temp_timing <- function(data_in, observe_col, predict_col, date_col,  date_range = 170:245) {
  max_fun_out <- data_in %>%
    summarize(max_temp_obs = max({{observe_col}}),
              max_timing_obs = lubridate::yday(date[which.max({{observe_col}})]),
              max_temp_mod = max({{predict_col}}),
              max_timing_mod = lubridate::yday(date[which.max({{predict_col}})]),
              summer_complete = all({{date_range}} %in% lubridate::yday({{date_col}}))) %>%
    mutate(error_obs_pred = abs(max_temp_obs - max_temp_mod),
           error_max_timing = abs(max_timing_obs - max_timing_mod )) %>%
    filter(summer_complete)
  return(max_fun_out)
}

max_process_metrics = cal_max_temp_timing(data_in = rgnc_by_seg_date, 
                                           observe_col = temp_c, predict_col = sntemp_temp_c, 
                                           date_col = date)
max_hybrid_metrics = cal_max_temp_timing(data_in = rgnc_by_seg_date, 
                                           observe_col = temp_c, predict_col = rgcn2_full_temp_c, 
                                           date_col = date)
## calculating Nash coefficient of efficiency (CE;Nash and Sutcliffe)
cal_nash <- function(observe_col, predict_col, n_digits = 2){
  nash = round(1 - (sum(observe_col - predict_col) ^ 2) / (sum(observe_col - mean(observe_col) ^2)), digits = n_digits)  
}
# nse values > 1 which mean the metric isn't calculting correctly.
nse_metric =rgnc_by_seg_date %>%
  group_by(seg_id_nat, lubridate::year(date)) %>%
  summarize(nse_process = cal_nash(temp_c, sntemp_temp_c),
            nse_hybrid = cal_nash(temp_c, rgcn2_full_temp_c))
## Calculating the exceedence metric. 
# Using if statements.  Not working
cal_exceedance_NOT <- function(observe_col, predict_col, temp_threshold = 25.5){
if ({{observe_col}} >= temp_threshold & {{predict_col}} >= temp_threshold){
    return("TRUE")
  }else if ({{observe_col}} >= temp_threshold & {{predict_col}} < temp_threshold ){
    return("False_negative")
  }
  else if ({{observe_col}} < temp_threshold & {{predict_col}} >= temp_threshold) {
    return("False_positive ")
  }
}
# Without if else statements. Working but not producing the desired output. 
cal_exceedance <- function(observe_col, predict_col, temp_threshold = 25.5){
  true = observe_col >= temp_threshold & predict_col >= temp_threshold
  False_negative = observe_col >= temp_threshold & predict_col < temp_threshold
  False_positive = observe_col < temp_threshold & predict_col >= temp_threshold
}
## trying to use the exceedance function. 
exc_test <- rgnc_by_seg %>%
  mutate(proc_test = cal_exceedance(temp_c, sntemp_temp_c),
         hyb_test =  cal_exceedance(temp_c, rgcn2_full_temp_c))

exceedance_metric <- rgnc_by_seg %>%
  summarize(proc_test = cal_exceedance(temp_c, sntemp_temp_c),
            hyb_test =  cal_exceedance(temp_c, rgcn2_full_temp_c))


