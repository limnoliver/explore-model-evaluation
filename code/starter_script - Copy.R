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
max_temp_timing <- rgnc_by_seg_date %>% 
  summarize(n_per_year = n(),
            max_temp_c = max(temp_c),
            max_timing_tempc = lubridate::yday(date[which.max(temp_c)]),
            max_tempc_dat = date[which.max(temp_c)],
            max_temp_proc = max(sntemp_temp_c),
            max_timing_proc = lubridate::yday(date[which.max(sntemp_temp_c)]),
            max_temp_proc_dat = date[which.max(sntemp_temp_c)],
            max_temp_hyb = max(rgcn2_full_temp_c),
            max_timing_hyb = lubridate::yday(date[which.max(rgcn2_full_temp_c)]),
            max_temp_hyb_dat = date[which.max(rgcn2_full_temp_c)],
            error_temp_obs_proc = abs(max_temp_c - max_temp_proc),
            error_time_obs_proc = abs(max_timing_tempc - max_timing_proc),
            error_temp_obs_hyb = abs(max_temp_c - max_temp_hyb),
            error_time_obs_hyb = abs(max_timing_tempc - max_timing_hyb),
            summer_complete = all(170:245 %in% lubridate::yday(date))) %>% 
  filter(summer_complete) %>%
  mutate(dev_mean_obs_proc = error_temp_obs_proc - mean(error_temp_obs_proc),
         dev_mean_obs_hyb = error_temp_obs_hyb - mean(error_temp_obs_hyb))

cal_max_temp_timing <- function(observe_data, predict_data, date) {
  max_temp_observe_data = max(observe_data)
  max_timing_observe_data = lubridate::yday(date[which.max(observe_data)])
  max_observe_data_date = date[which.max(observe_data)]
  max_temp_predict_data = max(predict_data)
  max_timing_predict_data = lubridate::yday(date[which.max(predict_data)])
  max_temp_predict_data_dat = date[which.max(predict_data)]
  error_obs_pred = abs(max_temp_observe_data - max_temp_predict_data)
  error_max_timing = abs(max_timing_observe_data - max_timing_predict_data)
  summer_complete = all(170:245 %in% lubridate::yday(date)) %>% 
    filter(summer_complete) %>%
    mutate(dev_mean_max_temp = error_obs_pred - mean(error_obs_pred),
           dev_mean_max_timing = error_max_timing - mean(error_max_timing))
           
}

max_temp_timing_fun <- rgnc_by_seg_date %>%  
  summarize(max_obs_procc = cal_max_temp_timing(temp_c, sntemp_temp_c, date),
            max_obs_hyb = cal_max_temp_timing(temp_c, rgcn2_full_temp_c, date))
  

median_metric <-data.frame(Process_Model_Max_Temperature = median(max_temp_timing$error_obs_proc),
                           Hybrid_Model_Max_Temerature = median(max_temp_timing$error_obs_hyb))
## calculating Nash coefficient of efficiency (CE;Nash and Sutcliffe)
cal_nash <- function(observe_data, predict_data, n_digits = 2){
  nash = round(1 - (sum(observe_data - predict_data) ^ 2) / (sum(observe_data - mean(observe_data) ^2)), digits = n_digits)  
}
nse =rgnc_by_seg_date %>%
  group_by(seg_id_nat, lubridate::year(date)) %>%
  summarize(nse_process = cal_nash(temp_c, sntemp_temp_c),
            nse_hybrid = cal_nash(temp_c, rgcn2_full_temp_c))
