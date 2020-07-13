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
cal_max_temp_timing <- function(data_in, observe_col, predict_col, date_col,  date_range = 170:245, n_digits = 2) {
  max_fun_out <- data_in %>%
    summarize(max_temp_obs = max({{observe_col}}),
              max_timing_obs = lubridate::yday(date[which.max({{observe_col}})]),
              max_temp_mod = max({{predict_col}}),
              max_timing_mod = lubridate::yday(date[which.max({{predict_col}})])) %>%
    mutate(error_obs_pred = round(abs(max_temp_obs - max_temp_mod), digits = n_digits),
           error_max_timing = abs(max_timing_obs - max_timing_mod))
  return(max_fun_out)
}

max_process_metrics = cal_max_temp_timing(data_in = rgnc_by_seg_date, 
                                           observe_col = temp_c, predict_col = sntemp_temp_c, 
                                           date_col = date)
max_hybrid_metrics = cal_max_temp_timing(data_in = rgnc_by_seg_date, 
                                           observe_col = temp_c, predict_col = rgcn2_full_temp_c, 
                                           date_col = date)

## Creating a funcion to calculate Nash coefficient of efficiency (Nash):
cal_nash <- function(observe_col, predict_col, n_digits = 2){
  nash = round(1 - ((sum((observe_col - predict_col) ^ 2)) / (sum((observe_col - mean(observe_col)) ^2))), digits = n_digits)  
}
# calculating Nash coefficient of efficiency (CE;Nash and Sutcliffe) values: 
nse_metric =rgnc_by_seg %>%
  summarize(nse_process = cal_nash(temp_c, sntemp_temp_c),
            nse_hybrid = cal_nash(temp_c, rgcn2_full_temp_c))
# To view the data associated with this segment we can filter through the dataset:
View(filter(rgnc_dat_filter, seg_id_nat %in% 1549)) #filter(data, column_name %in% the certian group)

## Calculating the exceedance metric. 
# Using if statements.
calc_exceedance <- function(observe_col, predict_col, metric, temp_threshold = 25.5){
  observe_exceeds <- observe_col > temp_threshold
  predict_exceeds <- predict_col > temp_threshold
  
  if (metric == 'prop_correct') {
    return(round(sum(observe_exceeds == predict_exceeds) / n(), 2))
  } else if (metric == 'prop_false_neg') {
    return(round(sum(observe_exceeds == TRUE & predict_exceeds == FALSE) / n(), 2))
  } else if (metric == 'prop_false_pos') {
    return(round(sum(observe_exceeds == FALSE & predict_exceeds == TRUE) / n(), 2))
  }
  else if (metric == 'prop_true_pos'){
    return(round(sum(observe_exceeds == TRUE & predict_exceeds == TRUE) / n(), 2))
  }
  else if (metric == 'prop_true_neg') {
    return(round(sum(observe_exceeds == FALSE & predict_exceeds == FALSE) / n(), 2))
  }
}
# Implementing exceedance function 
exceeds <- rgnc_dat_filter %>%
  summarize(n = n(),
            n_exceeds = sum(temp_c > 25.5),
            process_exceeds_true = calc_exceedance(temp_c, sntemp_temp_c, 'prop_correct'),
            hybrid_exceeds_true = calc_exceedance(temp_c, rgcn2_full_temp_c, 'prop_correct'),
            process_exceeds_false_neg = calc_exceedance(temp_c, sntemp_temp_c, 'prop_false_neg'),
            hybrid_exceeds_false_neg = calc_exceedance(temp_c, rgcn2_full_temp_c, 'prop_false_neg'),
            process_exceeds_false_pos = calc_exceedance(temp_c, sntemp_temp_c, 'prop_false_pos'),
            hybrid_exceeds_false_pos = calc_exceedance(temp_c, rgcn2_full_temp_c, 'prop_false_pos'),
            process_exceeds_true_pos = calc_exceedance(temp_c, sntemp_temp_c, 'prop_true_pos'),
            hybrid_exceeds_true_pos= calc_exceedance(temp_c, rgcn2_full_temp_c, 'prop_true_pos'),
            process_exceeds_true_neg = calc_exceedance(temp_c, sntemp_temp_c, 'prop_true_neg'),
            hybrid_exceeds_true_neg = calc_exceedance(temp_c, sntemp_temp_c, 'prop_true_neg'))

