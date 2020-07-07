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
rgnc_by_seg <- rgnc_dat_filter %>% group_by(seg_id_nat)

## Absolute Residual Error: calculating Mean Absolute Error (MAE) metric to compare both models prediction data vs observed data. We found the mae by finding the sum of the absolute value difference in predicted and observed temperature. Then divided the sum by the n row in grouped data.

cal_mae <- function(observe_data, predict_data) {
    mae = round((mean(abs(observe_data - predict_data), na.rm = TRUE )), digits = 2)
    return(mae)
}
mae <- rgnc_by_seg %>%  
  summarize(MAE_Process_Model = cal_mae(temp_c, sntemp_temp_c),
            MAE_Hybrid_Model = cal_mae(temp_c, rgcn2_full_temp_c))

# Absolute Residual Error: Root Mean Square Error (RMSE) square the difference in predicted and observed temperature.  sum the squared values, then divided by n of row. Finally,took the square root
cal_rmse <- function(observe_data, predict_data){
  rmse <- round(sqrt(mean((observe_data - predict_data) ^2, na.rm = TRUE )), digits = 2)
  return(rmse)
}
rmse <- rgnc_by_seg %>%
  summarize(RMSE_Process_Model = cal_rmse(temp_c, sntemp_temp_c),
            RMSE_Hybrid_Model = cal_rmse(temp_c, rgcn2_full_temp_c))

# Relative Parameter: Mean Absolute Relative Error (MARE) metric to compare both models prediction data vs observed data. We found MARE by: 1) dividing the difference between the predicted and observed data by the observed measurements. 2) sum the division answer. 3) divide by number of rows in grouped data.  
## mare metric when removing inf terms from ratio of temp diff and observed temp. 
cal_mare <- function(observe_data, predict_data){
  mare <- round(mean((abs(observe_data - predict_data) / observe_data), na.rm = TRUE), digits = 2)
  return(mare)
}  
mare <- rgnc_by_seg %>%
  mutate(temp_c = ifelse( temp_c %in% 0, 0.1, temp_c)) %>%
  summarize( MARE_Process_Model = cal_mare(temp_c, sntemp_temp_c),
             MARE_Hybrid_Model = cal_mare(temp_c, rgcn2_full_temp_c))

compare_metric <- plyr:: join_all(list(mae, rmse, mare),  by = 'seg_id_nat', type = 'left')
summary(compare_metric) 
