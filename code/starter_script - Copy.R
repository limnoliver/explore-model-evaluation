# an example of paired predictions and observations
library(dplyr)
dat <- readRDS('data/predicted_observed_temps.rds')
head(dat)

# note that SNTemp predictions (uncalibrated process model)
# have predictions for all places/times
# here you just have the test period from the hybrid model (rgcn2_full_temp_c)
# and sparse observations (some reaches measured, some not)

summary(dat)
str(dat)
# example RMSE calculation
# https://github.com/USGS-R/delaware-model-prep/blob/master/3_predictions/src/munge_predictions.R#L103-L113

# removing rows with na values. 
rgnc_dat_filter <- dat %>% filter(!is.na(temp_c), !is.na(rgcn2_full_temp_c))
str(rgnc_dat_filter)
summary(rgnc_dat_filter)

## Absolute Residual Error: calculating Mean Absolute Error (MAE) metric to compare both models prediction data vs observed data. We found the mae by finding the sum of the absolute value difference in predicted and observed temperature. Then divided the sum by the n row in grouped data.

cal_mae <- function(observe_data, predict_data) {
    mae = (mean(abs(observe_data - predict_data), na.rm = TRUE ))
    return(mae)
}

mae_process_model <- rgnc_dat_filter %>% 
  group_by(seg_id_nat) %>%
  summarize(mae = round(cal_mae(temp_c, sntemp_temp_c), 2))

mae_hybrid_model <- rgnc_dat_filter %>% 
  group_by(seg_id_nat) %>% 
  summarize(mae = round(cal_mae(temp_c, rgcn2_full_temp_c), 2))

# Absolute Residual Error: Root Mean Square Error (RMSE) square the difference in predicted and observed temperature.  sum the squared values, then divided by n of row. Finally,took the square root
cal_rmse <- function(observe_data, predict_data){
  rmse <- sqrt(mean((observe_data - predict_data) ^2, na.rm = TRUE ))
  return(rmse)
}
rmse_process_model <- rgnc_dat_filter %>%
  group_by(seg_id_nat) %>%
  summarize(rmse = round( cal_rmse(temp_c, sntemp_temp_c), 2))

rmse_hybrid_model <- rgnc_dat_filter %>%
  group_by(seg_id_nat) %>%
  summarize(rmse = round( cal_rmse(temp_c, rgcn2_full_temp_c), 2))

# Relative Parameter: Mean Absolute Relative Error (MARE) metric to compare both models prediction data vs observed data. We found MARE by: 1) dividing the difference between the predicted and observed data by the observed measurements. 2) sum the division answer. 3) divide by number of rows in grouped data.  

## mare metric when removing inf terms from ratio of temp diff and observed temp. 
cal_mare <- function(observe_data, predict_data){
  mare <- mean((abs(observe_data - predict_data) / observe_data), na.rm = TRUE)
  return(mare)
}  

mare_process_model <- rgnc_dat_filter %>%
  group_by(seg_id_nat) %>%
  mutate(temp_c = ifelse( temp_c %in% 0, 0.1, temp_c) ) %>%
  #filter_all(all_vars(!is.infinite(.) ) ) %>%
  summarize( mare = round( cal_mare(temp_c, sntemp_temp_c) , 2) )

mare_hybrid_model <- rgnc_dat_filter %>%
  group_by(seg_id_nat) %>%
  mutate(temp_c = ifelse( temp_c %in% 0, 0.1, temp_c) ) %>%
  #filter_all(all_vars(!is.infinite(.) ) ) %>%
  summarize( mare = round( cal_mare(temp_c, rgcn2_full_temp_c) , 2) )


compare_metric <- plyr:: join_all( list (mae_process_model, mae_hybrid_model, rmse_process_model, rmse_hybrid_model, mare_process_model, mare_hybrid_model),  by = 'seg_id_nat', type = 'left')
colnames(compare_metric)[2] <- "MAE_Process_Model"
colnames(compare_metric)[3] <- "MAE_Hybrid_Model"
colnames(compare_metric)[4] <- "RMSE_Process_Model"
colnames(compare_metric)[5] <- "RMSE_Hybrid_Model"
colnames(compare_metric)[6] <- "MARE_Process_Model"
colnames(compare_metric)[7] <- "MARE_Hybrid_Model"
