# an example of paired predictions and observations
library(dplyr)
library(plyr)
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
  rgnc_dat_filter %>%
    mutate( diff_observed_predicted = observe_data - predict_data ) %>%
    group_by(seg_id_nat) %>%
    summarize( mae = round( mean( abs( diff_observed_predicted), na.rm = TRUE ), 2) )
}

mae_process_model <- cal_mae(rgnc_dat_filter$temp_c, rgnc_dat_filter$sntemp_temp_c)
mae_hybrid_model <- cal_mae(rgnc_dat_filter$temp_c, rgnc_dat_filter$rgcn2_full_temp_c)
mae <- left_join( mae_process_model, mae_hybrid_model, by = "seg_id_nat")
colnames(mae)[2] <- "MAE_Process_Model"
colnames(mae)[3] <- "MAE_Hybrid_Model"
# Absolute Residual Error: Root Mean Square Error (RMSE) square the difference in predicted and observed temperature.  sum the squared values, then divided by n of row. Finally,took the square root
cal_rmse <- function(observe_data, predict_data){
  rgnc_dat_filter %>%
    mutate( diff_observed_predicted = observe_data - predict_data ) %>%
    group_by(seg_id_nat) %>%
    summarize( rmse = round( sqrt( mean( diff_observed_predicted ^2, na.rm = TRUE ) ) , 2)  )
}
rmse_process_model <- cal_rmse(rgnc_dat_filter$temp_c, rgnc_dat_filter$sntemp_temp_c)
rmse_hybrid_model <- cal_rmse(rgnc_dat_filter$temp_c, rgnc_dat_filter$rgcn2_full_temp_c)
rmse <- left_join(rmse_process_model, rmse_hybrid_model, by = "seg_id_nat")
colnames(rmse)[2] <- "RMSE_Process_Model"
colnames(rmse)[3] <- "RMSE_Hybrid_Model"

# Relative Parameter: Mean Absolute Relative Error (MARE) metric to compare both models prediction data vs observed data. We found MARE by: 1) dividing the difference between the predicted and observed data by the observed measurements. 2) sum the division answer. 3) divide by number of rows in grouped data.  

## mare metric when removing inf terms from ratio of temp diff and observed temp. 

cal_mare <- function(observe_data, predict_data){
  rgnc_dat_filter %>%
    mutate( diff_observed_predicted = abs (observe_data - predict_data ) / observe_data )  %>%
    group_by("seg_id_nat") %>%
    filter_all(all_vars(!is.infinite(.) ) ) %>%
    summarize( mare = round( mean( diff_observed_predicted) , 2) )

  }         
mare_process_model <- cal_mare(rgnc_dat_filter$temp_c, rgnc_dat_filter$sntemp_temp_c)
mare_hybrid_model <- cal_mare(rgnc_dat_filter$temp_c, rgnc_dat_filter$rgcn2_full_temp_c)

#compare_metric <- plyr:: join_all( list (mae_metric, rmse_metric, mare_metric_wo_inf),  by = 'seg_id_nat', type = 'left')
