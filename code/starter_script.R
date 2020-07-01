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

# reomivg rows with na values. 
rgnc_dat_filter <- dat %>% filter(!is.na(temp_c), !is.na(rgcn2_full_temp_c))
str(rgnc_dat_filter)
summary(rgnc_dat_filter)
rgnc_sig_freq_table <- table(rgnc_dat_filter$seg_id_nat) # check the number of segments in the data
rgnc_site_freq_table <- table(rgnc_dat_filter$site_id)  # checking the number of sit_id 

## created a column for the difference in temperature measurements. then grouped data based on segment id. 
rgnc_dat_filter$process_mod_tem_diff <- rgnc_dat_filter$temp_c - rgnc_dat_filter$sntemp_temp_c
rgnc_dat_filter$hyprid_mod_tem_diff <- rgnc_dat_filter$sntemp_temp_c - rgnc_dat_filter$rgcn2_full_temp_c
rgnc_by_seg <- rgnc_dat_filter %>% group_by(seg_id_nat)

## Absolute Residual Error: calculating Mean Absolute Error (MAE) metric to compare both models prediction data vs observed data. We found the mae by finding the sum of the absolute value difference in predicted and observed temperature. Then divided the sum by the n row in grouped data.
mae_metric <- rgnc_by_seg %>%   summarize(mae_process = round( sum( abs( rgnc_by_seg$process_mod_tem_diff) )/ nrow(rgnc_by_seg), 2),
                            mae_hyprid = round( sum( abs( rgnc_by_seg$hyprid_mod_tem_diff) )/ nrow(rgnc_by_seg), 2) )

# Absolute Residual Error: Root Mean Square Error (RMSE) square the difference in predicted and observed temperature.  sum the squared values, then divided by n of row. Finally,took the square root
rmse_metric <- rgnc_by_seg %>% summarize(rmse_process = round( sqrt( sum(process_mod_tem_diff ^2) / nrow(rgnc_by_seg) ), 2), 
                                         rmse_hyprid = round( sqrt( sum(hyprid_mod_tem_diff ^2) / nrow(rgnc_by_seg)), 2) ) 

# Relative Parameter: Mean Absolute Relative Error (MARE) metric to compare both models prediction data vs observed data. We found MARE by: 1) dividing the difference between the predicted and observed data by the observed measurements. 2) sum the division answer. 3) divide by number of rows in grouped data.  
mare_metric <- rgnc_by_seg %>% summarize( mare_process = round( mean ( abs( rgnc_by_seg$process_mod_tem_diff) / rgnc_by_seg$temp_c ), 2 ) )
## mare_ process can't sum the values created by  `abs( rgnc_by_seg$process_mod_tem_diff) / rgnc_by_seg$temp_c )` 



    

                                                         
                                                          