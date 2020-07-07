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
summary(rgnc_dat_filter)
rgnc_sig_freq_table <- table(rgnc_dat_filter$seg_id_nat) # check the number of segments in the data
rgnc_site_freq_table <- table(rgnc_dat_filter$site_id)  # checking the number of sit_id 

## created a column for the difference in temperature measurements. then grouped data based on segment id. 
rgnc_dat_filter$process_mod_tem_diff <- rgnc_dat_filter$temp_c - rgnc_dat_filter$sntemp_temp_c
rgnc_dat_filter$hybrid_mod_tem_diff <- rgnc_dat_filter$temp_c - rgnc_dat_filter$rgcn2_full_temp_c
number_seg <- length(rgnc_sig_freq_table) ## We will compare the number of segments to the nrow in each metrics. 
number_site <- length(rgnc_site_freq_table)  # to check the number of sites. 
rgnc_by_seg <- rgnc_dat_filter %>% group_by(seg_id_nat)

## Absolute Residual Error: calculating Mean Absolute Error (MAE) metric to compare both models prediction data vs observed data. We found the mae by finding the sum of the absolute value difference in predicted and observed temperature. Then divided the sum by the n row in grouped data.
mae_metric <- rgnc_by_seg %>%   summarize( mae_process = round( sum( abs( process_mod_tem_diff), na.rm = TRUE )/ n(), 2),
                                           mae_hybrid = round( sum( abs( hybrid_mod_tem_diff), na.rm = TRUE )/ n(), 2) )

# Absolute Residual Error: Root Mean Square Error (RMSE) square the difference in predicted and observed temperature.  sum the squared values, then divided by n of row. Finally,took the square root
rmse_metric <- rgnc_by_seg %>% summarize( rmse_process = round( sqrt( sum( process_mod_tem_diff ^2, na.rm = TRUE ) / n() ), 2), 
                                         rmse_hybrid = round( sqrt( sum( hybrid_mod_tem_diff ^2, na.rm = TRUE ) / n() ), 2) ) 

# Relative Parameter: Mean Absolute Relative Error (MARE) metric to compare both models prediction data vs observed data. We found MARE by: 1) dividing the difference between the predicted and observed data by the observed measurements. 2) sum the division answer. 3) divide by number of rows in grouped data.  
mare_metric <- rgnc_by_seg %>% 
  mutate(proc_rel_abs_error = abs (process_mod_tem_diff) / temp_c ) %>% 
  mutate(hyp_rel_abs_error = abs (hybrid_mod_tem_diff) / temp_c) %>%
  summarize( mare_process = round ( sum ( proc_rel_abs_error, na.rm = TRUE) / n(), 2),
             mare_hybrid = round( sum( hyp_rel_abs_error, na.rm = TRUE ) / n(), 2) )

## mare when turning temp_c values that are 0 into something really small, like 0.001.
rgnc_seg_wo_zero <- rgnc_by_seg
#rgnc_seg_wo_zero$temp_c[rgnc_seg_wo_zero$temp_c %in% 0] <-0.1
#which(rgnc_seg_wo_zero$temp_c == 0)
mare_metric_wo_zero <-   rgnc_seg_wo_zero %>% 
  mutate(temp_c = ifelse( temp_c %in% 0, 0.1, temp_c) ) %>%
  mutate(process_rel_abs_error_wo = abs(process_mod_tem_diff) / temp_c ) %>%
  mutate(hyp_rel_abs_error_wo = abs(hybrid_mod_tem_diff) / temp_c) %>% 
  summarize( mare_process_wo_zero = round( sum( process_rel_abs_error_wo, na.rm = TRUE) / n(), 2),
            mare_hybrid_wo_zero = round( sum( hyp_rel_abs_error_wo, na.rm = TRUE)/ n(), 2) )

## mare metric when removing inf terms from ratio of temp diff and observed temp. 
mare_metric_wo_inf <- rgnc_by_seg %>%
  mutate(proc_rel_abs_error_wo_inf = abs (process_mod_tem_diff) / temp_c ) %>% 
  mutate(hyp_rel_abs_error_wo_inf = abs (hybrid_mod_tem_diff) / temp_c) %>%
  filter_all(all_vars(!is.infinite(.) ) ) %>%
  summarize( mare_process_wo_inf = round( sum( proc_rel_abs_error_wo_inf, na.rm = TRUE) / n(), 2),
             mare_hybrid_wo_inf = round( sum( hyp_rel_abs_error_wo_inf, na.rm = TRUE) / n(), 2) )
           

compare_mare <- plyr:: join_all( list(mare_metric, mare_metric_wo_zero, mare_metric_wo_inf), by = 'seg_id_nat', type = 'left')
compare_metric <- plyr:: join_all( list (mae_metric, rmse_metric, mare_metric_wo_inf),  by = 'seg_id_nat', type = 'left')
#plot each metric with proc error across from hybrid error.
par(mfrow = c (3,2) )
plot(mae_metric$seg_id_nat, mae_metric$mae_process, main = 'Process Model Evaluation', xlab = 'Segment ID', ylab = 'MAE') 
plot(mae_metric$seg_id_nat, mae_metric$mae_hybrid, main = 'Hybrid Model Evaluation', xlab = 'Segment ID', ylab = 'MAE')
plot(rmse_metric$seg_id_nat, rmse_metric$rmse_process, main = 'Process Model Evaluation', xlab = 'Segment ID', ylab = 'RMSE')
plot(rmse_metric$seg_id_nat, rmse_metric$rmse_hybrid, main = 'Hybrid Model Evaluation', xlab = 'Segment ID', ylab = 'RMSE')
plot(mare_metric_wo_zero$seg_id_nat, mare_metric_wo_zero$mare_process_wo_zero, main = 'Process Model Evaluation', xlab = 'Segment ID', ylab = 'MARE')
plot(mare_metric_wo_zero$seg_id_nat, mare_metric_wo_zero$mare_hybrid_wo_zero, main = 'Hybrid Model Evaluation', xlab = 'Segment ID', ylab = 'MARE')

#Creating histogram to compare the different errors we calculated. 
hist(mae_metric$mae_process, main = 'Process Model Evaluation', xlab = 'MAE')
hist(mae_metric$mae_hybrid, main = 'Hybrid Model Evaluation', xlab = 'MAE')
hist(rmse_metric$rmse_process, main = 'Process Model Evaluation', xlab = 'RMSE')
hist(rmse_metric$rmse_hybrid, main = 'Hybrid Model Evaluation', xlab = 'RMSE')
hist(mare_metric_wo_zero$mare_process_wo_zero, main = 'Process Model Evaluation', xlab = 'MARE')
hist(mare_metric_wo_zero$mare_hybrid_wo_zero, main = 'Hyprid Model Evaluation', xlab = 'MARE')

# Box-Plot to present summaries of Error Metrics
par(mfrow = c (1, 1) )
boxplot(compare_metric[2: 7], col = c("red", "blue") )
summary(compare_metric)                                                          
