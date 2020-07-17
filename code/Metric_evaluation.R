#Implementing the metrics function to evaluate the process and hybrid models. 
#an example of paired predictions and observations
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
print(rgnc_by_seg, n = 5)
#grouping data by seg_id and date.
rgnc_by_seg_date <- rgnc_dat_filter %>% group_by(seg_id_nat, year = lubridate::year(date)) 
summary(rgnc_by_seg_date)

# Mean Absolute Error (MAE) 
mae <- rgnc_by_seg %>%  
  summarize(MAE_Process_Model = calc_mae(temp_c, sntemp_temp_c),
            MAE_Hybrid_Model = calc_mae(temp_c, rgcn2_full_temp_c))

# Root Mean Square Error (RMSE) 
rmse <- rgnc_by_seg %>%
  summarize(RMSE_Process_Model = calc_rmse(temp_c, sntemp_temp_c),
            RMSE_Hybrid_Model = calc_rmse(temp_c, rgcn2_full_temp_c))

# Mean Absolute Relative Error (MARE) 
mare <- rgnc_by_seg %>%
  mutate(temp_c = ifelse( temp_c %in% 0, 0.1, temp_c)) %>%
  summarize( MARE_Process_Model = calc_mare(temp_c, sntemp_temp_c),
             MARE_Hybrid_Model = calc_mare(temp_c, rgcn2_full_temp_c))

compare_metric <- plyr:: join_all(list(mae, rmse, mare),  by = 'seg_id_nat', type = 'left')
summary(compare_metric) 

#Finding the max temperature for each segment and year
max_process_metrics = calc_max_timing(data_in = rgnc_by_seg_date, 
                                           observe_col = temp_c, predict_col = sntemp_temp_c, 
                                           date_col = date)
max_hybrid_metrics = calc_max_timing(data_in = rgnc_by_seg_date, 
                                           observe_col = temp_c, predict_col = rgcn2_full_temp_c, 
                                           date_col = date)

# calculating Nash coefficient of efficiency (CE;Nash and Sutcliffe) values: 
nse_metric =rgnc_by_seg %>%
  summarize(nse_process = calc_nash(temp_c, sntemp_temp_c),
            nse_hybrid = calc_nash(temp_c, rgcn2_full_temp_c))
# To view the data associated with this segment we can filter through the dataset:
View(filter(rgnc_dat_filter, seg_id_nat %in% 1549)) #filter(data, column_name %in% the certian group)

# Implementing exceedance function 
exceeds <- rgnc_dat_filter %>%
  summarize(n = n(),
            n_exceeds = sum(temp_c > 25.5),
            process_exceeds_false_neg = calc_exceedance(temp_c, sntemp_temp_c, 'prop_false_neg'),
            hybrid_exceeds_false_neg = calc_exceedance(temp_c, rgcn2_full_temp_c, 'prop_false_neg'),
            process_exceeds_false_pos = calc_exceedance(temp_c, sntemp_temp_c, 'prop_false_pos'),
            hybrid_exceeds_false_pos = calc_exceedance(temp_c, rgcn2_full_temp_c, 'prop_false_pos'),
            process_exceeds_true_pos = calc_exceedance(temp_c, sntemp_temp_c, 'prop_true_pos'),
            hybrid_exceeds_true_pos= calc_exceedance(temp_c, rgcn2_full_temp_c, 'prop_true_pos'),
            process_exceeds_true_neg = calc_exceedance(temp_c, sntemp_temp_c, 'prop_true_neg'),
            hybrid_exceeds_true_neg = calc_exceedance(temp_c, sntemp_temp_c, 'prop_true_neg'))

