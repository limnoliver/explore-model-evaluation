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

## Plotting the temperature
par(mfrow = c(3,1))
hist(rgnc_dat_filter$sntemp_temp_c, xlab = "Uncalibrated Process Model Prediction")
hist(rgnc_dat_filter$rgcn2_full_temp_c, xlab = "Hybrid Model Prediction")
hist(rgnc_dat_filter$temp_c, xlab = "Observed Temperature")

### Mean Absolute Error
## created a column for the difference in temperature measurements. then found the mae by summing the error and 
#dividing by n of row for the filtered data. 

rgnc_dat_filter$mae_process_error <- abs(rgnc_dat_filter$temp_c - rgnc_dat_filter$sntemp_temp_c) 
mae_by_seg <- rgnc_dat_filter %>% group_by(seg_id_nat) %>%    
  summarize(mae = sum(rgnc_dat_filter$mae_error)/ nrow(rgnc_dat_filter))
    

