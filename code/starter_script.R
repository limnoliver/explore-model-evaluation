# an example of paired predictions and observations

dat <- readRDS('data/predicted_observed_temps.rds')
head(dat)

# note that SNTemp predictions (uncalibrated process model)
# have predictions for all places/times
# here you just have the test period from the hybrid model (rgcn2_full_temp_c)
# and sparse observations (some reaches measured, some not)

summary(dat)

# example RMSE calculation
# https://github.com/USGS-R/delaware-model-prep/blob/master/3_predictions/src/munge_predictions.R#L103-L113

# 
