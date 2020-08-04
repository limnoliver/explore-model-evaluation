################ function to read RDS file and modify the data to remove all NA's

read_rds <- function(dat_in,  ...){
  dat_mod <- readRDS(file = dat_in)  %>%
    filter(!is.na(obs_temp_c), !is.na(pred_temp_c))
  return(dat_mod)
} 
