## Visualization of model evaluation metrics.

library(tidyverse)

stream_type <- readRDS('data/stream_type .RDS')
stream_type$seg_id_nat <- as.character(stream_type$seg_id_nat)

dot_size <- rgnc_dat_filter %>%
  group_by(seg_id_nat)  %>%
  summarize(n_obs = n())
# Plotting the calculated RMSE based on the stream type and number of observation for each reach. 
# left join (column) the number of observation for each reach with the calculated rmse. 
rmse_stream_type <- left_join(dot_size, rmse) %>%
  filter(!is.na(RMSE_Process_Model), !is.na(RMSE_Hybrid_Model))
# left join (column) the above with the strem_type. 
rmse_stream_type <- left_join(stream_type, rmse_stream_type) %>% 
  filter(!is.na(RMSE_Process_Model), !is.na(RMSE_Hybrid_Model))

# Plotting the calculated NSE (NASH) based on the stream type and number of observation for each reach.
# Left joining the number of observation for each reach and stream type. 
#nse_metric$seg_id_nat = as.integer(nse_metric$seg_id_nat) 
nse_stream_type <- left_join(dot_size, nse_metric) %>%
  filter(!is.na(nse_process), !is.na(nse_hybrid))
         
nse_stream_type <- left_join(stream_type, nse_stream_type) %>%
  filter(!is.na(nse_process), !is.na(nse_hybrid), 
         nse_process > 0, nse_hybrid > 0)
#finding mins and max for metrics using summary
summary(rmse_stream_type)
summary(nse_stream_type)

#plotting rmse metric
rmse_plot  <- 
  ggplot(rmse_stream_type, aes(x = RMSE_Process_Model, y = RMSE_Hybrid_Model)) +
  geom_point(aes(colour = reservoir_status, size = n_obs), alpha = 0.15) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  #geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0,9), breaks = 0:9) +
  scale_y_continuous(limits = c (0, 9), breaks = 0:9)+
  coord_fixed() + 
    labs(
    title = "Comparison Between Process and Hybrid Model RMSEs",
    x = "Process Model RMSE",
    y = "Hybrid Model RMSE"
  ) +
   scale_color_manual(values = c("grey27", "red")) + 
   theme_bw() +  # to remove the gray background.
    theme(plot.title = element_text(hjust =  0.15,vjust = 0.50, size = 10, face = "bold"),
          axis.title = element_text(size = 6, face = "bold"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 6))

#plotting nse metric
nse_plot  <-
  ggplot(nse_stream_type, aes(x = nse_process, y = nse_hybrid)) +
    geom_point(aes(colour = reservoir_status, size = n_obs), alpha = 0.15) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_x_continuous(limits = c(0, 1.25)) +
    scale_y_continuous(limits = c(0, 1.25)) +
    coord_fixed() +
    labs(
      title = "Comparison Between Process and Hybrid Model NSEs",
      x = "Process Model NSE",
      y = "Hybrid Model NSE"
    ) +
    scale_color_manual(values = c("grey27", "red")) + 
    theme_bw() +   
    theme(plot.title = element_text(hjust =  0.15, vjust = 0.70, size = 10, face = "bold"),
          axis.title = element_text(size = 6, face = "bold"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 6))

ggsave("rmse_plot.png", rmse_plot, height = 6, width = 4)
ggsave("nse_plot.png", nse_plot, height = 6, width = 4)
