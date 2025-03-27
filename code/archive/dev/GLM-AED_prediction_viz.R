# Visualization to illustrate initial conditions updating
# Author: Mary Lofton
# Date: 01MAY24

# Purpose: illustrate how initial conditions are update for iterative prediction
# model runs

ExampleInitialConditionsUpdating <- function(){

source("./code/function_library/predict/GLMAED.R")

dat_GLMAED <- read_csv("./data/data_processed/GLMAED.csv")

pred_GLMAED <- GLMAED(spinup_folder = "./code/model_files/GLM-AED/spinup",
                      prediction_folder = "./code/model_files/GLM-AED/prediction",
                      rerun_spinup = FALSE,
                      spinup_dates = c('2018-04-20 12:00:00','2021-01-01 12:00:00'),
                      pred_dates = seq.Date(from = as.Date("2021-01-01"), to = as.Date("2021-01-03"), by = "day"),
                      forecast_horizon = 35,
                      wq_vars = c('OXY_oxy','CAR_dic','CAR_pH','CAR_ch4','SIL_rsi','NIT_amm','NIT_nit','PHS_frp','OGM_doc','OGM_poc','OGM_don','OGM_pon','OGM_dop','OGM_pop','OGM_docr','OGM_donr','OGM_dopr','OGM_cpom','PHY_hot','PHY_cold','PHY_Nfixer'),
                      data = dat_GLMAED,
                      phyto_nml_file = "/aed/aed2_phyto_pars_16APR24_MEL.nml")

plot_pred_GLMAED <- pred_GLMAED %>%
  mutate(reference_datetime = as.Date(reference_datetime),
         ic = ifelse(datetime == reference_datetime, prediction, NA),
         prediction = ifelse(datetime == reference_datetime, NA, prediction)) %>%
  left_join(dat_GLMAED, by = "datetime") %>%
  rename(obs = Chla_ugL_mean) %>%
  select(-Flag_Chla_ugL_mean)

p1 <- ggplot(data = plot_pred_GLMAED)+
  geom_line(aes(x = datetime, y = prediction, 
                  group = as.factor(reference_datetime),
                  color = as.factor(reference_datetime)))+
  geom_point(aes(x = datetime, y = obs), color = "black")+
  geom_point(aes(x = datetime, y = ic,
                 group = as.factor(reference_datetime),
                 color = as.factor(reference_datetime)))+
  labs(color = "Reference datetime")+
  annotate(geom="text", x=as.Date("2022-01-01 12:00:00"), y=14, label="initial\ncondition\nvalues",
           color="black", hjust = 0)+
  annotate(geom="text", x=as.Date("2022-01-20 12:00:00"), y=18.5, label="model predictions",
           color="black")+
  annotate(geom="text", x=as.Date("2022-02-06 12:00:00"), y=12, label="observations\n(not seen\nby model)",
           color="black", hjust = 1)+
  geom_segment(aes(x = as.Date("2022-01-26 12:00:00"), y = 18, 
                   xend = as.Date("2022-01-28 12:00:00"), yend = 17), 
               color = "black",
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"))+
  geom_segment(aes(x = as.Date("2022-02-03 12:00:00"), y = 13.3, 
                   xend = as.Date("2022-02-01 12:00:00"), yend = 14.8), 
               color = "black", 
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"))+
  geom_segment(aes(x = as.Date("2022-01-03 12:00:00"), y = 12.65, 
                   xend = as.Date("2022-01-02 12:00:00"), yend = 11.95), 
               color = "black",
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"))+
  ylab("Chlorophyll-a (ug/L)")+
  xlab("")+
  ggtitle("GLM-AED predictions at FCR")+
  theme_bw()

return(p1)

}
