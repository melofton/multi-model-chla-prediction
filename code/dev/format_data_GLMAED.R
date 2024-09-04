# Format data GLM-AED
# Author: Mary Lofton
# Date: 18APR24

# Purpose: format inputs for GLM-AED (meteorology, inflows, outflows)

# eventually this code SHOULD pull directly from either EDI or VERA data
# products to get GLM-AED input files

# currently still in development

# load packages
library(tidyverse)
library(lubridate)
library(plotly)

# check date range of current driver files

# take-home: met and SSS inflow/outflow files already go through 2021; just need updated files for 
# weir inflow/outflow
met <- read_csv("./code/model_files/GLM-AED/prediction/inputs/met.csv") %>%
  arrange(time) %>%
  pivot_longer(AirTemp:Snow, names_to = "variable", values_to = "observation") %>%
  ggplot(aes(x = time, y = observation))+
  geom_line()+
  facet_wrap(facets = vars(variable), scales = "free")+
  theme_bw()
met
ggsave(met, filename = "./figures/GLM-AED_met_2015-2024.png", height = 9, width = 12, units = "in", 
       device = "png")

tail(met)

inf <- read_csv("./code/model_files/GLM-AED/prediction/inputs/FCR_weir_inflow_2013_2021_20240429_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
  arrange(time) %>%
  add_column(file_version = 1)
tail(inf)

sss <- read_csv("./code/model_files/GLM-AED/prediction/inputs/FCR_SSS_inflow_2013_2021_20240429_allfractions_2DOCpools.csv") %>%
  arrange(time) %>%
  add_column(file_version = 2021)
tail(sss)

out <- read_csv("./code/model_files/GLM-AED/prediction/inputs/FCR_spillway_outflow_WeirOnly_2013_2021_20220927.csv") %>%
  arrange(time) %>%
  add_column(file_version = 1)
tail(out)

# compare between 2020 and 2021 versions of weir inflow/outflow files
new_inf <- read_csv("./code/model_files/GLM-AED/calibration/inputs/FCR_weir_inflow_2013_2023_20240712_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
  arrange(time) %>%
  select(colnames(inf)[1:22],PHY_diatom, PHY_cyano, PHY_green) %>%
  rename(PHY_cold = PHY_diatom,
         PHY_Nfixer = PHY_cyano,
         PHY_hot = PHY_green) %>%
  add_column(file_version = 2)

all_inf <- bind_rows(inf, new_inf) %>%
  pivot_longer(FLOW:PHY_hot, names_to = "variable", values_to = "value") %>%
  mutate(file_version = factor(file_version, levels = c("2","1")))

inf_compare <- ggplot(all_inf, aes(x = time, y = value, group = file_version, color = file_version))+
  geom_line()+
  facet_wrap(facets = vars(variable), nrow = 19, ncol = 2, scales = "free_y")+
  theme_bw()
ggsave(inf_compare, filename = "./figures/compareGLMInflowFiles.png",
       device = "png", height = 18, width = 9, units = "in")
# I think the issue in 2022 might be due to higher inflow N from BVR drawdown

# same for sss inflow
new_sss <- read_csv("./code/model_files/GLM-AED/prediction/inputs/FCR_SSS_inflow_2013_2023_20240510_allfractions_2DOCpools.csv") %>%
  arrange(time) %>%
  select(colnames(sss)[1:22],PHY_diatom, PHY_cyano, PHY_green) %>%
  rename(PHY_cold = PHY_diatom,
         PHY_Nfixer = PHY_cyano,
         PHY_hot = PHY_green) %>%
  add_column(file_version = 2023) 

all_sss <- bind_rows(sss, new_sss) %>%
  pivot_longer(FLOW:PHY_Nfixer, names_to = "variable", values_to = "value") %>%
  mutate(file_version = factor(file_version))

sss_compare <- ggplot(all_sss, aes(x = time, y = value, group = file_version, color = file_version))+
  geom_line()+
  facet_wrap(facets = vars(variable), nrow = 19, ncol = 2, scales = "free_y")+
  theme_bw()
ggsave(sss_compare, filename = "./figures/compareGLMSSSInflowFiles.png",
       device = "png", height = 18, width = 9, units = "in")

new_out <- read_csv("./code/model_files/GLM-AED/prediction/inputs/FCR_spillway_outflow_WeirOnly_2013_2023_20240530.csv") %>%
  arrange(time) %>%
  add_column(file_version = 2023) 

all_out <- bind_rows(new_out, out) %>%
  mutate(file_version = factor(file_version))

out_compare <- ggplot(all_out, aes(x = time, y = FLOW, group = file_version, color = file_version))+
  geom_line()+
  theme_bw()
out_compare
ggsave(out_compare, filename = "./figures/compareGLMOutflowFiles.png",
       device = "png", height = 3, width = 5, units = "in")
# overall these look good, only major differences from 2020 to 2021 are:
#'1. adjustments to peak inflow events (peaks in 2021 are slightly lower)

# adjust column names of inflow files to match current phyto calibration

updated_sss <- new_sss %>%
  rename(PHY_hot = PHY_green,
         PHY_cold = PHY_diatom,
         PHY_Nfixer = PHY_cyano,
         PHY_hot_IN = PHY_green_IN,
         PHY_cold_IN = PHY_diatom_IN,
         PHY_Nfixer_IN = PHY_cyano_IN,
         PHY_hot_IP = PHY_green_IP,
         PHY_cold_IP = PHY_diatom_IP,
         PHY_Nfixer_IP = PHY_cyano_IP) %>%
  select(-file_version, -PHY_hot_IN, -PHY_hot_IP, -PHY_cold_IN, -PHY_cold_IP,
         -PHY_Nfixer_IN, -PHY_Nfixer_IP)
colnames(updated_sss)
updated_sss <- updated_sss[,c(1:22, 24, 25, 23)]

updated_inf <- new_inf %>%
  rename(PHY_hot = PHY_green,
         PHY_cold = PHY_diatom,
         PHY_Nfixer = PHY_cyano,
         PHY_hot_IN = PHY_green_IN,
         PHY_cold_IN = PHY_diatom_IN,
         PHY_Nfixer_IN = PHY_cyano_IN,
         PHY_hot_IP = PHY_green_IP,
         PHY_cold_IP = PHY_diatom_IP,
         PHY_Nfixer_IP = PHY_cyano_IP) %>%
  select(colnames(updated_sss)) 
colnames(updated_inf)
updated_inf <- updated_inf[,c(1:22, 24, 25, 23)]

write.csv(updated_sss, file = "./model_output/GLM-AED/inputs/FCR_SSS_inflow_2013_2021_20240429_allfractions_2DOCpools.csv", row.names = FALSE)
write.csv(updated_inf, file = "./model_output/GLM-AED/inputs/FCR_weir_inflow_2013_2021_20240429_allfractions_2poolsDOC_1dot5xDOCr.csv", row.names = FALSE)


#'Function to format data for updating chl-a initial conditions during
#'prediction period
#'@param res_url url to in situ reservoir targets data for VERA

#load packages
library(tidyverse)
library(lubridate)

format_data_GLMAED <- function(res_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz",
                            start_date = "2018-08-06",
                            end_date = "2023-12-31"){
  
  #get list of dates
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)
  daily_dates <- tibble(seq.Date(from = as.Date(start_date), to = as.Date(end_date), by = "day"))
  colnames(daily_dates)[1] <- "datetime"
  
  ##EXO----
  
  #message
  message("interpolating exo")
  chla <- read_csv(res_url) %>%
    filter(site_id == "fcre" & variable == "Chla_ugL_mean" & year(datetime) %in% c(2018:2023)) %>%
    select(datetime, observation) %>%
    rename(Chla_ugL_mean = observation) %>%
    filter(datetime >= start_date)
  
  #interpolation
  chla2 <- interpolate(daily_dates = daily_dates,
                       data = chla,
                       variables = c("Chla_ugL_mean"),
                       method = "linear")
  
  #format final data frame
  df.out <- chla2 %>%
    select(datetime, Chla_ugL_mean, Flag_Chla_ugL_mean)
  
  return(df.out)
}

dat_GLMAED <- format_data_GLMAED()
write.csv(dat_GLMAED, "./data/data_processed/GLMAED.csv",row.names = FALSE)

### checking on alternative options for met
url3 <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=PT1H/hourly-met-targets.csv.gz"
met_targets <- read_csv(url3, show_col_types = FALSE) %>%
  pivot_wider(names_from = "variable", values_from = "observation") 
check <- met_targets %>%
  filter(is.na(Rain_mm_sum))

plot(met_targets$datetime, met_targets$Rain_mm_sum)


## increasing inflow to try to mirror what happened during drawdown
inf <- read_csv("./code/model_files/GLM-AED/prediction_test_10xInflow/inputs/FCR_weir_inflow_2013_2023_20240712_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
  arrange(time) %>%
  mutate(FLOW = ifelse((time >= "2022-05-18" & time <= "2022-07-05"),FLOW*10,FLOW))
plot(inf$time, inf$FLOW)

out <- read_csv("./code/model_files/GLM-AED/prediction_test_10xInflow/inputs/FCR_spillway_outflow_WeirOnly_2013_2023_20240712.csv") %>%
  arrange(time) %>%
  mutate(FLOW = ifelse((time >= "2022-05-18" & time <= "2022-07-05"),FLOW*10,FLOW))

write.csv(inf, "./code/model_files/GLM-AED/prediction_test_10xInflow/inputs/fake_inf.csv", row.names = FALSE)
write.csv(out, "./code/model_files/GLM-AED/prediction_test_10xInflow/inputs/fake_out.csv", row.names = FALSE)

## testing hypothesis that blowing out weir during drawdown is the problem
inf <- read_csv("./code/model_files/GLM-AED/prediction_test_10xInflow/inputs/FCR_weir_inflow_2013_2023_20240712_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
  arrange(time) %>%
  mutate(FLOW = ifelse((time >= "2022-05-18" & time <= "2022-07-05"),FLOW*100,FLOW))
plot(inf$time, inf$FLOW)

out <- read_csv("./code/model_files/GLM-AED/prediction_test_10xInflow/inputs/FCR_spillway_outflow_WeirOnly_2013_2023_20240712.csv") %>%
  arrange(time) %>%
  mutate(FLOW = ifelse((time >= "2022-05-18" & time <= "2022-07-05"),FLOW*100,FLOW))

write.csv(inf, "./code/model_files/GLM-AED/prediction_test_100xInflow/inputs/fake_inf.csv", row.names = FALSE)
write.csv(out, "./code/model_files/GLM-AED/prediction_test_100xInflow/inputs/fake_out.csv", row.names = FALSE)

## testing hypothesis that high turbidity during drawdown is the problem
inf <- read_csv("./code/model_files/GLM-AED/prediction_test_100xOGM_docr/inputs/FCR_weir_inflow_2013_2023_20240712_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
  arrange(time) %>%
  mutate(OGM_docr = ifelse((time >= "2022-05-18" & time <= "2022-07-05"),OGM_docr*100,OGM_docr)) %>%
  mutate(FLOW = ifelse((time >= "2022-05-18" & time <= "2022-07-05"),FLOW*100,FLOW))
plot(inf$time, inf$FLOW)
plot(inf$time, inf$OGM_docr)

write.csv(inf, "./code/model_files/GLM-AED/prediction_test_100xOGM_docr/inputs/fake_inf.csv", row.names = FALSE)

check <- read_csv("./code/model_files/GLM-AED/prediction_test_100xOGM_docr/inputs/fake_out.csv")
plot(check$time, check$FLOW)
