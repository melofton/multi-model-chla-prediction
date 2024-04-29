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
met <- read_csv("./model_output/GLM-AED/inputs/met_avg_filtered.csv") %>%
  arrange(time) 
tail(met)

sss <- read_csv("./model_output/GLM-AED/inputs/inflow_SSS_K_elevation_waterquality.csv") %>%
  arrange(time) 
tail(sss)
ggplot(data = sss, aes(x = time, y = FLOW))+
  geom_line()+
  theme_bw()

inf <- read_csv("./model_output/GLM-AED/inputs/FCR_weir_inflow_2013_2020_20220411_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
  arrange(time) %>%
  add_column(file_version = 2020)
tail(inf)

out <- read_csv("./model_output/GLM-AED/inputs/FCR_spillway_outflow_WeirOnly_2013_2020_20211102.csv") %>%
  arrange(time) %>%
  add_column(file_version = 2020)
tail(out)

out_sss <- read_csv("./model_output/GLM-AED/inputs/outflow_K.csv") %>%
  arrange(time)
tail(out_sss)
ggplot(data = out_sss, aes(x = time, y = FLOW))+
  geom_line()+
  theme_bw()

# compare between 2020 and 2021 versions of weir inflow/outflow files
new_inf <- read_csv("./model_output/GLM-AED/inputs/FCR_weir_inflow_2013_2021_20220927_allfractions_2poolsDOC_1dot5xDOCr.csv") %>%
  arrange(time) %>%
  add_column(file_version = 2021) 

all_inf <- bind_rows(inf, new_inf) %>%
  pivot_longer(FLOW:BIV_filtfrac, names_to = "variable", values_to = "value") %>%
  mutate(file_version = factor(file_version))

inf_compare <- ggplot(all_inf, aes(x = time, y = value, group = file_version, color = file_version))+
  geom_line()+
  facet_wrap(facets = vars(variable), nrow = 19, ncol = 2, scales = "free_y")+
  theme_bw()
ggsave(inf_compare, filename = "./figures/compareGLMInflowFiles.png",
       device = "png", height = 18, width = 9, units = "in")
# overall these look good, only major differences from 2020 to 2021 are:
#'1. adjustments to peak inflow events (peaks in 2021 are slightly lower)
#'2. adjustment to CAR_pH (6.9 in 2020 to 6.7 in 2021)
#'3. addition of elevation column
#'4. turning on passive tracer

# same for sss inflow
new_sss <- read_csv("./model_output/GLM-AED/inputs/FCR_SSS_inflow_2013_2021_20220413_allfractions_2DOCpools.csv") %>%
arrange(time) %>%
  add_column(file_version = 2021) 

all_sss <- bind_rows(sss, new_sss) %>%
  pivot_longer(FLOW:BIV_filtfrac, names_to = "variable", values_to = "value") %>%
  mutate(file_version = factor(file_version))

sss_compare <- ggplot(all_sss, aes(x = time, y = value, group = file_version, color = file_version))+
  geom_line()+
  facet_wrap(facets = vars(variable), nrow = 19, ncol = 2, scales = "free_y")+
  theme_bw()
ggsave(inf_compare, filename = "./figures/compareGLMSSSInflowFiles.png",
       device = "png", height = 18, width = 9, units = "in")
# overall these look good, only major differences from 2020 to 2021 are:
#'1. adjustments to peak inflow events (peaks in 2021 are slightly lower)
#'2. adjustment to CAR_pH (6.9 in 2020 to 6.7 in 2021)
#'3. addition of elevation column
#'4. turning on passive tracer

new_out <- read_csv("./model_output/GLM-AED/inputs/FCR_spillway_outflow_WeirOnly_2013_2021_20220927.csv") %>%
  arrange(time) %>%
  add_column(file_version = 2021) 

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
