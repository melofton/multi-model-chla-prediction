# Analyze model output
# Author: Mary Lofton
# Date: 08OCT24

# Purpose: determine how model performance differs under different thermal 
# stratification conditions and during period of increasing/decreasing chl-a

# Tasks:
#' 1. calculate Schmidt stability
#' 2. determine periods that are mixed, stratified, or exhibit increasing/
#' decreasing stratification
#' 3. calculate daily rate of change in chl-a and plot as histogram
#' 4. determine thresholds for 10th and 90th quantiles for rate-of-change
#' values for chl-a
#' 

# Load packages
install.packages("rLakeAnalyzer")
library(rLakeAnalyzer)
library(tidyverse)
library(lubridate)

# assign important vars
pred_dates <- seq.Date(from = as.Date("2022-01-01"), to = as.Date("2023-11-26"), by = "day")

# 1. calculate Schmidt stability

#'@param res_url url to in situ reservoir targets data for VERA
res_url = "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
res <- read_csv(res_url) %>%
  filter(site_id == "fcre" & variable == "Temp_C_mean") %>%
  arrange(datetime, depth_m) %>%
  select(datetime, depth_m, observation) %>%
  filter(date(datetime) %in% pred_dates) %>%
  pivot_wider(names_from = depth_m, values_from = observation) 
colnames(res)[2:12] <- paste0("wtr_",colnames(res)[2:12])
res <- res[,c(1:3,12,4:11)]

bth <- read_csv("./data/data_raw/Bathymetry_comb.csv") %>%
  filter(Reservoir == "FCR") %>%
  select(Depth_m, SA_m2)
colnames(bth) <- c("depths","areas")

schmidt <- ts.schmidt.stability(wtr = res, bathy = bth, na.rm = TRUE) %>%
  mutate(datetime = date(datetime)) %>%
  filter(!datetime == "2022-05-24") %>% # bad data day; Schmidt stability of 0
  mutate(strat_bin = ifelse(schmidt.stability <= 1, "mixed",
                                ifelse(schmidt.stability >= 40, "stratified",
                                       ifelse((schmidt.stability > 1 & schmidt.stability < 40 & month(datetime) %in% c(1:7)),"onset","decline"))))

#' 2. determine periods that are mixed, stratified, or exhibit increasing/
#' decreasing stratification

ggplot(data = schmidt, aes(x = datetime, y = schmidt.stability, color = strat_bin))+
  geom_point()+
  theme_classic()
ggplot(data = schmidt, aes(x = schmidt.stability, group = strat_bin, fill = strat_bin))+
  geom_histogram(color = "black", binwidth = 1, boundary = 0)+
  theme_bw()

#' 3. calculate daily rate of change in chl-a and plot as histogram
obs <- read_csv("./data/data_processed/chla_obs.csv") %>%
  mutate(delta = c(NA,diff(Chla_ugL_mean, na.rm = TRUE)))
dens <- density(obs$delta, na.rm = TRUE)
q5 <- quantile(obs$delta, 0.05, na.rm = TRUE)
q95 <- quantile(obs$delta, 0.95, na.rm = TRUE)
dd <- with(dens,data.frame(x,y))
ggplot(data = dd, aes(x = x, y = y))+
  geom_line()+
  geom_ribbon(data=subset(dd,x>q5 & x<q95),aes(ymax=y),ymin=0,
              fill="gray",colour=NA,alpha=0.5)+
  geom_vline(xintercept = q5, linetype = "dashed")+
  geom_vline(xintercept = q95, linetype = "dashed")+
  xlab("Change in chl-a (ug/L)")+
  ylab("density")+
  theme_bw()

obs2 <- obs %>%
  filter(year(datetime) %in% c(2022:2023)) %>%
  mutate(chla_bin = ifelse(delta <= q5,"decreasing",
                           ifelse(delta >= q95, "increasing","stable")))
ggplot(data = obs2, aes(x = datetime, y = Chla_ugL_mean, color = chla_bin))+
  geom_point()+
  theme_classic()
