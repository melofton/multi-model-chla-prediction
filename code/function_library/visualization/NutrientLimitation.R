# Figure to show nutrient limitation in FCR
# Author: Mary Lofton
# Date: 24APR24

# Purpose: Assess nutrient limitation in FCR to inform calibration of GLM-AED

# Load packages
library(tidyverse)
library(lubridate)
library(cowplot)

PlotNutrientLimitation <- function(chem_data = "./data/data_raw/chemistry_2013_2023.csv", save_plot = TRUE){

# Load data
chem <- read_csv(chem_data) %>%
  filter(Reservoir == "FCR" & Site == 50 & year(DateTime) %in% c(2013:2021) & Depth_m == 1.6) %>%
  mutate(TN_umolL = TN_ugL*14.0067,
         TP_umolL = TP_ugL*30.9738,
         DIN_umolL = NH4_ugL*18.04*(14.0067/18.04) + NO3NO2_ugL*62.0049*(14.0067/62.0049),
         SRP_umolL = SRP_ugL*94.9714*(30.9738/94.9714),
         TNTP_ratio = TN_umolL/TP_umolL,
         DINSRP_ratio = DIN_umolL/SRP_umolL) %>%
  filter(!TNTP_ratio == Inf, !DINSRP_ratio == Inf) %>%
  mutate(DateTime = as.Date(DateTime))

#TNTP
tntp <- ggplot(data = chem, aes(x = DateTime, y = TNTP_ratio))+
  geom_point()+
  geom_hline(yintercept = 16, color = "red")+
  annotate(geom="text", x=as.Date("2015-05-01 12:00:00"), y=14, label="Redfield ratio (16:1)",
           color="red")+
  annotate(geom="text", x=as.Date("2014-10-01 12:00:00"), y=1, label="N-limited",
           color="black")+
  annotate(geom="text", x=as.Date("2014-10-01 12:00:00"), y=42, label="P-limited",
           color="black")+
  ylab("TN to TP molar ratio")+
  ggtitle("Falling Creek Reservoir")+
  theme_bw()

#DIN-SRP
dinsrp <- ggplot(data = chem, aes(x = DateTime, y = DINSRP_ratio))+
  geom_point()+
  geom_hline(yintercept = 16, color = "red")+
  annotate(geom="text", x=as.Date("2015-05-01 12:00:00"), y=14, label="Redfield ratio (16:1)",
           color="red")+
  annotate(geom="text", x=as.Date("2014-10-01 12:00:00"), y=7, label="N-limited",
           color="black")+
  annotate(geom="text", x=as.Date("2014-10-01 12:00:00"), y=32, label="P-limited",
           color="black")+
  ylab("DIN to SRP molar ratio")+
  ggtitle("Falling Creek Reservoir")+
  theme_bw()

limitation <- plot_grid(tntp, dinsrp, rel_widths = c(1,1))
print(limitation)
if(save_plot == TRUE){
  ggsave(limitation, filename = "./figures/NutrientLimitation.png",
         device = "png", height = 4, width = 10, units = "in")
}

}
