#Read in FCR data from EDI
#Author: Mary Lofton
#Date last updated: 12APR24

#Purpose: Download and read in data from Falling Creek Reservoir stored in the 
#Environmental Data Initiative Repository

#Data needed:
#'water chemistry (SRP, DIN)

options(timeout=10000)

# chemistry
inUrl1  <- "https://portal.edirepository.org/nis/dataviewer?packageid=edi.199.12&entityid=a33a5283120c56e90ea414e76d5b7ddb" 
infile1 <- paste0("./data/data_raw/chemistry_2013_2023.csv")
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

# CTD (for GLM-AED initial conditions)
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/13/27ceda6bc7fdec2e7d79a6e4fe16ffdf" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "Depth_m",     
                 "Temp_C",     
                 "DO_mgL",     
                 "DOsat_percent",     
                 "Cond_uScm",     
                 "SpCond_uScm",     
                 "Chla_ugL",     
                 "Turbidity_NTU",     
                 "pH",     
                 "ORP_mV",     
                 "PAR_umolm2s",     
                 "DescRate_ms",     
                 "Flag_DateTime",     
                 "Flag_Temp_C",     
                 "Flag_DO_mgL",     
                 "Flag_DOsat_percent",     
                 "Flag_Cond_uScm",     
                 "Flag_SpCond_uScm",     
                 "Flag_Chla_ugL",     
                 "Flag_Turbidity_NTU",     
                 "Flag_pH",     
                 "Flag_ORP_mV",     
                 "Flag_PAR_umolm2s",     
                 "Flag_DescRate_ms"    ), check.names=TRUE)
tail(dt1)
unlink(infile1)

dt2 <- dt1 %>%
  filter(Reservoir == "FCR" & Site == 50 & year(DateTime) %in% c(2018:2023))
write.csv(dt2, "./data/data_raw/CTD_2018_2022_FCR50.csv", row.names = FALSE)

#get FP data to help inform GLM-AED calibration
url <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
fp <- read_csv(url, show_col_types = FALSE) %>%
  filter(variable %in% c("GreenAlgae_ugL_sample","Bluegreens_ugL_sample","BrownAlgae_ugL_sample",
                         "MixedAlgae_ugL_sample"),
         site_id == "fcre",
         year(datetime) %in% c(2018:2023))
write.csv(fp, "./data/data_raw/FP_2018_2023_FCR50.csv", row.names = FALSE)

#get more FP data to help inform GLM-AED calibration
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/272/8/0359840d24028e6522f8998bd41b544e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "CastID",     
                 "Depth_m",     
                 "GreenAlgae_ugL",     
                 "Bluegreens_ugL",     
                 "BrownAlgae_ugL",     
                 "MixedAlgae_ugL",     
                 "TotalConc_ugL",     
                 "YellowSubstances_ugL",     
                 "Temp_C",     
                 "Transmission_perc",     
                 "RFU_370nm",     
                 "RFU_470nm",     
                 "RFU_525nm",     
                 "RFU_570nm",     
                 "RFU_590nm",     
                 "RFU_610nm",     
                 "Flag_GreenAlgae_ugL",     
                 "Flag_Bluegreens_ugL",     
                 "Flag_BrownAlgae_ugL",     
                 "Flag_MixedAlgae_ugL",     
                 "Flag_YellowSubstances_ugL",     
                 "Flag_TotalConc_ugL",     
                 "Flag_Temp_C",     
                 "Flag_Transmission_perc",     
                 "Flag_RFU_525nm",     
                 "Flag_RFU_570nm",     
                 "Flag_RFU_610nm",     
                 "Flag_RFU_370nm",     
                 "Flag_RFU_590nm",     
                 "Flag_RFU_470nm"    ), check.names=TRUE)

unlink(infile1)

fp_profiles <- dt1 %>%
  filter(Reservoir == "FCR" & Site == 50 & year(DateTime) %in% c(2018:2023)) %>%
  rowwise() %>%
  mutate(non_cyano = GreenAlgae_ugL + BrownAlgae_ugL + MixedAlgae_ugL) %>%
  select(DateTime, CastID, Depth_m, GreenAlgae_ugL, BrownAlgae_ugL, MixedAlgae_ugL, non_cyano)

write.csv(fp_profiles, "./data/data_raw/FP_2018_2023_profiles_FCR50.csv", row.names = FALSE)
