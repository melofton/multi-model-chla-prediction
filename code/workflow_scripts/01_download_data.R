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


