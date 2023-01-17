
# Data Cleaning "sundhed" "lksygdomskode"
# Maj Beldring Henningsen, majbh@sund.ku.dk
# loading and cleaning raw data "sundhed"

# Packages and settings -------------------------------------------

library(tidyverse) # includes dplyr and ggplot2
library(reshape2) # for melt commands
library(data.table) # for adding a row for occurences of each dyr_IDAN
library(readxl)
setwd('C:/Users/majhe/Google Drive/PhD/R_data') # set location
Sys.setlocale("LC_ALL","English") # change locale to English, for data formatting


# load 'sundhed'-----------------------------------------------

sundhed <- read.table("sundhed.csv",header=TRUE,sep=",")
sundhed = subset(sundhed, select = c(SYGDOMSDATO, LKSK_ID)) # removing unused columns
sundhed$SYGDOMSDATO = format(as.Date(sundhed$SYGDOMSDATO, format='%Y-%m-%d'),'%Y') # must be English locale
sundhed <- sundhed[!is.na(sundhed$SYGDOMSDATO), ]

# Mastitis: codes: 120011, 120012, 120015, 120094, 120095, 120179
sundhed_scc <- subset(sundhed, LKSK_ID %in% c(120011, 120012, 120014, 120015, 120094, 120095, 120179, 130011, 140011))
sundhed_scc <- subset(sundhed_scc, SYGDOMSDATO >= 2000 )
sundhed_scc <- subset(sundhed_scc, SYGDOMSDATO < 2020 ) # also removing 2020

sundhed_scc_year <- subset(sundhed_scc, select = c(SYGDOMSDATO))
year_mastitis <- stack(table(sundhed_scc_year))[2:1]
# renaming columns
year_mastitis <- year_mastitis %>%
  rename(
    year = ind,
    mastitis_treatments = values
  )

# adding a coloumn with mastitis per animal per year:
# create coloumn with animals per year - should total be producing animal?
#year_mastitis <- transform(year_mastitis, mastitis_cow = mastitis / animals)

write.csv(year_mastitis,"C:\\Users\\majhe\\OneDrive - Københavns Universitet\\data\\year_mastitis.csv", row.names = FALSE)



# antibiotic resistance, staph, 120216 -----------------------------------------------------------


sundhed_mdr <- subset(sundhed, LKSK_ID %in% c(120216))
sundhed_mdr <- subset(sundhed_mdr, SYGDOMSDATO >= 2000 ) # sundhed was made with only years in section above

year_mdr <- subset(sundhed_mdr, select = c(SYGDOMSDATO))
year_mdr <- stack(table(year_mdr))[2:1]
# renaming columns
year_mdr <- year_mdr %>%
  rename(
    year = ind,
    mdr = values
  )

# calculate mdr/animals in new coloumn....
# adding a row with difference between:
# year_mdr$mdr_cow <- (year_mdr@mdr) / (year_mdr@animals)

