#' 
#' 
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, August 2022
#' 
#' Paper Vetstat
#' Create df_treat data
#' 
#' 
#' FIX: 
#' KEEP ordinationsgrupper
#'
#'
# Packages and settings: ----------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling
#Sys.setlocale("LC_ALL","English") # for date formats
options(stringsAsFactors = FALSE) # prevent factorizing caracters


# Loading data: ------------------------------------------

# treatments
sundhed       <- read_csv("M:/data/sundhed.csv") 
lksygdomskode <- read.csv("M:/data/lksygdomskode_fixed.csv") #debugged prior loading
AB_treatments <- read_csv("M:/AB_treatments.csv") # AB treated diseases identified by Søren & Jeanette




# treatments -------------------------------------------
# (only keeping dry_off treatments with date and animal ID)

str(sundhed); str(lksygdomskode) # sygdomsdato format: Date

AB_treatments <- AB_treatments %>%
  dplyr::select(-n, -UsesAB, -UsesAB2, -Comment, -LKSYGTEKST)

lksygdomskode <- full_join(lksygdomskode, AB_treatments, by = "LKSYGKODE", sort="TRUE",allow.cartesian=TRUE) %>% 
  dplyr::slice(1:241) # remove duplicates

df_treat <- sundhed %>%
  filter(SYGDOMSDATO > as.Date("2015-12-31")) %>%
  dplyr::select(ID, BES_ID, DYR_ID, SYGDOMSDATO, LKSK_ID) %>%
  drop_na() %>%
  rename(TREAT_DATE = SYGDOMSDATO, ID = LKSK_ID, SUNDH_ID = ID)
  

df_treat <- left_join(df_treat, lksygdomskode , by = "ID") %>%
  dplyr::select(SUNDH_ID, BES_ID, DYR_ID, TREAT_DATE, LKSYGTEKST, AB) %>%
  rename(DISEASE = LKSYGTEKST)

rm(sundhed, lksygdomskode, AB_treatments); gc()

glimpse(df_treat)


# save cleaned data: --------------------------------------------

save.image("K:/paper_vetstat/002_treat.RData")

