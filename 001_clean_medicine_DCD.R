#' 
#' 
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, August 2022
#' 
#' Paper Vetstat
#' Cleaning medicin data
#'
#' FIX: find decoding to "MEDENHEDSKODE"(found in medicindyr); medicin enheds kode ?
#' "MEDENHEDSKODE" is NOT in (medicinanvendelseskode, sunmedenhedskode)
#'
# Packages and settings: ----------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling
# Sys.setlocale("LC_ALL","English") # for date formats
options(stringsAsFactors = FALSE) # prevent factorizing caracters



# Loading data: ------------------------------------------

# medicindyr: OSR only. AB amount. Link to sundhed, medicin, ordinationsgroup (LKSK)
medicin           <- read_csv("M:/data/medicin.csv") 
medicindyr        <- read_csv("M:/data/medicindyr.csv")
medicinanvendelseskode <- read_csv("M:/data/medicinanvendelseskode.csv") 
ordinationsgruppekode  <- read_csv("M:/data/ordinationsgruppekode.csv") 
sunmedenhedskode  <- read_csv("M:/data/sunmedenhedskode.csv") 




# df_med data ---------------------------------------------------------

str(medicin) #navn, styrke, AB mærket, ATCkode
str(medicindyr) # Dyre id via SUNDHED_ID. Så skal have sundhed ind igen kun for medicin
str(medicinanvendelseskode)
str(sunmedenhedskode) #vetstat kode

# medicindyr & medicinanvkode via "medicinankode"
df_med <- full_join(medicindyr, medicinanvendelseskode, by = "MEDICINANVKODE", sort="TRUE",allow.cartesian=TRUE) 
rm(medicinanvendelseskode); gc()
rm(medicindyr); gc()
df_med <- df_med %>%
  select(-TILBAGEHOLDELSEKOED, -TILBAGEHOLDELSEMAELK, -TBFENHEDKOEDKODE, -TBFENHEDMAELKKODE)

# df_med & ordiantionsgrupp via "ORDINATGRP_ID"
ordinationsgruppekode <- ordinationsgruppekode %>%
  rename(ORDINATGRP_ID = ID)
df_med <- full_join(df_med, ordinationsgruppekode, by = "ORDINATGRP_ID", sort="TRUE",allow.cartesian=TRUE) 
rm(ordinationsgruppekode); gc()
df_med <- df_med %>%
  select(-ORDINATGRP_ID, -MEDICINANVKODE)

# df_med & sunmedenhedskode via "SUNENHK_ID"
sunmedenhedskode <- sunmedenhedskode %>%
  rename(SUNENHK_ID = ID)
df_med <- full_join(df_med, sunmedenhedskode, by = "SUNENHK_ID", sort="TRUE",allow.cartesian=TRUE) 
rm(sunmedenhedskode); gc()
df_med <- df_med %>%
  select(-SUNENHK_ID)


# df_med & medicin via "MEDICIN_ID"
medicin <- medicin %>%
  rename(MEDICIN_ID = ID)
df_med <- full_join(df_med, medicin, by = "MEDICIN_ID", sort="TRUE",allow.cartesian=TRUE) 
df_med <- df_med %>%
  select(-MEDICIN_ID)
rm(medicin); gc()

glimpse(df_med)



# save cleaned data: --------------------------------------------

save.image("K:/paper_vetstat/001_med.RData")

