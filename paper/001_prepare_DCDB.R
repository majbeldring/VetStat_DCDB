#' 
#' 
#' majbeldring, majbh@sund.ku.dk
#' UCPH 2023
#' 
#' Paper Vetstat
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
medicin                 <- read_csv("M:/data/medicin.csv") 
medicindyr              <- read_csv("M:/data/medicindyr.csv")
medicinanvendelseskode  <- read_csv("M:/data/medicinanvendelseskode.csv") 
ordinationsgruppekode   <- read_csv("M:/data/ordinationsgruppekode.csv") 
sunmedenhedskode        <- read_csv("M:/data/sunmedenhedskode.csv") 
sundhed                 <- read_csv("M:/data/sundhed.csv") 
lksygdomskode           <- read.csv("M:/data/lksygdomskode_fixed.csv") #debugged prior loading
kaelvninger             <- read_csv("M:/data/kaelvninger.csv") 
dyrinfo                 <- read_csv("M:/data/dyrinfo.csv") 
racekode                <- read_csv("M:/data/racekode.csv")
brugsart                <- read_csv("M:/data/brugsart.csv")
brugsartkode            <- read_csv("M:/data/brugsartkode.csv")
beskart                 <- read_csv('M:/data/beskart.csv')



# df_med data ---------------------------------------------------------

str(medicin) #navn, styrke, AB mærket, ATCkode
str(medicindyr) # Dyre id via SUNDHED_ID. Så skal have sundhed ind igen kun for medicin
str(medicinanvendelseskode)
str(sunmedenhedskode) #vetstat kode

# medicindyr & medicinanvkode via "medicinankode"
df_med <- full_join(medicindyr, medicinanvendelseskode, by = "MEDICINANVKODE") 
rm(medicinanvendelseskode, medicindyr); gc()
df_med <- df_med %>%
  select(-TILBAGEHOLDELSEKOED, -TILBAGEHOLDELSEMAELK, -TBFENHEDKOEDKODE, -TBFENHEDMAELKKODE)

# df_med & ordiantionsgrupp via "ORDINATGRP_ID"
ordinationsgruppekode <- ordinationsgruppekode %>%
  rename(ORDINATGRP_ID = ID)
  
df_med <- full_join(df_med, ordinationsgruppekode, by = "ORDINATGRP_ID") 
rm(ordinationsgruppekode); gc()
df_med <- df_med %>%
  mutate(ID_disease_group = str_sub(ORDINATGRP_ID, 3, -1)) %>%
  select(-MEDICINANVKODE, -ORDINATGRP_ID)

# df_med & sunmedenhedskode via "SUNENHK_ID"
sunmedenhedskode <- sunmedenhedskode %>%
  rename(SUNENHK_ID = ID)
df_med <- full_join(df_med, sunmedenhedskode, by = "SUNENHK_ID") 
rm(sunmedenhedskode); gc()
df_med <- df_med %>%
  select(-SUNENHK_ID)


# df_med & medicin via "MEDICIN_ID"
medicin <- medicin %>%
  rename(MEDICIN_ID = ID)
df_med <- full_join(df_med, medicin, by = "MEDICIN_ID") 
df_med <- df_med %>%
  rename(ATC=ATCKODE) %>%
  select(-MEDICIN_ID)
rm(medicin); gc()

glimpse(df_med)





# treatments -------------------------------------------------------------------

str(sundhed); str(lksygdomskode) # sygdomsdato format: Date

df_treat <- sundhed %>%
  filter(SYGDOMSDATO > as.Date("2015-12-31")) %>%
  dplyr::select(ID, BES_ID, DYR_ID, SYGDOMSDATO, LKSK_ID) %>%
  drop_na() %>%
  rename(TREAT_DATE = SYGDOMSDATO, ID = LKSK_ID, SUNDH_ID = ID)


df_treat <- left_join(df_treat, lksygdomskode , by = "ID") %>%
  dplyr::select(SUNDH_ID, BES_ID, DYR_ID, TREAT_DATE, LKSYGTEKST) %>%
  rename(DISEASE = LKSYGTEKST)

rm(sundhed, lksygdomskode); gc()

glimpse(df_treat)



# calvings ------------------------------------------------------

str(kaelvninger) # kaelvedato format: POSIXc

calvings <- kaelvninger %>%
  mutate_if(~'POSIXt' %in% class(.x), as.Date) %>% # change date format
  dplyr::select(DYR_ID, KAELVEDATO, KAELVNINGSNR) %>%
  filter(KAELVEDATO > as.Date("2015-12-31")) %>%
  drop_na() %>%
  rename(CALVING_DATE = KAELVEDATO, PARITY = KAELVNINGSNR)

rm(kaelvninger); gc()




# breed and birth ----------------------------------------------------------

str(dyrinfo); str(racekode) # FOEDSELSDATO is date format

dyrinfo <- dyrinfo %>%
  dplyr::select(ID, RACE_ID, FOEDSELSDATO) %>%
  rename(BIRTH = FOEDSELSDATO, DYR_ID = ID)

racekode <- racekode %>%
  dplyr::select(ID, RACENAVN) %>%
  rename(RACE_ID = ID, RACE = RACENAVN )

# join racekode and dyrinfo to create breed:
breed <- left_join(dyrinfo, racekode , by = "RACE_ID") %>%
  dplyr::select(-RACE_ID) %>%
  drop_na()

breed <- breed %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Holstein"), "holstein", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Jersey"), "jersey", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "broget"), "other", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "alkerace$"), "other", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Krydsning"), "other", RACE))

rm(dyrinfo, racekode); gc()

# Note: use grepl! (or is it !grepl) to remove all other races than listed above




# brugsart and CHR ----------------------------------------------

str(brugsart); str(brugsartkode)

brugsart <- brugsart %>%
  rename(ID = BRUGSART_ID) %>%
  group_by(DATO_TIL) %>% 
  replace_na(list(DATO_TIL = as.Date("2020-06-01")))

# keep milk farms (14, 16, 19, 20)
Encoding(brugsartkode$BRUGSARTTEKST) <- "UTF-8"   # Set the encoding to UTF-8 (should already be, just to ensure it)
brugsartkode <- dplyr::filter(brugsartkode, grepl('lk', BRUGSARTTEKST, ignore.case = TRUE, useBytes = TRUE))
brugsartkode <- brugsartkode %>%
  dplyr::select(ID, BRUGSARTTEKST) %>%
  rename(HERD_TYPE = BRUGSARTTEKST) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "logisk$"), "eco", HERD_TYPE)) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "lk"), "con", HERD_TYPE))


# create herd dataset by joining code and brugsart:
herd <- left_join(brugsart, brugsartkode , by = "ID") %>%
  dplyr::select(-ID) %>%
  drop_na()


# Rename beskart to CHR, so i know what it is
CHR <- beskart %>%
  dplyr::select(CHRNR, BES_ID)


rm(brugsart, brugsartkode, beskart); gc()






# save cleaned data: --------------------------------------------

# saving: df_med, df_treat, herd and ani info
save.image("K:/paper_vetstat/001_data_DCDB.RData")

