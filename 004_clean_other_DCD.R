#' 
#' 
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, August 2022
#' 
#' Paper Vetstat
#' Create other variables including:
#' HERDTYPE
#' BREED
#'
#'
# Packages and settings: ----------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling
#Sys.setlocale("LC_ALL","English") # for date formats
options(stringsAsFactors = FALSE) # prevent factorizing caracters



# Loading data: ------------------------------------------

# calvings
kaelvninger   <- read_csv("M:/data/kaelvninger.csv") 
# production
# yktr          <- read_csv("M:/data/yktr.csv")
# breed and birth
dyrinfo       <- read_csv("M:/data/dyrinfo.csv") 
racekode      <- read_csv("M:/data/racekode.csv")
# herd
brugsart      <- read_csv("M:/data/brugsart.csv")
brugsartkode  <- read_csv("M:/data/brugsartkode.csv")
# Praksis and vet ID
# behandler     <- read_csv("M:/data/behandlerNY.csv") 
# CHR
beskart        <- read_csv('M:/data/beskart.csv')



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

brugsartkode <- dplyr::filter(brugsartkode, grepl('lk', BRUGSARTTEKST)) # keep only milk herds
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






# yktr -> production ------------------------------------------------------

# str(yktr) # kontroldato format: Date
# 
# production <- yktr %>%
#   dplyr::select(DYR_ID, BES_ID, KONTROLDATO, CELLETAL, KGMAELK) %>%
#   drop_na() %>%
#   filter(KONTROLDATO > as.Date("2015-12-31")) %>%
#   rename(SCC = CELLETAL, MILK = KGMAELK)
# 
# rm(yktr); gc()



# save cleaned data: --------------------------------------------

# Breed: Breed and birth
# calvings: Parity
# CHR: CHR vs BES_ID
# herd: con or eco

save.image("K:/paper_vetstat/misc/004_other_data.RData")

