#' 
#' 
#' majbeldring, majbh@sund.ku.dk
#' 
#' NOTE: aut.nr : Only very few in data. Can't use this as an indicator
#' NOTE: age is now days /365.25
#' 
#' 
#' FINAL DATA
#' CHR, SUNDH_ID, DYR_ID, BREED, AGE, PARITY, ATCKODE, NORDISKEVARENR, MED.Anvendelse, DIAGNOSE
#' UDD - used daily dosis
#' 
#' 
#' FIX: Keep ordinationsgruppe
#'
# Packages and settings: ----------------------------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling
#memory.size()            # Checking your memory size
#memory.limit()           # Checking the set limit
#memory.limit(size=56000) # suggest for 64 bit: size=56000


# Loading data: 
load("K:/paper_vetstat/misc/001_med.RData") 
load("K:/paper_vetstat/misc/002_treat.RData")
load("K:/paper_vetstat/misc/004_other_data.RData")




# Pre df_treat - keep only 2019 treatments: ------------------------------------------------------
df_treat <- df_treat |>
  filter(TREAT_DATE <= '2019-12-31') |>
  filter(TREAT_DATE >= '2019-01-01') 

gc()




# Pre df_med -------------------------------------------------------------------------------------

# ensuring potential merging ID is same format
df_med$SUNDH_ID <- as.character(df_med$SUNDH_ID)
df_treat$SUNDH_ID <- as.character(df_treat$SUNDH_ID)
str(df_med$SUNDH_ID); str(df_treat$SUNDH_ID)


# Coloumns: MEDICINANVTEKST
## "Optalt m\xe6ngde"             
## "Forbrugt"                    
## "Ordineret"                    
## "Reguleret"                    
## "Spild"                        
## "Udleveret til landmanden"    

## It is "forbrugt we will use, since we can not identify all "Forbrugt"with aut.nr..





# 1. herds: 2019 herdtypes only, adding CHR and identify duplicates ----------------------------
herd <- herd %>% 
  filter(DATO_FRA <= '2019-12-31') %>% 
  filter(DATO_TIL >= '2019-01-01') 
gc()

# herds changing eco/status in 2019:
repeated_herds <- herd %>% 
  group_by(BES_ID) %>% 
  filter(n()>1) %>%
  ungroup() %>%
  distinct(BES_ID, HERD_TYPE, .keep_all= TRUE) %>%
  group_by(BES_ID) %>% 
  filter(n()>1)
gc()

# adding CHR number to herds, NOTE: repeated herds are not removed
herds <- left_join(herd, CHR, by = "BES_ID", sort="TRUE",allow.cartesian=TRUE)
herds <- herds %>% relocate(CHRNR, .before = BES_ID)
rm(herd, CHR); gc()

# removing eco herds:
herds <- herds |>
  filter(HERD_TYPE == 'con') |>
  ungroup() |>
  dplyr::select(CHRNR, BES_ID) |>
  rename(CHR = CHRNR)

# remove herds changing status in 2019 FIX
herds |>
  anti_join(repeated_herds, by="BES_ID") ->
  herds

rm(repeated_herds); gc()


# Add CHR to treat data, df_terat -> treat_CHR
treat_CHR <- left_join(df_treat, herds, by = "BES_ID", sort="TRUE",allow.cartesian=TRUE)

# remove NA CHR (means they are eco or mixed in 2019)
treat_CHR <- treat_CHR |>
  filter(!is.na(CHR))

rm(herds, df_treat); gc()








# 2. animals: combing breeds and calvings to 1 complete animal info --------------------------------------

animals <- left_join(breed, calvings, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
rm(breed, calvings); gc()


# Keep only first calving and animals haven't calved at all
animals <- animals %>%
  group_by(DYR_ID) %>%
  arrange(CALVING_DATE, .by_group = TRUE) %>%
  distinct(DYR_ID, .keep_all = TRUE)
gc()


# merge animal info to treat info
treat_CHR_ani <- left_join(treat_CHR, animals, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
rm(animals, treat_CHR); gc()

# adding age coloumn for when treated # dummy way
treat_CHR_ani$AGE <- as.Date(as.character(treat_CHR_ani$TREAT_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_CHR_ani$BIRTH), format="%Y-%m-%d")
# change to years dummy way
treat_CHR_ani$AGE <- as.numeric(treat_CHR_ani$AGE ) %/% 365.25

gc()

# Keeping only adult animals: age >= years or Parity >= 1
# Removing animals < 2 years with no parity, and all animals =< 1 year.

# first remove all cows older>2 on treatment day with no parity
df_no_calvings <- treat_CHR_ani |>
  filter(is.na(PARITY)) |>
  filter(AGE >= 2)

df_calvings <-  treat_CHR_ani |>
  filter(AGE > 1  | PARITY > 0) |>
  ungroup() |>
  filter(TREAT_DATE >= CALVING_DATE)

# combing adult cattle with no calvings, with animals parity >= 1:
df_treat <- bind_rows(df_no_calvings, df_calvings)

rm(df_calvings, df_no_calvings, treat_CHR_ani); gc()




# treatments : merge df_med and treat data --------------------------------------- 

# Fix make sure Drug_Admin is kept after merging
treatments <- left_join(df_treat, df_med, by = "SUNDH_ID", sort="TRUE",allow.cartesian=TRUE)
rm(df_med, df_treat)
gc()





# Final Data --------------------------------------

DCDB_treatments <- treatments

DCDB_test <- dplyr::filter(DCDB_treatments, grepl('3852612|3917212|3858412|4008512', BES_ID))

# final data for analysis: Only AB treatments, Only 'USED' AB, calculating UDD
DCDB_AB_2019 <- DCDB_treatments |>
  filter(MEDICINANVTEKST == 'Forbrugt') |>
  filter(!is.na(ANTIBIOTIKAFLAG)) |>
  filter(ANTIBIOTIKAFLAG == 1) 

DCDB_UDD_DIAGNOSE <- DCDB_AB_2019 |>
  distinct(CHR, DYR_ID, RACE, PARITY, AGE, TREAT_DATE, ORDGRPTEKST, ATCKODE, MEDICINNAVN, NORDISKEVARENR) |>
  mutate(UDD = 1) 

# UDD per Nordisk vare nr (Vnr) FIX check with Jeanette it is what she calls it.
DCDB_UDD_CHR_varenr <- DCDB_UDD_DIAGNOSE |>
  dplyr::select(CHR, NORDISKEVARENR, UDD) |>
  group_by(CHR, NORDISKEVARENR, UDD) |>
  mutate(UDD_DCDB = n()) |>
  dplyr::select(-UDD) |>
  ungroup() |>
  rename(Vnr = NORDISKEVARENR) |>
  distinct()


DCDB_UDD_CHR_varenr <- DCDB_UDD_CHR_varenr |>
  ungroup() |>
  dplyr::select(-UDD)


# UDD per MEDICINNAVN
DCDB_UDD_CHR_DRUG <- DCDB_UDD_DIAGNOSE |>
  dplyr::select(CHR, MEDICINNAVN, UDD) |>
  group_by(CHR, MEDICINNAVN, UDD) |>
  mutate(UDD_DCDB = n()) |>
  dplyr::select(-UDD) |>
  ungroup() |>
  distinct()

DCDB_UDD_CHR_DRUG <- DCDB_UDD_CHR_DRUG |>
  ungroup() |>
  dplyr::select(-UDD)


# UDD per DIAGNOSE
DCDB_UDD_ORDGRP <- DCDB_UDD_DIAGNOSE |>
  dplyr::select(CHR, ORDGRPTEKST, UDD) |>
  group_by(CHR, ORDGRPTEKST, UDD) |>
  mutate(UDD_DCDB = n()) |>
  dplyr::select(-UDD) |>
  ungroup() |>
  distinct()

DCDB_UDD_ORDGRP <- DCDB_UDD_ORDGRP |>
  ungroup() |>
  dplyr::select(-UDD)


# UDD per ATCKODE
DCDB_UDD_ATC <- DCDB_UDD_DIAGNOSE |>
  dplyr::select(CHR, ATCKODE, UDD) |>
  group_by(CHR, ATCKODE, UDD) |>
  mutate(UDD_DCDB = n()) |>
  dplyr::select(-UDD) |>
  ungroup() |>
  distinct()

DCDB_UDD_ATC <- DCDB_UDD_ATC |>
  ungroup() |>
  dplyr::select(-UDD)


# Overall daily dosis: We cannot use this. We need to sort it by ATCkode, Diagnose etc.
DCDB_UDD_CHR <- DCDB_UDD_DIAGNOSE |>
  dplyr::select(CHR, UDD) |>
  group_by(CHR, UDD) |>
  mutate(UDD_DCDB = n()) |>
  dplyr::select(-UDD) |>
  ungroup() |>
  distinct()


DCDB_UDD_CHR <- DCDB_UDD_CHR |>
  ungroup() |>
  dplyr::select(-UDD)


gc()




save.image("K:/paper_vetstat/misc/005_merge.RData") 










