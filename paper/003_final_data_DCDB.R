#' 
#' Maj Beldring, majbh@sund.ku.dk
#' 
#' FINAL DATA
#' CHR, SUNDH_ID, DYR_ID, BREED, AGE, PARITY, ATCKODE, NORDISKEVARENR, MED.Anvendelse, DIAGNOSE
#' UDD - used daily dosis
#' 
#' FIX: Keep ordinationsgruppe (disease group: 10 11 12 13 14 16 00/98/99? )
#'
#'
# Packages and settings: ------------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling
#memory.size()            # Checking your memory size
#memory.limit()           # Checking the set limit
#memory.limit(size=56000) # suggest for 64 bit: size=56000


# Loading data (df_med, df_treat, ani + herd info)
load("K:/paper_vetstat/002_data_DCDB.RData") 



# 2019, AB treatments only ----------------------------------------------------

# Only AB treatments, Only 'USED' AB
treatments <- treatments |>
  filter(MEDICINANVTEKST == 'Forbrugt') |>
  filter(!is.na(ANTIBIOTIKAFLAG)) |>
  filter(ANTIBIOTIKAFLAG == 1) 


# 1 treatment per day per animal per disease:
# creating UDD, UDD per Herd, Ani, Date, Treatment, Disease, AB
treatments_UDD <- treatments |>
  distinct(CHR, DYR_ID, RACE, TREAT_DATE, ORDGRPTEKST, ATCKODE, MEDICINNAVN, NORDISKEVARENR) |>
  mutate(UDD_DCDB = 1) 



# UDD pr nordisk vare nr (Vnr) -----------------------------------------------------

# FIX check with Jeanette it is what she calls it.
DCDB_CHR_nordisk_UDD <- treatments_UDD |>
  dplyr::select(CHR, NORDISKEVARENR) |>
  group_by(CHR, NORDISKEVARENR) |>
  count() |>
  rename(UDD = n) |>
  ungroup() |>
  rename(Vnr = NORDISKEVARENR)


DCDB_nordisk_UDD <- treatments_UDD |>
  dplyr::select(NORDISKEVARENR) |>
  group_by(NORDISKEVARENR) |>
  count() |>
  rename(UDD = n) |>
  ungroup() |>
  rename(Vnr = NORDISKEVARENR)




# UDD per MEDICINNAVN --------------------------------------------------------

# Fix find VetStat name for medicinnavn and rename in this code
DCDB_CHR_medicinnavn_UDD <- treatments_UDD |>
  dplyr::select(CHR, MEDICINNAVN) |>
  group_by(CHR, MEDICINNAVN) |>
  count() |>
  rename(UDD = n) |>
  ungroup()

DCDB_medicinnavn_UDD <- treatments_UDD |>
  dplyr::select(MEDICINNAVN) |>
  group_by(MEDICINNAVN) |>
  count() |>
  rename(UDD = n) |>
  ungroup()




# UDD per ordinations gruppe ----------------------------------------------------

# Fix find VetStat name for ordinationsgruppe and rename in this code
# Fix also find the Vetstat levels names (e.g. Yver) and rename levels
DCDB_CHR_ordgruppe_UDD <- treatments_UDD |>
  dplyr::select(CHR, ORDGRPTEKST) |>
  group_by(CHR, ORDGRPTEKST) |>
  count() |>
  rename(UDD = n) |>
  ungroup()

DCDB_ordgrupp_UDD <- treatments_UDD |>
  dplyr::select(ORDGRPTEKST) |>
  group_by(ORDGRPTEKST) |>
  count() |>
  rename(UDD = n) |>
  ungroup()




# UDD per ATCKODE --------------------------------------------------

# FIX: rename: ATCkode = ATC
DCDB_CHR_ATC_UDD <- treatments_UDD |>
  dplyr::select(CHR, ATCKODE) |>
  group_by(CHR, ATCKODE) |>
  count() |>
  rename(UDD = n) |>
  rename(ATC=ATCKODE) |>
  ungroup()

DCDB_ATC_UDD <- treatments_UDD |>
  dplyr::select(ATCKODE) |>
  group_by(ATCKODE) |>
  count() |>
  rename(UDD = n) |>
  rename(ATC=ATCKODE) |>
  ungroup()

# FIX: check if we get the same with the following code:
df_check_ATC <- treatments_UDD |>
  dplyr::select(ATCKODE, UDD_DCDB) |>
  group_by(ATCKODE) |>
  summarise(UDD = sum(UDD_DCDB)) |>
  ungroup() |>
  # prepare DCDB
  rename(ATC=ATCKODE)




# UDD per herd ---------------------------------------------------------------

DCDB_CHR_UDD <- treatments_UDD |>
  dplyr::select(CHR) |>
  group_by(CHR) |>
  count() |>
  rename(UDD = n) |>
  ungroup()



# DCDB data ----------------------------------------------------------------


# For direct comparisons (BA plot etc. Also for summary tables...)
## DCDB_nordisk_UDD : UDD pr nordisk varenr (305 unique varenr.)
## DCDB_medicinnavn_UDD (80 unique medicin names)
## DCDB_ordgruppe_UDD (8 unique ord.gruppe)
## DCDB_ATC_UDD (41 unique ATC codes)
## DCDB_CHR_UDD (2213 unique conventionel herds)


# For correlations (colour codings)
## DCDB_CHR_nordisk_UDD: UDD per Vnr per Herd
## DCDB_CHR_medicinnavn_UDD : UDD per medicinnavn pre herd
## DCDB_CHR_ordgruppe_UDD
## DCDB_CHR_ATC_UDD


rm(treatments, treatments_UDD); gc()
save.image("K:/paper_vetstat/003_final_DCDB.RData") 



