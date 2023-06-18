#' 
#' Maj Beldring, majbh@sund.ku.dk
#' 
#' FINAL DATA
#' CHR, SUNDH_ID, DYR_ID, BREED, AGE, PARITY, ATCKODE, NORDISKEVARENR, MED.Anvendelse, DIAGNOSE
#' UDD - used daily dosis
#' 
#' FIX: Keep ordinationsgruppe (disease group: 10 11 12 13 14 16 00/98/99? ) (remove 12)
#' FIX: load vetstat and start with preparing same set-up
#'
#'
# Packages and settings: ------------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling
#memory.size()            # Checking your memory size
#memory.limit()           # Checking the set limit
#memory.limit(size=56000) # suggest for 64 bit: size=56000


# Loading data
load("K:/paper_vetstat/002_treatments_DCDB.RData")        # treatments
load("K:/paper_vetstat/VetStat_AMU_ADD_2019.RData") # VETSTAT_AMU_UDD_2019


# align data sets --------------------------------------------------------------

# Vetstat:
VetStat_AMU_ADD <- VetStat_AMU_ADD |>
  group_by(CHR, ID_disease_group, ATC) |>
  summarise(ADD = sum(ADD_sum_CHR_2019_cows)) |>
  ungroup() 
VetStat_AMU_ADD <- VetStat_AMU_ADD |> ungroup() # seems I have to do this again..



# DCDB:
treatments_AMU <- treatments |>
  filter(MEDICINANVTEKST == 'Forbrugt') |>
  filter(!is.na(ANTIBIOTIKAFLAG)) |>
  filter(ANTIBIOTIKAFLAG == 1) 

treatments_AMU <- treatments_AMU |>
  distinct(SUNDH_ID, CHR, DYR_ID, TREAT_DATE, ID_disease_group, ATC) |>
  mutate(UDD_ani = 1) 

DCDB_AMU_UDD <- treatments_AMU |>
  dplyr::select(CHR, ID_disease_group, ATC, UDD_ani) |>
  group_by(CHR, ID_disease_group, ATC) |>
  summarise(UDD = sum(UDD_ani)) |>
  ungroup() 
DCDB_AMU_UDD <- DCDB_AMU_UDD |> ungroup() # seems I have to do this again..


rm(treatments, treatments_AMU); gc()


# Keep only herds both in vetstat and DCDB ---------------

herds_dcdb <- DCDB_AMU_UDD |>
  ungroup() |>
  dplyr::select(CHR) |>
  mutate(CHR = as.character(CHR))

herds_vetstat <- VetStat_AMU_ADD |>
  ungroup() |>
  dplyr::select(CHR) |>
  mutate(CHR = as.character(CHR))

herds <- inner_join(herds_dcdb, herds_vetstat, by = "CHR") |>
  distinct()

DCDB_AMU_UDD <- DCDB_AMU_UDD |>
  mutate(CHR = as.character(CHR))

# keep only CHR appearing in herds: (can also be done with %in%)
DCDB_AMU_UDD <- inner_join(herds, DCDB_AMU_UDD, by = "CHR")
VetStat_AMU_ADD <- inner_join(herds, VetStat_AMU_ADD, by = "CHR")
rm(herds, herds_dcdb, herds_vetstat); gc()

# checking summaries
n_distinct(DCDB_AMU_UDD$ID_disease_group)
n_distinct(VetStat_AMU_ADD$ID_disease_group)
# CHR: 2197 in both
# ID_disease_group: 11 (Vetstat), 8 (DCDB) (vetstat has some, we now was not in lkssydomskode)
# ATC: 37 (vetstat), 41 (DCDB)


# same format variables -------------------------------------------------------
library(janitor)
compare_df_cols(DCDB_AMU_UDD, VetStat_AMU_ADD)

# ATC: convert to character in Vetstat
# ID_disease_group; convert to character in Vetstat
## Re-convert: ATC, ID_disease_group to factors in both data, when visualiszing!

VetStat_AMU_ADD <- VetStat_AMU_ADD |>
  mutate(ATC = as.character(ATC)) |>
  mutate(ID_disease_group = as.character(ID_disease_group))




# save the 2 aligned databases -----------------------------------------------

save.image("K:/paper_vetstat/003_final_data_DCDB_VetStat.RData") 



