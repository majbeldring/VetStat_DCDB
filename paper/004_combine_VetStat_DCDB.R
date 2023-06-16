#' 
#' 
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, December 2022
#' 
#' To Do:
#' Rename Diagnose to VetStat name
#' Create ATC level 2 (fjern sidste 4 cifre i ATC kode)
#' Merge with VetStat
#' New summary to table 1,2,3
#' 
#' # 1. Merge DCDB_UDD_DIAGNOSE with VetStat new -> DCDB_VetStat_new. Only include overlapping herds
# 2. Merge DCDB_UDD_DIAGNOSE with VetStat old -> DCDB_VetStat_old. Only include overlapping herdsCheck if it is the same herds as above. If not check overlapping old vs new vetstat
# 2. Retrieve from merged data:
## Animals per herd: group by(BES_ID, unique_animals), count ani per herd
## Breeds per herds: Like animals
## AB treatment frequency per animal (?)


# create (depending on what we have in Vetstat)
## df_CHR
## df_ATC
## df_ord
## df_medicin
## df_nordisk


# Packages and settings: ----------------------------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling

# load data. Vetstat and DCDB
load("K:/paper_vetstat/VetStat_AMU_ADD_2019.RData") 
load("K:/paper_vetstat/003_final_DCDB.RData")



# Keep only herds both in vetstat and DCDB ---------------

herds_dcdb <- DCDB_UDD_ATC |>
  ungroup() |>
  dplyr::select(CHR) |>
  mutate(CHR = as.character(CHR))

herds_vetstat <- VetStat_AMU_ADD |>
  ungroup() |>
  dplyr::select(CHR) |>
  mutate(CHR = as.character(CHR))

herds <- inner_join(herds_dcdb, herds_vetstat, by = "CHR") |>
  distinct()

DCDB_UDD_ATC <- DCDB_UDD_ATC |>
  mutate(CHR = as.character(CHR))

# keep only CHR appearing in herds: (can also be done with %in%)
DCDB_UDD_ATC <- inner_join(herds, DCDB_UDD_ATC, by = "CHR")
VetStat_AMU_ADD <- inner_join(herds, VetStat_AMU_ADD, by = "CHR")


# prepare pre-merge DCDB  ------------------------------
# prepare DCDB:
DCDB_ATC <- DCDB_UDD_ATC |>
  dplyr::select(ATCKODE, UDD_DCDB) |>
  group_by(ATCKODE) |>
  summarise(UDD = sum(UDD_DCDB)) |>
  ungroup() |>
  # prepare DCDB
  rename(ATC=ATCKODE)

DCDB_CHR <- DCDB_UDD_ATC |>
  dplyr::select(CHR, UDD_DCDB) |>
  group_by(CHR) |>
  summarise(UDD = sum(UDD_DCDB)) |>
  ungroup()

DCDB_ATC <- droplevels(DCDB_ATC)
DCDB_CHR <- droplevels(DCDB_CHR)


# Plot VetStat alone ------------------------------------------------

ggplot(data=VetStat_AMU_ADD, aes(x=ATC, y=ADD_sum_CHR_2019_cows)) +
  geom_bar(stat="identity")


# recrate above with merged Vetstat and DCDB. Fill = register type

# prepapre vetstat:
vetstat <- VetStat_AMU_ADD |>
  ungroup() |> # to avoid disease group is kept
  dplyr::select(CHR, ATC, ADD_sum_CHR_2019_cows) |>
  rename(ADD_new = ADD_sum_CHR_2019_cows)|>
  filter(!is.na(ATC))

vetstat_CHR <- vetstat |>
  dplyr::select(CHR, ADD_new) |>
  group_by(CHR) |>
  summarise(ADD = sum(ADD_new)) |>
  ungroup()

vetstat_ATC <- vetstat |>
  dplyr::select(ATC, ADD_new) |>
  group_by(ATC) |>
  summarise(ADD = sum(ADD_new)) |>
  ungroup()

vetstat_CHR <- droplevels(vetstat_CHR)
vetstat_ATC <- droplevels(vetstat_ATC)


# merge vetstat and DCDB -----------------

df_ATC <- left_join(DCDB_ATC, vetstat_ATC, by = "ATC", sort="TRUE", allow.cartesian=TRUE) |>
  filter(!is.na(ADD))

df_CHR <- left_join(DCDB_CHR, vetstat_CHR, by = "CHR", sort="TRUE",allow.cartesian=TRUE) |>
  filter(!is.na(ADD))




# save data -----------------------------------------------------------------

save.image("K:/paper_vetstat/004_data_ready.RData") 


