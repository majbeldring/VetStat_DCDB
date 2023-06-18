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


# Packages and settings: -----------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling

# load data. Vetstat and DCDB
load("K:/paper_vetstat/VetStat_AMU_ADD_2019.RData") 
load("K:/paper_vetstat/003_final_DCDB.RData")



# ATC -------------------------------------------------------------------------

DCDB_ATC <- DCDB_AMU_UDD |>
  dplyr::select(ATC, UDD) |>
  group_by(ATC) |>
  summarise(UDD = sum(UDD)) |>
  ungroup()

Vet_ATC <- VetStat_AMU_ADD |>
  dplyr::select(ATC, ADD) |>
  group_by(ATC) |>
  summarise(ADD = sum(ADD)) |>
  ungroup()

# Replacing NAs with zeros
df_ATC <- full_join(DCDB_ATC, Vet_ATC, by = "ATC") |>
  mutate_all(~replace_na(.x, 0))
rm(Vet_ATC, DCDB_ATC); gc()



# CHR -------------------------------------------------------------------------

DCDB_CHR <- DCDB_AMU_UDD |>
  dplyr::select(CHR, UDD) |>
  group_by(CHR) |>
  summarise(UDD = sum(UDD)) |>
  ungroup()

Vet_CHR <- VetStat_AMU_ADD |>
  dplyr::select(CHR, ADD) |>
  group_by(CHR) |>
  summarise(ADD = sum(ADD)) |>
  ungroup()

df_CHR <- full_join(DCDB_CHR, Vet_CHR, by = "CHR") 
rm(Vet_CHR, DCDB_CHR); gc()



# Disease code ----------------------------------------------------------------

DCDB_disease <- DCDB_AMU_UDD |>
  dplyr::select(ID_disease_group, UDD) |>
  group_by(ID_disease_group) |>
  summarise(UDD = sum(UDD)) |>
  ungroup()

Vet_disease <- VetStat_AMU_ADD |>
  dplyr::select(ID_disease_group, ADD) |>
  group_by(ID_disease_group) |>
  summarise(ADD = sum(ADD)) |>
  ungroup()

df_disease <- full_join(DCDB_disease, Vet_disease, by = "ID_disease_group") |>
  mutate_all(~replace_na(.x, 0))
rm(Vet_disease, DCDB_disease); gc()

# laveling the disease codes
disease_mapping <- data.frame(ID_disease_group = c("10", "11", "12", 
                                                   "13", "14", "15", 
                                                   "0", "23", "32", "34", "98", "99"),
                              Name = c("Reproduction_urogenital", "Udder", "Gastro", 
                                       "Respiratory", "Joint_Hoove_CentralN", "Joint_Hoove_CentralN", 
                                       "Other", "Other", "Other", "Other", "Other", "Other"))
df_disease_names <- left_join(df_disease, disease_mapping, by = "ID_disease_group")

df_disease_names <- df_disease_names |>
  select(-ID_disease_group) |>
  group_by(Name) |>
  summarize(sum_UDD = sum(UDD),
            sum_ADD = sum(ADD))

rm(disease_mapping); gc()




# save data -----------------------------------------------------------------

save.image("K:/paper_vetstat/004_data_for_BA.RData") 






