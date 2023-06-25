#' 
#' 
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, December 2022
#' 
#' 
#'
# Packages and settings: ----------------------------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling
library(janitor) # for compare coloumns
library(knitr) # for kable

load("K:/paper_vetstat/004_data_for_BA.RData")



# disease summary, table 1 -----------------------------------------------------

lksygdomskode           <- read.csv("M:/data/lksygdomskode_fixed.csv")
#diseases in df_disease:
diseases <- c("0", "10", "11", "12", "13", "14", "15", "23", "32", "34", "98", "99")
disease_codes <- lksygdomskode |>
  slice(-1) |> # removing first row, as there's not text here (it's group 10)
  mutate(ID_disease_group = str_sub(ORDINATGRP_ID, 3, -1)) |>
  select(-ID, -ORDINATGRP_ID, -LKSYGKODE) |>
  filter(ID_disease_group %in% diseases) |>
  distinct(ID_disease_group, .keep_all = TRUE)

# laveling the disease codes
disease_mapping <- data.frame(ID_disease_group = c("10", "11", "12", 
                                                "13", "14", "15", 
                                                "0", "23", "32", "34", "98", "99"),
                                 Name = c("Reproduction_urogenital", "Udder", "Gastro", 
                                                  "Respiratory", "Joint_Hoove_CentralN", "Joint_Hoove_CentralN", 
                                                  "Other", "Other", "Other", "Other", "Other", "Other"))
df_disease_names <- left_join(df_disease, disease_mapping, by = "ID_disease_group")
rm(disease_mapping, disease_codes, lksygdomskode, diseases); gc()

# summary diseases:
df_disease_names |>
  dplyr::select(-ID_disease_group) |>
  group_by(Name) |>
  summarize(sum_UDD = sum(UDD),
            sum_ADD = sum(ADD)) |>
  kable()


df_disease_names |>
  group_by(Name) |>
  kable()


# total AMU per farm, table 2 --------------------------------------------------

# Sums
df_CHR |>
  dplyr::select(-CHR) |>
  summarize(sum_UDD = sum(UDD),
            sum_ADD = sum(ADD)) |>
  kable()

# ADD and UDD summary
df_CHR |>
  dplyr::select(-CHR) |>
  summary() |>
  kable()




# Summary DCDB -----------------------------------------------------------------

# Unique animals AB treated per CHR:
DCDB_UDD_DIAGNOSE |>
  distinct(CHR, DYR_ID, .keep_all= TRUE) |>
  count(CHR, sort = TRUE) |>
  rename(AB_treated_unique_ani = n) |>
  summary()

# AB treatments per CHR:
DCDB_UDD_DIAGNOSE |>
  count(CHR, sort = TRUE) |>
  rename(AB_treatments_per_CHR = n) |>
  summary()




# Plot VetStat alone ------------------------------------------------

ggplot(data=VetStat_AMU_ADD, aes(x=ATC, y=ADD)) +
  geom_bar(stat="identity")


# recrate above with merged Vetstat and DCDB. Fill = register type




# Summary Statistics -------------------------------------------------------------------------

## for basic summary statitics include:
# Retrieve: Age, Race frequency, Parity (for those with a Parity) and NA parities
# retrieve: Amount of herds, Average amount of animals per herds, 

# 25%, 75%, median, mean
## treatments per CHR
## Animals per CHR
## Diagnose per CHR
## Animal Age
## Parity

DCDB_UDD_DIAGNOSE %>%
  group_by(RACE) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))

summary(DCDB_UDD_DIAGNOSE)


DCDB_UDD_DIAGNOSE |>
  distinct(CHR, DYR_ID, .keep_all= TRUE) |>
  count(CHR, sort = TRUE) |>
  rename(AB_treated_unique_ani = n) |>
  summary()

# AB treatments per CHR:
DCDB_UDD_DIAGNOSE |>
  count(CHR, sort = TRUE) |>
  rename(AB_treatments_per_CHR = n) |>
  summary()


# Animals per CHR. FIX finish:
DCDB_UDD_DIAGNOSE |>
  dplyr::select(DYR_ID, CHR) |>
  distinct(DYR_ID, .keep_all= TRUE) |>
  count(CHR, sort = TRUE) |>
  rename(Animals_per_CHR = n) |>
  summary()

DCDB_UDD_DIAGNOSE |>
  count(CHR, sort = TRUE)


