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



# Summaries -----------------------------------------------------

# table 1: summary diseases:
df_disease_names |>
  group_by(Name) |>
  kable()


# table 2: summary farms
df_CHR |>
  dplyr::select(-CHR) |>
  summary() |>
  kable()

# sum:
sum(df_disease$TF_UDD)
sum(df_disease$TF_ADD)

# table3: summary ATC
df_ATC |>
  group_by(ATC) |>
  kable()




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


