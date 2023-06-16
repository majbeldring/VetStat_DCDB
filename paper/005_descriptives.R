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

load("K:/paper_vetstat/004_data_ready.RData")





# Summary DCDB ---------------------------------------------------------------

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




# Reperated SUNDH_ID in 2019:
repeated_SUNDHID <- DCDB_treatments %>% 
  group_by(SUNDH_ID) %>% 
  filter(n()>1) %>%
  ungroup() 





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

# breeds - wait with this after merged with Vetstat. Make sure to merge with data containing Races etc..
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


