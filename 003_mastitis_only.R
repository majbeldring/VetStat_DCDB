
# retrieving Mastitis related treatments only

# majbh@sund.ku.dk ; 2022


# Packages and settings --------------------------------

library(tidyverse) 
library(data.table) # for adding a r for occurences of each dyr_IDAN

load("K:/paper_vetstat/misc/002_treat.RData")
glimpse(df_treat)

sundhed       <- read_csv("M:/data/sundhed.csv") 


# List of all diagnoses
DISEASES <- df_treat %>%
  dplyr::select(DISEASE) %>%
  distinct(DISEASE, .keep_all = TRUE)



# gå efter ordinationsgruppe: burde være 1211


# Retrieving Mastitis related diseases ------------------


mastitis <- sundhed %>%
  filter(SYGDOMSDATO > as.Date("2009-12-31")) %>%
  mutate(year = format(SYGDOMSDATO, "%Y")) %>%
  drop_na() %>%
  filter(LKSK_ID == 120011 | LKSK_ID == 120015 | LKSK_ID == 130011 |LKSK_ID == 140011) %>%
  dplyr::select(year)

mastitis_year <- stack(table(mastitis))[2:1]

# renaming columns
mastitis_year <- mastitis_year %>%
  rename(
    year = ind,
    mastitis = values
  )


# Mastitis: codes: 120011, 120014, 120015, 120094, 120095, 120179, 130011, 140011 ------ 

mastitis_all <- sundhed %>%
  filter(SYGDOMSDATO > as.Date("2009-12-31")) %>%
  mutate(year = format(SYGDOMSDATO, "%Y")) %>%
  drop_na() %>%
  filter(LKSK_ID == 120011 | LKSK_ID == 120015 | LKSK_ID == 130011 |LKSK_ID == 140011 |
            LKSK_ID == 120014 | LKSK_ID == 120094 | LKSK_ID == 120095 |LKSK_ID == 120179) %>%
  dplyr::select(year)

mastitis_all_year <- stack(table(mastitis_all))[2:1]

# renaming columns
mastitis_all_year <- mastitis_all_year %>%
  rename(
    year = ind,
    mastitis = values
  )


# save cleaned data: --------------------------------------------

rm(df_treat, sundhed); gc()
save.image("K:/paper_vetstat/misc/003_mastitis.RData")


