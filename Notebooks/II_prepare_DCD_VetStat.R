
# Medicindyr
# majbh@sund.ku.dk ; 2022


# TO DO:
# 1) merge SUNDHED and MEDICINDYR and LKSK_ID
# 2) Count BES_ID and DYR_ID in MEDICINDYR from 2015-2020 ?

# Packages and settings --------------------------------

library(tidyverse) 
library(data.table) # for adding a r for occurences of each dyr_IDAN
Sys.setlocale("LC_ALL","English") # change locale to English for date formatting

memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters

# load data ----------------------------------------------------------




# Merge sundhed and medicindyr ----------------------------------

# merge by: Sundhed - ID , medicindyr - SUNDH_ID




# Find amount of unique herds and animals in data ----------

# Find treatments per year - not diagnose related or amount of AB. Just treatments per year







# Mastitis: codes: 120011, 120015, 130011, 140011 ------------------
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



