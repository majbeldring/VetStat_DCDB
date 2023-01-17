

# CHR total in beskart data
# data from 2010-2019
# earlier or later observations should be disregarded

# Libraries and settings -----------------------------------

library(tidyverse) 
library(data.table)
Sys.setlocale("LC_ALL","English") # locale change for date formatting

# load beskart-------------------------------------------

beskart <- fread('M:/data/beskart.csv')
glimpse(beskart)


# DYRART == 12: Kvæg

# count unique CHR:
beskart %>%                    
  filter(!is.na(CHRNR)) %>%    
  summarise(Unique_Elements = n_distinct(CHRNR))
# 3934 unique

beskart %>%                    
  filter(!is.na(BES_ID)) %>%    
  summarise(Unique_Elements = n_distinct(BES_ID))
# 3941 unique (3942 unique in yktr)

# duplicated CHR: 8 styk
dup_CHR <- beskart %>%
  group_by(CHRNR) %>%
  filter(n()>1)

# duplicated BES: NONE
dup_BES <- beskart %>%
  group_by(BES_ID) %>%
  filter(n()>1)

# tjek 
beskart %>%
  filter(CHRNR == 38924)
