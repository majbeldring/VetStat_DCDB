#' 
#' 
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, December 2022
#' 
#' 
#' 4 herds bland altman plot
#' 
#'
# Packages and settings: ----------------------------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling


# Loading VetStat data
load("J:/IVH/Common/VETMYNdata/VetStat/Temporary/VetStat_AMU_ADD.RData") 

# DCDB data
load("K:/paper_vetstat/005_merge.RData")
rm(DCDB_treatments) # basic data with ALL treatments, including non-AB treatments
rm(DCDB_AB_2019) # basic with all AB registrations and all " original" variables. UDD not calculated
rm(DCDB_test) # test data with four basic herds
rm(DCDB_UDD_CHR) # useless. UDD not calculated in regards to diagnose, ATC or drug.
gc()

# Kept:
# DCDB_UDD_DIAGNOSE: Includes diagnose, ATC, Animal info etc. UDD coloumn created, not calculated
# DCDB_UDD_ATC: UDD per herd per ATC group
# DCDB_UDD_CHR_DRUG: UDD per herd per DRUG (NOTE: Some drug may be duplicated if name-change etc)
# DCDB_UDD_ORDGRP: UDD per herd per ordinationsgruppe
# DCDB_UDD_CHR_varenr: UDD per herd per Nordisk Varenummer



# for Jeanette - herds from DCDB ----------------------------------------------------

# taking: DCDB_UDD_DIAGNOSE 
# this is our basic data for all AB treatments with adult animals on conventionel farms

DCDB_herds <- DCDB_UDD_DIAGNOSE |>
  dplyr::select(CHR) |>
  distinct(CHR, .keep_all= TRUE)

write.csv(DCDB_herds,"C:\\Users\\zjt234\\PhD\\PaperIII_VetStat\\DCDB_herds.csv", row.names = FALSE)


# Summary Descriptives ---------------------------------------------------------------

# Summary DCDB - repeat with merged data
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


# 1. Merge DCDB_UDD_DIAGNOSE with VetStat new -> DCDB_VetStat_new. Only include overlapping herds
# 2. Merge DCDB_UDD_DIAGNOSE with VetStat old -> DCDB_VetStat_old. Only include overlapping herdsCheck if it is the same herds as above. If not check overlapping old vs new vetstat
# 2. Retrieve from merged data:
## Animals per herd: group by(BES_ID, unique_animals), count ani per herd
## Breeds per herds: Like animals
## AB treatment frequency per animal (?)






# histogram plot, showing AB per test herd ------------------------------------------------

# Vetstat new:
test_VetStat_new <- droplevels(test_VetStat_new)
levels(test_VetStat_new$product) # 8 levels

# removing non AB product:
Ab_vetstat <- test_VetStat_new %>%
  filter(!is.na(AB_class))

# drop unused levels and test only 8 are left:
Ab_vetstat <- droplevels(Ab_vetstat)
levels(Ab_vetstat$product) # 8 levels

p_hist_vetstat <-ggplot(data=Ab_vetstat, aes(x=product, y=ADD_rec)) +
  geom_bar(stat="identity")
p_hist_vetstat


# recrate above with Wetstat and DCDB. Fill = register type








# TEST 4 herds!!! DCDB and VetStat_new-------------------------------------

# FIX: Somethings ODD with the vetstat_old. looks like CHR and ATC are duplicated. Keep going with new only for now:

# prepapre vetstat:
vetstat_new <- test_VetStat_new |>
  dplyr::select(CHR, ATC, ADD_rec) |>
  rename(ADD_new = ADD_rec)|>
  filter(!is.na(ATC))

vetstat_test <- vetstat_new |>
  group_by(CHR, ATC) |>
  summarise(ADD = sum(ADD_new)) |>
  #dplyr::select(-UDD) |>
  ungroup() |>
  distinct()
vetstat_test <- droplevels(vetstat_test)

# prepare DCDB
DCDB_test_ATC <- dplyr::filter(DCDB_UDD_ATC, grepl('38526|39172|38584|40085', CHR)) |> # NOTE this is CHR and not BES_ID
  rename(ATC=ATCKODE)

# merge DCDB and VetStat:
DCDB_test_ATC$UDD_DCDB <- as.numeric(DCDB_test_ATC$UDD_DCDB)
DCDB_test_ATC$CHR <- as.character(DCDB_test_ATC$CHR)
vetstat_test$CHR <- as.character(vetstat_test$CHR)
vetstat_test$ATC <- as.character(vetstat_test$ATC)

df <- left_join(DCDB_test_ATC, vetstat_test, by = c("CHR", "ATC"), sort="TRUE",allow.cartesian=TRUE) |>
  filter(!is.na(ADD))

# Create Bland_altman plot:
df <- df |>
  select(ADD, UDD_DCDB)
str(df)
df$UDD_DCDB <- as.numeric(df$UDD_DCDB)
df$average <- rowMeans(df)


# create new column for difference measurement
df$difference <- df$UDD_DCDB - df$ADD

# calculate mean difference
mean_difference <- mean(df$difference)

# calculate uppr and lower limits of the 
# Confidence interval of 90%
lower_limit <- mean_difference - 1.91*sd(df$difference )
upper_limit <- mean_difference + 1.91*sd(df$difference )

# load library ggplot2
library(ggplot2)

# Plot the Bland-Altmon Plot
ggplot(df, aes(x = average, y = difference)) +
  geom_point(size=3) +
  geom_hline(yintercept = mean_difference, color= "red", lwd=1.5) +
  geom_hline(yintercept = lower_limit, color = "green", lwd=1.5) +
  geom_hline(yintercept = upper_limit, color = "green", lwd=1.5)









# TEST 4 herds: Old vetstat vs New vetstat with Vnr (FIX: is this Nordisk varenr? Doesn't correspond to DCDB) ----------------------------

load("J:/IVH/Common/VETMYNdata/VetStat/Temporary/test_bland_altmann.RData") 

# create new column for average measurement
df <- test_bland_altmann |>
  select(ADDs_per_product_old, ADDs_per_product_new)
str(df)
df$average <- rowMeans(df)


# create new column for difference measurement
df$difference <- df$ADDs_per_product_old - df$ADDs_per_product_new

# calculate mean difference
mean_difference <- mean(df$difference)

# calculate uppr and lower limits of the 
# Confidence interval of 90%
lower_limit <- mean_difference - 1.91*sd(df$difference )
upper_limit <- mean_difference + 1.91*sd(df$difference )

# load library ggplot2
library(ggplot2)

# Plot the Bland-Altmon Plot
ggplot(df, aes(x = average, y = difference)) +
  geom_point(size=3) +
  geom_hline(yintercept = mean_difference, color= "red", lwd=1.5) +
  geom_hline(yintercept = lower_limit, color = "green", lwd=1.5) +
  geom_hline(yintercept = upper_limit, color = "green", lwd=1.5)





