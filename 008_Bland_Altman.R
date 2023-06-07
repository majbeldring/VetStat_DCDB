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




#' 
#' 
#'
# Packages and settings: ----------------------------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling


# Loading VetStat data
load("K:/paper_vetstat/VetStat_AMU_ADD_2019.RData") 

# DCDB data (2019 only- filtered for this in 005)
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

df_ATC <- left_join(DCDB_ATC, vetstat_ATC, by = "ATC", sort="TRUE",allow.cartesian=TRUE) |>
  filter(!is.na(ADD))

df_CHR <- left_join(DCDB_CHR, vetstat_CHR, by = "CHR", sort="TRUE",allow.cartesian=TRUE) |>
  filter(!is.na(ADD))




# CHR Bland_altman plot ----------------------------------------

df_CHR <- df_CHR |>
  select(ADD, UDD)
str(df_CHR)
df_CHR$UDD <- as.numeric(df_CHR$UDD)
df_CHR$average <- rowMeans(df_CHR)

# create new column for difference measurement
df_CHR$difference <- df_CHR$UDD - df_CHR$ADD

# calculate mean difference
mean_difference <- mean(df_CHR$difference)

# calculate uppr and lower limits of the 
# Confidence interval of 90%
lower_limit <- mean_difference - 1.91*sd(df_CHR$difference )
upper_limit <- mean_difference + 1.91*sd(df_CHR$difference )


# Plot the Bland-Altmon Plot
ggplot(df_CHR, aes(x = average, y = difference)) +
  geom_point(size=1) +
  geom_hline(yintercept = mean_difference, color= "red", lwd=1.5) +
  geom_hline(yintercept = lower_limit, color = "green", lwd=1.5) +
  geom_hline(yintercept = upper_limit, color = "green", lwd=1.5)



# ATC Bland_altman plot ----------------------------------------

df_ATC <- df_ATC |>
  select(ADD, UDD)
str(df_CHR)
df_ATC$UDD <- as.numeric(df_ATC$UDD)
df_ATC$average <- rowMeans(df_ATC)

# create new column for difference measurement
df_ATC$difference <- df_ATC$UDD - df_ATC$ADD

# calculate mean difference
mean_difference <- mean(df_ATC$difference)

# calculate uppr and lower limits of the 
# Confidence interval of 90%
lower_limit <- mean_difference - 1.91*sd(df_ATC$difference )
upper_limit <- mean_difference + 1.91*sd(df_ATC$difference )


# Plot the Bland-Altmon Plot
ggplot(df_ATC, aes(x = average, y = difference)) +
  geom_point(size=1) +
  geom_hline(yintercept = mean_difference, color= "red", lwd=1.5) +
  geom_hline(yintercept = lower_limit, color = "green", lwd=1.5) +
  geom_hline(yintercept = upper_limit, color = "green", lwd=1.5)




# with bland-altman package: ---------------------------

install.packages("BlandAltmanLeh")
library(BlandAltmanLeh)
par() # trying to reset the par() command made earlier
bland.altman.plot(df_ATC$UDD, df_ATC$ADD, col.points = "blue", pch.points = 16, lty.lines = "dashed", col.lines = "red", xlab = "Average", ylab = "Difference", main = "Bland-Altman Plot")







# with another bland-altman package -----------------

# https://cran.r-project.org/web/packages/blandr/vignettes/introduction.html

devtools::install_github("deepankardatta/blandr")
library("blandr")
blandr.draw(df_CHR$UDD, df_CHR$ADD, plotTitle = "Bland-Altman Plot per CHR number")

blandr.draw(df_ATC$UDD, df_ATC$ADD, plotTitle = "Bland-Altman Plot per ATC")


# for paper
# interpret BA plot:
# Look for any systematic bias: If the mean difference is far from zero, it suggests a bias between the two measurements.
# Examine the spread of the differences: The dashed lines (limits of agreement) help assess the variability of the differences. If most of the data points lie within the limits, it indicates good agreement.
# Identify any outliers: Check if there are any data points that fall outside the limits of agreement, as they may represent significant discrepancies or measurement errors.
# Consider clinical or practical significance: Determine if the observed agreement is within an acceptable range based on the context and purpose of the analysis.
# 
# Overall, the Bland-Altman plot provides visual insight into the agreement, bias, and variability between two columns of data.



