#--------------------------------------------------------
# Maj Beldring Henningsen, majbh@sund.ku.dk
# Scripts for pre-cleaning Danish Cattle Database

#-------------------------------------------------------
# Packages and settings:

library(tidyverse)
#library(reshape2)
library(data.table)
#library(plotly)
#library(GGally)
#library(ggExtra)
#library(ggalluvial)
Sys.setlocale("LC_ALL","English") # data formatting
setwd("P:/paper1_pcr/pcr_stopmast")

#------------------------------------------------------
# generel cleaning to do:

# 1. load data! (fread or read.csv or spceiel case with lksygdomskode)
# 2. Date conversion
# 3. Remove data from before 2010
# 5. save as _fixed in fixed folder
# 4. replace codes
# 6. basic merging...
# 7. basic commands


#-------------------------------------------------------
# 1: Loading data:

##### mastitis data #####
# PCR recordings animals
vetpcr     <- fread("vetpcr.csv")
# clinical registrations
sunklinreg <- fread("sunklinreg.csv")
# disease records animals (treatment)
sundhed    <- fread("sundhed.csv")

##### herd and cow data #####
# calvings
kaelvninger <- fread("kaelvninger.csv")
# SCC
yktr        <- fread("yktr.csv")
# registered dates where cows were made dry
goldninger  <- fread("goldninger.csv")
# link between farm and herd
#beskart     <- fread("beskart.csv")
# Link between animal and herd
#dyrtilbes   <- fread("dyrtilbes.csv")
# animal movements
#oms         <- fread("oms.csv")
# animal information
#dyrinfo     <- fread("dyrinfo.csv")
# reproduction
#repro       <- fread("repro.csv")
# insemination
#ins         <- fread("ins.csv")
# organic/conventionel
brugsart     <- fread("brugsart.csv")

##### code tables #####
# PCR recording
vetpcrkode    <- fread("vetpcrkode.csv")
# brugsart
brugsbartkode <- fread("brugsartkode.csv")
# clinical registrations
sunklinparam  <- fread("sunklinparam.csv")
# diseases
lksygdomskode <- fread("lksygdomskode.csv")


#-------------------------------------------------------
# 2: basic cleaning - removing uneeded variables and changing data format

# yktr:
glimpse(yktr)
yktr <- subset(yktr, select = c(dyr_IDAN, bes_IDAN, KONTROLDATO, CELLETAL)) # Keep selected coloumns
yktr <- yktr[!is.na(yktr$CELLETAL), ] #remove NA from CELLETAL, overwriting yktr
yktr <- subset(yktr, CELLETAL >= 0 ) # removing NEG celletal, overwriting yktr
yktr[, KONTROLDATO:=as.Date(KONTROLDATO, "%d%b%Y")]

# kaelvinger
glimpse(kaelvninger)
kaelvninger <- subset(kaelvninger, select = c(dyr_IDAN, bes_IDAN, KAELVEDATO, KAELVNINGSNR)) # Keep selected coloumns
kaelvninger[, KAELVEDATO:=as.Date(KAELVEDATO, "%d%b%Y")] #Change date format

#pcr
glimpse(vetpcr)
vetpcr <- subset(vetpcr, select = c(bes_IDAN, dyr_IDAN, UDTAGDATO, VETPCRKD_ID, ANALYSEDATAEJAFR)) # Keep selected coloumns
vetpcr[, UDTAGDATO:=as.Date(UDTAGDATO, "%d%b%Y")]
sum(is.na(vetpcr$dyr_IDAN))

# goldning
goldninger[, GOLDNINGSDATO:=as.Date(GOLDNINGSDATO, "%d%b%Y")]

# sundhed
glimpse(sundhed)
sundhed[, SYGDOMSDATO:=as.Date(SYGDOMSDATO, "%d%b%Y")]
# sundhed: only "yver" related registrations:
sundhed    <- subset(sundhed, subset = LKSK_ID%in%lksygdomskode$ID[grepl("Yver",lksygdomskode$LKSYGTEKST)] |
                       LKSK_ID%in%lksygdomskode$ID[lksygdomskode$LKSYGKODE%in%c(13,seq(201,228))])

# sundklinreg
glimpse(sunklinreg)
sunklinreg <- subset(sunklinreg, subset = !duplicated(subset(sunklinreg, select=-IDAN)))
sunklinreg <- subset(sunklinreg, subset = SUNKPARAM_ID%in%seq(7,13,by=1))
sunklinparam  <- sunklinparam[ID%in%seq(7,13,by=1)] # only mastitis relevant codes
sunklinreg[, DATO:=as.Date(DATO, "%d%b%Y")]

gc() # checking Mb used

# save above workspace as: pcr1_2 in "maya"

#-------------------------------------------------------
# 3. POS PCR cases vs SCC: