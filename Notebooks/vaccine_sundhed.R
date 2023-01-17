#---------------------------------------------------------
# KU vet Phd Big data
# Maj Beldring Henningsen, majbh@sund.ku.dk
# cleaning + merging "sundhed" & "yktr"
# investigate @scc vs @vaccines

#### MUST UPDATE SCRIPT FOW NEW DATA

#---------------------------------------------------------
# Packages and settings
library(tidyverse) # includes dplyr and ggplot2
library(reshape2) # for melt commands
library(data.table) # for adding a row for occurences of each dyr_IDAN
setwd('C:/Users/majhe/Google Drive/PhD/R_data') # set location
Sys.setlocale("LC_ALL","English") # change locale to English, for data formatting

#----------------------------------------------------------
# import data: "sundhed", "yktr", "sygdomskoder". provided from Maya, kv?gdatabasen
sundhed <- read.table("data_maya/sundhed/sundhed.csv",header=TRUE,sep=",")
yktr <- read.table("data_maya/yktr/yktr.csv",header=TRUE,sep=",")
koder <- read.table("data_maya/Codings/lksygdomskode.csv",header=TRUE,sep=",")

# Cleaning data - Koder
head(koder)
str(koder)
range(sundhed$LKSK_ID) # range = 120000-130219
range(koder$ID) # range = 120000-125831 ; variable looks identical in sundhed and koder
levels(koder$LKSYGTEKST) # checking all the different treatments (factors)

# Cleaning data - yktr
head(yktr) #Look at header; noting
str(yktr) #Look at data structure
yktr = subset(yktr, select = -c(EFTERKONTROLFLAG, ANTAL_MALKNINGER, FEDTPCT, PROTEINPCT, KGMAELK, bes_IDAN, KONTROLNR)) # removing unused columns
range(yktr$CELLETAL) # Check for NAs
table(is.na(yktr$CELLETAL)) # how many of total values are NA
yktr <- yktr[!is.na(yktr$CELLETAL), ] #remove NA from CELLETAL, overwriting yktr
range(yktr$CELLETAL) # check range: -1029 to 9999: Should not be NEG. Show these:
yktr %>%
  filter(CELLETAL < 0) %>%  select(CELLETAL) #67 NEG of 32666498 cases. Will remove NEG:
yktr <- subset(yktr, CELLETAL > 0 ) # overwriting "yktr" with only POS CELLETAL
range(yktr$CELLETAL) # checking there is only POS values left in CELLETAL
yktr$KONTROLDATO = as.Date(yktr$KONTROLDATO, format='%d%b%Y:%H:%M:%S') #Change date format
names(yktr) <- c("dyr_IDAN", "dato_SCC", "SCC") # changing columns names

# Cleaning data - Sundhed
head(sundhed) #Look at header
str(sundhed) #Look at data structure
range(sundhed$LKSK_ID) # NO NAs and all POS, so no need to check for NAs
sundhed = subset(sundhed, select = -c(bes_IDAN, IDAN)) # removing unused columns
sundhed$SYGDOMSDATO = as.Date(sundhed$SYGDOMSDATO, format='%d%b%Y:%H:%M:%S') # must be English locale

sundhed <- subset(sundhed, LKSK_ID == 120330) #Keep only vaccine for bluetongue
sundhed[,'LKSK_ID']<-factor(sundhed[,'LKSK_ID']) # change "LKSK_ID" int to factors:
str(sundhed) # check LKSK_ID is a factor: YEs
levels(sundhed$LKSK_ID) <- c(levels(sundhed$LKSK_ID), "BlueTongue") # change 120330 to "BlueTongue"
sundhed$LKSK_ID[sundhed$LKSK_ID == '120330'] <- 'BlueTongue' # replacing 120330 w. BlueTongue
levels(sundhed$LKSK_ID)# I want to drop level 120330 LKSK_ID (replaced by bluetongue)
levels(droplevels(sundhed$LKSK_ID)) # dropping the unused 120330 level
sundhed$LKSK_ID <- factor(sundhed$LKSK_ID) # step 2 in dropping the unused level

names(sundhed) <- c("dyr_IDAN", "dato_treatment", "treatment") # changing column names
sundhed <- sundhed[order(sundhed$dato_treatment),] # order dato_treatment from old to new
sundhed <- sundhed[!duplicated(sundhed[c(1,3)]),] # deleting duplicates of dyr_IDAN and treatment, so only first treatment for each animal is listed
length(unique(sundhed$dyr_IDAN)) # checking only one animal per treatment

# Check range "yktr" and "sundhed" before merging - and further cleaning before merging
yktr <- yktr[order(yktr$dato_SCC),] # in order after dato_SCC
range(yktr$dyr_IDAN) # range = 1000025 6470964
range(sundhed$dyr_IDAN) # range = 1000025 6470964.
range(sundhed$dato_treatment) # "2004-04-08" to "2015-06-25"
range(yktr$dato_SCC) # "1989-10-17" to "2016-02-25"
yktr <- subset(yktr, dato_SCC >= as.Date("2004-01-01")) # deleting dates before 2004-01-01

#------------------------------------------------------------
# merging cleaned "yktr" and "sundhed" by dyr_IDAN keeping all rows
vaccine1 <- merge(yktr, sundhed, by="dyr_IDAN", sort=TRUE)

# keeping only dato_SCC up to 28 days before and after vaccination
vaccine2 <- subset(vaccine1, dato_SCC + 28 > dato_treatment &  dato_SCC - 28 < dato_treatment)
length(unique(vaccine2$dyr_IDAN))
vaccine2 = subset(vaccine2, select = -c(treatment)) # remove treatment column. All is blue tongue

# split data set into a df w. dates before treatment & df w. dates after treatment
pre <- subset(vaccine2, dato_SCC < dato_treatment) # pre treatment
post <- subset(vaccine2, dato_SCC > dato_treatment) # post treatment

# cleaning splitted data
pre <- pre[ order(pre$dato_SCC , decreasing = TRUE ),] # order dates - newest first
pre <- pre[!duplicated(pre$dyr_IDAN),] # keep only first dyr_IDAN apperance
length(unique(pre$dyr_IDAN)) # all dyr_IDAN is unique
post <- post[ order(post$dato_SCC , decreasing = TRUE ),] # order date - newest first
post <- post[!duplicated(post$dyr_IDAN),] # keep newest, hereby removing duplicates closer to treatment
length(unique(post$dyr_IDAN)) # all dyr_IDAN is unique
pre = subset(pre, select = -c(dato_SCC, dato_treatment) ) # remove dato_SCC and dato_treatment
names(pre) <- c("dyr_IDAN", "SCC_pre") # changing column names
post = subset(post, select = -c(dato_SCC, dato_treatment) ) # remove dato_SCC and dato_treatment
names(post) <- c("dyr_IDAN", "SCC_post") # changing column names

# merge pre and post by dyr_IDAN
vaccine <- merge(pre, post, by="dyr_IDAN", all=TRUE, sort=FALSE)
vaccine <- na.omit(vaccine) #deleting NA's overwriting

#-----------------------------------------------------------
# boxplot without factors of SCC pre and post a vaccine
tidy_vaccine <- vaccine %>% gather(variable, celletal, c("SCC_pre", "SCC_post"))
p <- ggplot(tidy_vaccine, aes(x = variable, y = celletal)) + geom_boxplot()
p # pretty awful plot - that we can't say anything about SCC pre/post vaccine

# remove SCC outliers, with SCC > 600 (above 400: unfit for consumption) and replot:
vaccine_clean <- subset(vaccine, SCC_pre < 500 & SCC_post < 500)
tidy_vaccine_clean <- vaccine_clean %>% gather(variable, celletal, c("SCC_pre", "SCC_post"))
p2 <- ggplot(tidy_vaccine_clean, aes(x = variable, y = celletal)) + geom_boxplot()
p2

