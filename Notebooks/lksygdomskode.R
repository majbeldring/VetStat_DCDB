
# Maj Beldring, majbh@sund.ku.dk
# Pre script forlksygdomskode
# manupulating data enabling loading

#-------------------------------------------------------
# lksygdomskode contains unfinished quotes and commas inside double quotes

# debugging data before loading:
textfile <- readLines("lksygdomskode.csv")
textfile <- gsub('\"\"', "'", textfile)
textfile <- gsub('\"', "", textfile)
textfile <- gsub("\'", '"', textfile)
cat(textfile, sep="\n", file="lksygdomskode_fixed.csv")


# loading disease codings... 
lksygdomskode <- read.csv("lksygdomskode_fixed.csv")
