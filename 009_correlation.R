# correlation analysis

# use spearman instead (vi assume normal distribution, which we DON'T have)

# Pearsons correlation CHR:
cor_herd <- cor(df_CHR$UDD, df_CHR$ADD, method = "spearman")
cor.test(df_CHR$UDD, df_CHR$ADD, method = "spearman")


# Pearsons correlation ATC:
cor_atc <- cor(df_ATC$UDD, df_ATC$ADD)
cor.test(df_ATC$UDD, df_ATC$ADD, method = "spearman")

hist(df_ATC$UDD)
hist(df_ATC$ADD)
# below: show Matt. Add to paper... Weird correlation for CHR
# Søren: Use this plot. R^2 might be enough.
# ask matt: log-log vs R^2.. Log-log might not be ideal with our distribution...


plot(df_ATC$ADD ~ df_ATC$UDD, xlim=c(0,500), ylim=c(0,500))

# colour by:
## herd size
## disease/treatment (mastitis etc): This could be use to explain the below
## several days treatment. This could explain: maybe the farmer only register once.. and not the following days..
## Add disease group