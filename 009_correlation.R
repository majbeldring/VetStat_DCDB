# correlation analysis



# Pearsons correlation CHR:
cor_herd <- cor(df_CHR$UDD, df_CHR$ADD)
cor.test(df_CHR$UDD, df_CHR$ADD)


# Pearsons correlation ATC:
cor_atc <- cor(df_ATC$UDD, df_ATC$ADD)
cor.test(df_ATC$UDD, df_ATC$ADD)
