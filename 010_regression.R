# log-log regressions analysis


log.log.CHR <- lm(log(ADD) ~ log(UDD), df_CHR)
summary(log.log.CHR)
par(mfrow=c(2,2))
plot(log.log.CHR, ncol = 3, label.size = 3)


log.log.ATC <- lm(log(ADD) ~ log(UDD), df_ATC)
summary(log.log.ATC)


save.image("K:/paper_vetstat/008_009_010_output.RData")
