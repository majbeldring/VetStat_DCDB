# log-log regressions analysis


log.log.CHR <- lm(log(ADD) ~ log(UDD), df_CHR)
summary(log.log.CHR)
par(mfrow=c(2,2))
plot(log.log.CHR, ncol = 3, label.size = 3)

log.log.ATC <- lm(log(ADD) ~ log(UDD), df_ATC)
summary(log.log.ATC)
par(mfrow=c(2,2))
plot(log.log.ATC, ncol = 3, label.size = 3)


# interpret summary results:
# https://quantifyinghealth.com/interpret-logistic-regression-coefficients/

save.image("K:/paper_vetstat/misc/008_009_010_output.RData")
