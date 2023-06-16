#' 
#' 
#' majbeldring, majbh@sund.ku.dk
#' UCPH 2023
#' 

#' 
#' 
#'
# Packages and settings: ----------------------------------------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling

# Loading VetStat data
load("K:/paper_vetstat/004_data_ready.RData") 




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
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/BlandAltman_CHR.tiff", width = 40, height = 20, units = "cm", dpi=300)
# width = 40, height = 20, units = "cm",


blandr.draw(df_ATC$UDD, df_ATC$ADD, plotTitle = "Bland-Altman Plot per ATC")
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/BlandAltman_ATC.tiff", width = 40, height = 20, units = "cm", dpi=300)
# width = 40, height = 20, units = "cm",


# for paper
# interpret BA plot:
# Look for any systematic bias: If the mean difference is far from zero, it suggests a bias between the two measurements.
# Examine the spread of the differences: The dashed lines (limits of agreement) help assess the variability of the differences. If most of the data points lie within the limits, it indicates good agreement.
# Identify any outliers: Check if there are any data points that fall outside the limits of agreement, as they may represent significant discrepancies or measurement errors.
# Consider clinical or practical significance: Determine if the observed agreement is within an acceptable range based on the context and purpose of the analysis.
# 
# Overall, the Bland-Altman plot provides visual insight into the agreement, bias, and variability between two columns of data.



