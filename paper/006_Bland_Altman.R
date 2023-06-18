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
load("K:/paper_vetstat/004_data_for_BA.RData") 



# manuel BA (for understanding what happens) ----------------------------------

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73","#CC79A7", "#F0E442", "#0072B2", "#D55E00")

# ATC
df_ATC_BA <- df_ATC |>
  select(ADD, UDD)
df_ATC_BA$average <- rowMeans(df_ATC_BA)
df_ATC_BA$difference <- df_ATC_BA$UDD - df_ATC_BA$ADD  # create column for difference measurement
mean_difference <- mean(df_ATC_BA$difference)    # calculate mean difference

# calculate uppr and lower limits of the CI of 90%
lower_limit <- mean_difference - 1.91*sd(df_ATC_BA$difference )
upper_limit <- mean_difference + 1.91*sd(df_ATC_BA$difference )

# Plot ATC
ggplot(df_ATC_BA, aes(x = average, y = difference)) +
  geom_hline(yintercept = mean_difference, color= "#D55E00", lwd=2) +
  geom_hline(yintercept = lower_limit, color = "#009E73", lwd=2) +
  geom_hline(yintercept = upper_limit, color = "#009E73", lwd=2) +
  geom_point(shape=23, fill="#56B4E9", size=4) +
  theme_bw() +
  theme(text = element_text(size = 22))
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/BA_ATC.tiff", width = 40, height = 20, units = "cm", dpi=300)


# CHR
df_CHR_BA <- df_CHR |>
  select(ADD, UDD)
df_CHR_BA$average <- rowMeans(df_CHR_BA)
df_CHR_BA$difference <- df_CHR_BA$UDD - df_CHR_BA$ADD  # create column for difference measurement
mean_difference <- mean(df_CHR_BA$difference)    # calculate mean difference

# calculate uppr and lower limits of the CI of 90%
lower_limit <- mean_difference - 1.91*sd(df_CHR_BA$difference )
upper_limit <- mean_difference + 1.91*sd(df_CHR_BA$difference )

# Plot CHR
ggplot(df_CHR_BA, aes(x = average, y = difference)) +
  geom_hline(yintercept = mean_difference, color= "#D55E00", lwd=2) +
  geom_hline(yintercept = lower_limit, color = "#009E73", lwd=2) +
  geom_hline(yintercept = upper_limit, color = "#009E73", lwd=2) +
  geom_point(shape=23, fill="#56B4E9", size=2.5) +
  theme_bw() +
  theme(text = element_text(size = 22))
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/BA_CHR.tiff", width = 40, height = 20, units = "cm", dpi=300)


# ID_disease_group
df_disease_BA <- df_disease_names |>
  select(sum_ADD, sum_UDD)
df_disease_BA$average <- rowMeans(df_disease_BA)
df_disease_BA$difference <- df_disease_BA$sum_UDD - df_disease_BA$sum_ADD  # create column for difference measurement
mean_difference <- mean(df_disease_BA$difference)    # calculate mean difference

# calculate uppr and lower limits of the CI of 90%
lower_limit <- mean_difference - 1.91*sd(df_disease_BA$difference )
upper_limit <- mean_difference + 1.91*sd(df_disease_BA$difference )

# Plot disease
ggplot(df_disease_BA, aes(x = average, y = difference)) +
  geom_hline(yintercept = mean_difference, color= "#D55E00", lwd=2) +
  geom_hline(yintercept = lower_limit, color = "#009E73", lwd=2) +
  geom_hline(yintercept = upper_limit, color = "#009E73", lwd=2) +
  geom_point(shape=23, fill="#56B4E9", size=4) +
  theme_bw() +
  theme(text = element_text(size = 22))
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/BA_disease.tiff", width = 40, height = 20, units = "cm", dpi=300)





# blandr bland-altman package -------------------------------------------------

# https://cran.r-project.org/web/packages/blandr/vignettes/introduction.html
# devtools::install_github("deepankardatta/blandr")

library("blandr")
blandr.draw(df_CHR$UDD, df_CHR$ADD, plotTitle = "Bland-Altman Plot per CHR number")
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/BA_blandr_CHR.tiff", width = 40, height = 20, units = "cm", dpi=300)
# width = 40, height = 20, units = "cm",


blandr.draw(df_ATC$UDD, df_ATC$ADD, plotTitle = "Bland-Altman Plot per ATC") +
  theme(text = element_text(size = 12)) 
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/BA_blandr_ATC.tiff", width = 40, height = 20, units = "cm", dpi=300)
# width = 40, height = 20, units = "cm",


blandr.draw(df_disease$UDD, df_disease$ADD, 
            plotTitle = "Bland-Altman Plot per disease group")
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/BA_blandr_disease.tiff", width = 40, height = 20, units = "cm", dpi=300)

# for paper
# interpret BA plot:
# Look for any systematic bias: If the mean difference is far from zero, it suggests a bias between the two measurements.
# Examine the spread of the differences: The dashed lines (limits of agreement) help assess the variability of the differences. If most of the data points lie within the limits, it indicates good agreement.
# Identify any outliers: Check if there are any data points that fall outside the limits of agreement, as they may represent significant discrepancies or measurement errors.
# Consider clinical or practical significance: Determine if the observed agreement is within an acceptable range based on the context and purpose of the analysis.
# 
# Overall, the Bland-Altman plot provides visual insight into the agreement, bias, and variability between two columns of data.


