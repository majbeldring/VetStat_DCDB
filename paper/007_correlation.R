# correlation analysis

# use Spearman (vi assume normal distribution, which we DON'T have)
# below: show Matt. Add to paper... Weird correlation for CHR
# Søren: Use this plot. R^2 might be enough.
# ask matt: log-log vs R^2.. Log-log might not be ideal with our distribution...

# colour by:
## herd size
## disease/treatment (mastitis etc): This could be use to explain the below
## several days treatment. This could explain: maybe the farmer only register once.. and not the following days..
## Add disease group

# packages -------------------------------------------------------------------
library(tidyverse)
library(ggpubr) # for stat_cor


# load data ------------------------------------------------------------------

load("K:/paper_vetstat/004_data_for_BA.RData") 

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73","#CC79A7", "#F0E442", "#0072B2", "#D55E00")


# FIX:
# combine DCDB_AMU_UDD and VetStat_AMU_ADD.
# We want:
# CHR; Name (disease); UDD; ADD

# 1. remove ATC from both -> summarize CHR and ID_disease_group
# 2. Add Name (diseases) -> remove ID_disease_group -> summarize name
# 3. Merge Vetstat and DCDB
# 4. Final data...


# correlations ---------------------------------------------------------------

# spearman CHR

cor_herd <- cor(df_CHR$UDD, df_CHR$ADD, method = "spearman"); print(cor_herd)
cor.test(df_CHR$UDD, df_CHR$ADD, method = "spearman")
hist(df_CHR$UDD); hist(df_CHR$ADD) # to see distributions of ADD and UDD

ggplot(df_CHR, aes(ADD, UDD)) + 
  geom_point(color='#009E73', size = 2.5) +
  xlim(0, 500) +
  ylim(0, 500) +
  stat_cor(method = "pearson") +   # note; R=0.78 applies to data within x,y lim
  theme_bw() +
  theme(text = element_text(size = 22))
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/Spearman_CHR.tiff", width = 40, height = 20, units = "cm", dpi=300)


# Spearman ratio, if splitting data 1:2 
ratio <- 6/1

# Filter the data based on the patterns
df1 <- df_CHR %>%
  filter(ADD <= ratio * UDD)

df2 <- df_CHR %>%
  filter(ADD > ratio * UDD)

# Calculate the Spearman correlation for each pattern
cor1 <- cor(df1$ADD, df1$UDD, method = "spearman")
cor2 <- cor(df2$ADD, df2$UDD, method = "spearman")

# Create the correlation data for plotting
correlation_data <- data.frame(
  pattern = c("Pattern 1", "Pattern 2"),
  correlation = c(cor1, cor2)
)

# Create the ggplot with points and correlations
correlation_plot <- ggplot(df_CHR, aes(ADD, UDD)) +
  geom_point(color = '#009E73', size = 2.5) +
  xlim(0, 500) +
  ylim(0, 500) +
  stat_cor(method = "pearson") +
  theme_bw() +
  theme(text = element_text(size = 22))

# Add lines for each pattern using the correlation as the slope
correlation_plot +
  geom_abline(slope = cor1, intercept = 0, color = "red", linetype = "dashed") +
  geom_abline(slope = cor2, intercept = 0, color = "red", linetype = "dashed")
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/Spearman_CHR.tiff", width = 40, height = 20, units = "cm", dpi=300)






# Spearman correlation disease (prescription group) --------------------------
cor_disease <- cor(df_CHR$UDD, df_CHR$ADD, method = "spearman")
cor.test(df_CHR$UDD, df_CHR$ADD, method = "spearman")
hist(df_CHR$UDD)
hist(df_CHR$ADD)

plot(df_CHR$ADD ~ df_CHR$UDD, xlim=c(0,500), ylim=c(0,500))

ggplot(df_CHR, aes(ADD, UDD)) + 
  geom_point()



# Spearman correlation ATC:
cor_atc <- cor(df_ATC$UDD, df_ATC$ADD)
cor.test(df_ATC$UDD, df_ATC$ADD, method = "spearman")

hist(df_ATC$UDD)
hist(df_ATC$ADD)

plot(df_ATC$ADD ~ df_ATC$UDD, xlim=c(0,500), ylim=c(0,500))

