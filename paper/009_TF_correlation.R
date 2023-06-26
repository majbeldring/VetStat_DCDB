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




# correlations TF data ---------------------------------------------------------------

# spearman CHR

cor_herd <- cor(df_CHR$TF_UDD, df_CHR$TF_ADD, method = "spearman"); print(cor_herd)
cor.test(df_CHR$TF_UDD, df_CHR$TF_ADD, method = "spearman")
hist(df_CHR$TF_UDD); hist(df_CHR$TF_ADD) # to see distributions of ADD and UDD

ggplot(df_CHR, aes(TF_ADD, TF_UDD)) + 
  geom_point(color='#009E73', size = 2.5) +
  stat_cor(method = "pearson", size = 7) +   # note; R=0.78 applies to data within x,y lim
  labs(x = "TF ADD", y = "TF UDD") +  # Set the x-axis and y-axis labels
  theme_bw() +
  theme(text = element_text(size = 22))
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/Spearman_TF_CHR.tiff", width = 40, height = 20, units = "cm", dpi=300)


# Spearman ratio, if splitting data 1:2 
ratio <- 6/1

# Filter the data based on the patterns
df1 <- df_CHR %>%
  filter(TF_ADD <= ratio * TF_UDD)

df2 <- df_CHR %>%
  filter(TF_ADD > ratio * TF_UDD)

# Calculate the Spearman correlation for each pattern
cor1 <- cor(df1$TF_ADD, df1$TF_UDD, method = "spearman")
cor2 <- cor(df2$TF_ADD, df2$TF_UDD, method = "spearman")

# Create the correlation data for plotting
correlation_data <- data.frame(
  pattern = c("Pattern 1", "Pattern 2"),
  correlation = c(cor1, cor2)
)

# Create the ggplot with points and correlations
correlation_plot <- ggplot(df_CHR, aes(TF_ADD, TF_UDD)) +
  geom_point(color = '#009E73', size = 2.5) +
  xlim(0, 20) +
  ylim(0, 20) +
  stat_cor(method = "spearman", size = 7) +
  labs(x = "TF ADD", y = "TF UDD") +  # Set the x-axis and y-axis labels
  theme_bw() +
  theme(text = element_text(size = 22))

# Add lines for each pattern using the correlation as the slope
correlation_plot +
  geom_abline(slope = cor1, intercept = 0, color = "red", linetype = "dashed") +
  geom_abline(slope = cor2, intercept = 0, color = "red", linetype = "dashed")
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/Spearman_TF_CHR_ratio.tiff", width = 40, height = 20, units = "cm", dpi=300)






# Spearman correlation TF_disease (prescription group) --------------------------
cor_disease <- cor(df_disease$TF_UDD, df_disease$TF_ADD, method = "spearman")
cor.test(df_disease$TF_UDD, df_disease$TF_ADD, method = "spearman")
hist(df_disease$TF_UDD)
hist(df_disease$TF_ADD)

ggplot(df_disease_names, aes(sum_TF_ADD, sum_TF_UDD)) +
  geom_point(size = 5, shape = 23, fill = "#009E73", color = "#D55E00") +
  geom_point(data = subset(df_disease_names, Name == "Udder"), 
             aes(sum_TF_ADD, sum_TF_UDD), size = 8, shape = 4, fill = "red", color = "black") +
  stat_cor(method = "pearson", size = 7) +
  theme_bw() +
  theme(text = element_text(size = 22))


ggplot(df_disease_names, aes(sum_TF_ADD, sum_TF_UDD)) +
  geom_point(size = 5, shape = 23, fill = "#009E73", color = "#D55E00") +
  geom_text(data = subset(df_disease_names, Name == "Udder"),
            aes(label = Name), hjust = 1, vjust = 1, color = "black", size = 7, nudge_x = 1, nudge_y = 1) +
  stat_cor(method = "pearson", size = 7) +
  labs(x = "TF ADD", y = "TF UDD") +  # Set the x-axis and y-axis labels
  theme_bw() +
  theme(text = element_text(size = 22))
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/Spearman_TF_disease.tiff", width = 40, height = 20, units = "cm", dpi=300)




# Spearman correlation TF_ATC --------------------------------------------------
cor_atc <- cor(df_ATC$TF_UDD, df_ATC$TF_ADD)
cor.test(df_ATC$TF_UDD, df_ATC$TF_ADD, method = "spearman")

hist(df_ATC$TF_UDD)
hist(df_ATC$TF_ADD)

# correlation plot
# QJ01CE09 is the outlier
ggplot(df_ATC, aes(TF_ADD, TF_UDD)) +
  geom_point(size = 5, shape = 23, fill = "#009E73", color = "#D55E00") +
  geom_text(data = subset(df_ATC, ATC == "QJ01CE09"),
            aes(label = ATC), hjust = 1, vjust = 1.3, color = "black", size = 7, nudge_x = 1, nudge_y = 1) +
  stat_cor(method = "pearson", size = 7) +
  labs(x = "TF ADD", y = "TF UDD") +  # Set the x-axis and y-axis labels
  theme_bw() +
  theme(text = element_text(size = 22))
ggsave("C:/Users/zjt234/PhD/PaperIII_VetStat/Spearman_TF_ATC.tiff", width = 40, height = 20, units = "cm", dpi=300)


