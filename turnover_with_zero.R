install.packages('tidyverse')
library(tidyverse)
install.packages('performance')
library(performance) # For model diagnostics
install.packages('sjPlot')
library(sjPlot)      # For model visualization

library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
library(broom)

turnover<- read.csv('turnover1.csv')
overall <- read.csv('overall_data.csv')
turnover1 <- turnover %>%
  filter(!is.na(Experiment) & !is.na(Carbon_turnover_rate)) %>%
  # Create a treatment variable combining source material and particle type
  mutate(Treatment = paste(`Species`, `Particle_type`, sep = " - "))
overall1 <- overall %>%
  filter(!is.na(Replicate) & !is.na(k.d.1.)) %>%
  # Create a treatment variable combining source material and particle type
  mutate(Treatment = paste(`Species`, `Particle_type`, sep = " - "))




#####try excluding the samples with zero carbon detected #####
# Create datasets: all data vs. excluding zero-carbon samples

data_no_zeros <- turnover1 %>%
  filter( Carbon_turnover_rate > 0) 
overallno0 <- overall1 %>%
  filter( k.d.1. > 0) 
overall_bacteria <- df_no_outliers%>%
  filter(Bacterial.count > 0)

# Plot 2: Excluding zero-carbon samples
p1_no_zeros <- ggplot(overallno0, aes(x = Treatment, y = overallno0$k.d.1.)) +
  geom_boxplot(aes(fill = Species), alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
  labs(
    title = "Excluding Zero-Carbon Samples",
    x = "Treatment",
    y = "Carbon Turnover Rate (d⁻¹)",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = c("G.huxleyi " = "#2E86AB", "Skeletonema sp. " = "#A23B72"))

print(p1_no_zeros)


###Check outliers######
# Calculate Q1, Q3, and IQR
Q1 <- quantile(overallno0$k.d.1., 0.25)
Q3 <- quantile(overallno0$k.d.1., 0.75)
IQR <- Q3 - Q1
# Define bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- overallno0$k.d.1. < lower_bound | overallno0$k.d.1. > upper_bound
# Z-score on sqrt-transformed data
z_scores <- scale(overallno0$k.d.1.)

# Detect outliers (|z| > 3)
outliers <- abs(z_scores) > 3

# View outliers
overallno0[outliers, ]

# Remove outliers
data_clean <- df_no_outliers[!outliers, ]

# View outliers
data_no_zeros[outliers, ]

# Remove outliers
df_no_outliers <- overallno0[!outliers, ]

#####Check normality####
hist(df_no_outliers$transform,
     main = "Histogram of Carbon Turnover Rate",
     xlab = "Carbon Turnover Rate",
     col = "skyblue",
     breaks = 15)

qqnorm(df_no_outliers$k.d.1.,
       main = "Q-Q Plot of Carbon Turnover Rate")
qqline(df_no_outliers$k.d.1., col = "red")

#transformation##
#df_no_outliers$transform <- sqrt(df_no_outliers$k.d.1.)
df_no_outliers$transform <- log2(df_no_outliers$k.d.1.)
# Add small constant to avoid infinite values
df_no_outliers$logit_turnover <- car::logit(df_no_outliers$Carbon_turnover_rate, adjust = 0.025)

data_clean <- na.omit(data_clean)
AIC(model_logit, model)


###### mean + sd #####
summary_stats <- df_no_outliers %>%
  group_by(Species, Particle_type) %>%
  summarise(
    n = n(),                                     # Sample size
    median = median(k.d.1.),
    mean = mean(k.d.1.),
    sd = sd(k.d.1.),
    se = sd / sqrt(n),                          # Standard error
    mean_minus_sd = mean - sd,
    mean_plus_sd = mean + sd,
    .groups = 'drop'
  ) %>%
  mutate(
    # Format the mean ± SD as text
    mean_sd_text = paste0(round(mean, 3), " ± ", round(sd, 3))
  )
print(summary_stats)


POC_no_zeros <- overallno0 %>%
  filter(POC_concentration.umol.L.1. > 0) 
summary_POC <- overallno0 %>%
  group_by(Species, Particle_type) %>%
  summarise(
    n = n(),                                     # Sample size
    median = median(POC_concentration.umol.L.1.),
    mean = mean(POC_concentration.umol.L.1.),
    sd = sd(POC_concentration.umol.L.1.),
    se = sd / sqrt(n),                          # Standard error
    mean_minus_sd = mean - sd,
    mean_plus_sd = mean + sd,
    .groups = 'drop'
  ) %>%
  mutate(
    # Format the mean ± SD as text
    mean_sd_text = paste0(round(mean, 3), " ± ", round(sd, 3))
  )
print(summary_POC)
 


df_no_outliers <- df_no_outliers %>%
  mutate(Species = str_trim(Species))  # remove leading/trailing spaces

# Violin plot 
unique(df_no_outliers$Species)
pal <- c("G. huxleyi" = "#32CD32",   # lighter green
         "S. costatum" = "#A9A9A9") # lighter grey
p4<- ggplot(df_no_outliers, aes(Particle_type, k.d.1., fill = Species)) +
  geom_violin(trim = FALSE, colour = "black", alpha = 0.8) +  # keep full smooth shape
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
             shape = 21, size = 2.3, colour = "black", alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", shape = 15, size = 3, colour = "black",
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = pal, name = "Species") +
  scale_y_continuous(
    limits = c(0, NA),                          # start at 0, no hard cap on top
    expand = expansion(mult = c(0, 0.05))       # tiny headroom at the top
  ) +
  labs(
    x = "Particle Type",
    y = expression(Carbon~Turnover~Rate~(d^{-1}))
  ) +
  theme_minimal(base_size = 14)
p4

ggsave("figure of turnover.jpg", plot = figure_turnover, dpi = 300, width = 6, height = 4, units = "in")









install.packages('simr')
library(simr)
library(emmeans)
install.packages('multcomp')
library(multcomp) 
library(lmerTest)
##Testing linear mixed model###
model <- lmer(transform ~ Particle_type * Species + (1|Replicate),
              data = df_no_outliers,
              REML = TRUE)
model2<- lmer(transform ~ Particle_type * Species * Bacterial.count + (1|Replicate),
              data = overall_bacteria)


aggregates_df <- subset(df_no_outliers, Particle_type == 'Aggregate')
model3 <- lmer(transform ~ Species +(1|Replicate),
               data = aggregates_df )
summary(model)
summary(model2)
summary(model3)
tab_model(model2, show.p = TRUE, show.stat = TRUE)





AIC<- AIC(model)
power_particle <- powerSim(model, test = fixed("Particle_type"), nsim = 100)
print(power_particle)
power_species <- powerSim(model, test = fixed("Species"), nsim = 100)
print(power_species)














