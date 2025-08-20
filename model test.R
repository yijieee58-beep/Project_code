SKA1T<- read.csv('SKA1T.csv')
SKA2T<- read.csv('SKA2T.csv')
SKFP1<- read.csv('SKFP1.csv')
GHA1T<- read.csv('GHA1T.csv')
GHA2T<- read.csv('GHA2T.csv')
#GHFP1T<- read.csv('GHFP1T.csv')
GHFP2T<- read.csv('GHFP2T.csv')

Data<- read.csv('Project_data.csv')
# Load required libraries
install.packages('tidyverse')
library(tidyverse)
install.packages('car')
library(car)        # for Levene's test
install.packages('emmeans')
library(emmeans)    # for post-hoc comparisons
library(broom)      # for tidy output

data <- data.frame(
  Particle_type = c(
    rep("Aggregate", 20),           # 10 G.hux + 10 Skeletonema
    rep("Faecal_pellets", 10)       # 5 G.hux + 5 Skeletonema (Rep 2 missing)
  ),
  Phyto_Species = c(
    rep("G.hux", 10), rep("Skeletonema", 10),        # Aggregates
    rep("G.hux", 5), rep("Skeletonema", 5)          # Faecal pellets
  ),
  Replicate = c(
    rep(1, 5), rep(2, 5),           # Aggregate G.hux
    rep(1, 5), rep(2, 5),           # Aggregate Skeletonema  
    rep(1, 5),           # Faecal pellets G.hux
    rep(1, 5)                       # Faecal pellets Skeletonema (only Rep 1)
  ),
  Sample = c(1:5, 1:5, 1:5, 1:5, 1:5, 1:5),
  Normalized_slope = c(
    # Aggregate G.hux Rep 1
    -0.851, -0.676, -0.938, -0.722, -0.946,
    # Aggregate G.hux Rep 2
    -0.785, -0.616, -0.553, -0.585, -0.569,
    # Aggregate Skeletonema Rep 1
    -0.219, -0.264, -0.307, -0.356, -0.519,
    # Aggregate Skeletonema Rep 2
    -0.346, -0.229, -0.303, -0.281, -0.525,
    # Faecal pellets G.hux Rep 1
    -0.400, -0.650, -1.180, -1.130, -1.080,
    # Faecal pellets Skeletonema Rep 1 (Rep 2 not done yet)
    -0.179, -0.265, -0.308, -0.551, -0.929
  )
)

# Convert to factors
data$Particle_type <- as.factor(data$Particle_type)
data$Phyto_Species <- as.factor(data$Phyto_Species)
data$Replicate <- as.factor(data$Replicate)

# Display the data structure
print("=== DATA STRUCTURE ===")
str(data)
print("\nFirst 10 rows:")
print(head(data, 10))
print("\nLast 10 rows:")
print(tail(data, 10))

# Check data completeness
print("\n=== DATA COMPLETENESS CHECK ===")
completeness <- data %>%
  group_by(Particle_type, Phyto_Species, Replicate) %>%
  summarise(n_samples = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Replicate, values_from = n_samples, 
              names_prefix = "Rep_", values_fill = 0)
print(completeness)

# Summary statistics by treatment combination
print("\n=== SUMMARY STATISTICS BY TREATMENT ===")
summary_stats <- data %>%
  group_by(Particle_type, Phyto_Species) %>%
  summarise(
    n_total = n(),
    n_replicates = n_distinct(Replicate),
    mean_slope = round(mean(Normalized_slope), 3),
    sd_slope = round(sd(Normalized_slope), 3),
    min_slope = round(min(Normalized_slope), 3),
    max_slope = round(max(Normalized_slope), 3),
    .groups = 'drop'
  )
print(summary_stats)



# Create treatment combinations for clearer analysis
data$Treatment <- paste(data$Particle_type, data$Phyto_Species, sep = "_")

print("\n=== TREATMENT MEANS ===")
treatment_means <- data %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    mean = round(mean(Normalized_slope), 3),
    sd = round(sd(Normalized_slope), 3),
    .groups = 'drop'
  )
print(treatment_means)

# ASSUMPTION CHECKS
print("\n=== ASSUMPTION CHECKS ===")

# 1. Normality check (overall)
print("1. Normality Test (Shapiro-Wilk) - Overall:")
shapiro_overall <- shapiro.test(data$Normalized_slope)
print(shapiro_overall)

# 2. Normality by treatment (if sample size allows)
print("\n2. Normality by Treatment:")
norm_by_treatment <- data %>%
  group_by(Treatment) %>%
  summarise(
    n = n(),
    shapiro_p = ifelse(n >= 3, shapiro.test(Normalized_slope)$p.value, NA),
    .groups = 'drop'
  )
print(norm_by_treatment)

# 3. Homogeneity of variance (Levene's test)
print("\n3. Homogeneity of Variance (Levene's Test):")
levene_test <- leveneTest(Normalized_slope ~ Particle_type * Phyto_Species, data = data)
print(levene_test)

# VISUALIZATION
print("\n=== CREATING VISUALIZATIONS ===")

# Boxplot
p1 <- ggplot(data, aes(x = Particle_type, y = Normalized_slope, fill = Phyto_Species)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2)) +
  labs(title = "Normalized Slope by Particle Type and Phyto Species",
       x = "Particle Type", y = "Normalized Slope") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# Interaction plot
interaction_data <- data %>%
  group_by(Particle_type, Phyto_Species) %>%
  summarise(mean_slope = mean(Normalized_slope), .groups = 'drop')

p2 <- ggplot(interaction_data, aes(x = Particle_type, y = mean_slope, 
                                   color = Phyto_Species, group = Phyto_Species)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(title = "Interaction Plot: Mean Normalized Slope",
       x = "Particle Type", y = "Mean Normalized Slope") +
  theme_minimal()

print(p2)

# TWO-WAY ANOVA
print("\n=== TWO-WAY ANOVA RESULTS ===")

# Fit the model
anova_model <- aov(Normalized_slope ~ Particle_type * Phyto_Species, data = data)

# ANOVA table
print("ANOVA Summary:")
anova_summary <- summary(anova_model)
print(anova_summary)

# Effect sizes (eta-squared)
print("\nEffect Sizes (Eta-squared):")
ss_total <- sum((data$Normalized_slope - mean(data$Normalized_slope))^2)
ss_particle <- anova_summary[[1]][1, 2]
ss_species <- anova_summary[[1]][2, 2]
ss_interaction <- anova_summary[[1]][3, 2]

eta_sq_particle <- ss_particle / ss_total
eta_sq_species <- ss_species / ss_total
eta_sq_interaction <- ss_interaction / ss_total

print(paste("Particle type η² =", round(eta_sq_particle, 3)))
print(paste("Phyto Species η² =", round(eta_sq_species, 3)))
print(paste("Interaction η² =", round(eta_sq_interaction, 3)))

# Model diagnostics
print("\n=== MODEL DIAGNOSTICS ===")
par(mfrow = c(2, 2))
plot(anova_model)
par(mfrow = c(1, 1))

# POST-HOC TESTS (if significant effects found)
print("\n=== POST-HOC COMPARISONS ===")

# Estimated marginal means
emm_particle <- emmeans(anova_model, ~ Particle_type)
emm_species <- emmeans(anova_model, ~ Phyto_Species)
emm_interaction <- emmeans(anova_model, ~ Particle_type * Phyto_Species)

print("Estimated Marginal Means:")
print("By Particle Type:")
print(emm_particle)
print("\nBy Phyto Species:")
print(emm_species)
print("\nBy Interaction:")
print(emm_interaction)



####Test Linear model and Linear mixed model#####
lm(Normalized_slope ~ Particle_type * Phyto_Species, data = data)

library(lme4)
remove.packages("Matrix")
remove.packages("lme4")

install.packages("Matrix", dependencies = TRUE)
install.packages("Matrix", repos = "https://cloud.r-project.org", dependencies = TRUE)

install.packages("lme4")
install.packages("lmerTest", dependencies = TRUE)

library(Matrix)
library(lme4)
library(lmerTest)  # Optional, for p-values

model <- lmer(Normalized_slope ~ Particle_type * Phyto_Species + (1 | Replicate), data = data)
summary(model)
install.packages("emmeans")
library(emmeans)

# Calculate estimated means for each combination
emm <- emmeans(model, ~ Particle_type * Phyto_Species)

# Pairwise comparisons
pairs(emm)




