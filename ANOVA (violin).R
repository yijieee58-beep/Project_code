# Load packages
library(tidyverse)
library(broom)

# Step 1: Read and tag datasets (edit file paths as needed)
ska1 <- read_csv("SKA1T.csv") %>% mutate(Phyto_Species = "Skeletonema", Particle_Type = "Aggregate", Replicate = 1)
ska2 <- read_csv("SKA2T.csv") %>% mutate(Phyto_Species = "Skeletonema", Particle_Type = "Aggregate", Replicate = 2)
ska3 <- read_csv("SKA3.csv") %>% mutate(Phyto_Species = "Skeletonema", Particle_Type = "Aggregate", Replicate = 3)
skfp1 <- read_csv("SkFP1.csv") %>% mutate(Phyto_Species = "Skeletonema", Particle_Type = "Faecal pellets", Replicate = 1)
skfp2 <- read_csv("SkFP2.csv") %>% mutate(Phyto_Species = "Skeletonema", Particle_Type = "Faecal pellets", Replicate = 2)
skfp3 <- read_csv("SkFP3.csv") %>% mutate(Phyto_Species = "Skeletonema", Particle_Type = "Faecal pellets", Replicate = 3)
ghfp2 <- read_csv("GHFP2T.csv") %>% mutate(Phyto_Species = "G. hux", Particle_Type = "Faecal pellets", Replicate = 1)
ghfp3 <- read_csv("GHFP3.csv") %>% mutate(Phyto_Species = "G. hux", Particle_Type = "Faecal pellets", Replicate = 2)
ghfp4 <- read_csv("GHFP4.csv") %>% mutate(Phyto_Species = "G. hux", Particle_Type = "Faecal pellets", Replicate = 3)
gha1 <- read_csv("GHA1T.csv") %>% mutate(Phyto_Species = "G. hux", Particle_Type = "Aggregate", Replicate = 1)
gha2 <- read_csv("GHA2T.csv") %>% mutate(Phyto_Species = "G. hux", Particle_Type = "Aggregate", Replicate = 2)
gha3 <- read_csv("GHA3.csv") %>% mutate(Phyto_Species = "G. hux", Particle_Type = "Aggregate", Replicate = 3)

# Step 2: Combine all
combined_data <- bind_rows(ska1, ska2, ska3, skfp1, skfp2, skfp3, ghfp2, ghfp3, ghfp4, gha1, gha2, gha3) %>%
  drop_na(Oxygen)

# Step 1: Calculate raw slopes per replicate
slopes_raw <- combined_data %>%
  filter(!grepl("MQ", Sample)) %>%  # remove control samples early
  group_by(Sample, Phyto_Species, Particle_Type, Replicate) %>%
  summarise(
    Slope = coef(lm(Oxygen ~ Time_hr))[2],
    .groups = "drop"
  )

# Step 2: Calculate MQ slopes per replicate
mq_slopes <- combined_data %>%
  filter(Sample == "MQ") %>%
  group_by(Replicate) %>%
  summarise(
    MQ_Slope = coef(lm(Oxygen ~ Time_hr))[2],
    .groups = "drop"
  )

# Step 3: Join MQ slope and compute normalized slope
slopes_df <- slopes_raw %>%
  left_join(mq_slopes, by = "Replicate") %>%
  mutate(MQ_Normalized_Slope = Slope - MQ_Slope)

# Step 4: Optional — remove negative values
slopes_filtered <- slopes_df %>%
  filter(MQ_Normalized_Slope < 0)

# Step 5: Violin plot
library(ggplot2)
# Violin plot without "Normalized" in title
ggplot(slopes_filtered, 
       aes(x = Particle_Type, 
           y = -MQ_Normalized_Slope, 
           fill = Phyto_Species)) +
  geom_violin(trim = FALSE, alpha = 0.5, position = position_dodge(width = 0.9)) +
  geom_jitter(aes(color = Phyto_Species), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), 
              size = 1.5, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", 
               aes(group = Phyto_Species), 
               position = position_dodge(width = 0.9),
               shape = 22, size = 3, fill = "black") +
  labs(x = "Particle Type",
       y = expression("Oxygen consumption Rate (µmol " * L^-1 * h^-1 * ")"),
       fill = "Species",
       color = "Species") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )





####check how many data points were removed###
# Count total data points before filtering
n_total <- nrow(slopes_df)

# Filter to keep only negative slopes
slopes_filtered <- slopes_df %>%
  filter(MQ_Normalized_Slope < 0)

# Count data points after filtering
n_filtered <- nrow(slopes_filtered)

# Calculate how many were removed
n_removed <- n_total - n_filtered

# Print results
cat("Total rows before filtering:", n_total, "\n")
cat("Total rows after filtering:", n_filtered, "\n")
cat("Number of data points removed:", n_removed, "\n")






#perform t test between two particle types and two phytoplankton species 
ghux <- slopes_filtered %>% filter(Phyto_Species == "G. hux") %>% pull(MQ_Normalized_Slope)
skel <- slopes_filtered %>% filter(Phyto_Species == "Skeletonema") %>% pull(MQ_Normalized_Slope)

t.test(ghux, skel)
# Subset slopes by particle type
agg <- slopes_filtered %>% filter(Particle_Type == "Aggregate") %>% pull(MQ_Normalized_Slope)
fp  <- slopes_filtered %>% filter(Particle_Type == "Faecal pellets") %>% pull(MQ_Normalized_Slope)

# Perform Welch's t-test
t.test(agg, fp)


##Perform square root transformation for ANOVA
slopes_filtered <- slopes_filtered %>%
  mutate(Sqrt_Slope = sqrt(-MQ_Normalized_Slope))  # use abs to handle negative values safely

model_sqrt <- aov(Sqrt_Slope ~ Phyto_Species * Particle_Type, data = slopes_filtered)
residuals_sqrt <- residuals(model_sqrt)

shapiro.test(residuals_sqrt)
qqnorm(residuals_sqrt); qqline(residuals_sqrt, col = "red")

summary(model_sqrt)








