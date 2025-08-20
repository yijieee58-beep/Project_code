# Load packages
install.packages('tidyverse')
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

library(dplyr)
# Step 2: Combine all
combined_data <- bind_rows(ska1, ska2, ska3, skfp1, skfp2, skfp3, ghfp2, ghfp3, ghfp4, gha1, gha2, gha3) %>%
  drop_na(Oxygen)

# STEP 1: Calculate slope per vial (Oxygen ~ Time_hr)
slopes_df <- combined_data %>%
  group_by(Sample, Phyto_Species, Particle_Type, Replicate) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(Oxygen ~ Time_hr, data = .x)),
    slope = map_dbl(model, ~ coef(.x)[["Time_hr"]])
  ) %>%
  ungroup()

# STEP 2: Calculate treatment mean slopes
treatment_means <- slopes_df %>%
  group_by(Phyto_Species, Particle_Type) %>%
  summarise(Mean_Slope = mean(slope), .groups = "drop")

# STEP 3: Find vial with slope closest to the mean for each treatment
rep_vials <- slopes_df %>%
  left_join(treatment_means, by = c("Phyto_Species", "Particle_Type")) %>%
  mutate(Diff = abs(slope - Mean_Slope)) %>%
  group_by(Phyto_Species, Particle_Type) %>%
  slice_min(order_by = Diff, n = 1) %>%
  ungroup()

# STEP 4: Filter original data to keep only representative vials
rep_vial_data <- combined_data %>%
  semi_join(rep_vials, by = c("Sample", "Replicate", "Phyto_Species", "Particle_Type"))

# STEP 5: Plot the oxygen time series for representative vials
ggplot(rep_vial_data, aes(x = Time_hr, y = Oxygen,
                          color = interaction(Phyto_Species, Particle_Type),
                          group = interaction(Sample, Replicate))) +
  geom_point(size = 2) +  # raw data points
  geom_smooth(method = "lm", se = FALSE, linetype = "solid", size = 1.2) +  # linear regression line
  labs(
    title = "Representative Oxygen Consumption Curves by Treatment",
    x = "Time (hr)",
    y = expression("Oxygen ("*mu*"mol L"^-1*")"),
    color = "Treatment"
  ) +
  theme_minimal()


# Export to CSV
write_csv(combined_data, "combined_data.csv")




