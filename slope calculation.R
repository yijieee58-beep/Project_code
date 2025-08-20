library(ggplot2)
library(dplyr)
combined <- read.csv('combined_data.csv')
# Function to fit linear model - corrected version
fit_oxygen_model <- function(combined) {
  if(nrow(combined) < 2) {
    return(data.frame(
      slope = NA, r_squared = NA, p_value = NA,
      n_points = nrow(combined), time_range = NA
    ))
  }
  
  model <- lm(Oxygen ~ Time_hr, data = combined)
  model_summary <- summary(model)
  
  return(data.frame(
    slope = coef(model)[2],
    r_squared = model_summary$r.squared,
    p_value = ifelse(nrow(combined) > 2, model_summary$coefficients[2, 4], NA),
    n_points = nrow(combined),
    time_range = max(combined$Time_hr) - min(combined$Time_hr)
  ))
}

# Calculate slopes for ALL samples
all_slopes <- combined %>%
  group_by(Phyto_Species, Particle_Type, Replicate, Trial, Sample) %>%
  do(fit_oxygen_model(.)) %>%
  ungroup() %>%
  mutate(
    oxygen_consumption_rate = abs(slope),  # Positive consumption rate (µmol/L/h)
    sample_type = ifelse(Trial == "MQ", "Control", "Sample"),
    Treatment = paste(Phyto_Species, Particle_Type, sep = "_")
  )

print(paste("Total samples with slopes:", nrow(all_slopes)))
print(paste("Samples with valid slopes:", sum(!is.na(all_slopes$slope))))
print("R-squared distribution:")
print(summary(all_slopes$r_squared))
print(paste("Samples with R² > 0.7:", sum(all_slopes$r_squared > 0.7, na.rm = TRUE)))
print(paste("Samples with R² > 0.5:", sum(all_slopes$r_squared > 0.5, na.rm = TRUE)))

control_check <- all_slopes %>%
  filter(Trial == "MQ") %>%
  select(Treatment, Phyto_Species, Particle_Type, Replicate, Sample, slope, r_squared)
print(control_check)
duplicate_controls <- control_check %>%
  group_by(Treatment, Replicate) %>%
  filter(n() > 1) %>%
  arrange(Treatment, Replicate)

# Calculate net consumption rates (sample - control)
net_consumption_all <- all_slopes %>%
  filter(Trial != "MQ") %>%  # Only sample trials
  left_join(control_slopes, by = c("Treatment", "Replicate")) %>%
  mutate(
    net_slope = slope - control_slope,  # Net slope (sample - control)
    net_consumption_rate = abs(net_slope)  # Net consumption rate (µmol/L/h)
  )

print(paste("Samples ready for carbon analysis:", nrow(net_consumption_all)))


write.csv(all_slopes, "all_slope1.csv", row.names = FALSE)





