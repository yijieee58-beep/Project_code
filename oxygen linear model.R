oxygen <- read.csv('overall_data.csv')
library(dplyr)
library(ggplot2)


hist(oxygen$O2_uptake_rate.umol_L.1_d.1.,
     main = "Histogram of oxygen consumption Rate ",
     xlab = "oxygen turnover Rate",
     col = "skyblue",
     breaks = 15)
oxygen$transform <- log2(oxygen$after.minus.MQ.slope)
#oxygen$transform <- log(oxygen$oxygen_slope_per_day + 1)  # or whatever small value makes sense
hist(oxygen$transform,
     main = "Histogram of oxygen consumption Rate ",
     xlab = "oxygen turnover Rate",
     col = "skyblue",
     breaks = 25)


model_oxygen <- lm(transform ~ Species * Particle_type, data = oxygen)
summary(model_oxygen)

install.packages("pscl")
install.packages("cplm")
library(cplm)






# Visual checks
par(mfrow = c(2, 2))
# QQ plot (most important!)
qqnorm(residuals)
qqline(residuals)

# Histogram of residuals
hist(residuals, main = "Histogram of Residuals", breaks = 15)




oxygen_formatted <- oxygen %>%
  group_by(Species, Particle_type) %>%
  summarise(
    n = n(),
    mean_O2 = mean(O2_uptake_rate.umol_L.1_d.1., na.rm = TRUE),
    sd_O2 = sd(O2_uptake_rate.umol_L.1_d.1., na.rm = TRUE),
    se_O2 = sd(O2_uptake_rate.umol_L.1_d.1., na.rm = TRUE) / sqrt(n()),
    mean_se = paste0(round(mean_O2, 2), " ± ", round(se_O2, 2)),
    min_O2 = min(O2_uptake_rate.umol_L.1_d.1., na.rm = TRUE),
    max_O2 = max(O2_uptake_rate.umol_L.1_d.1., na.rm = TRUE),
    range_O2 = max_O2 - min_O2,
    .groups = 'drop'
  )

print(oxygen_formatted)





##### Original version of violin plot #####
ggplot(oxygen, 
       aes(x = Particle_type, 
           y = O2_uptake_rate.umol_L.1_d.1., 
           fill = Species)) +
  geom_violin(trim = FALSE, alpha = 0.5, position = position_dodge(width = 0.9)) +
  geom_jitter(aes(color = Species), 
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), 
              size = 1.5, alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", 
               aes(group = Species), 
               position = position_dodge(width = 0.9),
               shape = 22, size = 3, fill = "black") +
  labs(x = "Particle Type",
       y = expression("Oxygen consumption Rate (µmol " * L^-1 * d^-1 * ")"),
       fill = "Species",
       color = "Species") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
