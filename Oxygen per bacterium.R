lability <- read.csv('lability.csv')
library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
lability$log <- log2(lability$O2_per_bacteria)
hist(lability$log,
     xlab = "Oxygen uptake per bacterium",
     col = "skyblue",
     breaks = 8)
test <- aov(log ~ Particle_type * Species, data = lability)
summary(test)

model3 <- lmer(log ~ Particle_type * Species + (1| Replicate), data = lability)
summary(model3)


ggplot(lability, aes(x = Particle_type, y = O2_per_bacteria, fill = Species)) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.6,
    outlier.shape = 21,
    alpha = 0.8
  ) +
  # add the mean as a diamond
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3,
    position = position_dodge(width = 0.75)
  ) +
  scale_fill_manual(
    values = c("G.hux" = "#1b9e77", "Skeletonema" = "#d95f02")
  ) +
  labs(
    x = "Particle Type",
    y = "Oxygen turnover Rate per capita(µmol  * L^-1 * d^-1 * )",
    fill = "Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(
      angle = 0, hjust = 0.5)
  )
