bacteria <- read.csv('Flow_Cytometry.csv')
anova <- aov(Event ~ Species * Particle_type, data = bacteria) 
summary(anova)
model_bacteria <- lmer(transform ~ Particle_type * Species + (1|Experiment), 
                       data = bacteria)
summary(model_bacteria)
lm_bacterial <- lm(transform ~ Particle_type * Species,
                   data = bacteria)
summary(lm_bacterial)

bacteria$transform <- log(bacteria$Density)
hist(bacteria$Density,
     xlab = "Bacterial abundance",
     col = "skyblue",
     breaks = 8)



library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)
library(scales)

# 2) Make sure your grouping factors are ordered how you like
bacteria <- bacteria %>%
  mutate(
    Particle_type = factor(Particle_type, levels = c("Aggregate", "Faecal_pellet")),
    Species       = factor(Species,       levels = c("G.hux", "Skeletonema"))
  )

# 3) Draw the boxplot
figure_bacteria <- ggplot(bacteria, aes(x = Particle_type, y = Density, fill = Species)) +
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
  scale_fill_manual(values = pal, name = "Species",
                    labels = c(expression(italic('G. huxleyi')),
                               expression(italic('S. costatum')))
  ) +
  labs(
    y = expression("Bacterial abundance " ~  (cells~mL^{-1})),
    x = "Particle Type"
  ) +
  scale_y_continuous(
    labels = function(x) {
      ifelse(x == 0, "0", 
             parse(text = gsub("e\\+?", " %*% 10^", scales::scientific_format()(x))))
    },
    expand = expansion(mult = c(0, 0.15))  # Reduced space for brackets
  ) +
  
  # Brackets between species for each particle type (lower level)
  # Aggregate comparison (G. huxleyi vs S. costatum)
  annotate("segment", x = 0.7, xend = 1.3, y = max(bacteria$Density, na.rm = TRUE) * 1.08, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.08) +
  annotate("segment", x = 0.7, xend = 0.7, y = max(bacteria$Density, na.rm = TRUE) * 1.06, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.08) +
  annotate("segment", x = 1.3, xend = 1.3, y = max(bacteria$Density, na.rm = TRUE) * 1.06, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.08) +
  annotate("text", x = 1, y = max(bacteria$Density, na.rm = TRUE) * 1.10, 
           label = "ns", size = 4) +
  
  # Faecal pellet comparison (G. huxleyi vs S. costatum)
  annotate("segment", x = 1.7, xend = 2.3, y = max(bacteria$Density, na.rm = TRUE) * 1.08, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.08) +
  annotate("segment", x = 1.7, xend = 1.7, y = max(bacteria$Density, na.rm = TRUE) * 1.06, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.08) +
  annotate("segment", x = 2.3, xend = 2.3, y = max(bacteria$Density, na.rm = TRUE) * 1.06, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.08) +
  annotate("text", x = 2, y = max(bacteria$Density, na.rm = TRUE) * 1.10, 
           label = "**", size = 4) +
  
  # Brackets between particle types within each species (upper level)
  # G. huxleyi: Aggregate vs Faecal pellet
  annotate("segment", x = 0.7, xend = 1.7, y = max(bacteria$Density, na.rm = TRUE) * 1.12, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.12) +
  annotate("segment", x = 0.7, xend = 0.7, y = max(bacteria$Density, na.rm = TRUE) * 1.11, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.12) +
  annotate("segment", x = 1.7, xend = 1.7, y = max(bacteria$Density, na.rm = TRUE) * 1.11, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.12) +
  annotate("text", x = 1.15, y = max(bacteria$Density, na.rm = TRUE) * 1.14, 
           label = "ns", size = 4) +
  
  # S. costatum: Aggregate vs Faecal pellet
  annotate("segment", x = 1.25, xend = 2.25, y = max(bacteria$Density, na.rm = TRUE) * 1.14, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.14) +
  annotate("segment", x = 1.25, xend = 1.25, y = max(bacteria$Density, na.rm = TRUE) * 1.13, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.14) +
  annotate("segment", x = 2.25, xend = 2.25, y = max(bacteria$Density, na.rm = TRUE) * 1.13, 
           yend = max(bacteria$Density, na.rm = TRUE) * 1.14) +
  annotate("text", x = 1.8, y = max(bacteria$Density, na.rm = TRUE) * 1.16, 
           label = "ns", size = 4) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(
      angle = 0, hjust = 0.5)
  )
figure_bacteria
 ggsave("figure of bacteria.jpg", plot = figure_bacteria, dpi = 300, width = 6, height = 4, units = "in")








bacterial_summary <- bacteria %>%
  group_by(Species, Particle_type) %>%
  summarise(
    n = n(),
    mean_bacteria = mean(Density, na.rm = TRUE),
    se_bacteria = sd(Density, na.rm = TRUE) / sqrt(n()),
    sd_bacteria = sd(Density, na.rm = TRUE),
    mean_se_bacteria = paste0(round(mean_bacteria, 1), " ± ", round(se_bacteria, 1)),
    .groups = 'drop'
  )
print(bacterial_summary)
