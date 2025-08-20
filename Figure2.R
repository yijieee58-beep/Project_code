data <- read.csv('overall_data.csv')
library(tidyverse)
library(broom)


# palette
pal <- c("G. huxleyi" = "#32CD32",    # lighter green
         "S. costatum" = "#A9A9A9")   # lighter grey


unique(data$Species)
data <- data %>%
  mutate(Species = str_trim(Species))  # remove leading/trailing spaces

p2 <- ggplot(data, aes(Particle_type, Daily_O2, fill = Species)) +
  geom_violin(trim = FALSE, colour = "black", alpha = 0.8) +  # keep full smooth shape
  geom_point(position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
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
    y = expression(Oxygen~Uptake~Rate~(mu*mol~L^{-1}~d^{-1}))
  ) +
  theme_minimal(base_size = 14)
p2
ggsave("figure of oxygen uptake.jpg", plot = oxygen, dpi = 300, width = 6, height = 4, units = "in")



dodge <- position_dodge2(width = 0.8, preserve = "single")

p2 <- ggplot(data, aes(x = Particle_type, 
                       y = O2_uptake_rate.umol_L.1_d.1., 
                       fill = Species, 
                       group = Species)) +
  geom_violin(trim = FALSE, colour = "black", alpha = 0.8,
              position = dodge, width = 0.8) +
  geom_point(position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
             shape = 21, size = 2.3, colour = "black", alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", shape = 15, size = 3, colour = "black",
               position = dodge) +
  scale_fill_manual(values = pal, name = "Species") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Particle Type",
    y = expression(Oxygen~Uptake~Rate~(mu*mol~L^{-1}~d^{-1}))
  ) +
  theme_minimal(base_size = 14)
p2
p2 <- ggplot(data, aes(Particle_type, O2_uptake_rate.umol_L.1_d.1., fill = Species)) +
  geom_violin(trim = FALSE, colour = "black", alpha = 0.8) +  # keep full smooth shape
  geom_point(position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
             shape = 21, size = 2.3, colour = "black", alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", shape = 15, size = 3, colour = "black",
               position = dodge) +
  scale_fill_manual(values = pal, name = "Species") +
  scale_y_continuous(
    limits = c(0, NA),                          # start at 0, no hard cap on top
    expand = expansion(mult = c(0, 0.05))       # tiny headroom at the top
  ) +
  labs(
    x = "Particle Type",
    y = expression(Oxygen~Uptake~Rate~(mu*mol~L^{-1}~d^{-1}))
  ) +
  theme_minimal(base_size = 14)
p2
