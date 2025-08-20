library(tidyverse)
dp <- read_csv("datapoint.csv", show_col_types = FALSE)
sl <- read_csv("slope.csv", show_col_types = FALSE)  # not used below

# Clean labels (in case your pellets are plural)
dp <- dp %>% mutate(Particle_Type = recode(Particle_Type, "Faecal pellets" = "Faecal pellet"))

# Get slope & intercept per (Species × Particle_Type) from the datapoints
fits <- dp %>%
  group_by(Phyto_Species, Particle_Type) %>%
  summarise(
    Slope = coef(lm(Oxygen ~ Timepoint))[["Timepoint"]],
    intercept = coef(lm(Oxygen ~ Timepoint))[["(Intercept)"]],
    .groups = "drop"
  )




###make legend in color
p1 <- ggplot(dp, aes(Timepoint, Oxygen)) +
  geom_point(aes(fill = Phyto_Species, shape = Particle_Type),
             color = "black", size = 3, stroke = 0.8) +
  geom_abline(data = fits,
              aes(slope = Slope, intercept = intercept,
                  group = interaction(Phyto_Species, Particle_Type),
                  color = Phyto_Species),
              linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(values = c("G. huxleyi" = "#32CD32",    # lighter green
                               "S. costatum" = "#A9A9A9")) +  # lighter grey
  scale_color_manual(values = c("G. huxleyi" = "#32CD32",
                                "S. costatum" = "#A9A9A9"),
                     guide = "none") +
  scale_shape_manual(values = c("Aggregate" = 24,  # filled triangle
                                "Faecal pellet" = 21)) + # filled circle
  guides(
    fill  = guide_legend(title = "Species",
                         override.aes = list(shape = 21, color = "black", size = 3)),
    shape = guide_legend(title = "Particle Type",
                         override.aes = list(fill = "white", color = "black", size = 3))
  ) +
  labs(x = "Time (hr)", y = expression(Oxygen~(mu*mol~L^{-1}))) +
  theme_minimal(base_size = 14)

p1
ggsave("figure of representative slope.jpg", plot = slope, dpi = 300, width = 6, height = 4, units = "in")




