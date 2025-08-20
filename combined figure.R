library(ggplot2)
install.packages('patchwork')
library(patchwork)
install.packages('ggsignif')
library(ggsignif)   # for significance brackets
library(dplyr)
library(stringr)
library(emmeans)
library(dplyr)
install.packages('ggpubr')
library(ggpubr)



#####p1#####
p1 <- ggplot(dp, aes(Timepoint, Oxygen)) +
  geom_point(aes(fill = Phyto_Species, shape = Particle_Type),
             color = "black", size = 3, stroke = 0.8) +
  geom_abline(data = fits,
              aes(slope = Slope, intercept = intercept,
                  group = interaction(Phyto_Species, Particle_Type),
                  color = Phyto_Species),
              linewidth = 1, show.legend = FALSE) +
  scale_fill_manual(
    values = c("G. huxleyi" = "#32CD32",
               "S. costatum" = "#A9A9A9"),
    labels = c(expression(italic("G. huxleyi")),
               expression(italic("S. costatum")))
  ) +
  scale_color_manual(values = c("G. huxleyi" = "#32CD32",
                                "S. costatum" = "#A9A9A9"),
                     guide = "none") +
  scale_shape_manual(values = c("Aggregate" = 24,
                                "Faecal pellet" = 21)) +
  guides(
    fill  = guide_legend(title = "Species",
                         override.aes = list(shape = 21, color = "black", size = 3)),
    shape = guide_legend(title = "Particle Type",
                         override.aes = list(fill = "white", color = "black", size = 3))
  ) +
  labs(x = "Time (hr)", y = expression(Oxygen~(mu*mol~L^{-1}))) +
  theme_minimal(base_size = 14)
p1



#### Plot 2: Oxygen uptake rate (FIXED positioning)####
data <- data %>%
  mutate(Species = str_trim(Species))  # remove leading/trailing spaces


###add bracket between smae particle type but different species
p2 <- ggplot(data, aes(Particle_type, Daily_O2, fill = Species, shape = Particle_type)) +
  geom_violin(trim = FALSE, colour = "black", alpha = 0.8, 
              position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.8),
             size = 2.3, colour = "black", alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", shape = 15, size = 3, colour = "black",
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = pal, name = "Species",
                    labels = c(expression(italic("G. huxleyi")), 
                               expression(italic("S. costatum")))) +
  scale_shape_manual(values = c("Aggregate" = 24, "Faecal pellet" = 21), 
                     name = "Particle Type") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.25))  # More space for multiple significance brackets
  ) +
  
  # Brackets between species for each particle type (lower level)
  # Aggregate comparison (G. huxleyi vs S. costatum)
  annotate("segment", x = 0.8, xend = 1.2, y = max(data$Daily_O2, na.rm = TRUE) * 1.15, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.15) +
  annotate("segment", x = 0.8, xend = 0.8, y = max(data$Daily_O2, na.rm = TRUE) * 1.12, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.15) +
  annotate("segment", x = 1.2, xend = 1.2, y = max(data$Daily_O2, na.rm = TRUE) * 1.12, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.15) +
  annotate("text", x = 1, y = max(data$Daily_O2, na.rm = TRUE) * 1.18, label = "*", size = 4) +
  
  # Faecal pellet comparison (G. huxleyi vs S. costatum)
  annotate("segment", x = 1.8, xend = 2.2, y = max(data$Daily_O2, na.rm = TRUE) * 1.15, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.15) +
  annotate("segment", x = 1.8, xend = 1.8, y = max(data$Daily_O2, na.rm = TRUE) * 1.12, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.15) +
  annotate("segment", x = 2.2, xend = 2.2, y = max(data$Daily_O2, na.rm = TRUE) * 1.12, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.15) +
  annotate("text", x = 2, y = max(data$Daily_O2, na.rm = TRUE) * 1.18, label = "**", size = 4) +
  
  # Brackets between particle types within each species (upper level)
  # G. huxleyi: Aggregate vs Faecal pellet
  annotate("segment", x = 0.8, xend = 1.8, y = max(data$Daily_O2, na.rm = TRUE) * 1.25, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.25) +
  annotate("segment", x = 0.8, xend = 0.8, y = max(data$Daily_O2, na.rm = TRUE) * 1.22, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.25) +
  annotate("segment", x = 1.8, xend = 1.8, y = max(data$Daily_O2, na.rm = TRUE) * 1.22, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.25) +
  annotate("text", x = 1.3, y = max(data$Daily_O2, na.rm = TRUE) * 1.28, 
           label = "*", size = 4) +  # Adjust significance level as needed
  
  # S. costatum: Aggregate vs Faecal pellet
  annotate("segment", x = 1.2, xend = 2.2, y = max(data$Daily_O2, na.rm = TRUE) * 1.35, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.35) +
  annotate("segment", x = 1.2, xend = 1.2, y = max(data$Daily_O2, na.rm = TRUE) * 1.32, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.35) +
  annotate("segment", x = 2.2, xend = 2.2, y = max(data$Daily_O2, na.rm = TRUE) * 1.32, 
           yend = max(data$Daily_O2, na.rm = TRUE) * 1.35) +
  annotate("text", x = 1.7, y = max(data$Daily_O2, na.rm = TRUE) * 1.38, 
           label = "*", size = 4) +  # Adjust significance level as needed
  
  labs(
    x = "Particle Type",
    y = expression(Oxygen~Uptake~Rate~(mu*mol~L^{-1}~d^{-1}))
  ) +
  theme_minimal(base_size = 14)
p2




# Plot 3: POC concentration (FIXED positioning)
p3 <- ggplot(Carbon1, aes(x = Particle_type, 
                          y = POC_concentration.umol.L.1., 
                          fill = Species, 
                          shape = Particle_type)) +  # Add this line
  geom_violin(trim = FALSE, colour = "black", alpha = 0.8,
              position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
             size = 2.3, colour = "black", alpha = 0.85) +  # Remove shape = 21
  stat_summary(fun = mean, geom = "point", shape = 15, size = 3, colour = "black",
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = pal, name = "Species",
                    labels = c(expression(italic('G. huxleyi')),
                               expression(italic('S. costatum')))
  ) +
  scale_shape_manual(values = c("Aggregate" = 24, "Faecal pellet" = 21), 
                     name = "Particle Type") +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  annotate("segment", x = 0.8, xend = 1.2, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.15, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.15) +
  annotate("segment", x = 0.8, xend = 0.8, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.12, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.15) +
  annotate("segment", x = 1.2, xend = 1.2, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.12, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.15) +
  annotate("text", x = 1, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.18, label = "ns", size = 4) +
  
  # Faecal pellet comparison (G. huxleyi vs S. costatum)
  annotate("segment", x = 1.8, xend = 2.2, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.15, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.15) +
  annotate("segment", x = 1.8, xend = 1.8, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.12, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.15) +
  annotate("segment", x = 2.2, xend = 2.2, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.12, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.15) +
  annotate("text", x = 2, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.18, label = "ns", size = 4) +
  
  # Brackets between particle types within each species (upper level)
  # G. huxleyi: Aggregate vs Faecal pellet
  annotate("segment", x = 0.8, xend = 1.8, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.25, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.25) +
  annotate("segment", x = 0.8, xend = 0.8, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.22, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.25) +
  annotate("segment", x = 1.8, xend = 1.8, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.22, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.25) +
  annotate("text", x = 1.3, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.28, 
           label = "ns", size = 4) +  # Adjust significance level as needed
  
  # S. costatum: Aggregate vs Faecal pellet
  annotate("segment", x = 1.2, xend = 2.2, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.35, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.35) +
  annotate("segment", x = 1.2, xend = 1.2, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.32, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.35) +
  annotate("segment", x = 2.2, xend = 2.2, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.32, 
           yend = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.35) +
  annotate("text", x = 1.7, y = max(Carbon1$POC_concentration.umol.L.1., na.rm = TRUE) * 1.38, 
           label = "ns", size = 4) +  # Adjust significance level as needed
  
  labs(
    x = "Particle Type",
    y = expression(POC~Concentration~(mu*mol~L^{-1}))
  ) +
  theme_minimal(base_size = 14)

p3




# Plot 4: Carbon turnover rate (FIXED positioning)
df_no_outliers <- df_no_outliers %>%
  mutate(Species = str_trim(Species))  # remove leading/trailing spaces

p4 <- ggplot(df_no_outliers, aes(Particle_type, k.d.1., 
                                 fill = Species, 
                                 shape = Particle_type)) +  # Add this line
  geom_violin(trim = FALSE, colour = "black", alpha = 0.8,
              position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
             size = 2.3, colour = "black", alpha = 0.85) +  # Remove shape = 21
  stat_summary(fun = mean, geom = "point", shape = 15, size = 3, colour = "black",
               position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = pal, name = "Species",
                    labels = c(expression(italic('G. huxleyi')),
                               expression(italic('S. costatum')))
  ) +
  scale_shape_manual(values = c("Aggregate" = 24, "Faecal pellet" = 21), 
                     name = "Particle Type") +
  # ... rest of your code remains the same
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  # Brackets between species for each particle type (lower level)
  # Aggregate comparison (G. huxleyi vs S. costatum)
  annotate("segment", x = 0.8, xend = 1.2, y = max(df_no_outliers$k.d.1., na.rm = TRUE) * 1.15, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.15) +
  annotate("segment", x = 0.8, xend = 0.8, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.12, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.15) +
  annotate("segment", x = 1.2, xend = 1.2, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.12, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.15) +
  annotate("text", x = 1, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.18, label = "ns", size = 4) +
  
  # Faecal pellet comparison (G. huxleyi vs S. costatum)
  annotate("segment", x = 1.8, xend = 2.2, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.15, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.15) +
  annotate("segment", x = 1.8, xend = 1.8, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.12, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.15) +
  annotate("segment", x = 2.2, xend = 2.2, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.12, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.15) +
  annotate("text", x = 2, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.18, label = "ns", size = 4) +
  
  # Brackets between particle types within each species (upper level)
  # G. huxleyi: Aggregate vs Faecal pellet
  annotate("segment", x = 0.8, xend = 1.8, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.25, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.25) +
  annotate("segment", x = 0.8, xend = 0.8, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.22, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.25) +
  annotate("segment", x = 1.8, xend = 1.8, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.22, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.25) +
  annotate("text", x = 1.3, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.28, 
           label = "ns", size = 4) +  # Adjust significance level as needed
  
  # S. costatum: Aggregate vs Faecal pellet
  annotate("segment", x = 1.2, xend = 2.2, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.35, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.35) +
  annotate("segment", x = 1.2, xend = 1.2, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.32, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.35) +
  annotate("segment", x = 2.2, xend = 2.2, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.32, 
           yend = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.35) +
  annotate("text", x = 1.7, y = max(df_no_outliers$k.d.1, na.rm = TRUE) * 1.38, 
           label = "ns", size = 4) +  # Adjust significance level as needed
  
  labs(
    x = "Particle Type",
    y = expression(Carbon~Turnover~Rate~(d^{-1}))
  ) +
  theme_minimal(base_size = 14)
p4


# Combine all plots into one figure
combined_plot <- (p1 | p2) / (p3 | p4) +
  plot_annotation(tag_levels = 'A') +
  plot_layout(guides = 'collect')  # This will collect all legends



# Remove legends from individual plots except one
p1 <- p1 + theme(legend.position = "none")
p2 <- p2  # Keep legend on this one
p3 <- p3 + theme(legend.position = "none") 
p4 <- p4 + theme(legend.position = "none")

combined_plot <- (p1 | p2) / (p3 | p4) +
  plot_annotation(tag_levels = 'A')
combined_plot

# Save the combined plot
ggsave("combined_figure.jpg", combined_plot, 
       width = 12, height = 10, dpi = 300, bg = "white")
