overall_bacteria <- read.csv('overall_bacteria.csv')
overall_bacteria <- read_csv("overall_bacteria.csv") %>%
  filter(!is.na(`Bacterial count`) & `Bacterial count` != "" & 
           !is.na(`k(d-1)`) & `k(d-1)` != 0) %>%
  mutate(
    bacterial_count = as.numeric(`Bacterial count`),
    k_rate = as.numeric(`k(d-1)`)
  )


overall_bacteria <- overall_bacteria %>%
  mutate(Species = str_trim(Species))  # remove leading/trailing spaces

# Plot
correlation <- ggplot(overall_bacteria, aes(x = Density, y = `k(d-1)`)) +
  geom_point(aes(fill = Species, shape = Particle_type), size = 3, stroke = 1, color = "black") +
  scale_fill_manual(values = pal, name = "Species",
                    labels = c(expression(italic('G. huxleyi')),
                               expression(italic('S. costatum')))
  ) +
  scale_shape_manual(values = c("Aggregate" = 21, "Faecal pellet" = 24)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_manual(values = c("G. huxleyi" = "#32CD32", "S. costatum" = "#A9A9A9")) +
  labs(
    x = "Bacterial abundance" ~ (cells ~ mL^{-1}),
    y = expression("Turnover rate" ~ italic(k) ~ (d^{-1}))
  ) +
  guides(
    fill  = guide_legend(title = "Species",
                         override.aes = list(shape = 21, color = "black", size = 3)),
    shape = guide_legend(title = "Particle Type",
                         override.aes = list(fill = "white", color = "black", size = 3))
  ) +
  scale_x_continuous(
    labels = function(x) {
      ifelse(x == 0, "0", 
             parse(text = gsub("e\\+?", " %*% 10^", scales::scientific_format()(x))))
    }
  ) +
  theme_minimal(base_size = 14)

correlation
ggsave("figure of correlation.jpg", plot = correlation, dpi = 300, width = 6, height = 4, units = "in")
    
# Correlation
Ghux<- read.csv('Ghux_bacteria.csv')
Skele <- read.csv('S_bacteria.csv')
cor.test(Ghux$Bacterial.count, Ghux$k.d.1.)
cor.test(Skele$Bacterial.count, Skele$k.d.1.)
Agg <- read.csv('Aggregates.csv')
FP <- read.csv('FP.csv')
cor.test(Agg$Bacterial.count, Agg$k.d.1)
cor.test(FP$Bacterial.count, FP$k.d.1)
cor.test(overall_bacteria$bacterial_count, overall_bacteria$k_rate)
cor.test(data$bacterial_count, data$k_rate)

cor.test(data$bacterial_count, datacor.test(data$bacterial_count, data$`POC_mass(ug)`)
cor.test(data$bacterial_count, data$`O2_uptake_rate(umol_L-1_d-1)`)
