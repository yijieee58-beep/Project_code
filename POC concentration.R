Carbon <- read.csv('overall_data.csv')
Carbon1 <- Carbon %>%
  filter( POC_concentration.umol.L.1. > 0) 


Carbon1 <- Carbon1 %>%
  mutate(Species = str_trim(Species))  # remove leading/trailing spaces

pal <- c("G. huxleyi" = "#32CD32",   # lighter green
         "S. costatum" = "#A9A9A9") # lighter grey
p3<- ggplot(Carbon1, aes(Particle_type, POC_concentration.umol.L.1., fill = Species)) +
  geom_violin(trim = FALSE, colour = "black", alpha = 0.8) +  # keep full smooth shape
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
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
    y = expression(POC~Concentration~(mu*mol~L^{-1}))
  ) +
  theme_minimal(base_size = 14)
p3

ggsave("figure of POC.jpg", plot = POC, dpi = 300, width = 6, height = 4, units = "in")

POC <- lm(POC_concentration.umol.L.1. ~ Particle_type * Species, data = Carbon1)
summary(POC)
POC1 <- lmer(POC_concentration.umol.L.1. ~ Particle_type * Species + (1|Replicate), data = Carbon1)
summary(POC1)
library(emmeans)
emmeans(POC, pairwise ~ Particle_type * Species)


####pairwise comparison

pairwise <- emmeans(POC, ~ Particle_type | Species)
pairwise_contrasts <- pairs(pairwise)
# Get overall pairwise comparisons (all combinations)
overall_pairwise <- emmeans(POC, ~ Particle_type * Species)
all_contrasts <- pairs(overall_pairwise)
print(pairwise_contrasts)
print(all_contrasts)




