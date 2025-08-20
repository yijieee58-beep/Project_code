library(tidyverse)
# Load your edited combined data
combined_data <- read_csv("combined_data.csv")
oxygen <- read.csv('turnover1.csv')
library(tidyverse)

# 1) Load your updated combined_data.csv
combined_data <- read_csv("combined_data.csv", show_col_types = FALSE)

# 2) Remove the MQ controls and treat Sample as SampleID
combined_data <- combined_data %>%
  rename(SampleID = Sample)                 # use Sample as SampleID directly
oxygen <- oxygen %>%
  rename(SampleID = Normalized)                 # use Sample as SampleID directly

# 3) Hand‑pick the four SampleIDs you actually want
manual_ids <- data.frame(
  experiment = c("GHA1", "GHFP2", "SKA2", "SKFP3"),
  trial = c(1, 1, 5, 1)
)

# 4) Filter to only those
rep_data <- oxygen %>%
  filter(SampleID %in% manual_ids)

# 5) Label treatments for plotting
rep_data <- rep_data %>%
  mutate(Treatment = case_when(
    SampleID == "GHA1-1"  ~ "G. hux - Aggregate",
    SampleID == "GHFP2-1" ~ "G. hux - Faecal Pellet",
    SampleID == "SKA2-5"  ~ "Skeletonema - Aggregate",
    SampleID == "SKFP3-1" ~ "Skeletonema - Faecal Pellet"
  ))

# 6) Plot
ggplot(rep_data, aes(x = Time_hr, y = Oxygen, color = Treatment)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    x     = "Time (hr)",
    y     = expression(Oxygen~(mu*mol~L^{-1}))
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

