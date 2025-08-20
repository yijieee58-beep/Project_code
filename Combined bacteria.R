###Combine bacterial abundance figure and the correlation of turnover and bacterial count figure 
# Combine with shared legend on the right
combined_plot <- (figure_bacteria + correlation) +
  plot_layout(widths = c(1, 1), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "right")# Show
combined_plot



# Save if needed
ggsave("combined_bacteria.jpg", combined_plot, width = 12, height = 6, dpi = 300)

