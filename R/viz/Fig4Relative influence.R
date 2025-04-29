library(ggplot2)
library(gridExtra)

# Set working directory (ensure the path is correct and accessible)
setwd('~data/RelativeImportance')

# Read the data from CSV files
FC_importance_summary <- read.csv('MeanTCtrend_BRT_Importance_Summary99runs.csv')
FCI_importance_summary <- read.csv('MeanTCItrend_BRT_Importance_Summary99runs.csv')
FHI_importance_summary <- read.csv('MeanTHItrend_BRT_Importance_Summary99runs.csv')

head(FC_importance_summary)

class_to_names <- c('AMT'='Mean annual temperature','ATP'='Annual total precipitation',
                    'DEM'='Elevation','Slope'='Slope','DroughtIntensity'='Drought intensity', 
                    'WildfireIntensity'='Wildfire intensity','WindstormIntensity'='Windstorm intensity','FormalCroplandFraction'='Former cropland fraction',
                    'ProtectionAreaFraction'='Protected area fraction','ForestManagementIntensity'='Forest management intensity',
                    'Accessibility2City'='Accessibility to city','DePOPfraction'='Depopulation area fraction')

FC_importance_summary$names <- class_to_names[as.character(FC_importance_summary$var)]
FCI_importance_summary$names <- class_to_names[as.character(FCI_importance_summary$var)]
FHI_importance_summary$names <- class_to_names[as.character(FHI_importance_summary$var)]


# Plot for FC
FC_importance_plot <- ggplot(FC_importance_summary, aes(x = reorder(names, mean), y = mean)) +
  geom_hline(yintercept = 5, col = gray(0.2), lty = 2) +  # Line for a threshold or reference value
  geom_point(shape = 16, color = '#008000',size = 5) +  # Use reordered 'names' for consistency
  geom_errorbar(aes(ymin = percentile_2.5, ymax = percentile_97.5), width = 1) +
  coord_flip() +  # Flip axes to make it horizontal
  labs(x = "Variables", y = "Relative Influence (%)",  title = "(a) TC") +
  theme_bw() +  # Clean theme
  ylim(0,50) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15), 
    panel.border = element_rect(colour = '#008000', fill = NA),
    axis.text = element_text(size = 14),  # Size of axis text
    panel.grid.minor = element_blank(),  # Remove minor grid
    plot.title = element_text(colour = '#008000', size = 15,hjust = 0.03, vjust = -8)
  )

# Plot for FCI
FCI_importance_plot <- ggplot(FCI_importance_summary, aes(x = reorder(names, mean), y = mean)) +
  geom_hline(yintercept = 5, col = gray(0.2), lty = 2) +  # Line for a threshold or reference value
  geom_point(shape = 16, color = "#8199F5",size = 5) +  # Use reordered 'names' for consistency
  geom_errorbar(aes(ymin = percentile_2.5, ymax = percentile_97.5), width = 1) +
  coord_flip() +  # Flip axes to make it horizontal
  labs(x = "Variables", y = "Relative Influence (%)", title = "(b) TCI") +
  theme_bw() +  # Clean theme
  ylim(0,50) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 15), 
    panel.border = element_rect(colour = "#8199F5", fill = NA),
    axis.text = element_text(size = 14),  # Size of axis text
    panel.grid.minor = element_blank(),  # Remove minor grid
    plot.title = element_text(colour =  "#8199F5", size = 15,hjust = 0.03, vjust = -8)
  )

# Plot for FHI
FHI_importance_plot <- ggplot(FHI_importance_summary, aes(x = reorder(names, mean), y = mean)) +
  geom_hline(yintercept = 5, col = gray(0.2), lty = 2) +  # Line for a threshold or reference value
  geom_point(shape = 16, color = "#EF9BE1",size = 5) +  # Use reordered 'names' for consistency
  geom_errorbar(aes(ymin = percentile_2.5, ymax = percentile_97.5), width = 1) +
  coord_flip() +  # Flip axes to make it horizontal
  labs(x = "Variables", y = "Relative Influence (%)", title = "(c) THI") +
  theme_bw() +  # Clean theme
  ylim(0,50) +
  theme(
    axis.title.y = element_blank(),
    panel.border = element_rect(colour = "#EF9BE1", fill = NA),
    axis.title.x = element_text(size = 15), 
    axis.text = element_text(size = 14),  # Size of axis text
    panel.grid.minor = element_blank(),  # Remove minor grid
    plot.title = element_text(colour = "#EF9BE1", size = 15,hjust = 0.03, vjust = -8)
  )

# Combine the plots
plots <- list(FC_importance_plot, FCI_importance_plot, FHI_importance_plot)
combined_plot <- grid.arrange(grobs = plots, ncol = 1, nrow = 3)

combined_plot <- grid.arrange(
  grobs = plots, 
  ncol = 1, 
  nrow = 3, 
  heights = c(0.8, 0.8, 0.8)
)

# Save the combined plot
ggsave(
  'Combined_relative_importance.png',
  plot = combined_plot,
  height = 12, 
  width = 9,
  dpi = 1000, 
  units = "in"
)
