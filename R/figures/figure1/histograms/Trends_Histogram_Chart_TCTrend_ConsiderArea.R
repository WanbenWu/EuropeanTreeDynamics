# Load the necessary libraries
library(ggplot2)
library(terra)
library(dplyr)

# Set working directory

# Load the raster data using terra
Trend_Sig_lt0_05 <- rast(file.path('Data', 'partitioned_TC_trend_WGS84.tif'))

# Calculate pixel areas in square meters
area_raster <- cellSize(Trend_Sig_lt0_05, unit = "m")

# Convert raster data to dataframe
Trend_Sig_lt0_05_dataframe <- as.data.frame(cbind(values(Trend_Sig_lt0_05), values(area_raster)), na.rm = TRUE)
colnames(Trend_Sig_lt0_05_dataframe) <- c("Trend", "Area")

# Remove NA values
Trend_Sig_lt0_05_Value <- Trend_Sig_lt0_05_dataframe %>% filter(!is.na(Trend))

# Define bins and labels
bins <- c(-Inf, -0.4, -0.2, 0, 0.2, 0.4, Inf)
labels <- c("< -0.4", "[-0.4, -0.2)", "[-0.2, 0)", "[0, 0.2)", "[0.2, 0.4)", ">= 0.4")

# Assign groups based on the bins
Trend_Sig_lt0_05_Value$group <- cut(Trend_Sig_lt0_05_Value$Trend, breaks = bins, labels = labels, include.lowest = TRUE)

# Calculate area-weighted density
density_data <- Trend_Sig_lt0_05_Value %>%
  group_by(group) %>%
  summarise(Total_Area = sum(Area, na.rm = TRUE)) %>%
  mutate(density = Total_Area / sum(Total_Area, na.rm = TRUE) * 100)

# Define color list
color_list <- c('#c51b7c', '#de77ae', '#f2b6da', '#b8e185', '#80bc42', '#4d9120')

# Save the plot as a TIFF file
tiff(file = file.path("outputs", "TC_Trend_His.tiff"), width = 12.4, height = 10.6, units = "cm", res = 300, compression = "lzw", pointsize = 0)

# Plot histogram
ggplot(density_data, aes(x = group, y = density, fill = group)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(round(density, 1))), vjust = -0.5, size = 7) + 
  scale_fill_manual(values = color_list) +
  labs(y = "Density (%)", x = NULL) +
  ylim(0, max(density_data$density) * 1.1) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 22, hjust = 0.5),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  )

dev.off()




