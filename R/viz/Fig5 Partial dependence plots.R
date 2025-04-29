library(ggplot2)
library(gridExtra)
library(grid)
setwd('~data/ResponseCurve_csv')

AMT_TC_data<-read.csv('Response_Curve_AMT_MeanTCtrend_99runs.csv')
AMT_TC_plot <- ggplot(AMT_TC_data, aes(x =AMT-273.5, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Mean annual temperature (℃) 42%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

FormerCropland_TC_data<-read.csv('Response_Curve_FormerCroplandFraction_MeanTCtrend_99runs.csv')
# Load required libraries
FormerCropland_TC_plot <- ggplot(FormerCropland_TC_data, aes(x =FormerCroplandFraction, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Former cropland fraction (%) 14.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

WindstormIntensity_TC_data<-read.csv('Response_Curve_WindstormIntensity_MeanTCtrend_99runs.csv')
# Load required libraries
WindstormIntensity_TC_plot <- ggplot(WindstormIntensity_TC_data, aes(x =WindstormIntensity, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Windstorm intensity (m/s) 19.7%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

ForestManagementIntensity_TC_data<-read.csv('Response_Curve_ForestManagementIntensity_MeanTCtrend_99runs.csv')
# Load required libraries
ForestManagementIntensity_TC_plot <- ggplot(ForestManagementIntensity_TC_data, aes(x =ForestManagementIntensity, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Forest management intensity 7.5%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )



Accessibility2City_TC_data<-read.csv('Response_Curve_Accessibility2City_MeanTCtrend_99runs.csv')
# Load required libraries
Accessibility2City_TC_plot <- ggplot(Accessibility2City_TC_data, aes(x =Accessibility2City, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Accessibility to city (min) 1%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

WildfireIntensity_TC_data<-read.csv('Response_Curve_WildfireIntensity_MeanTCtrend_99runs.csv')
# Load required libraries
WildfireIntensity_TC_plot <- ggplot(WildfireIntensity_TC_data, aes(x =WildfireIntensity, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Wildfire intensity 5.1%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

ProtectionAreaFraction_TC_data<-read.csv('Response_Curve_ProtectionAreaFraction_MeanTCtrend_99runs.csv')
# Load required libraries
ProtectionAreaFraction_TC_plot <- ggplot(ProtectionAreaFraction_TC_data, aes(x =ProtectionAreaFraction, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Protected area fraction (%) 2.3%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


DEM_TC_data<-read.csv('Response_Curve_DEM_MeanTCtrend_99runs.csv')
# Load required libraries
DEM_TC_plot <- ggplot(DEM_TC_data, aes(x =DEM, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Elevation (m) 2.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )



Slope_TC_data<-read.csv('Response_Curve_Slope_MeanTCtrend_99runs.csv')
# Load required libraries
Slope_TC_plot <- ggplot(Slope_TC_data, aes(x =Slope, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Slope  1.1%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

DePOPfraction_TC_data<-read.csv('Response_Curve_DePOPfraction_MeanTCtrend_99runs.csv')
# Load required libraries
DePOPfraction_TC_plot <- ggplot(DePOPfraction_TC_data, aes(x =DePOPfraction, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Depopulation area fraction (%) 1.8%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )



ATP_TC_data<-read.csv('Response_Curve_ATP_MeanTCtrend_99runs.csv')
# Load required libraries
ATP_TC_plot <- ggplot(ATP_TC_data, aes(x =ATP, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Annual total precipitation (mm) 1.9%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

DroughtIntensity_TC_data<-read.csv('Response_Curve_DroughtIntensity_MeanTCtrend_99runs.csv')
# Load required libraries
DroughtIntensity_TC_plot <- ggplot(DroughtIntensity_TC_data, aes(x =DroughtIntensity, y = y, group = run)) +
  geom_line(color = '#008000', alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Drought intensity 1.4%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = '#008000', fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


##############################################################################################
##################################TCI###########################################################

AMT_TCI_data<-read.csv('Response_Curve_AMT_MeanTCItrend_99runs.csv')
# Load required libraries
AMT_TCI_plot <- ggplot(AMT_TCI_data, aes(x =AMT-273.5, y=y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Mean annual temperature (℃) 31.6%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

FormerCropland_TCI_data<-read.csv('Response_Curve_FormerCroplandFraction_MeanTCItrend_99runs.csv')
# Load required libraries
FormerCropland_TCI_plot <- ggplot(FormerCropland_TCI_data, aes(x =FormerCroplandFraction, y=y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Former cropland fraction (%) 7.3%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

ForestManagementIntensity_TCI_data<-read.csv('Response_Curve_ForestManagementIntensity_MeanTCItrend_99runs.csv')
# Load required libraries
ForestManagementIntensity_TCI_plot <- ggplot(ForestManagementIntensity_TCI_data, aes(x =ForestManagementIntensity, y=y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Forest management intensity 7.5%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

Accessibility2City_TCI_data<-read.csv('Response_Curve_Accessibility2City_MeanTCItrend_99runs.csv')
# Load required libraries
Accessibility2City_TCI_plot <- ggplot(Accessibility2City_TCI_data, aes(x =Accessibility2City, y=y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Accessibility to city (min) 3.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

WindstormIntensity_TCI_data<-read.csv('Response_Curve_WindstormIntensity_MeanTCItrend_99runs.csv')
# Load required libraries
WindstormIntensity_TCI_plot <- ggplot(WindstormIntensity_TCI_data, aes(x =WindstormIntensity, y = y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Windstorm intensity (m/s) 12.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

WildfireIntensity_TCI_data<-read.csv('Response_Curve_WildfireIntensity_MeanTCItrend_99runs.csv')
# Load required libraries
WildfireIntensity_TCI_plot <- ggplot(WildfireIntensity_TCI_data, aes(x =WildfireIntensity, y = y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Wildfire intensity 0.6%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

ProtectionAreaFraction_TCI_data<-read.csv('Response_Curve_ProtectionAreaFraction_MeanTCItrend_99runs.csv')
# Load required libraries
ProtectionAreaFraction_TCI_plot <- ggplot(ProtectionAreaFraction_TCI_data, aes(x =ProtectionAreaFraction, y = y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Protected area fraction (%) 6.0%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


DEM_TCI_data<-read.csv('Response_Curve_DEM_MeanTCItrend_99runs.csv')
# Load required libraries
DEM_TCI_plot <- ggplot(DEM_TCI_data, aes(x =DEM, y = y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Elevation (m) 4.4%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )



Slope_TCI_data<-read.csv('Response_Curve_Slope_MeanTCItrend_99runs.csv')
# Load required libraries
Slope_TCI_plot <- ggplot(Slope_TCI_data, aes(x =Slope, y = y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Slope  8.5%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


DePOPfraction_TCI_data<-read.csv('Response_Curve_DePOPfraction_MeanTCItrend_99runs.csv')
# Load required libraries
DePOPfraction_TCI_plot <- ggplot(DePOPfraction_TCI_data, aes(x =DePOPfraction, y = y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Depopulation area fraction (%) 2.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )



ATP_TCI_data<-read.csv('Response_Curve_ATP_MeanTCItrend_99runs.csv')
# Load required libraries
ATP_TCI_plot <- ggplot(ATP_TCI_data, aes(x =ATP, y = y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Annual total precipitation (mm) 13.4%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

DroughtIntensity_TCI_data<-read.csv('Response_Curve_DroughtIntensity_MeanTCItrend_99runs.csv')
# Load required libraries
DroughtIntensity_TCI_plot <- ggplot(DroughtIntensity_TCI_data, aes(x =DroughtIntensity, y = y*1000, group = run)) +
  geom_line(color = "#8199F5", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Drought intensity 3.1%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#8199F5", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


##############################################################################################
##################################THI###########################################################

WindstormIntensity_THI_data<-read.csv('Response_Curve_WindstormIntensity_MeanTHItrend_99runs.csv')
# Load required libraries
WindstormIntensity_THI_plot <- ggplot(WindstormIntensity_THI_data, aes(x =WindstormIntensity, y = y*100, group = run)) +
  geom_line(color = "#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Windstorm intensity (m/s) 8.5%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

WildfireIntensity_THI_data<-read.csv('Response_Curve_WildfireIntensity_MeanTHItrend_99runs.csv')
# Load required libraries
WildfireIntensity_THI_plot <- ggplot(WildfireIntensity_THI_data, aes(x =WildfireIntensity, y = y*100, group = run)) +
  geom_line(color ="#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Wildfire intensity 1.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour ="#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

ProtectionAreaFraction_THI_data<-read.csv('Response_Curve_ProtectionAreaFraction_MeanTHItrend_99runs.csv')
# Load required libraries
ProtectionAreaFraction_THI_plot <- ggplot(ProtectionAreaFraction_THI_data, aes(x =ProtectionAreaFraction, y = y*100, group = run)) +
  geom_line(color ="#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Protected area fraction (%) 0%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour ="#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


DEM_THI_data<-read.csv('Response_Curve_DEM_MeanTHItrend_99runs.csv')
# Load required libraries
DEM_THI_plot <- ggplot(DEM_THI_data, aes(x =DEM, y = y*100, group = run)) +
  geom_line(color ="#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Elevation (m) 11.5%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour ="#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


Slope_THI_data<-read.csv('Response_Curve_Slope_MeanTHItrend_99runs.csv')
# Load required libraries
Slope_THI_plot <- ggplot(Slope_THI_data, aes(x =Slope, y = y*100, group = run)) +
  geom_line(color ="#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Slope  3.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour ="#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


DePOPfraction_THI_data<-read.csv('Response_Curve_DePOPfraction_MeanTHItrend_99runs.csv')
# Load required libraries
DePOPfraction_THI_plot <- ggplot(DePOPfraction_THI_data, aes(x =DePOPfraction, y = y*100, group = run)) +
  geom_line(color ="#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Depopulation area fraction (%) 1.3%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour ="#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


ATP_THI_data<-read.csv('Response_Curve_ATP_MeanTHItrend_99runs.csv')
# Load required libraries
ATP_THI_plot <- ggplot(ATP_THI_data, aes(x =ATP, y = y*100, group = run)) +
  geom_line(color ="#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Annual total precipitation (mm) 6%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour ="#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

DroughtIntensity_THI_data<-read.csv('Response_Curve_DroughtIntensity_MeanTHItrend_99runs.csv')
# Load required libraries
DroughtIntensity_THI_plot <- ggplot(DroughtIntensity_THI_data, aes(x =DroughtIntensity, y = y*100, group = run)) +
  geom_line(color ="#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Drought intensity 6.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour ="#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

AMT_THI_data<-read.csv('Response_Curve_AMT_MeanTHItrend_99runs.csv')
# Load required libraries
AMT_THI_plot <- ggplot(AMT_THI_data, aes(x =AMT-273.5, y=y*100, group = run)) +
  geom_line(color = "#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Mean annual temperature (℃) 21.2%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

FormerCropland_THI_data<-read.csv('Response_Curve_FormerCroplandFraction_MeanTHItrend_99runs.csv')
# Load required libraries
FormerCropland_THI_plot <- ggplot(FormerCropland_THI_data, aes(x =FormerCroplandFraction, y=y*100, group = run)) +
  geom_line(color = "#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Former cropland fraction (%) 1.8%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

ForestManagementIntensity_THI_data<-read.csv('Response_Curve_ForestManagementIntensity_MeanTHItrend_99runs.csv')
# Load required libraries
ForestManagementIntensity_THI_plot <- ggplot(ForestManagementIntensity_THI_data, aes(x =ForestManagementIntensity, y=y*100, group = run)) +
  geom_line(color = "#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Forest management intensity 1.6%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

Accessibility2City_THI_data<-read.csv('Response_Curve_Accessibility2City_MeanTHItrend_99runs.csv')
# Load required libraries
Accessibility2City_THI_plot <- ggplot(Accessibility2City_THI_data, aes(x =Accessibility2City, y=y*100, group = run)) +
  geom_line(color = "#EF9BE1", alpha = 0.05) +  # Use a fixed color for all lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8)+
  theme_bw() +                              # Use a minimal theme for a cleaner look
  labs(
    x = "Accessibility to city (min) 37.5%",
    y = "Fitted function"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    plot.title = element_blank(),
    panel.border = element_rect(colour = "#EF9BE1", fill=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )


#'AMT','ATP','DEM','Slope','DroughtIntensity', 'WildfireIntensity','WindstormIntensity','FormerCroplandFraction','ProtectionAreaFraction',
#'ForestManagementIntensity','Accessibility2City','DePOPfraction'


##################################Top4 relative importance variables
plots<-list((AMT_TC_plot+ylim(-0.1,0.2)+labs(y = bquote("TC trend (" * '%' * yr^-1 * ")"))
             +
               annotation_custom(
                 grob = textGrob("(a)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            
            (WindstormIntensity_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank())
             +
               annotation_custom(
                 grob = textGrob("(b)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            (FormerCropland_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank())
             +
               annotation_custom(
                 grob = textGrob("(c)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            (ForestManagementIntensity_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank())
             +
               annotation_custom(
                 grob = textGrob("(d)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            
            AMT_TCI_plot+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))+labs(y = bquote("TCI trend (" ~ 10^-3 ~ " "* yr^-1 * ")"))
            +
              annotation_custom(
                grob = textGrob("(e)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
              ),
            (ATP_TCI_plot+ylim(-0.5,0.5)+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
             +
               annotation_custom(
                 grob = textGrob("(f)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            (WindstormIntensity_TCI_plot+ylim(-0.5,0.5)+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
             +
               annotation_custom(
                 grob = textGrob("(g)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            (Slope_TCI_plot+ylim(-0.5,0.5)+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
             +
               annotation_custom(
                 grob = textGrob("(h)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            
            Accessibility2City_THI_plot+ylim(-0.4,0.3)+labs(y = bquote("THI trend (" ~ 10^-2 ~ " "* yr^-1 * ")"))
            +
              annotation_custom(
                grob = textGrob("(i)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
              ),
            (AMT_THI_plot+ylim(-0.4,0.3)+ theme(axis.title.y = element_blank())
             +
               annotation_custom(
                 grob = textGrob("(j)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            (DEM_THI_plot+ylim(-0.4,0.3)+ theme(axis.title.y = element_blank())
             +
               annotation_custom(
                 grob = textGrob("(k)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                 xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
               )),
            (WindstormIntensity_THI_plot+ylim(-0.4,0.3)+theme(axis.title.y = element_blank()))
            +
              annotation_custom(
                grob = textGrob("(l)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
              ))


# Arrange the plots in a 4x4 grid
combined_plot <- grid.arrange(grobs = plots, ncol = 4, nrow = 3,
                              widths = unit(c(1, 1, 1,1), "null"))


# Save the plot
ggsave(
  "Fig6_Response_Curve_Top4.png",
  plot = combined_plot,
  height = 8.5, 
  width = 11.3,
  dpi = 1000, 
  unit = "in"
)

##################################Last8 relative importance variables
plots_TC8v<-list((WildfireIntensity_TC_plot+ylim(-0.1,0.2)
                  +
                    annotation_custom(
                      grob = textGrob("(a)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                    )),#+labs(y = bquote("TC trend (" * '%' * yr^-1 * ")"))
                 (ProtectionAreaFraction_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank())
                  +
                    annotation_custom(
                      grob = textGrob("(b)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                    )),
                 (DEM_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank())
                  +
                    annotation_custom(
                      grob = textGrob("(c)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                    )),
                 (ATP_TC_plot+ylim(-0.1,0.2)
                  +
                    annotation_custom(
                      grob = textGrob("(d)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                    )),#+ labs(y = bquote("TC trend (" * '%' * yr^-1 * ")"))
                 (DePOPfraction_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank())
                  +
                    annotation_custom(
                      grob = textGrob("(e)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                    )),
                 (DroughtIntensity_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank())
                  +
                    annotation_custom(
                      grob = textGrob("(f)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                    )),
                 (Slope_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank())
                  +
                    annotation_custom(
                      grob = textGrob("(g)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                    )),
                 (Accessibility2City_TC_plot+ylim(-0.1,0.2)+ theme(axis.title.y = element_blank()))
                 +
                   annotation_custom(
                     grob = textGrob("(h)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = '#008000')),
                     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                   ))

# Arrange the plots in a 4x4 grid
combined_plot_TC8v <- grid.arrange(grobs = plots_TC8v, ncol = 4, nrow = 2,
                                   widths = unit(c(1.2, 1, 1,1), "null"))


# Save the plot
ggsave(
  "FigS6_Response_Curve_TC_8variables.png",
  plot = combined_plot_TC8v,
  height = 5.7, 
  width = 11.3,
  dpi = 1000, 
  unit = "in"
)



plots_TCI8v<-list((ForestManagementIntensity_TCI_plot+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
                   +
                     annotation_custom(
                       grob = textGrob("(a)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),#+labs(y = bquote("TCI trend (" ~ 10^-3 ~ " "* yr^-1 * ")"))
                  (FormerCropland_TCI_plot+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
                   +
                     annotation_custom(
                       grob = textGrob("(b)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (ProtectionAreaFraction_TCI_plot+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
                   +
                     annotation_custom(
                       grob = textGrob("(c)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (DEM_TCI_plot+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
                   +
                     annotation_custom(
                       grob = textGrob("(d)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (Accessibility2City_TCI_plot+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
                   +
                     annotation_custom(
                       grob = textGrob("(e)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),#+labs(y = bquote("TCI trend (" ~ 10^-3 ~ " "* yr^-1 * ")"))
                  (DroughtIntensity_TCI_plot+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
                   +
                     annotation_custom(
                       grob = textGrob("(f)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (DePOPfraction_TCI_plot+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
                   +
                     annotation_custom(
                       grob = textGrob("(g)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (WildfireIntensity_TCI_plot+ theme(axis.title.y = element_blank())+scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25), labels = seq(-0.5, 0.5, 0.25), limits = c(-0.5, 0.5))
                   +
                     annotation_custom(
                       grob = textGrob("(h)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#8199F5")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )))


# Arrange the plots in a 4x4 grid
combined_plot_TCI8v <- grid.arrange(grobs = plots_TCI8v, ncol = 4, nrow = 2,
                                    widths = unit(c(1.2, 1, 1,1), "null"))


# Save the plot
ggsave(
  "FigS7_Response_Curve_TCI_8variables.png",
  plot = combined_plot_TCI8v,
  height = 5.7, 
  width = 11.3,
  dpi = 1000, 
  unit = "in"
)




plots_THI8v<-list((DroughtIntensity_THI_plot+ylim(-0.4,0.3)
                   +
                     annotation_custom(
                       grob = textGrob("(a)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),#+labs(y = bquote("THI trend (" ~ 10^-2 ~ " "* yr^-1 * ")"))
                  (ATP_THI_plot+ theme(axis.title.y = element_blank())+ylim(-0.4,0.3)
                   +
                     annotation_custom(
                       grob = textGrob("(b)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (Slope_THI_plot+ theme(axis.title.y = element_blank())+ylim(-0.4,0.3)
                   +
                     annotation_custom(
                       grob = textGrob("(c)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (FormerCropland_THI_plot+ theme(axis.title.y = element_blank())+ylim(-0.4,0.3)
                   +
                     annotation_custom(
                       grob = textGrob("(d)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (ForestManagementIntensity_THI_plot+ylim(-0.4,0.3)
                   +
                     annotation_custom(
                       grob = textGrob("(e)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),#+labs(y = bquote("THI trend (" ~ 10^-2 ~ " "* yr^-1 * ")"))
                  (DePOPfraction_THI_plot+ theme(axis.title.y = element_blank())+ylim(-0.4,0.3)
                   +
                     annotation_custom(
                       grob = textGrob("(f)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (WildfireIntensity_THI_plot+ theme(axis.title.y = element_blank())+ylim(-0.4,0.3)
                   +
                     annotation_custom(
                       grob = textGrob("(g)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                       xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                     )),
                  (ProtectionAreaFraction_THI_plot+ theme(axis.title.y = element_blank())+ylim(-0.4,0.3))
                  +
                    annotation_custom(
                      grob = textGrob("(h)", x = 0, y = 1, hjust = -0.15, vjust = 1.15, gp = gpar(fontsize = 12, col = "#EF9BE1")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
                    ))


# Arrange the plots in a 4x4 grid
combined_plot_THI8v <- grid.arrange(grobs = plots_THI8v, ncol = 4, nrow = 2,
                                    widths = unit(c(1.2, 1, 1,1), "null"))


# Save the plot
ggsave(
  "FigS8_Response_Curve_THI_8variables.png",
  plot = combined_plot_THI8v,
  height = 5.7, 
  width = 11.3,
  dpi = 1000, 
  unit = "in"
)
