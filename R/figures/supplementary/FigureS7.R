# Load necessary libraries
library(corrplot)

# Read the CSV file
data <- read.csv(file.path('Data', 'TStrend_Variables_updatedWind.csv'))
 



# Select only the columns for correlation analysis

data_selected <- data[c('FC_trend_mean', 'FCI_trend_mean',  'FHI_trend_mean','AMT','ATP','DEM',
                        'Slope','DroughtIntensity', 'WildfireIntensity','FAarea','PA','FMI','Accessibility2City',
                        'DePOPfraction')]
names(data_selected)<-c('TC trend', 'TCI trend',  'THI trend','AMT','ATP','Elevation',
                        'Slope','Drought intensity', 'Wildfire intensity','Former cropland fraction','Protection area fraction','Forest management intensity',
                        'Accessibility to city','Depopulation area fraction')
data_selected<-na.omit(data_selected)


# Calculate the correlation matrix
cor_matrix <- cor(data_selected, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix,  type = 'lower', order = "original", 
         method = 'ellipse',
         addCoef.col = "black", # Add correlation coefficients
         tl.col = "black", tl.srt = 45, # Adjust text color and angle
         cl.cex = 0.75, # Scale for color legend
         cl.ratio = 0.1,
         number.cex = 0.7,
         col = COL2('PiYG')) # Size of correlation coefficients





