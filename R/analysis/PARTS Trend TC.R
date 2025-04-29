# Load required packages
library(terra)
library(remotePARTS)
library(dplyr)
library(ggplot2)

# Set the folder path where GeoTIFF files are stored
data_folder <- "~data/Tree Coverage/Geotiff"
# Get all GeoTIFF file paths
file_list <- list.files(data_folder, pattern = "\\.tif$", full.names = TRUE)

# Ensure the files are sorted by year (e.g., "ndvi_1982.tif", "ndvi_1983.tif")
file_list <- file_list[order(file_list)]

# Read all GeoTIFF files into a SpatRaster stack
r_stack <- rast(file_list)

wgs84_crs <- "EPSG:4326"

r_stack_wgs84 <- project(r_stack, wgs84_crs)

# Extract years from file names (assuming format "ndvi_YYYY.tif")
years <- as.numeric(gsub(".*_(\\d{4})\\.tif$", "\\1", file_list))

# Check if the years are sorted correctly
if (is.unsorted(years)) {
  stop("GeoTIFF files are not sorted by time!")
}

# Convert raster data into a data frame with coordinates and values
r_df <- as.data.frame(r_stack_wgs84, xy = TRUE, na.rm = TRUE)
colnames(r_df)[3:ncol(r_df)] <- paste0("tc", years)
head(r_df)

# Extract the time series matrix (Y) and coordinate matrix (coords)
Y <- as.matrix(r_df[, -(1:2)])  # NDVI time series
coords <- as.matrix(r_df[, 1:2])  # Longitude and latitude coordinates

# Ensure there are no missing values in the data
if (anyNA(Y)) stop("There are missing values in the time series. Please clean the data!")

# Use fitAR_map to calculate pixel-level time trends
ARfit <- fitAR_map(Y = Y, coords = coords)

save(ARfit, file = "~out/PARTS Trend/partTCARfit.rda")
ARfit$coefficients[, "t"] <- ARfit$coefficients[,"t"]/rowMeans(r_df[, -(1:2)])
r_df$AR_coef <- coefficients(ARfit)[, "t"] # save time trend coefficient

# Extract time trend coefficients (slopes) and p value
trend <- coefficients(ARfit)[, "t"]
pvalue<-ARfit$pvals[, "t"]

# Save the trend and p-value as a GeoTIFF file
trend_raster <- rast(cbind(coords, trend), type = "xyz", crs = crs(r_stack))
pvalue_raster<-rast(cbind(coords, pvalue), type = "xyz", crs = crs(r_stack))

writeRaster(trend_raster, "~out/partitioned_TC_trend.tif", overwrite = TRUE)
writeRaster(pvalue_raster, "~out/partitioned_TC_trend_pvalue.tif", overwrite = TRUE)

