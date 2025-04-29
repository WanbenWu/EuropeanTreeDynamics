# --------------------------------------------------------
# Load required packages
# --------------------------------------------------------
library(landscapemetrics)
library(terra)
library(sf)
library(doSNOW)
library(foreach)

# --------------------------------------------------------
# Define the landscape metrics function
# --------------------------------------------------------
calculate_landscape_metrics <- function(FM, FN, what) {
  # sample_lsm() calculates metrics for polygons (FN) based on the raster (FM).
  res <- sample_lsm(FM, FN, plot_id = FN$Code, what = what)
  return(as.data.frame(res))
}

# --------------------------------------------------------
# Main loop over years
# --------------------------------------------------------
for (year in 2001:2021) {
  
  # Create a parallel cluster (adjust cores as needed)
  cl <- makeCluster(28)  
  registerDoSNOW(cl)
  
  # Define intervals for subsetting features
  min_vals <- seq(0, 5120000, by = 10000)
  max_vals <- seq(10000, 5130000, by = 10000)
  
  # Track computation time
  start_time <- Sys.time()
  
  # Run parallel computation
  # Each iteration reads the raster and shapefile fresh, then subsets the shapefile
  # using the interval boundaries.
  result_list <- foreach(
    i = seq_along(min_vals),
    .packages = c("landscapemetrics","terra","sf","doSNOW","foreach")
  ) %dopar% {
    
    # Read raster (0/1 forest map)
    FM <- rast(paste0(
      "~data/",
      "Tree canopy extent and height/Tree_extent_",
      year, "_UTM32.tif"
    ))
    
    # Read shapefile (1 km fishnet)
    FN <- st_read(
      "~data/",
      "ForestTriAngle_1km/fishnetin1km/EU_1km_fishnet_UTM32.shp"
    )
    
    # Subset features based on Code values
    subset_fn <- FN[(FN$Code > min_vals[i]) & (FN$Code <= max_vals[i]), ]
    
    # Calculate chosen metrics: edge density, patch density, mean patch area
    calculate_landscape_metrics(FM, subset_fn, 
                                c("lsm_c_ed", "lsm_c_pd", "lsm_c_area_mn"))
  }
  
  end_time <- Sys.time()
  
  # Optional: print or store runtime in hours
  print(difftime(end_time, start_time, units = "hours"))
  
  # Stop the parallel cluster
  stopCluster(cl)
  
  # Combine all results into one data frame and write to disk
  result_df <- do.call(rbind, result_list)
  write.csv(
    result_df, 
    paste0(
      "~output/",
      "ForestTriAngle_1km/lsm_1km/lsm_", year, ".csv"
    ),
    row.names = FALSE
  )
}
