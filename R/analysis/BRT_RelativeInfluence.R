# Load required libraries (removed the unused ones)
library(gbm)
library(readr)
library(dplyr)
library(purrr)
library(caret)

# Set working directory
setwd("~data")

# Read CSV file
data_path <- "TStrend_Variables.csv"
data <- read_csv(data_path)

# Prepare the training data
datatrain <- data[c(
  "FCI_trend_mean", "FC_trend_mean", "FHI_trend_mean", 
  "AMT","ATP","DEM","Slope","DroughtIntensity", "WildfireIntensity","Windspeed",
  "FAarea","PA","FMI","Accessibility2City","DePOPfraction"
)]

names(datatrain) <- c(
  "FCI_trend_mean", "FC_trend_mean", "FHI_trend_mean", 
  "AMT","ATP","DEM","Slope","DroughtIntensity", "WildfireIntensity","WindstormIntensity",
  "FormalCroplandFraction","ProtectionAreaFraction","ForestManagementIntensity",
  "Accessibility2City","DePOPfraction"
)

datatrain <- na.omit(datatrain)

# -----------------------------------------------------
# 1) Model for TC_trend_mean
# -----------------------------------------------------
fit_brt_models <- function(data, n_runs = 99) {
  r_squared_list <- numeric(n_runs)
  pearson_r_list <- numeric(n_runs)
  models <- vector("list", n_runs)
  importance_list <- vector("list", n_runs)
  
  for (i in 1:n_runs) {
    trainIndex <- createDataPartition(data$FC_trend_mean, p = 0.8, list = FALSE)
    trainData <- data[trainIndex, ]
    validationData <- data[-trainIndex, ]
    
    set.seed(i)
    brt_model <- gbm(
      FC_trend_mean ~ 
        AMT + ATP + DEM + Slope +
        DroughtIntensity + WildfireIntensity + WindstormIntensity +
        FormalCroplandFraction + ProtectionAreaFraction +
        ForestManagementIntensity +
        Accessibility2City + DePOPfraction,
      data = data, 
      distribution = "gaussian", 
      n.trees = 1000, 
      interaction.depth = 2, 
      shrinkage = 0.01, 
      cv.folds = 5, 
      keep.data = TRUE, 
      verbose = FALSE
    )
    
    models[[i]] <- brt_model
    importance_list[[i]] <- summary(brt_model, cBars = 20, plot = FALSE)
    
    # Make predictions
    predictions <- predict(brt_model, newdata = validationData, n.trees = 1000)
    
    # Calculate R-squared and Pearson's r
    actual <- validationData$FC_trend_mean
    rss <- sum((predictions - actual) ^ 2)
    tss <- sum((actual - mean(actual)) ^ 2)
    r_squared_list[i] <- 1 - (rss / tss)
    pearson_r_list[i] <- cor(actual, predictions)
  }
  
  list(
    models = models,
    importance = importance_list,
    mean_r_squared = mean(r_squared_list),
    mean_pearson_r = mean(pearson_r_list),
    r_squared_list = r_squared_list,
    pearson_r_list = pearson_r_list
  )
}

# Run the modeling function for FC_trend_mean
results <- fit_brt_models(datatrain)

# Aggregate importance across runs
importance_data <- map_dfr(results$importance, ~ as.data.frame(.x), .id = "run")

importance_summary <- importance_data %>%
  group_by(var) %>%
  summarise(
    mean = mean(rel.inf),
    percentile_2.5 = quantile(rel.inf, probs = 0.025),
    percentile_97.5 = quantile(rel.inf, probs = 0.975)
  ) %>%
  arrange(desc(mean))

importance_summary$var <- gsub("_", " ", importance_summary$var)
write.csv(
  importance_summary,
  "~out/MeanTCtrend_BRT_Importance_Summary99runs.csv"
)

# -----------------------------------------------------
# 2) Model for TCI_trend_mean
# -----------------------------------------------------
fit_brt_models <- function(data, n_runs = 99) {
  r_squared_list <- numeric(n_runs)
  pearson_r_list <- numeric(n_runs)
  models <- vector("list", n_runs)
  importance_list <- vector("list", n_runs)
  
  for (i in 1:n_runs) {
    trainIndex <- createDataPartition(data$FCI_trend_mean, p = 0.8, list = FALSE)
    trainData <- data[trainIndex, ]
    validationData <- data[-trainIndex, ]
    
    set.seed(i)
    brt_model <- gbm(
      FCI_trend_mean ~ 
        AMT + ATP + DEM + Slope +
        DroughtIntensity + WildfireIntensity + WindstormIntensity +
        FormalCroplandFraction + ProtectionAreaFraction +
        ForestManagementIntensity +
        Accessibility2City + DePOPfraction,
      data = data, 
      distribution = "gaussian", 
      n.trees = 1000, 
      interaction.depth = 2, 
      shrinkage = 0.01, 
      cv.folds = 5, 
      keep.data = TRUE, 
      verbose = FALSE
    )
    
    models[[i]] <- brt_model
    importance_list[[i]] <- summary(brt_model, cBars = 20, plot = FALSE)
    
    predictions <- predict(brt_model, newdata = validationData, n.trees = 1000)
    actual <- validationData$FCI_trend_mean
    
    rss <- sum((predictions - actual) ^ 2)
    tss <- sum((actual - mean(actual)) ^ 2)
    r_squared_list[i] <- 1 - (rss / tss)
    pearson_r_list[i] <- cor(actual, predictions)
  }
  
  list(
    models = models,
    importance = importance_list,
    mean_r_squared = mean(r_squared_list),
    mean_pearson_r = mean(pearson_r_list),
    r_squared_list = r_squared_list,
    pearson_r_list = pearson_r_list
  )
}

# Run the modeling function for FCI_trend_mean
results <- fit_brt_models(datatrain)

# Aggregate importance across runs
importance_data <- map_dfr(results$importance, ~ as.data.frame(.x), .id = "run")

importance_summary <- importance_data %>%
  group_by(var) %>%
  summarise(
    mean = mean(rel.inf),
    percentile_2.5 = quantile(rel.inf, probs = 0.025),
    percentile_97.5 = quantile(rel.inf, probs = 0.975)
  ) %>%
  arrange(desc(mean))

importance_summary$var <- gsub("_", " ", importance_summary$var)
write.csv(
  importance_summary,
  "~out/MeanTCItrend_BRT_Importance_Summary99runs.csv"
)

# -----------------------------------------------------
# 3) Model for THI_trend_mean
# -----------------------------------------------------
fit_brt_models <- function(data, n_runs = 99) {
  r_squared_list <- numeric(n_runs)
  pearson_r_list <- numeric(n_runs)
  models <- vector("list", n_runs)
  importance_list <- vector("list", n_runs)
  
  for (i in 1:n_runs) {
    trainIndex <- createDataPartition(data$FHI_trend_mean, p = 0.8, list = FALSE)
    trainData <- data[trainIndex, ]
    validationData <- data[-trainIndex, ]
    
    set.seed(i)
    brt_model <- gbm(
      FHI_trend_mean ~ 
        AMT + ATP + DEM + Slope +
        DroughtIntensity + WildfireIntensity + WindstormIntensity +
        FormalCroplandFraction + ProtectionAreaFraction +
        ForestManagementIntensity +
        Accessibility2City + DePOPfraction,
      data = data, 
      distribution = "gaussian", 
      n.trees = 1000, 
      interaction.depth = 2, 
      shrinkage = 0.01, 
      cv.folds = 5, 
      keep.data = TRUE, 
      verbose = FALSE
    )
    
    models[[i]] <- brt_model
    importance_list[[i]] <- summary(brt_model, cBars = 20, plot = FALSE)
    
    predictions <- predict(brt_model, newdata = validationData, n.trees = 1000)
    actual <- validationData$FHI_trend_mean
    
    rss <- sum((predictions - actual) ^ 2)
    tss <- sum((actual - mean(actual)) ^ 2)
    r_squared_list[i] <- 1 - (rss / tss)
    pearson_r_list[i] <- cor(actual, predictions)
  }
  
  list(
    models = models,
    importance = importance_list,
    mean_r_squared = mean(r_squared_list),
    mean_pearson_r = mean(pearson_r_list),
    r_squared_list = r_squared_list,
    pearson_r_list = pearson_r_list
  )
}

# Run the modeling function for FHI_trend_mean
results <- fit_brt_models(datatrain)

# Aggregate importance across runs
importance_data <- map_dfr(results$importance, ~ as.data.frame(.x), .id = "run")

importance_summary <- importance_data %>%
  group_by(var) %>%
  summarise(
    mean = mean(rel.inf),
    percentile_2.5 = quantile(rel.inf, probs = 0.025),
    percentile_97.5 = quantile(rel.inf, probs = 0.975)
  ) %>%
  arrange(desc(mean))

importance_summary$var <- gsub("_", " ", importance_summary$var)
write.csv(
  importance_summary,
  "~out/MeanTHItrend_BRT_Importance_Summary99runs.csv"
)
