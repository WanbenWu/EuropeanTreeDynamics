library(gbm)
library(dplyr)
library(purrr)
library(caret)
data_path <- file.path("Data", "TStrend_Variables.csv")
data <- read.csv(data_path)
colnames(data)

datatrain <- data[c('FCI_trend_mean', 'FCI_trend_sum', 'FC_trend_mean', 'FC_trend_sum', 'FHI_trend_mean', 'FHI_trend_sum',
                    'AMT','ATP','DEM','Slope','DroughtIntensity', 'WildfireIntensity','FAarea','PA','FMI',
                    'Accessibility2City','HMI')]


names(datatrain)<-c('FCI_trend_mean', 'FCI_trend_sum', 'FC_trend_mean', 'FC_trend_sum', 'FHI_trend_mean', 'FHI_trend_sum',
                    'AMT','ATP','DEM','Slope','DroughtIntensity', 'WildfireIntensity','FormalCroplandFraction','ProtectionAreaFraction',
                    'ForestManagementIntensity','Accessibility2City','HMI')

datatrain<-na.omit(datatrain)
######################################TC
fit_brt_models <- function(data, n_runs = 99) {
  r_squared_list <- numeric(n_runs)
  pearson_r_list <- numeric(n_runs)
  models <- vector("list", n_runs)
  importance_list <- vector("list", n_runs)
  for (i in 1:n_runs) {
    trainIndex <- createDataPartition(data$FC_trend_mean, p = 0.8, list = FALSE)
    trainData <- data[trainIndex, ]
    validationData <- data[-trainIndex, ]
    
    set.seed(i)  # Ensure reproducibility
    brt_model <- gbm(
      FC_trend_sum ~ 
      #FCI_trend_sum ~ 
      #FHI_trend_sum ~ 
      AMT+ATP+DEM+Slope+
      DroughtIntensity+WildfireIntensity+
      FormalCroplandFraction+ProtectionAreaFraction+
      ForestManagementIntensity+
      Accessibility2City+HMI,
                     data = data, distribution = "gaussian", 
                     n.trees = 1000, interaction.depth = 2, shrinkage = 0.01, 
                     cv.folds = 5, keep.data = TRUE, verbose = FALSE)
    
    models[[i]] <- brt_model
    importance_list[[i]] <- summary(brt_model, cBars = 20, plot = FALSE)
    # Make predictions
    predictions <- predict(brt_model, newdata = validationData, n.trees = 1000)
    
    # Calculate R-squared value
    actual <- validationData$FC_trend_sum
    rss <- sum((predictions - actual) ^ 2)
    tss <- sum((actual - mean(actual)) ^ 2)
    r_squared <- 1 - (rss / tss)
    
    # Calculate Pearson's r
    pearson_r <- cor(actual, predictions)
    
    # Store results
    r_squared_list[i] <- r_squared
    pearson_r_list[i] <- pearson_r
    
  }
  
  list(models = models, importance = importance_list,
    mean_r_squared = mean(r_squared_list),
    mean_pearson_r = mean(pearson_r_list),
    r_squared_list = r_squared_list,
    pearson_r_list = pearson_r_list
  )
}

# Run the modeling function
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
  arrange(desc(mean))  # Order by mean importance for a nicer plot

importance_summary$var <- gsub("_", " ", importance_summary$var)


write.csv(importance_summary,file.path('outputs', 'NetTC_BRT_Importance_Summary99runs.csv'))

######################################TCI
fit_brt_models <- function(data, n_runs = 99) {
  r_squared_list <- numeric(n_runs)
  pearson_r_list <- numeric(n_runs)
  models <- vector("list", n_runs)
  importance_list <- vector("list", n_runs)
  for (i in 1:n_runs) {
    trainIndex <- createDataPartition(data$FCI_trend_sum, p = 0.8, list = FALSE)
    trainData <- data[trainIndex, ]
    validationData <- data[-trainIndex, ]
    
    set.seed(i)  # Ensure reproducibility
    brt_model <- gbm(
      #FC_trend_sum ~ 
        FCI_trend_sum ~ 
        #FHI_trend_sum ~ 
          AMT+ATP+DEM+Slope+
          DroughtIntensity+WildfireIntensity+
          FormalCroplandFraction+ProtectionAreaFraction+
          ForestManagementIntensity+
          Accessibility2City+HMI,
      data = data, distribution = "gaussian", 
      n.trees = 1000, interaction.depth = 2, shrinkage = 0.01, 
      cv.folds = 5, keep.data = TRUE, verbose = FALSE)
    
    models[[i]] <- brt_model
    importance_list[[i]] <- summary(brt_model, cBars = 20, plot = FALSE)
    # Make predictions
    predictions <- predict(brt_model, newdata = validationData, n.trees = 1000)
    
    # Calculate R-squared value
    actual <- validationData$FCI_trend_sum
    rss <- sum((predictions - actual) ^ 2)
    tss <- sum((actual - mean(actual)) ^ 2)
    r_squared <- 1 - (rss / tss)
    
    # Calculate Pearson's r
    pearson_r <- cor(actual, predictions)
    
    # Store results
    r_squared_list[i] <- r_squared
    pearson_r_list[i] <- pearson_r
    
  }
  
  list(models = models, importance = importance_list,
       mean_r_squared = mean(r_squared_list),
       mean_pearson_r = mean(pearson_r_list),
       r_squared_list = r_squared_list,
       pearson_r_list = pearson_r_list
  )
}

# Run the modeling function
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
  arrange(desc(mean))  # Order by mean importance for a nicer plot

importance_summary$var <- gsub("_", " ", importance_summary$var)


write.csv(importance_summary,file.path('outputs', 'NetTCI_BRT_Importance_Summary99runs.csv'))

######################################FHI
fit_brt_models <- function(data, n_runs = 99) {
  r_squared_list <- numeric(n_runs)
  pearson_r_list <- numeric(n_runs)
  models <- vector("list", n_runs)
  importance_list <- vector("list", n_runs)
  for (i in 1:n_runs) {
    trainIndex <- createDataPartition(data$FHI_trend_sum, p = 0.8, list = FALSE)
    trainData <- data[trainIndex, ]
    validationData <- data[-trainIndex, ]
    
    set.seed(i)  # Ensure reproducibility
    brt_model <- gbm(
      #FC_trend_sum ~ 
        #FCI_trend_sum ~ 
        FHI_trend_sum ~ 
          AMT+ATP+DEM+Slope+
          DroughtIntensity+WildfireIntensity+
          FormalCroplandFraction+ProtectionAreaFraction+
          ForestManagementIntensity+
          Accessibility2City+HMI,
      data = data, distribution = "gaussian", 
      n.trees = 1000, interaction.depth = 2, shrinkage = 0.01, 
      cv.folds = 5, keep.data = TRUE, verbose = FALSE)
    
    models[[i]] <- brt_model
    importance_list[[i]] <- summary(brt_model, cBars = 20, plot = FALSE)
    # Make predictions
    predictions <- predict(brt_model, newdata = validationData, n.trees = 1000)
    
    # Calculate R-squared value
    actual <- validationData$FHI_trend_sum
    rss <- sum((predictions - actual) ^ 2)
    tss <- sum((actual - mean(actual)) ^ 2)
    r_squared <- 1 - (rss / tss)
    
    # Calculate Pearson's r
    pearson_r <- cor(actual, predictions)
    
    # Store results
    r_squared_list[i] <- r_squared
    pearson_r_list[i] <- pearson_r
    
  }
  
  list(models = models, importance = importance_list,
       mean_r_squared = mean(r_squared_list),
       mean_pearson_r = mean(pearson_r_list),
       r_squared_list = r_squared_list,
       pearson_r_list = pearson_r_list
  )
}

# Run the modeling function
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
  arrange(desc(mean))  # Order by mean importance for a nicer plot

importance_summary$var <- gsub("_", " ", importance_summary$var)


write.csv(importance_summary,file.path('outputs', 'NetTHI_BRT_Importance_Summary99runs.csv'))





