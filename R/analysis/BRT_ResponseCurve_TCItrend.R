library(gbm)
library(dismo)
library(readr)
library(dplyr)
library(scales)
library(purrr)
library(ggplot2)
library(gbm)
library(gridExtra)  # For arranging multiple plots

setwd('~data/')
data_path <- "TStrend_Variables.csv"
data <- read_csv(data_path)
head(data)


datatrain <- data[c('FCI_trend_mean', 
                    'AMT','ATP','DEM','Slope','DroughtIntensity', 'WildfireIntensity','Windspeed','FAarea','PA','FMI',
                    'Accessibility2City','DePOPfraction')]


head(datatrain)
names(datatrain)<-c('FCI_trend_mean',  
                    'AMT','ATP','DEM','Slope','DroughtIntensity', 'WildfireIntensity','WindstormIntensity','FormalCroplandFraction','ProtectionAreaFraction',
                    'ForestManagementIntensity','Accessibility2City','DePOPfraction')

datatrain<-na.omit(datatrain)

####################################################################################################
#fit_brt_models <- function(data, n_runs = 99, response_var = responsevar) {
  #models <- vector("list", n_runs)
#importance_list <- vector("list", n_runs)
plot_data_AMT_all <- data.frame()
plot_data_ATP_all <- data.frame()
plot_data_DEM_all <- data.frame()
plot_data_Slope_all <- data.frame()
plot_data_DroughtIntensity_all <- data.frame()
plot_data_WildfireIntensity_all <- data.frame()
plot_data_WindstormIntensity_all <- data.frame()
plot_data_FormalCroplandFraction_all <- data.frame()
plot_data_ProtectionAreaFraction_all <- data.frame()
plot_data_ForestManagementIntensity_all <- data.frame()
plot_data_Accessibility2City_all <- data.frame()
plot_data_DePOPfraction_all <- data.frame()

data<-datatrain
n_runs <- 99
for (i in 1:n_runs) {
    set.seed(i)  # Ensure reproducibility
    brt_model <- gbm(
      FCI_trend_mean ~ 
        AMT+ATP+DEM+Slope+
        DroughtIntensity+WildfireIntensity+WindstormIntensity+
        FormalCroplandFraction+ProtectionAreaFraction+
        ForestManagementIntensity+
        Accessibility2City+DePOPfraction,
      data = data, distribution = "gaussian", 
      n.trees = 1000, interaction.depth = 2, shrinkage = 0.01, 
      cv.folds = 5, keep.data = TRUE, verbose = FALSE)
    
    plot_data_AMT <- plot.gbm(brt_model, i.var = 'AMT', return.grid = TRUE)
    plot_data_AMT$run <- i  # Add a run identifier
    plot_data_AMT_all <- rbind(plot_data_AMT_all, plot_data_AMT)
    ####ATP
    plot_data_ATP <- plot.gbm(brt_model, i.var = 'ATP', return.grid = TRUE)
    plot_data_ATP$run <- i  # Add a run identifier
    plot_data_ATP_all <- rbind(plot_data_ATP_all, plot_data_ATP)
    ####DEM
    plot_data_DEM <- plot.gbm(brt_model, i.var = 'DEM', return.grid = TRUE)
    plot_data_DEM$run <- i  # Add a run identifier
    plot_data_DEM_all <- rbind(plot_data_DEM_all, plot_data_DEM)
    ####Slope
    plot_data_Slope <- plot.gbm(brt_model, i.var = 'Slope', return.grid = TRUE)
    plot_data_Slope$run <- i  # Add a run identifier
    plot_data_Slope_all <- rbind(plot_data_Slope_all, plot_data_Slope)
    ####DroughtIntensity
    plot_data_DroughtIntensity <- plot.gbm(brt_model, i.var = 'DroughtIntensity', return.grid = TRUE)
    plot_data_DroughtIntensity$run <- i  # Add a run identifier
    plot_data_DroughtIntensity_all <- rbind(plot_data_DroughtIntensity_all, plot_data_DroughtIntensity)
    ####WildfireIntensity
    plot_data_WildfireIntensity <- plot.gbm(brt_model, i.var = 'WildfireIntensity', return.grid = TRUE)
    plot_data_WildfireIntensity$run <- i  # Add a run identifier
    plot_data_WildfireIntensity_all <- rbind(plot_data_WildfireIntensity_all, plot_data_WildfireIntensity)
    ####WildfireIntensity
    plot_data_WindstormIntensity <- plot.gbm(brt_model, i.var = 'WindstormIntensity', return.grid = TRUE)
    plot_data_WindstormIntensity$run <- i  # Add a run identifier
    plot_data_WindstormIntensity_all <- rbind(plot_data_WindstormIntensity_all, plot_data_WindstormIntensity)
    ####FormalCroplandFraction
    plot_data_FormalCroplandFraction <- plot.gbm(brt_model, i.var = 'FormalCroplandFraction', return.grid = TRUE)
    plot_data_FormalCroplandFraction$run <- i  # Add a run identifier
    plot_data_FormalCroplandFraction_all <- rbind(plot_data_FormalCroplandFraction_all, plot_data_FormalCroplandFraction)
    ####ProtectionAreaFraction
    plot_data_ProtectionAreaFraction <- plot.gbm(brt_model, i.var = 'ProtectionAreaFraction', return.grid = TRUE)
    plot_data_ProtectionAreaFraction$run <- i  # Add a run identifier
    plot_data_ProtectionAreaFraction_all <- rbind(plot_data_ProtectionAreaFraction_all, plot_data_ProtectionAreaFraction)
    ####ForestManagementIntensity
    plot_data_ForestManagementIntensity <- plot.gbm(brt_model, i.var = 'ForestManagementIntensity', return.grid = TRUE)
    plot_data_ForestManagementIntensity$run <- i  # Add a run identifier
    plot_data_ForestManagementIntensity_all <- rbind(plot_data_ForestManagementIntensity_all, plot_data_ForestManagementIntensity)
    ####Accessibility2City
    plot_data_Accessibility2City <- plot.gbm(brt_model, i.var = 'Accessibility2City', return.grid = TRUE)
    plot_data_Accessibility2City$run <- i  # Add a run identifier
    plot_data_Accessibility2City_all <- rbind(plot_data_Accessibility2City_all, plot_data_Accessibility2City)
    ####DePOPfraction
    plot_data_DePOPfraction <- plot.gbm(brt_model, i.var = 'DePOPfraction', return.grid = TRUE)
    plot_data_DePOPfraction$run <- i  # Add a run identifier
    plot_data_DePOPfraction_all <- rbind(plot_data_DePOPfraction_all, plot_data_DePOPfraction)
    }
  write.csv(plot_data_AMT_all, "~out/Response_Curve_AMT_MeanTCItrend_99runs.csv")
  write.csv(plot_data_ATP_all, "~out/Response_Curve_ATP_MeanTCItrend_99runs.csv")
  write.csv(plot_data_DEM_all, "~out/Response_Curve_DEM_MeanTCItrend_99runs.csv")
  write.csv(plot_data_Slope_all, "~out/Response_Curve_Slope_MeanTCItrend_99runs.csv")
  write.csv(plot_data_DroughtIntensity_all, "~out/Response_Curve_DroughtIntensity_MeanTCItrend_99runs.csv")
  write.csv(plot_data_WildfireIntensity_all, "~out/Response_Curve_WildfireIntensity_MeanTCItrend_99runs.csv")
  write.csv(plot_data_WindstormIntensity_all, "~out/Response_Curve_WindstormIntensity_MeanTCItrend_99runs.csv")
  write.csv(plot_data_FormalCroplandFraction_all, "~out/Response_Curve_FormalCroplandFraction_MeanTCItrend_99runs.csv")
  write.csv(plot_data_ProtectionAreaFraction_all, "~out/Response_Curve_ProtectionAreaFraction_MeanTCItrend_99runs.csv")
  write.csv(plot_data_ForestManagementIntensity_all, "~out/Response_Curve_ForestManagementIntensity_MeanTCItrend_99runs.csv")
  write.csv(plot_data_Accessibility2City_all, "~out/Response_Curve_Accessibility2City_MeanTCItrend_99runs.csv")
  write.csv(plot_data_DePOPfraction_all, "~out/Response_Curve_DePOPfraction_MeanTCItrend_99runs.csv")
