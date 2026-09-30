library(ggplot2)
library(raster)


# Load raster data
Trend_Sig_lt0_05 <- raster(file.path('Data', 'FCI_trend_fcthas10_sig_WGS84.tif'))
# Convert raster to dataframe
Trend_Sig_lt0_05_dataframe <- as.data.frame(Trend_Sig_lt0_05, xy = TRUE)
# Rename columns
names(Trend_Sig_lt0_05_dataframe) <- c('Longitude','Latitude','FCtrend')
Trend_Sig_lt0_05_dataframe<- na.omit(Trend_Sig_lt0_05_dataframe)
# Convert Latitude to numeric
Trend_Sig_lt0_05_dataframe$Latitude <- as.numeric(Trend_Sig_lt0_05_dataframe$Latitude)
#Trend_Sig_lt0_05_dataframe$Latitude <- 30+cut(Trend_Sig_lt0_05_dataframe$Latitude, 
#                                     breaks = seq(30, 80, by = 0.1), 
#                                     labels = FALSE)*0.1
# Aggregate FC trend by Latitude
summary_values <- aggregate(FCtrend ~ Latitude, data = Trend_Sig_lt0_05_dataframe, 
                            FUN = function(x) {
                              mean_val <- mean(x, na.rm = TRUE)
                              sd_val <- sd(x, na.rm = TRUE) / sqrt(length(x))
                              c(mean = mean_val, sd = sd_val)
                            })
summary_values<- na.omit(summary_values)
latitude <- summary_values$Latitude
mean_values <- summary_values$FCtrend[,1]
std_values <- summary_values$FCtrend[,2]
transformed_data <- data.frame(Latitude = latitude,
                               Mean = mean_values,
                               Std = std_values)
transformed_data <- na.omit(transformed_data)
transformed_data  <- transformed_data[transformed_data$Std < 0.1, ]


################################decreasing
{
  Trend_Sig_lt0_05_dataframe$Latitude <- as.numeric(Trend_Sig_lt0_05_dataframe$Latitude)
  #Trend_Sig_lt0_05_dataframe$Latitude <- 30+cut(Trend_Sig_lt0_05_dataframe$Latitude, 
  #                                     breaks = seq(30, 80, by = 0.1), 
  #                                     labels = FALSE)*0.1
  Trend_Sig_lt0_05_dataframe_de<-Trend_Sig_lt0_05_dataframe[Trend_Sig_lt0_05_dataframe$FCtrend < 0, ]
  # Aggregate FC trend by Latitude
  summary_values <- aggregate(FCtrend ~ Latitude, data = Trend_Sig_lt0_05_dataframe_de, 
                              FUN = function(x) {
                                mean_val <- mean(x, na.rm = TRUE)
                                sd_val <- sd(x, na.rm = TRUE) / sqrt(length(x))
                                c(mean = mean_val, sd = sd_val)
                              })
  summary_values<- na.omit(summary_values)
  latitude <- summary_values$Latitude
  mean_values <- summary_values$FCtrend[,1]
  std_values <- summary_values$FCtrend[,2]
  transformed_data_de <- data.frame(Latitude = latitude,
                                 Mean = mean_values,
                                 Std = std_values)
  transformed_data_de <- na.omit(transformed_data_de)
  transformed_data_de  <- transformed_data_de[transformed_data_de$Std < 0.1, ]
}

################################increasing
{
  Trend_Sig_lt0_05_dataframe$Latitude <- as.numeric(Trend_Sig_lt0_05_dataframe$Latitude)
  #Trend_Sig_lt0_05_dataframe$Latitude <- 30+cut(Trend_Sig_lt0_05_dataframe$Latitude, 
  #                                     breaks = seq(30, 80, by = 0.1), 
  #                                     labels = FALSE)*0.1
  Trend_Sig_lt0_05_dataframe_in<-Trend_Sig_lt0_05_dataframe[Trend_Sig_lt0_05_dataframe$FCtrend > 0, ]
  # Aggregate FC trend by Latitude
  summary_values <- aggregate(FCtrend ~ Latitude, data = Trend_Sig_lt0_05_dataframe_in, 
                              FUN = function(x) {
                                mean_val <- mean(x, na.rm = TRUE)
                                sd_val <- sd(x, na.rm = TRUE) / sqrt(length(x))
                                c(mean = mean_val, sd = sd_val)
                              })
  summary_values<- na.omit(summary_values)
  latitude <- summary_values$Latitude
  mean_values <- summary_values$FCtrend[,1]
  std_values <- summary_values$FCtrend[,2]
  transformed_data_in <- data.frame(Latitude = latitude,
                                    Mean = mean_values,
                                    Std = std_values)
  transformed_data_in <- na.omit(transformed_data_in)
  transformed_data_in  <- transformed_data_in[transformed_data_in$Std < 0.1, ]
}



tiff(file = file.path("outputs", "FCITrendLat.tiff"),width = 9.95, height = 18.2, units = "cm", res = 300,compression = "lzw",pointsize = 0)


ggplot(transformed_data, aes(x = Latitude, y = Mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#dedede",size=0.8) + 
  geom_line(data = transformed_data, aes(y = Mean,x = Latitude), color = "#a2a3a1")+
  geom_line(data = transformed_data_de, aes(y = Mean,x = Latitude), color = "#ffbe90")+
  geom_line(data = transformed_data_in, aes(y = Mean,x = Latitude), color = "#cdeb90")+
  labs(x = "Latitude", y = bquote("TCI trend (" * yr^-1 * ")")) +  
  coord_flip() +
  scale_x_continuous(labels = function(x) paste0(x, "°N"), limits = c(35, 65)) +
  #xlim(35, 70) +
  scale_y_continuous(breaks = c(-0.01, -0.005, 0, 0.005, 0.01),
                     labels = c('-0.01','-0.005', '0', '0.005', '0.01'),
                     limits = c(-0.01, 0.01)) +
  theme_bw() +   
  theme(
    axis.text.x = element_text(hjust = 0.5, size = 17,family = "Times New Roman"),
    axis.text.y = element_text(size = 18,angle = 90, hjust = 0.5),
    axis.title.x = element_text(size = 17, hjust = 0.5,family = "Times New Roman"),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    plot.title = element_text(size = 14, hjust = 0.5,family = "Times New Roman")
  )
dev.off()




