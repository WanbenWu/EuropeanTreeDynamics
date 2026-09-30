library(ggplot2)
library(raster)
library(reshape2)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  list(p = p.mat)
}


FCtrend<-raster(file.path('Data', 'partitioned_TC_trend_WGS84.tif'))
FCItrend<-raster(file.path('Data', 'partitioned_TCI_trend_WGS84.tif'))
FHItrend<-raster(file.path('Data', 'partitioned_THI_trend_WGS84.tif'))
EDtrend<-raster(file.path('Data', 'ED_trend_WGS84.tif'))
PDtrend<-raster(file.path('Data', 'PD_trend_WGS84.tif'))
area_mn_trend<-raster(file.path('Data', 'MPA_trend_WGS84.tif'))
TH_Het_trend<-raster(file.path('Data', 'TH_Het_trend_WGS84.tif'))
kNDVI_Het_trend<-raster(file.path('Data', 'kNDVI_Heter_Index_trend_WGS84.tif'))



reference_raster <- FCtrend

FCItrend <- resample(FCItrend, reference_raster, method="bilinear")
FHItrend <- resample(FHItrend, reference_raster, method="bilinear")
EDtrend <- resample(EDtrend, reference_raster, method="bilinear")
PDtrend <- resample(PDtrend, reference_raster, method="bilinear")
area_mn_trend <- resample(area_mn_trend, reference_raster, method="bilinear")
TH_Het_trend <- resample(TH_Het_trend, reference_raster, method="bilinear")
kNDVI_Het_trend <- resample(kNDVI_Het_trend, reference_raster, method="bilinear")


trends<-stack(FCtrend,FCItrend, FHItrend, EDtrend,PDtrend,area_mn_trend,TH_Het_trend,kNDVI_Het_trend)
names(trends) <- c("TC", "TCI", "THI",'ED','PD','MPA','Height Het.','Greenness Het.')

trends_dataframe<-as.data.frame(as(trends,'SpatialPixelsDataFrame'))


numeric_df <- trends_dataframe[names(trends)]

cor_results <- cor(numeric_df, use = "pairwise.complete.obs")
p_results <- cor.mtest(numeric_df)$p

cor_melt <- melt(cor_results)
p_melt <- melt(p_results)
names(cor_melt) <- c("Var1", "Var2", "Correlation")
names(p_melt) <- c("Var1", "Var2", "P_value")

heatmap_data <- merge(cor_melt, p_melt, by = c("Var1", "Var2"))
heatmap_data$Significance <- cut(
  heatmap_data$P_value, 
  breaks = c(-Inf, 0.01, 0.05, 0.1, 1), 
  labels = c("***", "**", "*", "."), 
  right = FALSE
)

heatmap_data_lower <- heatmap_data[
  as.numeric(factor(heatmap_data$Var1)) < as.numeric(factor(heatmap_data$Var2)), 
]

heatmap_data_lower$R2 <- heatmap_data_lower$Correlation^2

custom_palette <- c('#c659a0','#d289b1','#deabc3','#e8c8d7','#f1e6ec',
                    '#e4ece4','#bfd7c0','#9ac39b','#68b06a','#599c59')

bubble_plot_with_R2 <- ggplot(heatmap_data_lower, aes(x = Var1, y = Var2, size = abs(Correlation), fill = Correlation)) +
  geom_point(shape = 21, color = "white") +
  geom_text(aes(label = Significance), color = "black", size = 3, vjust = -2, hjust = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 3) +
  geom_text(aes(label = paste0("R", expression("^2"), ": ", sprintf("%.2f", R2))), 
            color = "black", size = 3, vjust = 2, parse = TRUE) +
  scale_fill_gradientn(
    colors = custom_palette, 
    name = "Pearson Correlation",
    guide = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 8,
      barheight = 1
    )
  ) +
  scale_size(range = c(3, 12),guide = "none") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12, hjust = 1),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = c(0.85, 0.2),
    legend.direction = "horizontal",
    legend.title = element_text(hjust = 0.5),
    legend.box = "horizontal"
  ) +
  coord_fixed()

ggsave(file.path("outputs", "FigureS8.tif"), bubble_plot_with_R2, width = 10, height = 8, dpi = 300)







