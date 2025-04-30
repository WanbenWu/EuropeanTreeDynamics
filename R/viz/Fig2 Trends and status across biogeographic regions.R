library(dplyr)
library(ggplot2)
library(gridExtra)

setwd('~data/data for fig2')
custom_order <- c('alpine','atlantic','boreal','continental','mediterranean','pannonian/steppic')

# Load trend data for each metric
TC_trend_Biogeo  <- read.csv('TC_Trend_Biogeo.csv')
TCI_trend_Biogeo <- read.csv('TCI_Trend_Biogeo.csv')
THI_trend_Biogeo <- read.csv('THI_Trend_Biogeo.csv')

head(TC_trend_Biogeo)
colnames(TC_trend_Biogeo)
TC_trend_Biogeo$names <- factor(TC_trend_Biogeo$name, levels = custom_order)
TCI_trend_Biogeo$names <- factor(TCI_trend_Biogeo$name, levels = custom_order)
THI_trend_Biogeo$names <- factor(THI_trend_Biogeo$name, levels = custom_order)

mean_TC_Trend <- aggregate(trend ~ name, TC_trend_Biogeo, mean)
mean_TCI_Trend <- aggregate(trend ~ name, TCI_trend_Biogeo, mean)
mean_THI_Trend <- aggregate(trend ~ name, THI_trend_Biogeo, mean)


#---------------------------------------
# Plot (a): TC trend across biogeographic regions
#---------------------------------------

p_a <-ggplot(TC_trend_Biogeo, aes(x = name, y = trend, group = name)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8) +
  geom_violin(trim = TRUE, width = 1.2, alpha = 0.5, color = 'white', aes(fill = factor(name, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.1) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1, aes(fill = factor(name, levels = custom_order))) +
  geom_point(data = mean_TC_Trend, aes(x = name, y = trend), size = 2, shape = 23, color = 'white', fill = 'white') +
  scale_fill_manual(values = (c('#3bcbb5','#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5'))) +
  scale_x_discrete(labels = (c('Alpine',
                               'Atlantic',
                               'Boreal',
                               'Continental',
                               'Mediterranean',
                               'Pannonian/Steppic'))) +
  ggtitle("(a)") +
  theme_bw() +   
  scale_y_continuous(breaks = seq(-1, 1, 0.5), labels = seq(-1, 1, 0.5), limits = c(-1, 1)) +
  labs( x ="" ,y = bquote("TC trend (" * '%' * yr^-1 * ")")) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=-0.01, size = 15, family = "Times New Roman",face = "italic"),
    axis.text.y = element_text(size = 13, hjust = 0.5, family = "Times New Roman"),
    axis.title.y = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    axis.title.x = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, family = "Times New Roman", hjust = 0.03, vjust = -8)
  )


#---------------------------------------
# Plot (b): TCI trend across biogeographic regions
#---------------------------------------


p_b <-ggplot(TCI_trend_Biogeo, aes(x = name, y = trend, group = name)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8) +
  geom_violin(trim = TRUE, width = 1.2, alpha = 0.5, color = 'white', aes(fill = factor(name, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.1) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1, aes(fill = factor(name, levels = custom_order))) +
  geom_point(data = mean_TCI_Trend, aes(x = name, y = trend), size = 2, shape = 23, color = 'white', fill = 'white') +
  scale_fill_manual(values = (c('#3bcbb5','#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5'))) +
  scale_x_discrete(labels = (c('Alpine',
                               'Atlantic',
                               'Boreal',
                               'Continental',
                               'Mediterranean',
                               'Pannonian/Steppic'))) +
  ggtitle("(b)") +
  theme_bw() +   
  scale_y_continuous(breaks = c(-0.01, 0, 0.01),
                     labels = c('-0.01','0', '0.01'),
                     limits = c(-0.01, 0.01)) +
  labs( x ="" ,y = bquote("TCI trend ("  * yr^-1 * ")"))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=-0.01, size = 15, family = "Times New Roman",face = "italic"),
    axis.text.y = element_text(size = 13, hjust = 0.5, family = "Times New Roman"),
    axis.title.y = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    axis.title.x = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, family = "Times New Roman", hjust = 0.03, vjust = -8)
  )


#---------------------------------------
# Plot (c): THI trend across biogeographic regions
#---------------------------------------

p_c <-ggplot(THI_trend_Biogeo, aes(x = name, y = trend, group = name)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#8e8e8e", size = 0.8) +
  geom_violin(trim = TRUE, width = 0.6, alpha = 0.5, color = 'white', aes(fill = factor(name, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.1) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1, aes(fill = factor(name, levels = custom_order))) +
  geom_point(data = mean_THI_Trend, aes(x = name, y = trend), size = 2, shape = 23, color = 'white', fill = 'white') +
  scale_fill_manual(values = (c('#3bcbb5','#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5'))) +
  scale_x_discrete(labels = (c('Alpine',
                               'Atlantic',
                               'Boreal',
                               'Continental',
                               'Mediterranean',
                               'Pannonian/Steppic'))) +
  ggtitle("(c)") +
  theme_bw() +   
  scale_y_continuous(breaks = c(-0.01, 0, 0.01),
                     labels = c('-0.01','0', '0.01'),
                     limits = c(-0.01, 0.01)) +
  labs( x ="" ,y = bquote("THI trend ("  * yr^-1 * ")"))+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=-0.01, size = 15, family = "Times New Roman",face = "italic"),
    axis.text.y = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    axis.title.y = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    axis.title.x = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, family = "Times New Roman", hjust = 0.03, vjust = -8)
  )


# Load 2021 data for each metric
TC_2021_Biogeo  <- read.csv('TC_2021_Biogeo.csv')
TCI_2021_Biogeo <- read.csv('TCI_2021_Biogeo.csv')
THI_2021_Biogeo <- read.csv('THI_2021_Biogeo.csv')
head(TC_2021_Biogeo)

TC_2021_Biogeo$name  <- factor(TC_2021_Biogeo$name,  levels = custom_order)
TCI_2021_Biogeo$name <- factor(TCI_2021_Biogeo$name, levels = custom_order)
THI_2021_Biogeo$name <- factor(THI_2021_Biogeo$name, levels = custom_order)


# Calculate mean values by management type and overall mean
mean_TC_2021      <- aggregate(TC_2021  ~ name, TC_2021_Biogeo,  mean)
mean_TCI_2021     <- aggregate(TCI_2021 ~ name, TCI_2021_Biogeo, mean)
mean_THI_2021     <- aggregate(THI_2021 ~ name, THI_2021_Biogeo, mean)

mean_TC_2021_value  <- mean(TC_2021_Biogeo$TC_2021)
mean_TCI_2021_value <- mean(na.omit(TCI_2021_Biogeo$TCI_2021))
mean_THI_2021_value <- mean(na.omit(THI_2021_Biogeo$THI_2021))

#---------------------------------------
# Plot (d): TC in 2021 across biogeographic regions
#---------------------------------------
head(TC_2021_Biogeo)
head(mean_TC_Trend)
p_d <-ggplot(TC_2021_Biogeo,aes(x=as.numeric(factor(name,levels = custom_order)),#+0.1
                                 y=TC_2021,group=factor(name,levels = custom_order)))+
  geom_hline(yintercept = mean_TC_2021_value, linetype = "dashed", color = "#87373b",size=0.8)+
  geom_violin(trim = TRUE, width = 1.2, alpha = 0.5, color = 'white', aes(fill = factor(name, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.1) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1, aes(fill = factor(name, levels = custom_order))) +
  geom_point(data = mean_TC_2021, aes(x = name, y = TC_2021), size = 2, shape = 23, color = 'white', fill = 'white') +
  scale_fill_manual(values = (c('#3bcbb5','#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5'))) +
  scale_x_discrete(labels = (c('Alpine',
                               'Atlantic',
                               'Boreal',
                               'Continental',
                               'Mediterranean',
                               'Pannonian/Steppic'))) +
  ggtitle("(d)") +
  ylim(0,100)+
  labs(x ="", y = "TC in 2021 (%)") +  
  theme_bw() +   
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=-0.01, size = 15, family = "Times New Roman",face = "italic"),
    axis.text.y = element_text(size = 13, hjust = 0.5, family = "Times New Roman"),
    axis.title.y = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    axis.title.x = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, family = "Times New Roman", hjust = 0.03, vjust = -8)
  )



#---------------------------------------
# Plot (e): TCI in 2021 across biogeographic regions
#---------------------------------------


p_e <-ggplot(TCI_2021_Biogeo,aes(x=as.numeric(factor(name,levels = custom_order)),#+0.1
                                 y=TCI_2021,group=factor(name,levels = custom_order)))+
  geom_hline(yintercept = mean_TCI_2021_value, linetype = "dashed", color = "#87373b",size=0.8)+
  geom_violin(trim = TRUE, width = 1.2, alpha = 0.5, color = 'white', aes(fill = factor(name, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.1) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1, aes(fill = factor(name, levels = custom_order))) +
  geom_point(data = mean_TCI_2021 , aes(x = name, y = TCI_2021), size = 2, shape = 23, color = 'white', fill = 'white') +
  scale_fill_manual(values = (c('#3bcbb5','#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5'))) +
  scale_x_discrete(labels = (c('Alpine',
                               'Atlantic',
                               'Boreal',
                               'Continental',
                               'Mediterranean',
                               'Pannonian/Steppic'))) +
  ggtitle("(e)") +
  ylim(0,1)+
  labs(x ="", y = "TCI in 2021") +  
  theme_bw() +   
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=-0.01, size = 15, family = "Times New Roman",face = "italic"),
    axis.text.y = element_text(size = 13, hjust = 0.5, family = "Times New Roman"),
    axis.title.y = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    axis.title.x = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, family = "Times New Roman", hjust = 0.03, vjust = -8)
  )


#---------------------------------------
# Plot (f): THI in 2021 across biogeographic regions
#---------------------------------------

p_f <-ggplot(THI_2021_Biogeo,aes(x=as.numeric(factor(name,levels = custom_order)),#+0.1
                                 y=THI_2021,group=factor(name,levels = custom_order)))+
  geom_hline(yintercept = mean_THI_2021_value, linetype = "dashed", color = "#87373b",size=0.8)+
  geom_violin(trim = TRUE, width = 1.2, alpha = 0.5, color = 'white', aes(fill = factor(name, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.1) +
  geom_boxplot(width = 0.1, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1, aes(fill = factor(name, levels = custom_order))) +
  geom_point(data = mean_THI_2021 , aes(x = name, y = THI_2021), size = 2, shape = 23, color = 'white', fill = 'white') +
  scale_fill_manual(values = (c('#3bcbb5','#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5', '#3bcbb5'))) +
  scale_x_discrete(labels = (c('Alpine',
                               'Atlantic',
                               'Boreal',
                               'Continental',
                               'Mediterranean',
                               'Pannonian/Steppic'))) +
  ggtitle("(f)") +
  ylim(0,1)+
  labs(x ="", y = "THI in 2021") +  
  theme_bw() +   
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5,vjust=-0.01, size = 15, family = "Times New Roman",face = "italic"),
    axis.text.y = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    axis.title.y = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    axis.title.x = element_text(size = 15, hjust = 0.5, family = "Times New Roman"),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 15, family = "Times New Roman", hjust = 0.03, vjust = -8)
  )



plots <- list(p_a+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), 
              p_d+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()),
              p_b+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()),
              p_e+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()),
              p_c,p_f)

combined_plot <- grid.arrange(
  grobs = plots, 
  ncol = 2, 
  nrow = 3, 
  heights = c(0.8, 0.8, 1.3)
)

# Save the combined plot
ggsave(
  'Fig2.png',
  plot = combined_plot,
  height = 12, 
  width = 12,
  dpi = 1000, 
  units = "in"
)
