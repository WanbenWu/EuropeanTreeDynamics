library(dplyr)
library(ggplot2)

# Set working directory
setwd('~data/TCacrossLMs/')

custom_order <- c('AbndndF','A','B','C','D','E','F','Strct_W','NonStrictPA')
# Load trend data for each metric
TC_trend_LMs  <- read.csv('TC_trend_LandManagments.csv')
TCI_trend_LMs <- read.csv('TCI_trend_LandManagments.csv')
THI_trend_LMs <- read.csv('THI_trend_LandManagments.csv')

TC_trend_LMs$names <- factor(TC_trend_LMs$names, levels = custom_order)
TCI_trend_LMs$names <- factor(TCI_trend_LMs$names, levels = custom_order)
THI_trend_LMs$names <- factor(THI_trend_LMs$names, levels = custom_order)

mean_TC_trend <- aggregate(TC_trend ~ names, TC_trend_LMs, mean)
mean_TCI_trend <- aggregate(TCI_trend ~ names, TCI_trend_LMs, mean)
mean_THI_trend <- aggregate(THI_trend ~ names, THI_trend_LMs, mean)


#---------------------------------------
# Plot (a): TC trend across management types
#---------------------------------------

p_a <-ggplot(TC_trend_LMs, aes(x = names, y = TC_trend, group = names)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#8e8e8e', size = 0.8) +  # zero reference
  geom_violin(trim = TRUE, width = 2.5, alpha = 0.5, color = 'white',
              aes(fill = factor(names, levels = custom_order))) +                 # distribution
  stat_boxplot(geom = 'errorbar', width = 0.2) +                # whiskers
  geom_boxplot(width = 0.15, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1,
               aes(fill = factor(names, levels = custom_order))) +                # boxes
  geom_point(data = mean_TC_trend, aes(x = names, y = TC_trend),
             size = 2, shape = 23, color = 'white', fill = 'white') +              # mean values
  scale_fill_manual(values = rep('#3bcbb5', length(custom_order))) +             # uniform fill
  scale_x_discrete(labels = c('Former cropland','Strict nature management','Close to nature management',
                              'Low intensity management','Multifunctional management',
                              'Intensive management','Very intensive management',
                              'Strict protected area','Non-strict protected area')) +
  ggtitle('(a)') +
  scale_y_continuous(breaks = seq(-1, 1, 0.5), labels = seq(-1, 1, 0.5), limits = c(-1, 1)) +
  labs(x = NULL, y = bquote('TC trend ('*'%'*yr^-1*')')) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = -0.01,
                               size = 15, family = 'Times New Roman', face = 'italic'),
    axis.text.y = element_text(size = 13, family = 'Times New Roman'),
    axis.title = element_text(size = 15, family = 'Times New Roman'),
    legend.position = 'none',
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, family = 'Times New Roman', hjust = 0.03, vjust = -8)
  )


#---------------------------------------
# Plot (b): TCI trend (scaled ×100)
#---------------------------------------


p_b <-ggplot(TCI_trend_LMs, aes(x = names, y = TCI_trend * 100, group = names)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#8e8e8e', size = 0.8) +
  geom_violin(trim = TRUE, width = 2, alpha = 0.5, color = 'white',
              aes(fill = factor(names, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1,
               aes(fill = factor(names, levels = custom_order))) +
  geom_point(data = mean_TCI_trend, aes(x = names, y = TCI_trend * 100),
             size = 2, shape = 23, color = 'white', fill = 'white') +
  scale_fill_manual(values = rep('#3bcbb5', length(custom_order))) +
  scale_x_discrete(labels = c('Former cropland','Strict nature management','Close to nature management',
                              'Low intensity management','Multifunctional management',
                              'Intensive management','Very intensive management',
                              'Strict protected area','Non-strict protected area')) +
  ggtitle('(b)') +
  scale_y_continuous(breaks = seq(-1, 1, 0.5), labels = seq(-1, 1, 0.5), limits = c(-1, 1)) +
  labs(x = NULL, y = bquote('TCI trend ('~10^-2~yr^-1*')')) +
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = -0.01,
                               size = 15, family = 'Times New Roman', face = 'italic'),
    axis.text.y = element_text(size = 13, family = 'Times New Roman'),
    axis.title = element_text(size = 15, family = 'Times New Roman'),
    legend.position = 'none',
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, family = 'Times New Roman', hjust = 0.03, vjust = -8)
  )


#---------------------------------------
# Plot (c): THI trend (scaled ×100)
#---------------------------------------

p_c <-ggplot(THI_trend_LMs, aes(x = names, y = THI_trend * 100, group = names)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = '#8e8e8e', size = 0.8) +
  geom_violin(trim = TRUE, width = 1.5, alpha = 0.5, color = 'white',
              aes(fill = factor(names, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1,
               aes(fill = factor(names, levels = custom_order))) +
  geom_point(data = mean_THI_trend, aes(x = names, y = THI_trend * 100),
             size = 2, shape = 23, color = 'white', fill = 'white') +
  scale_fill_manual(values = rep('#3bcbb5', length(custom_order))) +
  scale_x_discrete(labels = c('Former cropland','Strict nature management','Close to nature management',
                              'Low intensity management','Multifunctional management',
                              'Intensive management','Very intensive management',
                              'Strict protected area','Non-strict protected area')) +
  ggtitle('(c)') +
  scale_y_continuous(breaks = seq(-1, 1, 0.5), labels = seq(-1, 1, 0.5), limits = c(-1, 1)) +
  labs(x = NULL, y = bquote('THI trend ('~10^-2~yr^-1*')')) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = -0.01,
                               size = 15, family = 'Times New Roman', face = 'italic'),
    axis.text.y = element_text(size = 13, family = 'Times New Roman'),
    axis.title = element_text(size = 15, family = 'Times New Roman'),
    legend.position = 'none',
    panel.grid = element_blank(),
    plot.title = element_text(size = 18, family = 'Times New Roman', hjust = 0.03, vjust = -8)
  )

dev.off()

# Load 2021 data for each metric
TC_2021_LMs  <- read.csv('TC_2021_LMs.csv')
TCI_2021_LMs <- read.csv('TCI_2021_LMs.csv')
THI_2021_LMs <- read.csv('THI_2021_LMs.csv')


TC_2021_LMs$names  <- factor(TC_2021_LMs$names,  levels = custom_order)
TCI_2021_LMs$names <- factor(TCI_2021_LMs$names, levels = custom_order)
THI_2021_LMs$names <- factor(THI_2021_LMs$names, levels = custom_order)


# Calculate mean values by management type and overall mean
mean_TC_2021      <- aggregate(TC_2021  ~ names, TC_2021_LMs,  mean)
mean_TCI_2021     <- aggregate(TCI_2021 ~ names, TCI_2021_LMs, mean)
mean_THI_2021     <- aggregate(THI_2021 ~ names, THI_2021_LMs, mean)

mean_TC_2021_value  <- mean(TC_2021_LMs$TC_2021)
mean_TCI_2021_value <- mean(na.omit(TCI_2021_LMs$TCI_2021))
mean_THI_2021_value <- mean(na.omit(THI_2021_LMs$THI_2021))

#---------------------------------------
# Plot (d): TC in 2021 across management types
#---------------------------------------


p_d <-ggplot(TC_2021_LMs, aes(x = names, y = TC_2021, group = names)) +
  geom_hline(yintercept = mean_TC_2021_value, linetype = 'dashed', color = '#87373b', size = 0.8) +  # overall mean
  geom_violin(trim = TRUE, width = 1.2, alpha = 0.5, color = 'white',
              aes(fill = factor(names, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1,
               aes(fill = factor(names, levels = custom_order))) +
  geom_point(data = mean_TC_2021,
             aes(x = names, y = TC_2021), shape = 23, size = 2, color = 'white', fill = 'white') +
  scale_fill_manual(values = rep('#3bcbb5', length(custom_order))) +  # uniform fill
  scale_x_discrete(labels = c('Former cropland','Strict nature management','Close to nature management',
                              'Low intensity management','Multifunctional management',
                              'Intensive management','Very intensive management',
                              'Strict protected area','Non-strict protected area')) +  # x-axis labels
  ggtitle('(d)') +
  ylim(0, 100) +
  labs(x = NULL, y = bquote('TC in 2021 (%)')) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = -0.01, size = 15,
                               family = 'Times New Roman', face = 'italic'),
    axis.text.y = element_text(size = 13, family = 'Times New Roman'),
    axis.title  = element_text(size = 15, family = 'Times New Roman'),
    legend.position = 'none', panel.grid = element_blank(),
    plot.title = element_text(size = 15, family = 'Times New Roman', hjust = 0.03, vjust = -8)
  )


#---------------------------------------
# Plot (e): TCI in 2021 across management types
#---------------------------------------


p_e <-ggplot(TCI_2021_LMs, aes(x = names, y = TCI_2021, group = names)) +
  geom_hline(yintercept = mean_TCI_2021_value, linetype = 'dashed', color = '#87373b', size = 0.8) +
  geom_violin(trim = TRUE, width = 1.2, alpha = 0.5, color = 'white',
              aes(fill = factor(names, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1,
               aes(fill = factor(names, levels = custom_order))) +
  geom_point(data = mean_TCI_2021,
             aes(x = names, y = TCI_2021), shape = 23, size = 2, color = 'white', fill = 'white') +
  scale_fill_manual(values = rep('#3bcbb5', length(custom_order))) +
  scale_x_discrete(labels = c('Former cropland','Strict nature management','Close to nature management',
                              'Low intensity management','Multifunctional management',
                              'Intensive management','Very intensive management',
                              'Strict protected area','Non-strict protected area')) +
  ggtitle('(e)') +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.75,1), labels = c('0', '0.25', '0.5','0.75','1'), limits = c(0, 1)) +
  labs(x = NULL, y = bquote('TCI in 2021')) +
  theme_bw() +   
  theme(
    axis.text.x = element_text(angle = 90, vjust = -0.01, size = 15,
                               family = 'Times New Roman', face = 'italic'),
    axis.text.y = element_text(size = 13, family = 'Times New Roman'),
    axis.title  = element_text(size = 15, family = 'Times New Roman'),
    legend.position = 'none', panel.grid = element_blank(),
    plot.title = element_text(size = 15, family = 'Times New Roman', hjust = 0.03, vjust = -8)
  )


#---------------------------------------
# Plot (f): THI in 2021 across management types
#---------------------------------------

p_f <-ggplot(THI_2021_LMs, aes(x = names, y = THI_2021, group = names)) +
  geom_hline(yintercept = mean_THI_2021_value, linetype = 'dashed', color = '#87373b', size = 0.8) +
  geom_violin(trim = TRUE, width = 1.2, alpha = 0.5, color = 'white',
              aes(fill = factor(names, levels = custom_order))) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  geom_boxplot(width = 0.15, outlier.shape = NA, color = 'black', fill = '#5c6a72', alpha = 1,
               aes(fill = factor(names, levels = custom_order))) +
  geom_point(data = mean_THI_2021,
             aes(x = names, y = THI_2021), shape = 23, size = 2, color = 'white', fill = 'white') +
  scale_fill_manual(values = rep('#3bcbb5', length(custom_order))) +
  scale_x_discrete(labels = c('Former cropland','Strict nature management','Close to nature management',
                              'Low intensity management','Multifunctional management',
                              'Intensive management','Very intensive management',
                              'Strict protected area','Non-strict protected area')) +
  ggtitle('(f)') +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.75,1), labels = c('0', '0.25', '0.5','0.75','1'), limits = c(0, 1)) +
  labs(x = NULL, y = bquote('THI in 2021')) +
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = -0.01, size = 15,
                               family = 'Times New Roman', face = 'italic'),
    axis.text.y = element_text(size = 13, family = 'Times New Roman'),
    axis.title  = element_text(size = 15, family = 'Times New Roman'),
    legend.position = 'none', panel.grid = element_blank(),
    plot.title = element_text(size = 15, family = 'Times New Roman', hjust = 0.03, vjust = -8)
  )

library(gridExtra)

plots <- list(p_a+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()), 
              p_d+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()),
              p_b+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()),
              p_e+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()),
              p_c,p_f)

combined_plot <- grid.arrange(
  grobs = plots, 
  ncol = 2, 
  nrow = 3, 
  heights = c(0.8, 0.8, 1.5)
)

# Save the combined plot
ggsave(
  'Fig3.png',
  plot = combined_plot,
  height = 12, 
  width = 12,
  dpi = 1000, 
  units = "in"
)
