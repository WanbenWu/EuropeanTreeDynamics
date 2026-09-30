# install.packages("ggplot2")
library(ggplot2)
library(dplyr)

df<-read.csv(file.path('Data', 'Bioregions_Area_Statistics-D54696.csv'))

df <- df %>%
  filter(!code %in% c("Arctic", "BlackSea")) %>%
  mutate(code = ifelse(code %in% c("Pannonian", "Steppic"), "Pannonian/Steppic", code)) %>%
  group_by(class, code) %>%
  summarise(Area = sum(Area), .groups = "drop") %>%
  group_by(code) %>%
  mutate(pct = Area / sum(Area) * 100) %>%
  ungroup()

tiff(file = file.path("outputs", "ModesByBiomesLandscape.tif"),width = 28, height = 16, units = "cm", res = 300,compression = "lzw",pointsize = 12)
ggplot(df, aes(x = code, y = pct, fill = factor(class))) +
  geom_bar(stat = "identity", width = 0.7,alpha=0.8, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = c(
    '1' = '#1c5f2c',
    '2' = '#b4c58f',
    '3' = '#b8d9ea',
    '4' = '#446c9f',
    '5' = '#d99182',
    '6' = '#fea500',
    '7' = '#9470dc',
    '8' = '#aa0000'
  )) +
  labs(x = NULL, y = "Percentage (%)", fill = "Class") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "none")+
  coord_flip()+
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )

dev.off()




df<-read.csv(file.path('Data', 'Bioregions_Area_Statistics-D54696.csv'))

df <- df %>%
  filter(!code %in% c("Arctic", "BlackSea")) %>%
  mutate(code = ifelse(code %in% c("Pannonian", "Steppic"), 
                       "Pannonian/Steppic", 
                       code)) %>%
  group_by(class, code) %>%
  summarise(Area = sum(Area), .groups = "drop")

df$class <- as.factor(df$class)
df$code  <- as.factor(df$code)


ggplot(df, aes(x = code, y = Area/100000, fill = factor(class))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c(
    '1' = '#1c5f2c',
    '2' = '#b4c58f',
    '3' = '#b8d9ea',
    '4' = '#446c9f',
    '5' = '#d99182',
    '6' = '#fea500',
    '7' = '#9470dc',
    '8' = '#aa0000'
  )) +
  labs(x = NULL,  y = expression(Area~(10^5~km^2)), fill = "Class") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "none")+
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    plot.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
    #plot.margin = margin(1, 1, 1, 1, "cm")  # Set even margins
  )




