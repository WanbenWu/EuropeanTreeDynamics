library(ggplot2)
library(dplyr)
df <- read.csv(file.path('Data', 'Country_Area_Statistics.csv'))
df <- df %>%
  filter(!NAME_ENGL %in% c("Faroes", "Monaco", "Cyprus", "Malta", "Iceland", 
                           "Vatican City", "San Marino", "Gibraltar", "Andorra", 
                           "Liechtenstein", "Isle of Man", "Türkiye", "Luxembourg"))

df <- df %>%
  group_by(NAME_ENGL) %>%
  mutate(total_area = sum(Area),
         pct = Area / total_area * 100) %>%
  ungroup()

df$code <- factor(df$NAME_ENGL, 
                  levels = df %>% 
                    group_by(NAME_ENGL) %>% 
                    summarise(total = sum(Area)) %>% 
                    arrange(desc(total)) %>% 
                    pull(NAME_ENGL))

df$class <- as.factor(df$class)

tiff(file = file.path("outputs", "ModesByCountrieslandscape.tif"),width = 28, height = 16, units = "cm", res = 300,compression = "lzw",pointsize = 12)
ggplot(df, aes(x = code, y = pct, fill = factor(class))) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8, position = position_stack(reverse = TRUE)) +
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
        legend.position = "none") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "#8e8e8e", size = 0.8) + 
  coord_flip() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())
dev.off()

ggplot(df, aes(x = code, y = Area / 100000, fill = factor(class))) +
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
  labs(x = NULL, y = expression(Area~(10^5~km^2)), fill = "Class") +
  theme(panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = "none") +
  coord_flip() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        plot.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank())




