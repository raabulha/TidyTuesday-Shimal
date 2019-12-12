library(tidyverse)

# data read
diseases <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/diseases.csv")

# data restructure
measles <- diseases %>% 
  filter(disease == "Measles", !(state %in% c("Hawaii", "Alaska"))) %>% 
  mutate(Rate = ((count * 52) / weeks_reporting)  / (population / 100000)) 

# colour palette (adopted from https://simplystatistics.org/2019/08/28/you-can-replicate-almost-any-plot-with-ggplot2/)
jet.colors <- colorRampPalette(c("#F0FFFF", "cyan", "#007FFF", "yellow", "#FFBF00", "orange", "red", "#7F0000"), bias = 2.25)

# replicated plot
measles %>% 
  ggplot(aes(year, reorder(state, desc(state)), fill=Rate)) + 
  geom_tile(color = "white") + 
  scale_fill_gradientn(colours = jet.colors(16), 
                       na.value = "white", labels = c("0","1k", "2k", "3k","4k")) + 
  coord_cartesian(expand = FALSE, clip = "off") + 
  labs(x = "\nYear", y = NULL, title = "Measles") + 
  geom_vline(xintercept = 1963, size = 0.8) + 
  annotate(geom = "text", x = 1963, y = 50.3, label = "Vaccine introduced", size = 3, hjust = 0) + 
  theme_bw() + 
  theme(axis.text = element_text(size = 7, color = "black"), 
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(size = 0.5),
        legend.position = "bottom")
