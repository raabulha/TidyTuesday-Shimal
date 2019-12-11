library(tmap)
library(tidyverse)
library(gganimate)

#data read
diseases <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/diseases.csv")

us_map <- map_data("state")

#data restructure
measles$state_sim <- tolower(measles$state)

measles_map_dat <- measles %>% 
  select(state_sim, year, Rate)

us_map <- us_map %>% inner_join(measles_map_dat, by = c("region" = "state_sim"))
us_map$year <- as.integer(us_map$year)

# colour palette (adopted from https://simplystatistics.org/2019/08/28/you-can-replicate-almost-any-plot-with-ggplot2/)

col.pal <- c("#F0FFFF", "cyan", "#007FFF", "yellow", "#FFBF00", "orange", "red", "#7F0000")

# main map plot
m_anim <- us_map %>% 
  ggplot(aes(x = long, y = lat, group = group, fill = Rate)) + 
  coord_fixed(1.3) + 
  labs(y = NULL, x = NULL) + 
  scale_fill_gradientn(colors = col.pal, limits = c(0,3700), 
                       labels = c("0","1k", "2k", "3k"," "), na.value = "white") + 
  geom_polygon(color = "black") + 
  theme_bw() 

# animated map plot
anim_measles <- m_anim + transition_time(year) +
  labs(title = "Measles cases per 100k people",
       subtitle = "Year: {frame_time}", 
       caption = "\nData source: Rafael Irizarry / dslabs") + 
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        legend.position = "bottom",
        plot.title = element_text(size = 13, face = "bold", color = "black"),
        plot.subtitle = element_text(size = 11, color = "black"),
        plot.caption = element_text(hjust = 1, size = 11))

# animation, frame control
anim_measles <- animate(anim_measles, fps = 3)

# saving animation
anim_save("anim_measles.gif")
