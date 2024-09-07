# Set the working directory
setwd('/path/Erasmus +')

# Load LIBRARIES
library(tidyverse)
library(utf8)
library(ggbump) # creates elegant bump charts in ggplot
# To work with custom fonts
library(extrafont)
font_import()
loadfonts()
# To add the PNG logo to the Plot
# To use annotation_custom() along with rasterGrob() from the grid package to overlay the logo on your plot.
library(rsvg)
library(grid)
library(magick)

# Import ZAXIS logo
# Convert SVG to a high-resolution PNG
rsvg_png('Zaxis_logo.svg', 'Zaxis_logo.png', width = 1000, height = 1000)
# Read in the logo as a raster image
logo <- magick::image_read('Zaxis_logo.png')
# Convert to grob (graphical object)
#logo_grob <- rasterGrob(logo, x = 0.054, y = 0.05, width = unit(1.5, 'cm'), height = unit(1.5, 'cm'), just = c('right', 'top'))



# Viz_4.1-4.3. Duration of mobility | Bump Chart

# Select the column of '`Mobility Duration`'
mob_dur <- erasmus_ge %>% 
  select(`Mobility Duration`, year)
# Replace column name
colnames(mob_dur)[which(names(mob_dur) == 'Mobility Duration')] <- 'duration'

# Calculate the participation by duration and year
mob_dur_aggr <- mob_dur %>% 
  group_by(year, duration) %>%
  summarise(number = n())


# Plot bump chart | Viz_4.1
viz_4.1 <- 
  ggplot(mob_dur_aggr, aes(x=duration, y=number)) +
  geom_bump(size = 0.6, alpha=1, color='#62789a') + 
  facet_grid(year~., scales = 'free') + 
  scale_x_continuous(breaks = c(10, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 943)) + 
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამით დაფინანსებული სტიპენდიების ხანგრძლივობა, 2014-2022', 
    caption = 'მონაცემები: Erasmus + stats', 
    x='დღეების რაოდენობა',
    y='სტიპენდიის რაოდენობა'
  ) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(colour = '#bdbdbd', linewidth = 0.5, linetype = 'dotted'), 
    panel.grid.major.y = element_line(colour = '#bdbdbd', linewidth = 0.2, linetype = 'dotted'), 
    #panel.grid.minor = element_line(colour = 'grey', linewidth = 0.1, linetype = 'dotted'), 
    axis.ticks = element_line(colour = '#737373', linewidth = 0.4), 
    legend.position = 'bottom',
    legend.title = element_blank(), 
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 9), 
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    axis.title.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 9),
    axis.title.y = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.y = element_text(family = 'FiraGo Regular', colour = '#737373', size = 7), 
    strip.background = element_rect(fill = '#f7da6f', color = '#f7da6f'), 
    strip.text = element_text(family = 'FiraGo Medium', colour = '#737373', size = 9))

viz_4.1

# Create the base plot
viz_4.1_with_logo <- ggdraw(viz_4.1) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_4.1_with_logo

ggsave(plot = viz_4.1_with_logo, 'Viz/Viz_4.1.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)



# Filter out the duration | Days < 30
mob_dur_aggr_filt1 <- mob_dur_aggr %>% 
  filter(duration<30)

# Plot bump chart | Viz_4.2
viz_4.2 <- 
  ggplot(mob_dur_aggr_filt1, aes(x=duration, y=number)) +
  geom_bump(size = 0.9, alpha=1, color='#62789a') + 
  facet_grid(year~., scales = 'free') + 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
                                21, 22, 23, 24, 25, 26, 27, 28, 29, 30)) + 
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამით დაფინანსებული სტიპენდიების ხანგრძლივობა, 2014-2022 | დღეების ხანგრძლივობა < 30', 
    caption = 'მონაცემები: Erasmus + stats', 
    x='დღეების რაოდენობა',
    y='სტიპენდიის რაოდენობა'
  ) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(colour = '#bdbdbd', linewidth = 0.4, linetype = 'dotted'), 
    panel.grid.major.y = element_line(colour = '#bdbdbd', linewidth = 0.1, linetype = 'dotted'), 
    #panel.grid.minor = element_line(colour = 'grey', linewidth = 0.1, linetype = 'dotted'), 
    axis.ticks = element_line(colour = '#737373', linewidth = 0.4), 
    legend.position = 'bottom',
    legend.title = element_blank(), 
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 9), 
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    axis.title.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 9),
    axis.title.y = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.y = element_text(family = 'FiraGo Regular', colour = '#737373', size = 7), 
    strip.background = element_rect(fill = '#f7da6f', color = '#f7da6f'), 
    strip.text = element_text(family = 'FiraGo Medium', colour = '#737373', size = 9))

viz_4.2

# Create the base plot
viz_4.2_with_logo <- ggdraw(viz_4.2) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_4.2_with_logo

ggsave(plot = viz_4.2_with_logo, 'Viz/Viz_4.2.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)



# Filter out the duration | Days > 30 & < 180
mob_dur_aggr_filt2 <- mob_dur_aggr %>% 
  filter(duration>90 & duration<180)

# Plot bump chart | Viz_4.3
viz_4.3 <- 
  ggplot(mob_dur_aggr_filt2, aes(x=duration, y=number)) +
  geom_bump(size = 0.8, alpha=1, color='#62789a') + 
  facet_grid(year~., scales = 'free') + 
  scale_x_continuous(breaks = c(90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160, 165, 170, 175, 180)) + 
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამით დაფინანსებული სტიპენდიების ხანგრძლივობა, 2014-2022 | დღეების ხანგრძლივობა > 30 & < 180', 
    caption = 'მონაცემები: Erasmus + stats', 
    x='დღეების რაოდენობა',
    y='სტიპენდიის რაოდენობა'
  ) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(colour = '#bdbdbd', linewidth = 0.4, linetype = 'dotted'), 
    panel.grid.major.y = element_line(colour = '#bdbdbd', linewidth = 0.1, linetype = 'dotted'), 
    #panel.grid.minor = element_line(colour = 'grey', linewidth = 0.1, linetype = 'dotted'), 
    axis.ticks = element_line(colour = '#737373', linewidth = 0.4), 
    legend.position = 'bottom',
    legend.title = element_blank(), 
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 9), 
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    axis.title.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 9),
    axis.title.y = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.y = element_text(family = 'FiraGo Regular', colour = '#737373', size = 7), 
    strip.background = element_rect(fill = '#f7da6f', color = '#f7da6f'), 
    strip.text = element_text(family = 'FiraGo Medium', colour = '#737373', size = 9))

viz_4.3

# Create the base plot
viz_4.3_with_logo <- ggdraw(viz_4.3) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_4.3_with_logo

ggsave(plot = viz_4.3_with_logo, 'Viz/Viz_4.3.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)

# Remove unnecessary data
rm(logo, mob_dur, mob_dur_aggr, mob_dur_aggr_filt1, mob_dur_aggr_filt2, 
   viz_4.1, viz_4.1_with_logo, viz_4.2, viz_4.2_with_logo, viz_4.3, viz_4.3_with_logo)
