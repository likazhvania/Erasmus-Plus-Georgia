# Set the working directory
setwd('/path/Erasmus +')

# Load LIBRARIES
library(tidyverse)
library(utf8)
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
#logo_grob <- rasterGrob(logo, x = 0.054, y = 0.05, width = unit(1.5, 'cm), height = unit(1.5, 'cm), just = c('right', 'top'))



# Viz_2. Number of participants by age | Point chart

# Calculate number of participants by age and year
part_age <- erasmus_ge %>% 
  mutate(age = as.numeric(`Participant Age`)) %>% 
  group_by(year, age) %>%
  summarise(number = n()) %>% 
  filter(age > 0 & age < 70)


# Plot point chart
viz_2 <- 
  ggplot(part_age, aes(year, age)) + 
  geom_point(aes(size=number, alpha=0.5), color='#f7ca04') +
  scale_y_continuous(breaks = c(12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 
                                31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 
                                51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70)) + 
  #scale_size_continuous(breaks = c(1, 5, 10, 50, 100, 150, 200, 250, 300, 350)) + 
  scale_size(range = c(1, 10)) + 
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამაში მონაწილეთა ასაკი, 2014-2022', 
    caption = 'მონაცემები: Erasmus + stats', 
    x='',
    y=''
  ) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(colour = '#bdbdbd', linewidth = 0.5, linetype = 'dotted'), 
    panel.grid.major.y = element_line(colour = '#bdbdbd', linewidth = 0.1, linetype = 'dotted'), 
    #panel.grid.minor = element_line(colour = 'grey', linewidth = 0.1, linetype = 'dotted'), 
    axis.ticks = element_line(colour = '#737373', linewidth = 0.4), 
    legend.position = 'none',
    legend.title = element_blank(), 
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 9), 
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    axis.text.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.y = element_text(family = 'FiraGo Regular', colour = '#737373', size = 7))

viz_2

# Create the base plot
viz_2_with_logo <- ggdraw(viz_2) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_2_with_logo

ggsave(plot = viz_2_with_logo, 'Viz/Viz_2.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)

# Remove unnecessary data
rm(logo, logo_grob, part_age, viz_2, viz_2_with_logo)
