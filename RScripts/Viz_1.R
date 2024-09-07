# Set the working directory
setwd('/path/Erasmus +')

# Load LIBRARIES
library(tidyverse)
library(utf8)
library(ggbump) # creates elegant bump charts in ggplot
library(cowplot) # to place the logo outside the panel area.
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


# Viz_1. Number of scholarships & total participants | Bump chart

# Calculate number of grants by years
part_numb1 <- erasmus_ge %>% 
  group_by(year) %>% 
  summarise(number = n()) %>% 
  mutate(type = 'დაფინანსებული სტიპენდიის რაოდენობა')

# Calculate number of total participants by years
part_numb2 <- erasmus_ge %>% 
  group_by(year) %>% 
  summarise(number = sum(`Actual Participants`)) %>% 
  mutate(type = 'მონაწილეთა რაოდენობა')

# Bind both datasets ('part_numb1' & 'part_numb2')
part_numb <- rbind(part_numb1, part_numb2)


# Plot bump chart
viz_1 <- 
  ggplot(part_numb, aes(as.numeric(year), number, color = type)) +
  geom_bump(size=0.8) +
  geom_point(size=6) +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) + 
  scale_y_continuous(breaks = c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000), 
                     labels = c('500', '1,000', '1,500', '2,000', '2,500', '3,000', '3,500', '4,000')) + 
  scale_color_manual(values = c('#f7ca04', '#62789a')) + 
  labs(
    title = 'Erasmus +', 
    subtitle = 'როგორ იცვლებოდა ერაზმუს + პროგრამის დაფინანსება საქართველოში, 2014-2022', 
    caption = 'მონაცემები: Erasmus + stats', 
    x='',
    y=''
  ) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(colour = '#bdbdbd', linewidth = 0.2, linetype = 'dotted'), 
    panel.grid.major.y = element_line(colour = '#bdbdbd', linewidth = 0.2, linetype = 'dotted'), 
    #panel.grid.minor = element_line(colour = 'grey', linewidth = 0.1, linetype = 'dotted'), 
    axis.ticks = element_line(colour = '#737373', linewidth = 0.4), 
    legend.position = 'bottom',
    legend.title = element_blank(), 
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 9), 
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.spacing.x = unit(1.5, 'cm'), 
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    axis.text.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.y = element_text(family = 'FiraGo Regular', colour = '#737373', size = 8),
    
    plot.margin = margin(15, 15, 15, 15)  # increase plot margins
    )

viz_1

# Create the base plot
viz_1_with_logo <- ggdraw(viz_1) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_1_with_logo

# Save plot
ggsave(plot = viz_1_with_logo, 'Viz/Viz_1.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)

# Remove unnecessary data
rm(logo, logo_grob, part_numb, part_numb1, part_numb2, viz_1, viz_1_with_logo)
