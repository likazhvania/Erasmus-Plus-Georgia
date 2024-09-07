# Set the working directory
setwd('/path/Erasmus +')

# Load LIBRARIES
library(tidyverse)
library(utf8)
library(ggstream) # allows creating streamplots in ggplot2
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



# Viz_5. Types of mobility | Stream chart

# Identify the unique types of mobility activity
unique(erasmus_ge$`Activity (mob)`)

# Define a function to categorize products (activities)
categorize_product <- function(product) {
  case_when(
    product %in% c('European Voluntary Service - Partner Countries',
                   'European Voluntary Service - Programme Countries',
                   'Group Volunteering Activities',
                   'Individual Volunteering Activities') ~ 'Volunteering',
    
    product %in% c('Mobility of youth workers - Partner Countries',
                   'Mobility of youth workers - Programme Countries',
                   'Youth Exchanges - Partner Countries',
                   'Youth Exchanges - Programme Countries') ~ 'Youth Exchanges/Mobility',
    
    product %in% c('Student mobility for studies between Programme Countries',
                   'Student mobility for Studies To/From Partner Countries') ~ 'Student Study mobility',
    
    product %in% c('Student mobility for traineeships between Programme Countries',
                   'Student mobility for traineeships To/From Partner Countries') ~ 'Student Traineeship mobility',
    
    product %in% c('Advance Planning Visit – EVS',
                   'Advance Planning Visit – Youth Exchange',
                   'Advance Planning Visit',
                   'Structured Courses/Training Events') ~ 'Advance Planning Visit',
    
    product %in% c('Staff mobility for Teaching To/From Partner Countries',
                   'Staff mobility for teaching between Programme Countries',
                   'ErasmusPro - Mobility of VET learners (3 to 12 months)',
                   'Mobility of VET learners (2 weeks up to 3 months)') ~ 'Staff mobility/VET learners for teaching',
    
    product %in% c('Staff mobility for Training To/From Partner Countries',
                   'Staff mobility for training between Programme Countries',
                   'Staff training abroad',
                   'VET learners traineeships in companies abroad',
                   'VET learners traineeships in vocational institutes abroad') ~ 'Staff mobility/VET learners for training'
  )
}

# Create a new field 'type' based on the `Activity (mob)` field
erasmus_ge <- erasmus_ge %>%
  mutate(type = sapply(`Activity (mob)`, categorize_product))

# Check the unique types
unique(erasmus_ge$type)

# Calculate the numbers of participation by mobility activity (type) and year
mob_types <- erasmus_ge %>% 
  group_by(year, type) %>% 
  summarise(number = n())

# Check the unique types
unique(mob_types$type)

# Give factors Georgian labels
mob_types$type <- factor(mob_types$type, 
                         levels = c(
                           'Student Study mobility',
                           'Student Traineeship mobility',
                           'Staff mobility/VET learners for teaching',
                           'Staff mobility/VET learners for training',
                           'Youth Exchanges/Mobility',
                           'Advance Planning Visit',
                           'Volunteering'
                         ), 
                         labels = c(
                           'სტუდენტების სასწავლო მობილობა',
                           'სტუდენტების სტაჟირებითი მობილობა',
                           'დასაქმებულთა სასწავლო მობილობა',
                           'დასაქმებულთა სტაჟირებითი მობილობა',
                           'ახალგაზრდების გაცვლითი მობილობა',
                           'მაღალკვალიფიციური ვიზიტი',
                           'მოხალისეობრიობითი პროექტები'
                         ))


# Plot stream chart
viz_5 <- 
  ggplot(mob_types, aes(x = as.numeric(year), y = number, group = type)) +
  geom_stream(aes(fill = type, 
                  color = type), bw = .8, # Controls smoothness
              alpha = 0.8, 
              true_range = 'both', 
              extra_span = 0.05) +
  scale_x_continuous(breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) + 
  scale_fill_manual(breaks = c('სტუდენტების სასწავლო მობილობა',
                               'სტუდენტების სტაჟირებითი მობილობა',
                               'დასაქმებულთა სასწავლო მობილობა',
                               'დასაქმებულთა სტაჟირებითი მობილობა',
                               'ახალგაზრდების გაცვლითი მობილობა',
                               'მაღალკვალიფიციური ვიზიტი',
                               'მოხალისეობრიობითი პროექტები'
                               ), 
                    values = c(
                      '#f5ca06','#a0bfe9','#925928','#b9c2be',
                      '#627eba','#aa8649','#8cbcdc'
                    )) + 
  scale_color_manual(breaks = c('სტუდენტების სასწავლო მობილობა',
                                'სტუდენტების სტაჟირებითი მობილობა',
                                'დასაქმებულთა სასწავლო მობილობა',
                                'დასაქმებულთა სტაჟირებითი მობილობა',
                                'ახალგაზრდების გაცვლითი მობილობა',
                                'მაღალკვალიფიციური ვიზიტი',
                                'მოხალისეობრიობითი პროექტები'
                                ), 
                     values = c(
                       '#f5ca06','#a0bfe9','#925928','#b9c2be',
                       '#627eba','#aa8649','#8cbcdc'
                     )) + 
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროექტების სახეობები, 2014-2022', 
    caption = 'მონაცემები: Erasmus + stats', 
    x='',
    y=''
  ) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(colour = '#bdbdbd', linewidth = 0.5, linetype = 'dotted'), 
    panel.grid.major.y = element_blank(), 
    #panel.grid.minor = element_line(colour = 'grey', linewidth = 0.1, linetype = 'dotted'), 
    axis.ticks.x = element_line(colour = '#737373', linewidth = 0.4), 
    axis.ticks.y = element_blank(),
    legend.position = 'bottom',
    legend.title = element_blank(), 
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 8), 
    legend.key.size = unit(0.3, 'cm'),
    legend.key.spacing.x = unit(1.5, 'cm'), 
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    axis.text.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.y = element_blank())

viz_5

# Create the base plot
viz_5_with_logo <- ggdraw(viz_5) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_5_with_logo

# Save plot
ggsave(plot = viz_5_with_logo, 'Viz/Viz_5.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)

# Remove unnecessary data
rm(logo, categorize_product, 
   mob_dur, mob_types, viz_5, viz_5_with_logo)
