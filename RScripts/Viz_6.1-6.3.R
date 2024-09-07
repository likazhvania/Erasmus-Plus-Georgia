# Set the working directory
setwd('/path/Erasmus +')

# Load LIBRARIES
library(tidyverse)
library(utf8)
library(ggbump) # creates elegant bump charts in ggplot
library(treemapify) # creates treemaps in ggplot2
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



# Viz_6.1-6.3. Number of scholarships by the receiving countries | Treemap charts & Bump chart

# Calculate the number of grants by receiving countries and year
mob_countries <- erasmus_ge %>% 
  group_by(year, `Sending Country`, `Receiving Country`) %>% 
  summarise(number = n())

# Replace the column names
colnames(mob_countries)[which(names(mob_countries) == 'Sending Country')] <- 'sending'
colnames(mob_countries)[which(names(mob_countries) == 'Receiving Country')] <- 'receiving'

# Define the 'extract_name' function
extract_name <- function(country_name) {
  strsplit(country_name, ' - ')[[1]][2]
}

# Create 'sending_code' and 'receiving_code' fields
mob_countries <- mob_countries %>%
  mutate(
    sending_country = sapply(sending, extract_name),
    receiving_country = sapply(receiving, extract_name)
  )

# Extract all unique country codes
unique_names <- unique(c(mob_countries$sending_country, mob_countries$receiving_country))
print(unique_names)

# Create a mapping of country names to their names in Georgian
country_georgian <- tibble::tibble(
  country = c('Armenia', 'Azerbaijan', 'Belgium',                        
              'Czech Republic', 'Germany', 'Denmark',                        
              'Estonia', 'Greece', 'Spain',                          
              'France', 'Georgia', 'Croatia',
              'Italy', 'Lithuania', 'Netherlands',                    
              'Poland', 'Romania', 'Sweden',                         
              'Turkey', 'United Kingdom', 'Austria',                        
              'Bulgaria', 'Cyprus', 'Hungary', 
              'Ireland', 'Latvia', 'Norway',     
              'Slovenia', 'Slovakia', 'Ukraine',
              'Finland', 'The Republic of North Macedonia', 'Serbia', 
              'Russian Federation', 'Albania', 'Belarus',         
              'Israel', 'Morocco', 'Portugal',   
              'United States', 'South Africa', 'Egypt', 
              'Moldova (Republic of)', 'Liechtenstein', 'Bosnia and Herzegovina', 
              'Malta', 'Iceland', 'Luxembourg',  
              'Montenegro', 'Palestine', 'Fiji',   
              'Canada', 'China (Peoples Republic of)', 'Tunisia',      
              'Indonesia', 'Korea (Republic of)', 'Lebanon'),  # Add all unique codes here
  georgian_name = c('სომხეთი', 'აზერბაიჯანი', 'ბელგია',
                    'ჩეხეთის რესპუბლიკა', 'გერმანია', 'დანია',
                    'ესტონეთი', 'საბერძნეთი', 'ესპანეთი',
                    'საფრანგეთი', 'საქართველო', 'ხორვატია',
                    'იტალია', 'ლიეტუვა', 'ნიდერლანდები',
                    'პოლონეთი', 'რუმინეთი', 'შვედეთი',
                    'თურქეთი', 'დიდი ბრიტანეთი', 'ავსტრია',
                    'ბულგარეთი', 'კვიპროსი', 'უნგრეთი',
                    'ირლანდია', 'ლატვია', 'ნორვეგია',
                    'სლოვენია', 'სლოვაკეთი', 'უკრაინა',
                    'ფინეთი', 'ჩრდ.მაკედონიის რესპუბლიკა', 'სერბეთი',
                    'რუსეთის ფედერაცია', 'ალბანეთი', 'ბელარუსი',
                    'ისრაელი', 'მოროკო', 'პორტუგალია',
                    'აშშ', 'სამხრ.აფრიკა', 'ეგვიპტე',
                    'მოლდოვა', 'ლიხტენშტეინი', 'ბოსნია-ჰერცოგოვინა',
                    'მალტა', 'ისლანდია', 'ლუქსემბურგი',
                    'მონტენეგრო', 'პალესტინა', 'ფიჯი',
                    'კანადა', 'ჩინეთი', 'ტუნისია',
                    'ინდონეზია', 'სამხრ.კორეა', 'ლიბანი')  # Corresponding Georgian names
)

# Join the mapping with the original data
mob_countries <- mob_countries %>%
  left_join(country_georgian, by = c('sending_country' = 'country')) %>%
  rename(sending_ka = georgian_name) %>%
  left_join(country_georgian, by = c('receiving_country' = 'country')) %>%
  rename(receiving_ka = georgian_name) %>%
  select(-sending_country, -receiving_country)

# Calculate number of grants by receiving countries
receiving_countries <- mob_countries %>% 
  group_by(year, receiving_ka) %>% 
  summarise(number = sum(number)) %>% 
  group_by(receiving_ka) %>% 
  summarise(number = sum(number)) %>%
  arrange(desc(number))


# Create and plot the treemap using ggplot2 | Viz_6.1
viz_6.1 <- 
  ggplot(receiving_countries, aes(area = number, fill = number, label = receiving_ka)) +
  geom_treemap(layout = 'squarified', 
               start = 'topleft', 
               color='white',
               size = 1.5) + 
  
  geom_treemap_text(start = 'topleft', family = 'FiraGo Regular', color = '#ffffff', place = 'centre', grow = FALSE, size = 9) + 
  scale_fill_gradientn(colors=c('#6c84b3', '#f7da6f', '#c2e0e4', '#a0bfe9', '#f5ca06', 
                                '#8dacd3', '#86a3de', '#7d99c4', '#6e88b5', '#62789a', '#3668a4', '#336ba2', '#51586e', '#3e4865')) + 
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამის ფარგლებში დაფინანსებული სტიპენდიანტების მიმღები ქვეყნები, 2014-2022', 
    caption = 'მონაცემები: Erasmus + stats', 
    fill = 'სტიპენდიის რაოდენობა'
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = 'none',
    legend.title = element_text(family = 'FiraGo Medium', colour = '#737373', size = 9),
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 8),
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    
    plot.margin = margin(11, 11, 11, 11)  # increase plot margins
  ) + 
  guides(size = 'none') # Remove the size legend

viz_6.1

# Create the base plot
viz_6.1_with_logo <- ggdraw(viz_6.1) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_6.1_with_logo

# Save plot
ggsave(plot = viz_6.1_with_logo, 'Viz/Viz_6.1.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)



# Calculate number of grants by receiving countries and years
receiving_count_years <- mob_countries %>% 
  group_by(year, receiving_ka) %>% 
  summarise(number = sum(number))

receiving_count_years %>% 
  filter(receiving_ka != 'საქართველო')
  
# Plot bump chart | Viz_6.2
viz_6.2 <- 
  ggplot(receiving_count_years, aes(year, number, group=receiving_ka)) + 
  geom_bump(size=0.5, color='#f7ca04', alpha=0.6) +
  geom_point(size=1.8, color='#566c98') + 
  
  scale_y_continuous(breaks = c(10, 50, 100, 150, 200, 250, 300, 350, 400, 450, 600)) + 
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამის ფარგლებში დაფინანსებული სტიპენდიანტების მიმღები ქვეყნები, 2014-2022', 
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
    legend.position = 'none',
    legend.title = element_blank(), 
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 9), 
    legend.key.size = unit(0, 'cm'), #change legend key size
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    axis.text.x = element_text(family = 'FiraGo Medium', colour = '#737373', size = 8),
    axis.text.y = element_text(family = 'FiraGo Regular', colour = '#737373', size = 8))

viz_6.2

# Create the base plot
viz_6.2_with_logo <- ggdraw(viz_6.2) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_6.2_with_logo

# Save plot
ggsave(plot = viz_6.2_with_logo, 'Viz/Viz_6.2.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)



# Calculate number of grants by sending countries and dis-select Georgia as sending country
sending_countries <- mob_countries %>% 
  group_by(year, sending_ka) %>% 
  summarise(number = sum(number)) %>% 
  group_by(sending_ka) %>% 
  summarise(number = sum(number)) %>%
  arrange(desc(number)) %>% 
  filter(sending_ka != 'საქართველო')

# Create and plot the treemap using ggplot2 | Viz_6.3
viz_6.3 <- 
  ggplot(sending_countries, aes(area = number, fill = number, label = sending_ka)) +
  geom_treemap(layout = 'squarified', 
               start = 'topleft', 
               color='white',
               size = 1.5) + 
  
  geom_treemap_text(start = 'topleft', family = 'FiraGo Regular', color = '#ffffff', place = 'centre', grow = FALSE, size = 9) + 
  scale_fill_gradientn(colors=c('#6c84b3', '#f7da6f', '#c2e0e4', '#a0bfe9', '#f5ca06', 
                                '#8dacd3', '#86a3de', '#7d99c4', '#6e88b5', '#62789a', '#3668a4', '#336ba2', '#51586e', '#3e4865')) + 
  
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამის ფარგლებში დაფინანსებული სტიპენდიანტების გამგზავნი ქვეყნები, 2014-2022 *', 
    caption = 'მონაცემები: Erasmus + stats
    იგულისხმება, სხვა ქვეყანაში სწავლის განმავლობაში იმავე ქვეყანაში მოპოვებული გაცვლითი პროგრამით მესამე ქვეყანაში მობილობა *', 
    fill = 'სტიპენდიის რაოდენობა'
  ) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = 'none',
    legend.title = element_text(family = 'FiraGo Medium', colour = '#737373', size = 9),
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 8),
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8)
  ) + 
  guides(size = 'none') # Remove the size legend

viz_6.3

# Create the base plot
viz_6.3_with_logo <- ggdraw(viz_6.3) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_6.3_with_logo

# Save plot
ggsave(plot = viz_6.3_with_logo, 'Viz/Viz_6.3.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)

# Remove unnecessary data
rm(logo, unique_names, extract_name, country_georgian, mob_countries, 
   receiving_count_years, receiving_countries, sending_countries, 
   viz_6.1, viz_6.1_with_logo, viz_6.2, viz_6.2_with_logo, viz_6.3, viz_6.3_with_logo)
