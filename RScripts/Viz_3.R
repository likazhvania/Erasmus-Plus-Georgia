# Set the working directory
setwd('/path/Erasmus +')

# Load LIBRARIES
library(tidyverse)
library(utf8)
# To create graph chart
library(tidygraph)
library(igraph)
library(ggraph)
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



# Viz_3. Number of participants by sex | Graph chart

# Calculate number of participants by sex and year
part_sex <- erasmus_ge %>%
  group_by(year, `Participant Gender`) %>% 
  summarise(number = n())

# Change the column name, to give it more appropriate name
colnames(part_sex)[which(names(part_sex) == 'Participant Gender')] <- 'sex'
# Give factors Georgian labels
part_sex$sex <- factor(part_sex$sex, levels = c('Female', 'Male', 'Undefined'), 
                       labels = c('მდედრობითი', 'მამრობითი', 'არ არის მითითებული'))
# Convert year to character
part_sex$year <- as.character(part_sex$year)

# Add a new field combining year and sex
part_sex$year_sex <- paste(part_sex$year, part_sex$sex, sep = '-')
# Add a new field combining sex and number
part_sex$sex_number <- paste(part_sex$sex, part_sex$number, sep = '-')

# Add a root node 'Participation'
edges_root <- data.frame(from = 'Participation', to = unique(part_sex$year))
# Create edges from year to sex
edges_year_sex <- data.frame(from = part_sex$year, to = part_sex$year_sex)
# Create edges from sex to number
edges_sex_number <- data.frame(from = part_sex$year_sex, to = part_sex$number)
# Bind all the edges into one dataset
edges <- rbind(edges_root, edges_year_sex)

# Define a function to categorize edges
categorize_edge <- function(from, to) {
  if (from == 'Participation') {
    return('root_to_year')
  } else if (to %in% unique(part_sex$year_sex)) {
    return('year_to_sex')
  } else if (to %in% unique(part_sex$number)) {
    return('sex_to_number')
  } else {
    return('unknown')
  }
}

# Create a new field 'category' based on the 'from' field
edges <- edges %>%
  mutate(category = mapply(categorize_edge, from, to))

# Create a vector of unique node names
name <- unique(c(as.character(edges$from), as.character(edges$to)))

# Create a data frame for node attributes
vertices <- data.frame(
  name = name,
  type = ifelse(name %in% part_sex$year, 'year', ifelse(name %in% part_sex$year_sex, 'sex', ifelse(name %in% part_sex$number, 'number', 'root'))),
  stringsAsFactors = FALSE
)

# Create a data frame with the number attribute for sex groups
number_df <- part_sex %>%
  pivot_longer(cols = c(year_sex), names_to = 'key', values_to = 'name') %>%
  select(name, number) %>%
  distinct()

# Merge the number attribute with the vertices data frame
vertices <- left_join(vertices, number_df, by = 'name')

# Replace NA values in 'number' with 0 for nodes that are not sex groups
vertices$number[is.na(vertices$number)] <- 0

# Ensure that there are no duplicate vertex names
vertices <- vertices %>% distinct(name, .keep_all = TRUE)

# Sum up the numbers by years
total_numbers <- erasmus_ge %>% 
  dplyr::group_by(year) %>% 
  summarise(total = n()) %>% 
  mutate(name = year)

# Join the summed numbers with 'vertices' dataset
vertices <- vertices %>%
  left_join(total_numbers, by = 'name')
vertices <- vertices %>%
  mutate(number = ifelse(number == 0, total, number)) %>% 
  select(-c(year.y, total))
vertices <- vertices %>%
  rename(year = year.x)
# Assign value 0 to 'root' vertice, so the dot will be displayed on the visualisation.
vertices <- vertices %>% 
  mutate(number = ifelse(is.na(number), 0, number))


# Create the graph object with the new edge attribute
mygraph <- graph_from_data_frame(edges, vertices = vertices)

# Plot the graph with dendrogram layout
viz_3 <- 
ggraph(mygraph, layout = 'dendrogram') + 
  geom_edge_diagonal(edge_width = 0.2, color = '#bdbdbd') +
  
  
  geom_node_text(aes(label = name, filter = (type == 'year'), 
                     family = 'FiraGo Medium', 
                     color = '#bdbdbd', 
                     size=8), nudge_x = -1) +
  
  geom_node_text(aes(label = ifelse(type == 'sex', as.character(number), name), filter = (type == 'sex'), 
                     family = 'FiraGo Light', 
                     color = '#d9d9d9', 
                     size=5), nudge_y = -0.1) +
  
  geom_node_point(aes(size = number, color = name)) +
  scale_size_continuous(range = c(3, 15), guide = FALSE) +  # increase point sizes
  
  scale_color_manual(values = c(
    '2014'='#a9b3b5', 
    '2015'='#a9b3b5', 
    '2016'='#a9b3b5', 
    '2017'='#a9b3b5', 
    '2018'='#a9b3b5', 
    '2019'='#a9b3b5', 
    '2020'='#a9b3b5', 
    '2021'='#a9b3b5', 
    '2022'='#a9b3b5',
    
    '2014-მდედრობითი'='#f7ca04', 
    '2015-მდედრობითი'='#f7ca04', 
    '2016-მდედრობითი'='#f7ca04', 
    '2017-მდედრობითი'='#f7ca04', 
    '2018-მდედრობითი'='#f7ca04', 
    '2019-მდედრობითი'='#f7ca04', 
    '2020-მდედრობითი'='#f7ca04', 
    '2021-მდედრობითი'='#f7ca04', 
    '2022-მდედრობითი'='#f7ca04', 
    '2014-მამრობითი'='#62789a',
    '2015-მამრობითი'='#62789a',
    '2016-მამრობითი'='#62789a',
    '2017-მამრობითი'='#62789a',
    '2018-მამრობითი'='#62789a',
    '2019-მამრობითი'='#62789a',
    '2020-მამრობითი'='#62789a',
    '2021-მამრობითი'='#62789a',
    '2022-მამრობითი'='#62789a',
    '2015-არ არის მითითებული'='#94b8ed',
    '2016-არ არის მითითებული'='#94b8ed',
    '2017-არ არის მითითებული'='#94b8ed',
    '2018-არ არის მითითებული'='#94b8ed',
    '2019-არ არის მითითებული'='#94b8ed',
    '2021-არ არის მითითებული'='#94b8ed',
    '2022-არ არის მითითებული'='#94b8ed'), 
    breaks = c('2014-მდედრობითი', '2014-მამრობითი', '2018-არ არის მითითებული'), 
    labels = c(
      '2014-მდედრობითი' = 'მდედრობითი' ,   
      '2014-მამრობითი'= 'მამრობითი',
      '2018-არ არის მითითებული' = 'არ არის მითითებული'
      )
    )  + 
  
  # Customize legend
  guides(color = guide_legend(
    override.aes = list(size = 3),  # Adjust the size of the legend items if necessary
    title = 'Sex Category'
  )) + 
  
  #ylim(-.6, NA) + 
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამაში მონაწილეთა სქესი, 2014-2022', 
    caption = 'მონაცემები: Erasmus + stats', 
    x='',
    y=''
  ) + 
  theme(
    plot.background = element_rect(fill = 'white', color = 'white'),  # White background
    panel.background = element_rect(fill = 'white', color = 'white'),  # White panel
    panel.grid = element_blank(), 
    axis.ticks = element_blank(), 
    legend.position = 'bottom',
    legend.title = element_blank(), 
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 9), 
    legend.key.size = unit(2, 'cm'), #change legend key size
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8), 
    axis.text = element_blank(), 
    
    # Add margins to create space around the plot
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

viz_3

# Create the base plot
viz_3_with_logo <- ggdraw(viz_3) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_3_with_logo

ggsave(plot = viz_3_with_logo, 'Viz/Viz_3.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)

# Remove datasets & variables that are not needed anymore
rm(edges, edges_root, edges_sex_number, edges_year_sex, vertices, mygraph, 
   number_df, part_sex, name, categorize_edge, total_numbers, 
   logo, viz_3, viz_3_with_logo)
