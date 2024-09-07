# Set the working directory
setwd('/path/Erasmus +')

# Load LIBRARIES
library(tidyverse)
library(utf8)
library(packcircles) # to create circle packing layout
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



# Viz_7. Number of grants by sending cities of Georgia | Circle charts

# Filter out the rows with following: 'Sending country: Georgia'
sending_ge <- erasmus_ge %>% 
  filter(`Sending Country` == 'GE - Georgia')
# Remove leading/trailing spaces
sending_ge <- sending_ge %>%
  mutate(`Sending City` = str_trim(`Sending City`))

# Calculate the number of grants by sending cities
sending_city_ge <- sending_ge %>% 
  dplyr::group_by(`Sending City`) %>% 
  summarise(n = n())

# Clean the values in the field of 'Sending City'
mapping <- data.frame(
  incorrect = c('AKHALTSIKHE',  'Abastumani', 'Akhalakalaki', 'Akhalkalaki', 'Akhaltsikhe', 
                'Akhmeta', 'Aspindza townlet', 'BATUMI', 'Bakuriani', 'Batumi', 
                'Biougra', 'Borjomi',  'Budapest', 'COIMBRA', 'Chakvinji', 
                'Chokhatauri', 'Chokhatauri Municipality', 'Coimbra, Portugalska', 'Copenhagen', 'DUSHETI', 
                'Dedoplistskaro', 'Dedoplistskaro, Kakheti', 'EISENSTADT', 'GORI', 'Gardabani', 
                'Gergeti Subani', 'Gomi', 'Gori', 'Gori distric', 'Gori district', 
                'Gumbati', 'Gurjaani', 'Gurjaani Municipality', 'HELSINKI', 'Imereti', 
                'KUTAISI', 'KUTAISIS', 'Kakheti', 'Kakheti, Georgia', 'Kareli', 
                'Kaspi', 'Kazbegi', 'Kazbegi, Stepantsminda', 'Kharagauli', 'Khashuri', 
                'Khelvachauri', 'Khobi', 'Khoni', 'Kobuleti', 'Kopitnari', 
                'Kutaisi', 'Kutaissi', 'Kvareli', 'Kvemo Alvani', 
                'Lagodekhi', 'Liepāja', 'Marneuli', 'Martvili', 'Mestia', 
                'Mtshketa', 'Mtskheta', 'Mzcheta', 'New Castle', 'OZURGETI', 
                'Oni', 'Oslo', 'Ozurgeti', 'Pankisi', 'Poti', 
                'RUSTAVI', 'Ruistewi', 'Ruse', 'Rustavi', 'Rustawi', 
                'Sachkhere', 'Sagarejo', 'Saguramo', 'Samtredia', 'Sanavardo', 
                'Savaneti', 'Sighnaghi', 'Signagi', 'Stephantsminda', "T'bilisi", 
                'TA XBIEX', 'TBILII', 'TBILISI', 'TBilisi', 'TELAVI', 
                'TKIBULI', 'TSNORI', 'Tbilisi', 'Tbilisi, Georgia', 'Tbilissi', 
                'Tbillisi', 'Tbilsi', 'Tblilisi', 'Tblisi', 'Telavi', 
                'Tetritskaro', 'Tibilisi', 'Tiblisi', 'Tiflis', 'Tkibuli', 
                'Trabzon', 'Trabzone', 'Tsalka', 'Tschiatura', 'Tserovani, Mtskheta-Mtianeti, Georgia', 
                'Tsibilisi', 'Tsnori', 'Udabno', 'Ude', 'Vachnadziani', 
                'Vale, Akhaltsikhe', 'Ventimiglia', 'Village Dighomi, Tbilisi', 'Village Dunta', 'Village Gamarjveba', 
                'Warszawa', 'ZUGDIDI', 'Zemo Khvedureti', 'Zestafoni', 'Zestaponi', 
                'Zugdidi', 'Zugdidi, Samegrelo-Upper Svaneti, Georgia', 'imereti zestafoni village futi', 'kaspi', 'kutaisi', 
                'poti', 'tbilisi', 'tblisi'),
  correct = c('ახალციხე',  'აბასთუმანი', 'ახალქალაქი', 'ახალქალაქი', 'ახალციხე', 
              'ახმეტა', 'ასპინძა', 'ბათუმი', 'ბაკურიანი', 'ბათუმი', 
              'NA', 'ბორჯომი', 'NA', 'NA', 'ჭაქვინჯი', 
              'ჩოხატაური', 'ჩოხატაური', 'NA', 'NA', 'დუშეთი', 
              'დედოფლისწყარო', 'დედოფლისწყარო', 'NA', 'გორი', 'გარდაბანი', 
              'გერგეთისუბანი', 'გომი', 'გორი', 'გორი', 'გორი', 
              'გუმბათი', 'გურჯაანი', 'გურჯაანი', 'NA', 'იმერეთი', 
              'ქუთაისი', 'ქუთაისი', 'კახეთი', 'კახეთი', 'ქარელი', 
              'კასპი', 'ყაზბეგი', 'ყაზბეგი', 'ხარაგაული', 'ხაშური', 
              'ხელვაჩაური', 'ხობი', 'ხონი', 'ქობულეთი', 'კოპიტნარი', 
              
              'ქუთაისი', 'ქუთაისი', 'ყვარელი', 'ქვემო ალვანი', 
              
              'ლაგოდეხი', 'NA', 'მარნეული', 'მარტვილი', 'მესტია', 
              'მცხეთა', 'მცხეთა', 'მცხეთა', 'NA', 'ოზურგეთი', 
              'ონი', 'NA', 'ოზურგეთი', 'პანკისი', 'ფოთი', 
              'რუსთავი', 'რუსთავი', 'NA', 'რუსთავი', 'რუსთავი', 
              'საჩხერე', 'საგარეჯო', 'საგურამო', 'სამტრედია', 'სანავარდო', 
              'სავანეთი', 'სიღნაღი', 'სიღნაღი', 'სტეფანწმინდა', 'თბილისი', 
              'NA', 'თბილისი', 'თბილისი', 'თბილისი', 'თელავი', 
              'ტყიბული', 'წნორი', 'თბილისი', 'თბილისი', 'თბილისი', 
              'თბილისი', 'თბილისი', 'თბილისი', 'თბილისი', 'თელავი', 
              'თეთრიწყარო', 'თბილისი', 'თბილისი', 'თბილისი', 'ტყიბული', 
              'NA', 'NA', 'წალკა', 'ჭიათურა', 'წეროვანი', 
              'თბილისი', 'წნორი', 'უდაბნო', 'უდე', 'ვაჩნაძიანი', 
              'ვალე', 'NA', 'თბილისი', 'დუნთა', 'გამარჯვება', 
              'NA', 'ზუგდიდი', 'ზემო ხვედურეთი', 'ზესტაფონი', 'ზესტაფონი', 
              'ზუგდიდი', 'ზუგდიდი', 'ფუთი', 'კასპი', 'ქუთაისი', 
              'ფოთი', 'თბილისი', 'თბილისი'),
  stringsAsFactors = FALSE
)

# Join 'mapping' (corrected city names) to 'sending_city_ge' dataset
sending_city_ge <- sending_city_ge %>% 
  left_join(mapping, by = c(`Sending City` = 'incorrect')) %>% 
  mutate(city = correct) %>% 
  select(-correct) %>%
  filter(city!='NA')

# Sum the numbers by the same cities (after correction)
sending_city_ge <- sending_city_ge %>% 
  dplyr::group_by(city) %>% 
  summarise(number = sum(n))


# Preparation for circle chart

# Circle packing layout
packing <- circleProgressiveLayout(sending_city_ge$number, sizetype = 'area')

# Add coordinates to the data
sending_city_ge <- cbind(sending_city_ge, packing)
dat.gg <- circleLayoutVertices(packing, npoints = 50)

# Set a threshold for minimum size of the circles to show labels
min_size_for_label <- 10  # Adjust this threshold as needed
min_number_for_label <- 50  # Threshold based on the number of participants


# Plot circle chart
viz_7 <- 
  ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill = sending_city_ge$number[id]), color = 'white') +
  
  geom_text(data = sending_city_ge %>% 
              filter(radius > min_size_for_label | number > min_number_for_label), 
            aes(x, y, label = city), 
            size = 3, family = 'FiraGo Regular', color = 'white') +
  
  # Apply the gradient color palette based on the number
  scale_fill_gradientn(colors = c('#3e4865', '#6e88b5', '#7d99c4', '#62789a', '#86a3de', '#8dacd3', '#c2e0e4','#62789a',
                                  
                                  '#627eba', '#8cbcdc', '#a0bfe9', '#a0bfe9', '#b9c2be', '#c2e0e4', '#f5ca06', '#f7da6f'
                                  )) + 
  
  theme_void() +
  
  labs(
    title = 'Erasmus +', 
    subtitle = 'ერაზმუს + პროგრამაში სტიპენდიების რაოდენობა, მონაწილეთა საქართველოდან გამგზავნი ქალაქების მიხედვით, 2014-2022', 
    caption = 'მონაცემები: Erasmus + stats', 
    x='',
    y=''
  ) + 
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    legend.position = 'none',
    legend.title = element_blank(), 
    plot.background = element_rect(fill='white', color='white'),
    plot.title = element_text(family = 'FiraGo', face = 'bold', colour = '#525252', size = 14), 
    plot.subtitle = element_text(family = 'FiraGo Medium', colour = '#737373', size = 12), 
    legend.text = element_text(family = 'FiraGo Regular', colour = '#737373', size = 9), 
    legend.key.size = unit(1, 'cm'), #change legend key size
    plot.caption = element_text(family = 'FiraGo Light', colour = '#969696', size = 8),
    
    # Add margins to create space around the plot
    plot.margin = margin(5, 5, 5, 5)  # Increase these values to add more space
    )

viz_7

# Create the base plot
viz_7_with_logo <- ggdraw(viz_7) + 
  draw_grob(rasterGrob(logo), 
            x = 0.0, y = -0.02, width = 0.08, height = 0.08)  # Adjust x, y, width, and height as needed
viz_7_with_logo

# Save plot
ggsave(plot = viz_7_with_logo, 'Viz/Viz_7.png', units = 'cm', dpi = 300, height = 21.0 , width = 29.7, device = png)

# Remove unnecessary data
rm(logo, min_number_for_label, min_size_for_label, 
   dat.gg, mapping, packing, sending_city_ge, sending_ge, viz_7, viz_7_with_logo)
