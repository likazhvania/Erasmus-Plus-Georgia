# Set the working directory
setwd('/path/Erasmus +')

# Load LIBRARIES
library(tidyverse)
library(utf8)


# Read data (the downloaded data is presented by each year in separate files)
data_14 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2014.csv')
data_15 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2015.csv')
data_16 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2016.csv')
data_17 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2017.csv')
data_18 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2018.csv')
data_19 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2019.csv')
data_20 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2020.csv')
data_21 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2021.csv')
data_22 <- read_csv('Data/working/csv/Erasmus-KA1-Mobility-Data-2022.csv')

# Some column names from the data of 2014 and 2018 year are not matching to the columns of other years.
# The column names are replaced to match other datasets for binding.
colnames(data_14)[which(names(data_14) == 'Sending Organisation')] <- 'Sending Organization'
colnames(data_14)[which(names(data_14) == 'Receiving Organisation')] <- 'Receiving Organization'
colnames(data_14)[which(names(data_14) == 'Mobility Duration - calendar days')] <- 'Mobility Duration'

colnames(data_18)[which(names(data_18) == 'Mobility Start Month')] <- 'Mobility Start Year/Month'


# Filter out the records representing Georgia.
ge_data_14 <- data_14 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2014')
ge_data_15 <- data_15 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2015')
ge_data_16 <- data_16 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2016')
ge_data_17 <- data_17 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2017')
ge_data_18 <- data_18 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2018')
ge_data_19 <- data_19 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2019')
ge_data_20 <- data_20 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2020')
ge_data_21 <- data_21 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2021')
ge_data_22 <- data_22 %>% 
  filter(`Participant Country` == 'Georgia') %>% 
  mutate(year = '2022')


# Remove the original data which is not needed since the subset of data representing Georgia is created.
rm(data_14, data_15, data_16, data_17, data_18, data_19, data_20, data_21, data_22)


# Bind all the datasets into one
erasmus_ge <- rbind(ge_data_14, ge_data_15, ge_data_16, ge_data_17, ge_data_18, ge_data_19, ge_data_20,
                    ge_data_21, ge_data_22)


# Remove the subdatas (data by years) since the bind data is created.
rm(ge_data_14, ge_data_15, ge_data_16, ge_data_17, ge_data_18, ge_data_19, ge_data_20,
   ge_data_21, ge_data_22)


# Write a csv file (save)
#write_csv(erasmus_ge, 'Data/working/erasmus_ge.csv')
