#Created by Keith Post on 9/22/22
#EDA

#### Load Packages==================================================================================
pacman::p_load(here,tidyverse,skimr)


#### Data Import====================================================================================
### Set path
BMIenvDF_path<-here("data","tidy_data","alpine_bmi_2022-09-22.rds")

### Read in data
BMIenvDF<-readRDS(BMIenvDF_path)



#### Data Exploration===============================================================================
### Basic informaiton
dim(BMIenvDF)
summary(BMIenvDF)
glimpse(BMIenvDF)
skim(BMIenvDF)

BMIenvDF %>%
  filter(is.na(Order)|is.na(Family))
