#Created by Keith Post on 9/22/22
#EDA

#### Load Packages==================================================================================
pacman::p_load(here,tidyverse,skimr,naniar)


#### Data Import====================================================================================
### Set path
BMI_tidy_filename<-dir(path=here("data","tidy_data"),pattern=paste0("alpine_bmi_env_n_",".+",".rds")) %>%
  sort() %>%
  tail(n=1)
BMIenvDF_path<-here("data","tidy_data",BMI_tidy_filename)

### Read in data
BMIenvDF<-readRDS(BMIenvDF_path)



#### Data Checking==================================================================================
### Basic information
dim(BMIenvDF)
summary(BMIenvDF)
glimpse(BMIenvDF)
skim(BMIenvDF)

### Missing information
miss_case_summary(BMIenvDF) %>% distinct(n_miss,pct_miss) #none
miss_var_table(BMIenvDF) #none
miss_case_table(BMIenvDF) #none


##### Exploratory Data Analysis=====================================================================
#### Taxonomic data
### Location-counts
BMIenvDF %>%
  group_by(location) %>%
  summarize(count=sum(count)) %>%
  mutate(location=fct_reorder(location,count,.fun=sum,.desc=TRUE)) %>%
  ggplot(aes(x=location,y=count)) +
  geom_col(color="black",fill="steelblue") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() 

## Order-counts
BMIenvDF %>%
  group_by(order) %>%
  summarize(count=sum(count)) %>%
  mutate(order=fct_reorder(order,count,.fun=sum,.desc=TRUE)) %>%
  ggplot(aes(x=order,y=count)) +
  geom_col(color="black",fill="steelblue") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() +
  theme(axis.text.x=element_text(vjust=grid::unit(c(rep(c(-2,0),3),-2),"points")))

### Location-Order
BMIenvDF %>%
  group_by(order) %>%
  summarize(count=sum(count),
            location=location) %>%
  distinct() %>%
  mutate(order=fct_reorder(order,count,.fun=sum,.desc=TRUE)) %>%
  ggplot(aes(x=order,y=count)) +
  geom_col(color="black",aes(fill=location)) +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() 


## Family-counts
BMIenvDF %>%
  group_by(family) %>%
  summarize(count=sum(count)) %>%
  mutate(order=fct_reorder(family,count,.fun=sum,.desc=TRUE)) %>%
  ggplot(aes(x=order,y=count)) +
  geom_col(color="black",fill="steelblue") +
  scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=75,vjust=0.5))


###
  




                