#Created by Keith Post on 9/22/22
#Import, clean, and join data 

#### Load Packages==================================================================================
pacman::p_load(here,tidyverse,naniar)


#### Data Import====================================================================================
### Set paths
BMIn_input_path<-here("data","raw_data","glv_macroinvert_abund18.pj.data.csv")
BMItrait_input_path<-here("data","raw_data","glv_macroinvert_trait18.pj.data.csv")
envVar_input_path<-here("data","raw_data","glv_macroinvert_sitequality18.pj.data.csv")

### Read in data
BMIn_inputDF<-read_csv(BMIn_input_path,guess_max=5000)
BMItrait_inputDF<-read_csv(BMItrait_input_path,guess_max=5000)
envVar_inputDF<-read_csv(envVar_input_path,guess_max=5000)

list_df<-mget(ls(pattern="DF$"))


#### Data Checking, Cleaning, and Joining===========================================================
### Checking
## Dimensions, types, summaries
map(list_df,dim)
glimpse(BMIn_inputDF)
glimpse(BMItrait_inputDF)
glimpse(envVar_inputDF)
#classification issues

## Missing data (from pull)
map(list_df,n_miss) #trait DF has many missing values; others have none
miss_case_summary(BMItraitDF) %>% distinct(n_miss,pct_miss) #either 0/1 or 1/5.26% missing
miss_var_table(BMItraitDF) #summary similar to above line
miss_case_table(BMItraitDF) #99.4% of cases are missing one field
n_miss(BMItraitDF %>% select(-notes)) #only notes field contains missing values

### Cleaning
## Abundance data
BMIn_inputDF %>%
  #reclassify location as factor
  mutate(location=as.factor(location)) %>%
  #remove LTER_site and genus (incomplete information)
  select(-c(LTER_site,Genus)) -> BMInDF

## Env data
envVar_inputDF %>%
  #reclassify cols
  mutate(across(c(location,shore),~as.factor(.x)),
    across(elevation:nitrate,~as.numeric(.x))) %>%
  #count # of NAs per row
  mutate(NA_tot=rowSums(is.na(.))) %>% 
  #keep samples without missing data
  filter(NA_tot==0) %>%
  #remove LTER_site & NA_tot
  select(-c(LTER_site,NA_tot)) -> envVarDF


### Join data
envVarDF %>%
  left_join(BMInDF,
            by=c("local_site","project_site","date","location")) %>% 
  #reorder variables
  relocate(lat:long,.after=last_col()) %>%
  relocate(c(location,shore),.after="nitrate") %>%
  distinct() -> fullBMIenvDF

#subset data (for use in analysis): select ~10-d period in late July-early August
  #reasons: 1) short timespan, 2) many sites sampled in this period, 3) nearly all sites sampled 
    #twice, and 4) many BMI found
fullBMIenvDF %>%
  filter(date>="2018-07-31" & date<="2018-08-09") -> BMIenvDF


#### Write data file================================================================================
saveRDS(BMIenvDF,here("data","tidy_data",paste0("alpine_bmi_",Sys.Date(),".rds")))
  

### Remove extraneous objects
rm(list=setdiff(ls(),"BMIenvDF"))

