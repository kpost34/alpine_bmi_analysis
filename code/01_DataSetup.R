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

## Missing data (i.e., NAs, from pull)
map(list_df,n_miss) #trait DF has many missing values; others have none
miss_case_summary(BMItrait_inputDF) %>% distinct(n_miss,pct_miss) #either 0/1 or 1/5.26% missing
miss_var_table(BMItrait_inputDF) #summary similar to above line
miss_case_table(BMItrait_inputDF) #99.4% of cases are missing one field
n_miss(BMItrait_inputDF %>% select(-notes)) #only notes field contains missing values
#not a problem as notes field not expected to be complete for all cases


### Cleaning
## Abundance data
BMIn_inputDF %>%
  #reclassify location as factor
  mutate(location=as.factor(location)) %>%
  #remove LTER_site 
  select(-LTER_site) -> BMInDF

## Env data
envVar_inputDF %>%
  #reclassify cols
  mutate(across(c(location,shore),~as.factor(.x)),
    across(elevation:nitrate,~as.numeric(.x))) %>%
  #count # of NAs per row
  #mutate(NA_tot=rowSums(is.na(.))) %>% 
  #keep samples without missing data
  #filter(NA_tot==0) %>%
  #remove LTER_site & NA_tot
  select(-LTER_site) -> envVarDF


### Join data
envVarDF %>%
  left_join(BMInDF %>%
              select(-location),
            by=c("local_site","project_site","date")) %>% 
  #reorder variables
  relocate(lat:long,.after=last_col()) %>%
  relocate(fish_presence,.after="shore") -> fullBMIenvDF


### Check join
fullBMIenvDF %>%
  filter(is.na(count)) 
#ALB_creek_belowroad

envVarDF %>% filter(project_site=="ALB_creek_belowroad") #1 result
BMInDF %>% filter(project_site=="ALB_creek_belowroad") #no results

fullBMIenvDF %>%
  filter(project_site=="ALB_creek_aboveroad") %>% 
  duplicated() %>% sum() #6 duplicates
#it seems like ALB_creek_belowroad samples were misclassified as ALB_creek_aboveroad in the abundance DF


## Check trait DF
# Make subset DF
BMItrait_inputDF %>%
  filter(project_site %in% c("ALB_creek_aboveroad","ALB_creek_belowroad")) %>%
  group_by(local_site,location,project_site,date,order,family,genus) %>%
  summarize(count=n()) -> trait_ALB_creek_countDF

# Compare trait DF creek N data to abundance DF
trait_ALB_creek_countDF %>% 
  setdiff(BMInDF %>%
            filter(project_site=="ALB_creek_aboveroad") %>%
            rename_with(cols=Order:Genus,.fn=~tolower(.x)))
#7 rows that are all from ALB_creek_belowroad

# Join the subsets of both data frames
trait_ALB_creek_countDF %>%
  left_join(
    BMInDF %>%
      filter(project_site %in% c("ALB_creek_aboveroad","ALB_creek_belowroad")) %>%
      rename_with(cols=Order:Genus,.fn=~tolower(.x)),
    by=c("local_site","location","date","order","family","genus","count"),
    suffix=c(".trait",".abundance"))
#this shows that project sites categorized as ALB_creek_belowroad appear to be aboveroad...except for one site
#this site has 127 records of chironomids collected from the creek-belowroad


## Deeper comparison
# Make abundance DF from trait data
BMItrait_inputDF %>%
  group_by(local_site,location,project_site,date,order,family,genus) %>%
  rename_with(.cols=order:genus,.fn=~str_to_title(.x)) %>%
  summarize(count=n()) %>%
  ungroup() %>%
  #make location a factor to match classes with abundance data
  mutate(location=as.factor(location)) -> traitCountDF

# Compare to abundance DF
BMInDF %>%
  filter(count!=0) %>%
  #left join them together
  left_join(traitCountDF ,
            by=c("local_site","location", "project_site","date", "Order","Family","Genus")) %>%
  filter(is.na(count.y))
  #114/159 sites do not join...

# Compare col values
unique(BMInDF$local_site) %>% setequal(unique(traitCountDF$local_site)) #TRUE
unique(BMInDF$location) %>% setequal(unique(traitCountDF$location)) #TRUE
unique(BMInDF$project_site) %>% setequal(unique(traitCountDF$project_site)) #FALSE
unique(BMInDF$project_site) %>% setdiff(unique(traitCountDF$project_site)) #17 results
unique(traitCountDF$project_site) %>% setdiff(unique(BMInDF$project_site)) #18 results
#lowercase vs uppercase "l" in "Gl" (vs. "GL") in project_site

# Correct project_site case
traitCountDF %>%
  mutate(project_site=str_replace(project_site,"^Gl","GL")) -> traitCountReDF


#Re-join with uppercase project_site in trait DF
BMInDF %>%
  #remove rows where no BMI found (in abundance DF)
  filter(count!=0) %>%
  #convert location to character (so that same class)
  mutate(location=as.character(location)) %>% 
  #left join them together
  left_join(traitCountReDF,
            by=c("local_site","location", "project_site","date", "Order","Family","Genus")) %>%
  #calculate difference in counts
  mutate(diff=count.x-count.y) %>%
  #find where count is not 0 or where join did not occur
  filter(diff!=0|is.na(count.y)) #%>% View()
#the creek data can be explained others would require deeper investigation. Given the objectives, it's
#best to simply use the abundances from the trait data as they are from individual specimens


### New join using abundances from trait data
#more appropriate to do a full join--recall that trait data is based on specimen traits so if no
#specimens collected for a given location and date, then a 0 would not show up; conversely, if
#env data do not appear where there are specimen data, then there is some sort of issue
## Initial join
envVarDF %>%
  select(-location) %>%
  full_join(traitCountReDF,
            by=c("local_site","project_site","date")) -> tmpJoinDF

## Check on joined data
# First check
#View(tmpJoinDF)
#missing waterfall low 
#abundance data seem ok (no NAs)

# Second check
tmpJoinDF %>%
  group_by(local_site,project_site,location,date,Order,Family,Genus) %>%
  summarize(n=n()) %>% 
  filter(n>1)
#there's instances of duplicate data...it appears that waterfall high/low not distinguished in env
  #data--further review is able to differentiate them (note that -105.36942 is waterfall high)


## Correct misclassification and re-join
envVarDF %>%
  select(-location) %>%
  mutate(project_site=ifelse(lat==40.03264 & long==-105.3694,
                             "GL4_waterfall_low",
                             project_site)) %>%
  full_join(traitCountReDF,
            by=c("local_site","project_site","date")) %>%
  #rename cols
  rename_with(.cols=Order:Genus,.fn=~tolower(.x)) %>%
  #reorder variables
  relocate(lat:long,.after=last_col()) %>%
  relocate(fish_presence,.after="shore") -> fullBMIenvDF
  
  
## Subset data (by date range) and group by order-family
#subset data (for use in analysis): select ~10-d period in late July-early August
  #reasons: 1) short timespan, 2) many sites sampled in this period, 3) nearly all sites sampled 
    #once, and 4) many BMI found
fullBMIenvDF %>%
  filter(date>="2018-07-31" & date<="2018-08-09") %>% 
  #count # of NAs per row
  mutate(NA_tot=rowSums(is.na(.))) %>% 
  #keep samples without missing data
  filter(NA_tot==0) %>%
  #sum by order-family
  group_by(order,family) %>% 
  mutate(count=sum(count)) %>% 
  select(-c(genus,NA_tot)) %>%
  ungroup() %>%
  distinct() -> BMIenvDF

#### Write data file================================================================================
saveRDS(BMIenvDF,here("data","tidy_data",paste0("alpine_bmi_env_n_",Sys.Date(),".rds")))
  

### Remove extraneous objects
rm(list=setdiff(ls(),"BMIenvDF"))

