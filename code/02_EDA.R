#Created by Keith Post on 9/22/22
#EDA

#### Load Packages and Source DFs====================================================================
pacman::p_load(here,tidyverse,scales,GGally)
source(here("code","01_DataSetup.R"))
source(here("code","02a_EDA.R"))


#### Data Import====================================================================================
### Set path
# BMI_tidy_filename<-dir(path=here("data","tidy_data"),pattern=paste0("alpine_bmi_env_n_",".+",".rds")) %>%
#   sort() %>%
#   tail(n=1)
# BMIenvDF_path<-here("data","tidy_data",BMI_tidy_filename)

### Read in data
# BMIenvDF<-readRDS(BMIenvDF_path)


##### Exploratory Data Analysis=====================================================================
#### Taxonomic data---------------------------------------------------------------------------------
### Location----------------------------------------------------------------------------------------
## Total counts
barplotter_tot(BMIcountTidyDF,location,count)

## Average counts
# Bar plot
barplotter_avg(BMIcountTidyDF,location,count) 

## Counts
# Boxplot
boxplotter(BMIcountTidyDF,location,count)


### local_site--------------------------------------------------------------------------------------
## Total counts
barplotter_tot(BMIcountTidyDF,local_site,count)

## Average counts
# Bar plot
barplotter_avg(BMIcountTidyDF,local_site,count) 

## Counts
# Boxplot
boxplotter(BMIcountTidyDF,local_site,count)



### Order-------------------------------------------------------------------------------------------
## Total counts
barplotter_tot(BMIcountTidyDF,order,count,angled=TRUE)

## Average counts
barplotter_avg(BMIcountTidyDF,order,count,angled=TRUE)


### Family
## Total counts
barplotter_tot(BMIcountTidyDF,family,count,angled=TRUE)


### Average counts
barplotter_avg(BMIcountTidyDF,family,count,angled=TRUE)


### Order-Location----------------------------------------------------------------------------------
## Average counts
# Stacked
barplotter_avg2(BMIcountTidyDF,order,location,count,angled=TRUE)

# Grouped
barplotter_avg2(BMIcountTidyDF,order,location,count,pos="dodge",angled=TRUE)


### Order-local_site--------------------------------------------------------------------------------
# Stacked
barplotter_avg2(BMIcountTidyDF,order,local_site,count,angled=TRUE)

# Grouped
barplotter_avg2(BMIcountTidyDF,order,local_site,count,pos="dodge",angled=TRUE)



#### Environmental data-----------------------------------------------------------------------------
### Faceted barplot of quantitative variables
## By location
bar_faceter(BMIenvTidyDF,location,variable,value,angled=TRUE) 

## By local_site
bar_faceter(BMIenvTidyDF,local_site,variable,value,angled=TRUE) 


### Shore
## By location
# Total count
BMIenvTidyDF %>%
  group_by(location,shore) %>%
  summarize(total_count = n()) %>% 
  mutate(location = fct_reorder(location,total_count,.fun=sum,.desc=TRUE)) %>%
  ggplot(aes(x=location,y=total_count,fill=shore)) +
  geom_col(color="black") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5))

# Proportion
BMIenvWideDF %>%
  group_by(location,shore) %>%
  summarize(num = n())
            prop = n()) #%>% 
  mutate(location = fct_reorder(location,total_count,.fun=sum,.desc=TRUE)) %>%
  ggplot(aes(x=location,y=total_count,fill=shore)) +
  geom_col(color="black") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5))


## By local_site
bar_faceter(BMIenvWideDF,location,variable,value,angled=TRUE) 

BMIenvTidyDF %>%
  group_by(local_site,shore) %>%
  summarize(mean_value = mean(value)) %>% 
  ungroup() %>%
  mutate(location = fct_reorder(local_site,mean_value,.fun=sum,.desc=TRUE)) %>%
  ggplot(aes(x=local_site,y=mean_value,fill=shore)) +
  geom_col(color="black") +
  scale_fill_viridis_d() +
  #geom_errorbar(aes(ymin=lower,ymax=upper),width=0.4) +
  #scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5),
        legend.) +
  facet_wrap(~shore,scales="free")


###  




## Need to commit
  #more updates to plots
#created faceted barplot function

## Need to do
  #pairwise plots for all quantitative (continuous) env vars
  #look at assumptions of PCA and see if transforms needed--distribution (symmetrical), multicollinearity

                