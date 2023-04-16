#Created by Keith Post on 9/22/22
#EDA

#### Load Packages and Source DFs====================================================================
pacman::p_load(here,scales,GGally)
source(here::here("code","01_DataSetup.R"))
source(here::here("code","02a_EDA_functions.R"))


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
n_loc_bar<-barplotter_tot(BMIcountTidyDF,location,count,col="darkred")

## Average counts
# Bar plot
barplotter_avg(BMIcountTidyDF,location,count) 

## Counts
# Boxplot
boxplotter(BMIcountTidyDF,location,count)


### local_site--------------------------------------------------------------------------------------
## Total counts
n_site_bar<-barplotter_tot(BMIcountTidyDF,local_site,count,col="steelblue")

## Average counts
# Bar plot
barplotter_avg(BMIcountTidyDF,local_site,count) 

## Counts
# Boxplot
boxplotter(BMIcountTidyDF,local_site,count)



### Order-------------------------------------------------------------------------------------------
## Total counts
n_ord_bar<-barplotter_tot(BMIcountTidyDF,order,count,angled=TRUE,col="darkgreen")

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
n_loc_ord_bar_stack<-barplotter_avg2(BMIcountTidyDF,order,location,count,angled=TRUE)

# Grouped
barplotter_avg2(BMIcountTidyDF,order,location,count,pos="dodge",angled=TRUE)


### Order-local_site--------------------------------------------------------------------------------
# Stacked
barplotter_avg2(BMIcountTidyDF,order,local_site,count,angled=TRUE)

# Grouped
barplotter_avg2(BMIcountTidyDF,order,local_site,count,pos="dodge",angled=TRUE)



#### Environmental data-----------------------------------------------------------------------------
### Faceted barplot of quantitative variables--faceted by variable
## X-axis by location
mean_env_loc_bar_facet<-bar_faceter(BMIenvTidyDF,location,variable,value,angled=TRUE)

## By local_site
mean_env_loc_bar_facet<-bar_faceter(BMIenvTidyDF,local_site,variable,value,angled=TRUE) 

## By shore
bar_faceter(BMIenvTidyDF,shore,variable,value,angled=TRUE) 



### Shore
## By location
# Total count
BMIenvWideDF %>%
  ggplot() +
  geom_bar(aes(x=location,fill=shore),color="black") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5))

# Proportion
BMIenvWideDF %>%
  ggplot() +
  geom_bar(aes(x=location,y=..count../sum(..count..),fill=shore),color="black") +
  scale_fill_viridis_d() +
  labs(y="proportion") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5))



### Scatterplots
## No groups
# Subset (without smoother)
BMIenvWideDF %>% 
  select(elevation:sat) %>%
  ggpairs() +
  theme_bw() 

# Subset (with smoother)
BMIenvWideDF %>% 
  select(elevation:sat) %>%
  ggpairs(lower=list(continuous=wrap(smoother))) +
  theme_bw() 


# Full dataset (without smoother)
BMIenvWideDF %>% 
  select(elevation:nitrate) %>%
  ggpairs() +
  theme_bw()

# Full dataset (with smoother)
env_scatter_smooth<-BMIenvWideDF %>% 
  select(elevation:nitrate) %>%
  ggpairs(lower=list(continuous=wrap(smoother))) +
  theme_bw()


## By lotic/lentic
# Subset (without smoother)
BMIenvWideDF %>% 
  select(elevation:sat,lotic) %>%
  ggpairs(columns=1:3,aes(color=as.factor(lotic))) +
  theme_bw()

# Subset (with smoother)
BMIenvWideDF %>% 
  select(elevation:sat,lotic) %>%
  ggpairs(columns=1:3,aes(color=as.factor(lotic)),lower=list(continuous=wrap(smoother))) +
  theme_bw()


# Full dataset (without smoother)
BMIenvWideDF %>% 
  select(elevation:nitrate,lotic) %>%
  ggpairs(columns=1:6,aes(color=as.factor(lotic))) +
  theme_bw()

# Full dataset (with smoother)
env_byLotic_scatter_smooth<-BMIenvWideDF %>% 
  select(elevation:nitrate,lotic) %>%
  ggpairs(columns=1:6,aes(color=as.factor(lotic)),lower=list(continuous=wrap(smoother))) +
  theme_bw()


## By fish presence 
# Subset (without smoother)
BMIenvWideDF %>% 
  select(elevation:sat,fish_presence) %>%
  ggpairs(columns=1:3,aes(color=as.factor(fish_presence))) +
  theme_bw()


# Subset (with smoother)
BMIenvWideDF %>% 
  select(elevation:sat,fish_presence) %>%
  ggpairs(columns=1:3,aes(color=as.factor(fish_presence)),lower=list(continuous=wrap(smoother))) +
  theme_bw()

# Full dataset (without smoother)
BMIenvWideDF %>% 
  select(elevation:nitrate,fish_presence) %>%
  ggpairs(columns=1:6,aes(color=as.factor(fish_presence))) +
  theme_bw()

# Full dataset (with smoother)
BMIenvWideDF %>% 
  select(elevation:nitrate,fish_presence) %>%
  ggpairs(columns=1:6,aes(color=as.factor(fish_presence)),lower=list(continuous=wrap(smoother))) +
  theme_bw()


### Heat maps (full dataset)
cor_env_heatmap<-BMIenvWideDF %>% 
  select(elevation:nitrate) %>%
  ggcorr() +
  # ggcorr(low = "#F21A00",
  #        mid = "#EEEEEE",
  #        high = "#3B9AB2") +
  #scale_fill_distiller(palette ="RdBu", direction = 1) +
  theme_bw()



#### Environmental and Taxonomic Data---------------------------------------------------------------
### Data wrangling
## 
BMIenvcountWideDF %>%
  rowwise() %>%
  mutate(count=sum(c_across(cols=starts_with("Amph"):last_col())),
         .keep="unused") -> BMIenvtotWideDF

### Mean counts by categorical env vars
## fish_presence
barplotter_avg(BMIenvtotWideDF,fish_presence,count,col="darkred")

## lotic
barplotter_avg(BMIenvtotWideDF,lotic,count)

## shore
barplotter_avg(BMIenvtotWideDF,shore,count,col="darkgreen")


### BMI Counts by Continuous Variable
BMIenvtotWideDF %>%
  













                