#Created by Keith Post on 9/22/22
#EDA

#### Load Packages and Source DFs===================================================================
pacman::p_load(here,scales,GGally,cowplot,ggpubr)
source(here::here("code","01_DataSetup.R"))
source(here::here("code","02a_EDA_functions.R"))


#### Data Wrangling=================================================================================
### Taxonomic Data
BMIcountTidyDF %>%
  select(-c(order,family)) %>%
  group_by(project_site) %>%
  mutate(count=sum(count),) %>%
  distinct() -> BMItotTidyDF


##### Exploratory Data Analysis=====================================================================
#### Taxonomic data---------------------------------------------------------------------------------
### Location----------------------------------------------------------------------------------------
## Total counts
barplotter_tot(BMIcountTidyDF,location,count,col="darkred")

## Average counts
# Bar plot
barplotter_avg(BMIcountTidyDF,location,count) 

## Counts
# Boxplot
boxplotter(BMIcountTidyDF,location,count)


### local_site--------------------------------------------------------------------------------------
## Total counts
barplotter_tot(BMIcountTidyDF,local_site,count,col="steelblue")

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



### Combine into one plot---------------------------------------------------------------------------
## Mean counts by categorical env vars
a<-list("location","local_site")
b<-list("darkred","steelblue")

mean_n_cat_env_bar<-list(a,b) %>%
  pmap(function(a,b){
    barplotter_avg(dat=BMItotTidyDF,ind=!!sym(a),dep=count,col=b) 
}) %>%
  set_names(as.character(a)) %>%
  plot_grid(plotlist=.,nrow=2)


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
bar_faceter(BMIenvTidyDF,location,variable,value,angled=TRUE)

## By local_site
mean_env_locsite_bar_facet<-bar_faceter(BMIenvTidyDF,local_site,variable,value,angled=TRUE) 

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
## Sum all BMI and retain env vars
BMIenvcountWideDF %>%
  rowwise() %>%
  mutate(count=sum(c_across(cols=starts_with("Amph"):last_col())),
         .keep="unused") -> BMIenvtotWideDF


### Mean counts by categorical env vars
x<-list("fish_presence","lotic","shore")
y<-list("darkred","steelblue","darkgreen")

mean_n_cat_env_bar<-list(x,y) %>%
  pmap(function(x,y){
    barplotter_avg(dat=BMIenvtotWideDF,ind=!!sym(x),dep=count,col=y) 
}) %>%
  set_names(as.character(x)) %>%
  plot_grid(plotlist=.,nrow=3)



### BMI abundance vs continuous env var
n_env_scatter_smooth<-BMIenvtotWideDF %>%
  # mutate(across(fish_presence:lotic,as.integer)) %>%
  pivot_longer(elevation:nitrate,names_to="variable",values_to="value") %>%
  ggplot(aes(x=value,y=count,color=variable)) +
  geom_point(size=2.5,show.legend=FALSE) +
  geom_smooth(aes(x=value,y=count),method="lm") +
  stat_cor(label.y=375,label.sep="\n") +
  facet_wrap(~variable,scales="free_x") +
  expand_limits(y=c(0,400)) +
  scale_color_viridis_d(end=0.8) +
  theme_bw()





  













                