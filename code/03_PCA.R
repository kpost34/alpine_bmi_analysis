#Created by Keith Post on 10/1/22
#PCA on environmental variables


#### Source DFs (and load packages)=================================================================
pacman::p_load(here,rstatix,MASS)
source(here("code","01_DataSetup.R"))
source(here("code","03a_PCA_functions.R"))

select<-dplyr::select


##### Test Assumptions of PCA========================================================================
#### Assumption 1: Distributions of all variables follow a normal distribution (or are at least 
  #symmetrical)

### Test assumption
### Histogram
BMIenvTidyDF %>% 
  filter(!variable %in% c("lotic","fish_presence")) %>%
  histogrammer(var=variable,meas=value,8)


### Q-q plots
BMIenvTidyDF %>% 
  filter(!variable %in% c("lotic","fish_presence")) %>%
  qqplotter(variable,value)

### Shapiro tests
BMIenvWideDF %>%
  shapiro_test(elevation,temp,sat,DO,ph,nitrate)
#significant (non-normal): DO, nitrate, ph, and temp
#normal: sat and elevation (barely)



### Transformations (using BoxCox)
## Transform data
# Wide format
BMIenvWideDF %>%
  mutate(across(c(elevation,temp,DO:nitrate),~boxcoxer(.x),.names="{.col}_trans")) %>%
  select(-c(elevation,temp,DO:nitrate)) -> BMIenvWideDF_trans

# Tidy format
BMIenvWideDF_trans %>% 
  pivot_longer(cols=!c(local_site:date,shore), names_to="variable",values_to="value") -> BMIenvTidyDF_trans


## Test assumptions
# Histograms
BMIenvTidyDF_trans %>%
  filter(!variable %in% c("fish_presence","lotic")) %>%
  histogrammer(variable,value,8)

# Density plot
BMIenvTidyDF_trans %>%
  filter(!variable %in% c("fish_presence","lotic")) %>%
  ggplot() +
  geom_density(aes(value)) +
  facet_wrap(~variable,scales="free") +
  theme_bw() 

##Q-q plots
BMIenvTidyDF_trans %>% 
  filter(!variable %in% c("fish_presence","lotic")) %>%
  qqplotter(variable,value)


# Shapiro tests
BMIenvWideDF_trans %>%
  shapiro_test(sat,elevation_trans,temp_trans,DO_trans,ph_trans,nitrate_trans)
#significant (non-normal): nitrate_trans, ph_trans



## More transformations
# Wide format
BMIenvWideDF %>%
  #log, sqrt, inverse transforms
  mutate(across(ph:nitrate,list(boxcox=boxcoxer,log=log10,sqrt=sqrt,inverse=inverse))) -> BMIenvWideDF_trans2

# Tidy format
BMIenvWideDF_trans2 %>% 
  pivot_longer(cols=!c(local_site:date,shore), names_to="variable",values_to="value") -> BMIenvTidyDF_trans2



## Test assumption
# Histograms
BMIenvTidyDF_trans2 %>% 
  filter(str_detect(variable,"ph_|nitrate_")) %>%
  histogrammer(variable,value,cols=2)


# Q-Q Plot
BMIenvTidyDF_trans2 %>% 
  filter(str_detect(variable,"ph_|nitrate_")) %>%
  qqplotter(variable,value,cols=2)

# Shapiro tests
BMIenvWideDF_trans2 %>%
  shapiro_test(nitrate_boxcox,nitrate_inverse,nitrate_log,nitrate_sqrt,
               ph_boxcox,ph_inverse,ph_log,ph_sqrt)
#no transformation, whether BoxCox, log, sqrt, inverse, etc. was able to make these apparently
#bimodal distributions normally distributed....so decided to use BoxCox transforms (or 
#untransformed)



#### Assumption 2: Any systematic relationships between variables are linear
### Non-transformed
BMIenvWideDF_trans %>% 
  select(sat,ends_with("_trans")) %>%
  ggpairs(lower=list(continuous=wrap(smoother))) +
  theme_bw()

### Transformed
BMIenvWideDF %>% 
  select(elevation:nitrate) %>%
  ggpairs(lower=list(continuous=wrap(smoother))) +
  theme_bw()
#aside from a few pairs (e.g., sat-nitrate, elevation-nitrate, and DO-nitrate), there tends to be
  #linear relationships among pairs of variables
#nitrates apparent non-linear relationships with other variables and its clear bimodal distribution
  #will be kept in mind and could lead to a re-analysis of the data




#----------------------------------------------------------------------------------------------

## DONE
#created boxcox and inverse transformation functions
#completed testing of assumption 1
#created and implemented new functions histogrammer nad qqplotter
#tested assumption 2



## LAST COMMIT
#constructed heat map all continuous variables and scatterplots of all continuous variables, either
#as is or using groups
#testing assumptions of PCA
#renamed EDA function script
#created new script for PCA functions

