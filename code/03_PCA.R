#Created by Keith Post on 10/1/22
#PCA on environmental variables


#### Source DFs (and load packages)=================================================================
pacman::p_load(here,rstatix,MASS,GGally,vegan,ggfortify,ggbiplot)
source(here("code","01_DataSetup.R"))
source(here("code","02a_EDA_functions.R"))
source(here("code","03a_PCA_functions.R"))

select<-dplyr::select


##### Test Assumptions of PCA=======================================================================
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
## Transform data (5/6 quantitative vari)
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
### Transformed
BMIenvWideDF_trans %>% 
  select(sat,ends_with("_trans")) %>%
  ggpairs(lower=list(continuous=wrap(smoother))) +
  theme_bw()

### Non-transformed
BMIenvWideDF %>% 
  select(elevation:nitrate) %>%
  ggpairs(lower=list(continuous=wrap(smoother))) +
  theme_bw()
#aside from a few pairs (e.g., sat-nitrate, elevation-nitrate, and DO-nitrate), there tends to be
  #linear relationships among pairs of variables
#nitrates apparent non-linear relationships with other variables and its clear bimodal distribution
  #will be kept in mind and could lead to a re-analysis of the data




##### PCA===========================================================================================
### Run PCA
## prcomp
BMIenvWideDF_trans %>%
  #select sites + env vars
  select(project_site,sat,elevation_trans:last_col()) %>%
  #formula input using scaled and centered variables
  prcomp(~sat + elevation_trans + temp_trans + DO_trans + ph_trans + nitrate_trans,
         data=.,scale.=TRUE,center=TRUE) -> envPCA1


## princomp
BMIenvWideDF_trans %>%
  #select sites + env vars
  select(project_site,sat,elevation_trans:last_col()) %>%
  #formula input using correlation matrix
  princomp(~sat + elevation_trans + temp_trans + DO_trans + ph_trans + nitrate_trans,
           data=.,cor=TRUE,scores=TRUE,fix_sign=TRUE) -> envPCA2




 ### Display output (statistics)
## sds
envPCA1
envPCA2

## sds, prop of var, and cumulative proportion
summary(envPCA1)
summary(envPCA2)
#PC1 = ~ 49% of variance; PC2 = ~ 25% of variance

## isolate sds
envPCA1$sdev
envPCA2$sdev


## EVs
envPCA1$sdev^2
envPCA2$sdev^2

## loadings
envPCA1$rotation
envPCA2$loadings
#PC1: negative: sat & elevation; positive: temp & DO
#PC2: negative: nitrate, pH, & temp

## scores
envPCA1$x
envPCA2$scores


### Choosing PCs
## Scree plot
screeplot(envPCA1,bstick=TRUE,type="barplot",
          main="PCA of alpine lakes environmental variables")
abline(h=mean(envPCA1$sdev^2))
#1) EVs drop the most from PC1 to PC2
#2) PC1-PC3 above mean EV
#3) PC1-PC3 above broken stick distribution


### Visualize biplot
## Base R
biplot(envPCA1)

#ggbiplot
ggbiplot(envPCA1) +
  theme_bw()

## ggplot with ggfortify
autoplot(envPCA1,loadings=TRUE,loadings.label=TRUE,shape=FALSE,
         loadings.label.repel=TRUE) +
  expand_limits(x=c(-0.5,0.5)) +
  theme_bw()






### Run PCA (after removing nitrate)
## prcomp
BMIenvWideDF_trans %>%
  #select sites + env vars
  select(project_site,sat,elevation_trans:ph_trans) %>%
  #formula input using scaled and centered variables
  prcomp(~sat + elevation_trans + temp_trans + DO_trans + ph_trans,
         data=.,scale.=TRUE,center=TRUE) -> envPCA3

summary(envPCA3)

screeplot(envPCA3,bstick=TRUE,type="barplot",
          main="PCA of alpine lakes environmental variables")
abline(h=mean(envPCA3$sdev^2))

envPCA1$rotation
envPCA3$rotation

biplot(envPCA1)
biplot(envPCA3)


#----------------------------------------------------------------------------------------------

## DONE
#began PCA of env vars
#computed stats and made biplots of PCAs using prcomp and princomp and prcomp with and without
  #nitrate



## LAST COMMIT
#created boxcox and inverse transformation functions
#completed testing of assumption 1
#created and implemented new functions histogrammer nad qqplotter
#tested assumption 2

## TO DO
#consider doing some plots of diversity (family richness, index)


