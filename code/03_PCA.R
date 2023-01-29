#Created by Keith Post on 10/1/22
#PCA on environmental variables


#### Source DFs (and load packages)=================================================================
pacman::p_load(here,rstatix,MASS,GGally,vegan,ggfortify,ggbiplot)

select<-dplyr::select
filter<-dplyr::filter

source(here::here("code","01_DataSetup.R"))
source(here::here("code","02a_EDA_functions.R"))
source(here::here("code","03a_PCA_functions.R"))
source(here::here("code","Skalski_ANoD_function.R"))





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
## Transform data (5/6 quantitative var)
# Wide format
BMIenvWideDF %>%
  mutate(across(c(elevation,temp,DO:nitrate),~boxcoxer(.x),.names="{.col}_trans")) %>% 
  select(-c(elevation,temp,DO:nitrate)) -> BMIenvWideDF_trans

# Tidy format
BMIenvWideDF_trans %>% 
  #make lotic and fish presence numeric for pivot
  mutate(across(fish_presence:lotic,as.numeric)) %>%
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
  mutate(across(ph:nitrate,
                list(boxcox=boxcoxer,log=log10,sqrt=sqrt,inverse=inverse))) -> BMIenvWideDF_trans2

# Tidy format
BMIenvWideDF_trans2 %>% 
  #make fish_presence and lotic numeric for pivot
  mutate(across(fish_presence:lotic,as.numeric)) %>%
  pivot_longer(cols=!c(local_site:date,shore), 
               names_to="variable",values_to="value") -> BMIenvTidyDF_trans2



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
#nitrate's apparent non-linear relationships with other variables and its clear bimodal distribution
  #will be kept in mind and could lead to a re-analysis of the data




##### PCA===========================================================================================
#### Run PCA----------------------------------------------------------------------------------------
### prcomp
BMIenvWideDF_trans %>%
  #select sites + env vars
  select(project_site,sat,elevation_trans:last_col()) %>%
  #formula input using scaled and centered variables
  prcomp(~sat + elevation_trans + temp_trans + DO_trans + ph_trans + nitrate_trans,
         data=.,scale.=TRUE,center=TRUE) -> envPCA1_pr


### princomp
BMIenvWideDF_trans %>%
  #select sites + env vars
  select(project_site,sat,elevation_trans:last_col()) %>%
  #formula input using correlation matrix
  princomp(~sat + elevation_trans + temp_trans + DO_trans + ph_trans + nitrate_trans,
           data=.,cor=TRUE,scores=TRUE,fix_sign=TRUE) -> envPCA1_prin




#### Display output (statistics)--------------------------------------------------------------------
### sds
envPCA1_pr
envPCA1_prin

### sds, prop of var, and cumulative proportion
summary(envPCA1_pr)
summary(envPCA1_prin)
#PC1 = ~ 49% of variance; PC2 = ~ 25% of variance

### isolate sds
envPCA1_pr$sdev
envPCA1_prin$sdev


### EVs
envPCA1_pr$sdev^2
envPCA1_prin$sdev^2

### loadings
envPCA1_pr$rotation
envPCA1_prin$loadings
#PC1: negative: sat & elevation; positive: temp & DO
#PC2: negative: nitrate, pH, & temp

### scores
envPCA1_pr$x
envPCA1_prin$scores


#### Choosing PCs
### Scree plot
screeplot(envPCA1_pr,bstick=TRUE,type="barplot",
          main="PCA of alpine lakes environmental variables")
abline(h=mean(envPCA1_pr$sdev^2))
#1) EVs drop the most from PC1 to PC2
#2) PC1-PC3 above mean EV
#3) PC1-PC3 above broken stick distribution



#### Visualize biplot-------------------------------------------------------------------------------
### Base R
biplot(envPCA1_pr)

## ggbiplot
ggbiplot(envPCA1_pr) +
  theme_bw()

## ggplot with ggfortify
autoplot(envPCA1_pr,loadings=TRUE,loadings.label=TRUE,shape=FALSE,
         loadings.label.repel=TRUE) +
  expand_limits(x=c(-0.5,0.5)) +
  theme_bw()




#### Run PCA (after removing nitrate)---------------------------------------------------------------
### prcomp
BMIenvWideDF_trans %>%
  #select sites + env vars
  select(project_site,sat,elevation_trans:ph_trans) %>%
  #formula input using scaled and centered variables
  prcomp(~sat + elevation_trans + temp_trans + DO_trans + ph_trans,
         data=.,scale.=TRUE,center=TRUE) -> envPCA2_pr


#### Statistics
### sds
envPCA1_pr[1]
envPCA2_pr[1]
#SDs have decreased, particularly starting from PC2


### sds, prop of var, and cumulative proportion
summary(envPCA1_pr)
summary(envPCA2_pr)
#PC1 = ~ 59% of variance; PC2 = ~ 22% of variance
#now first two PCs explain ~81% of variance (compared to ~74%)

### EVs
envPCA1_pr$sdev^2
envPCA2_pr$sdev^2

### loadings
envPCA1_pr$rotation
envPCA2_pr$rotation
#PC1: negative: sat & elevation; positive: temp & DO
#PC2: positive: pH

### scores
envPCA1_pr$x
envPCA2_pr$x


#### Choosing PCs
screeplot(envPCA2_pr,bstick=TRUE,type="barplot",
          main="PCA of alpine lakes environmental variables")
abline(h=mean(envPCA2_pr$sdev^2))
#1) EVs drop the most from PC1 to PC2
#2) PC1-PC2 above mean EV
#3) PC1 only above broken stick distribution
 

#### Visualizations---------------------------------------------------------------------------------
### Biplot (base)
biplot(envPCA1_pr)
biplot(envPCA2_pr)

### ggbiplot
## Distance-preserving
ggbiplot(envPCA2_pr,scale=0,labels=1:22) +
  expand_limits(x=c(NA,2),
                y=c(NA,2)) +
  theme_bw() 



## With groups
#example with lotic as group and with ellipses drawn
ggbiplot(envPCA2_pr,scale=0,labels=1:22,groups=BMIenvWideDF_trans$lotic,ellipse=TRUE) +
  expand_limits(x=c(NA,2),
                y=c(NA,2)) +
  theme_bw()


# map 
bmi_cat_vars<-c("local_site","location", "fish_presence","lotic","shore")

#without ellipse
bmi_cat_vars %>%
  set_names() %>%
  map(function(g){
    ggbiplot(envPCA2_pr,scale=0) +
      ggtitle(paste("Environmental variables grouped by",g)) +
      expand_limits(x=c(NA,2),
                    y=c(NA,2.5)) +
      geom_point(aes(color=BMIenvWideDF_trans[[g]])) +
      labs(color=g) +
      scale_color_viridis_d(end=0.8) +
      theme_bw() 
  }) -> bmi_ggbiplot_list
#poor discrimination: location and shore
#better discrimination: local_site, fish_presence, and lotic

#with ellipses
bmi_cat_vars %>%
  set_names() %>%
  map(function(g){
    ggbiplot(envPCA2_pr,scale=0,groups=BMIenvWideDF_trans[[g]],ellipse=TRUE) +
      ggtitle(paste("Environmental variables grouped by",g)) +
      expand_limits(x=c(NA,2),
                    y=c(NA,2.5)) +
      geom_point(aes(color=BMIenvWideDF_trans[[g]])) +
      labs(color=g) +
      scale_color_viridis_d(end=0.8) +
      theme_bw() 
  }) -> bmi_ggbiplot_list_ell


## ggplot with ggfortify (and shore as a categorical variable)
autoplot(envPCA2_pr,loadings=TRUE,loadings.label=TRUE,shape=FALSE,
         loadings.label.repel=TRUE) +
  geom_point(aes(color=BMIenvWideDF_trans$shore),alpha=0.3) +
  labs(color="shore") +
  theme_bw()


## Variable-preserving
ggbiplot(envPCA2_pr,scale=1,labels=1:22) +
  expand_limits(x=c(NA,2),
                y=c(NA,2)) +
  theme_bw() 



#### Run ANOVAs (distance-preserving)---------------------------------------------------------------
### Wrangle data
BMIenvWideDF_trans %>%
  select(local_site,location,fish_presence,lotic) %>% 
  mutate(site=row_number() %>% as.character,.before="local_site") %>%
  left_join(
    envPCA2_pr$x %>%
      as.data.frame() %>%
      select(PC1,PC2) %>%
      rownames_to_column(var="site")
  ) %>%
  pivot_longer(cols=starts_with("PC"),names_to="PC",values_to="scores") -> envPCA2_pr_anovaDF
             
            
### Run ANOVAs
## local_site
# Omnibus test
envPCA2_pr_anovaDF %>%
  select(site,local_site,PC,scores) %>%
  pivot_wider(id_cols=c("site","local_site"),names_from="PC",values_from="scores")  %>%
  select(-site) %>%
  as.data.frame() %>%
  Skalski.adonis(PC.axes=c(2,3),Groups=1) #significantly different

# By axis
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  anova_test(scores ~ local_site,detailed=TRUE)
#significant differences among local_sites on each PC

#groups form clear clusters in PC space, which is affected by both PC1 and 2 (PC1 has greater 
  #affect from a visual standpoint, which is supported by the ANOVA).


## location
# Omnibus test
envPCA2_pr_anovaDF %>%
  select(site,location,PC,scores) %>%
  pivot_wider(id_cols=c("site","location"),names_from="PC",values_from="scores")  %>%
  select(-site) %>%
  as.data.frame() %>%
  Skalski.adonis(PC.axes=c(2,3),Groups=1) 
#NS


# By axis
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  anova_test(scores ~ location,detailed=TRUE)
#significant differences for PC2 only

#looking at the biplot, it's clear that PC2 does a 'better job' of discriminating the categories
  #than PC1. However, there is stil clear overlap on this axis for a subset of the groups
#further, the anova results indicate that a low within-group variances along PC2 may be driving
  #this relationship


## fish_presence
# Omnibus test
envPCA2_pr_anovaDF %>%
  select(site,fish_presence,PC,scores) %>%
  pivot_wider(id_cols=c("site","fish_presence"),names_from="PC",values_from="scores")  %>%
  select(-site) %>%
  as.data.frame() %>%
  Skalski.adonis(PC.axes=c(2,3),Groups=1) #significantly different

# By axis
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  t_test(scores ~ fish_presence,detailed=TRUE)
#significant differences between fish_presence groups for PC1 only

#interpretation from biplot:
#supported in that ellipses show clear separation between the two groups; however, PC1 is clearly
  #more discriminatory. PC1 is negatively associated with sat and elevation and positively with
  #temp & DO, which likely influence fish ecology than pH, which is positively associated with PC2




## lotic
# Omnibus test
envPCA2_pr_anovaDF %>%
  select(site,lotic,PC,scores) %>%
  pivot_wider(id_cols=c("site","lotic"),names_from="PC",values_from="scores")  %>%
  select(-site) %>%
  as.data.frame() %>%
  Skalski.adonis(PC.axes=c(2,3),Groups=1) #significant

# By axis
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  t_test(scores ~ lotic,detailed=FALSE)
#significant differences between lotic for PC 2 only

#interpretation: clear separation between lotic groups, which is mostly due to PC2 (although
  #a shift along PC2), which is supported by omnibus and PC-separated ANOVAs










 #----------------------------------------------------------------------------------------------


## LAST COMMIT
#ran an ANOVA and t-tests on scores from distance-preserving biplots according to the three most
  #discriminating factors/cat vars
#ran omnibus tests and began to interpret results using test results, biplots, and loadings




