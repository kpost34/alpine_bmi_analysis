#Created by Keith Post on 10/1/22
#PCA on environmental variables


#### Source DFs (and load packages)=================================================================
pacman::p_load(here,rstatix,MASS,GGally,vegan,ggfortify,ggbiplot,moments)

source(here::here("code","01_DataSetup.R"))
source(here::here("code","02a_EDA_functions.R"))
source(here::here("code","03a_PCA_functions.R"))
source(here::here("code","Skalski_ANoD_function.R"))

select<-dplyr::select
filter<-dplyr::filter
mutate<-dplyr::mutate



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
  qqplotter(variable,value) -> BMIenv_qqplot

BMIenv_qqplot

### Shapiro tests
BMIenvWideDF %>%
  shapiro_test(elevation,temp,sat,DO,ph,nitrate) -> BMIenv_shapiro

BMIenv_shapiro
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
  qqplotter(variable,value) -> BMIenv_qqplot_trans

BMIenv_qqplot_trans


# Shapiro tests
BMIenvWideDF_trans %>%
  shapiro_test(sat,elevation_trans,temp_trans,DO_trans,ph_trans,nitrate_trans) -> BMIenv_shapiro_trans

BMIenv_shapiro_trans
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


## Test symmetry of Boxcox-transformed pH and nitrate
# Measure skewness
BMIenvWideDF_trans2 %>% 
  select(ph_trans="ph_boxcox",nitrate_trans="nitrate_boxcox") %>%
  skewness() -> ph_nitrate_boxcox_skewness

ph_nitrate_boxcox_skewness

# Statistically test it
c("ph_boxcox","nitrate_boxcox") %>%
  purrr::map_df(function(x){
    BMIenvWideDF_trans2 %>%
      pull(x) %>%
      jarque.test() %>%
      .[c("statistic","p.value")] %>%
      as_tibble() %>%
      mutate(variable=str_replace(x,"_boxcox$","_trans"),.before=statistic)
}) -> ph_nitrate_boxcox_jarque_test

ph_nitrate_boxcox_jarque_test
#neither is significant--have symmetrical distributions
  



#### Assumption 2: Any systematic relationships between variables are linear
### Non-transformed
BMIenvWideDF %>% 
  select(elevation:nitrate) %>%
  ggpairs(lower=list(continuous=wrap(smoother,method="loess"))) +
  theme_bw()

### Transformed
BMIenvWideDF_trans %>% 
  mutate(elevation_trans=elevation_trans/1000000) %>%
  select(sat,ends_with("_trans")) %>%
  ggpairs(lower=list(continuous=wrap(smoother))) +
  scale_x_continuous(n.breaks=4) +
  theme_bw(base_size=12) +
  labs(caption="Note: elevation_trans are in x 10^6") -> boxcox_scatter

boxcox_scatter
#aside from a few pairs (e.g., sat-nitrate, elevation-nitrate, and DO-nitrate), there tends to be
  #linear relationships among pairs of variables
#nitrate's apparent non-linear relationships with other variables and its clear bimodal distribution
  #will be kept in mind and could lead to a re-analysis of the data



#### Save transformed data frames
BMIenvWideDF_trans %>%
  left_join(BMIcountWideDF) -> BMIenvcountWideDF_trans
# saveRDS(BMIenvWideDF_trans,here::here("data","tidy_data",paste0("alpine_bmi_env_trans_",Sys.Date(),".rds")))
# saveRDS(BMIenvcountWideDF_trans,here::here("data","tidy_data",paste0("alpine_bmi_env_n_trans_",Sys.Date(),".rds")))

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
                y=c(NA,2.5)) +
  theme_bw(base_size=12) -> bmi_ggbiplot_nogroup



## With groups
#example with lotic as group and with ellipses drawn
ggbiplot(envPCA2_pr,scale=0,labels=1:22,groups=BMIenvWideDF_trans$lotic,ellipse=TRUE) +
  expand_limits(x=c(NA,2),
                y=c(NA,2)) +
  theme_bw(base_size=12)


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
      theme_bw(base_size=12) 
  }) -> bmi_ggbiplot_list
#poor discrimination: location and shore
#better discrimination: local_site, fish_presence, and lotic

#with ellipses
bmi_cat_vars %>%
  set_names() %>%
  map(function(g){
    ggbiplot(envPCA2_pr,scale=0,groups=BMIenvWideDF_trans[[g]],ellipse=TRUE) +
      ggtitle(paste("Grouped by",g)) +
      expand_limits(x=c(NA,2.5),
                    y=c(NA,3.5)) +
      geom_point(aes(color=BMIenvWideDF_trans[[g]])) +
      labs(color=g) +
      scale_color_viridis_d(end=0.8) +
      theme_bw(base_size=12) +
      theme(legend.position="bottom")
  }) -> bmi_ggbiplot_list_ell

#graph all in a single plot
bmi_ggbiplot_list_ell[c(3,4,1)] %>%
  plot_grid(plotlist=.,nrow=2,labels=c("A","B","C"),align="h") -> bmi_pca_biplot_groups


## ggplot with ggfortify (and shore as a categorical variable)
autoplot(envPCA2_pr,loadings=TRUE,loadings.label=TRUE,shape=FALSE,
         loadings.label.repel=TRUE) +
  geom_point(aes(color=BMIenvWideDF_trans$shore),alpha=0.3) +
  labs(color="shore") +
  theme_bw(base_size=12)


## Variable-preserving
ggbiplot(envPCA2_pr,scale=1,labels=1:22) +
  expand_limits(x=c(NA,2),
                y=c(NA,2)) +
  theme_bw(base_size=12) 



#### Run ANOVAs (distance-preserving)---------------------------------------------------------------
### Wrangle data
BMIenvWideDF_trans %>%
  select(local_site,location,fish_presence,lotic,shore) %>% 
  mutate(site=row_number() %>% as.character,.before="local_site") %>%
  left_join(
    envPCA2_pr$x %>%
      as.data.frame() %>%
      select(PC1,PC2) %>%
      rownames_to_column(var="site")
  ) %>%
  pivot_longer(cols=starts_with("PC"),names_to="PC",values_to="scores") -> envPCA2_pr_anovaDF
             
            
### Run ANOVAs
## Omnibus test
envPCA2_pr_anovaDF %>%
  select(local_site:shore) %>%
  names() %>%
  map_df(run_omnibus_anova,data=envPCA2_pr_anovaDF) %>%
  arrange(null.prob) %>%
  dplyr::rename(variable="categorical.variable") -> bmi_pca_omnibus

bmi_pca_omnibus
#significantly different: local_site, fish_presence, and lotic
#NS: location and shore

## By axis tests (for significant test results only)
envPCA2_pr_anovaDF %>%
  select(local_site,fish_presence,lotic) %>%
  names() %>%
  map_df(function(x){
    envPCA2_pr_anovaDF %>%
      group_by(PC) %>%
      anova_test(dv=scores, wid=site, between=x,detailed=TRUE) %>%
      as_tibble() %>%
      arrange(PC)
  }) -> bmi_pca_byaxis
bmi_pca_byaxis


## Interpretation of by-axis tests
# local_site
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  anova_test(scores ~ local_site,detailed=TRUE)
#significant differences among local_sites on each PC

#groups form clear clusters in PC space, which is affected by both PC1 and 2 (PC1 has greater 
  #affect from a visual standpoint, which is supported by the ANOVA).


# location
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  anova_test(scores ~ location,detailed=TRUE)
#significant differences for PC2 only

#looking at the biplot, it's clear that PC2 does a 'better job' of discriminating the categories
  #than PC1. However, there is stil clear overlap on this axis for a subset of the groups
#further, the anova results indicate that a low within-group variances along PC2 may be driving
  #this relationship


# fish_presence
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  t_test(scores ~ fish_presence,detailed=TRUE)
#significant differences between fish_presence groups for PC1 only

#interpretation from biplot:
#supported in that ellipses show clear separation between the two groups; however, PC1 is clearly
  #more discriminatory. PC1 is negatively associated with sat and elevation and positively with
  #temp & DO, which likely influence fish ecology than pH, which is positively associated with PC2


# lotic
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  t_test(scores ~ lotic,detailed=FALSE)
#significant differences between lotic for PC 2 only

#interpretation: clear separation between lotic groups, which is mostly due to PC2 (although
  #a shift along PC2), which is supported by omnibus and PC-separated ANOVAs


# shore
envPCA2_pr_anovaDF %>%
  group_by(PC) %>%
  t_test(scores ~ shore,detailed=FALSE) #all NS



  
  
  
  
  

