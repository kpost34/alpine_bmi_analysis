#Created by Keith Post on 12/23/22
#Correspondence Analysis and Detrended Correspondence Analysis on Taxa-Site Data


#### Source DFs (and load packages)=================================================================
pacman::p_load(here,tidyverse,MASS,vegan,rstatix,GGally,viridis)

select<-dplyr::select
filter<-dplyr::filter


# source(here::here("code","01_DataSetup.R"))
BMIenvWideDF_trans<-readRDS(here::here("data","tidy_data","alpine_bmi_env_trans_2023-02-12.rds"))
BMIenvcountWideDF_trans0<-readRDS(here::here("data","tidy_data","alpine_bmi_env_n_trans_2023-02-12.rds"))

source(here::here("code","DCA_Env_Arrows_function.R"))


#### Correspondence Analysis========================================================================
### Update DF by removing absent genera
## Identify genera that are present
dca_tax_nm<-BMIenvcountWideDF_trans0 %>%
  select(starts_with("Amphi"):last_col()) %>%
  summarize(across(everything(),sum)) %>%
  select_if(~any(.>0)) %>%
  names() 

## Retain those genera
BMIenvcountWideDF_trans<-BMIenvcountWideDF_trans0 %>% 
  select(local_site:nitrate_trans,all_of(dca_tax_nm))
  

### Start with site x species matrix with sites as rows, species (taxa) as cols, and cells as counts, 
  #biomass, etc
BMIenvcountWideDF_trans %>% 
  #select taxonomic cols only
  select(Amphipoda_Gammaridae:last_col()) %>%
  #rename for easy plotting
  rename_with(.cols=everything(),
              .fn=~paste0(str_sub(.x,1,3),str_extract(.x,"_[:alpha:]{5}")))  %>% 
  #ira=1 indicates correspondence analysis
  decorana(ira=1) -> bmiCA_dec

### Look at analysis output
#note that decorana always outputs four axes
bmiCA_dec
summary(bmiCA_dec)


bmiCA_dec$evals 
#vector of total variance accounted for by each axis
bmiCA_dec$rproj
#matrix of site scores on each axis
bmiCA_dec$cproj
#matrix of species scores on each axis

bmiCA_dec$evals[1]
bmiCA_dec$rproj[,1]
bmiCA_dec$cproj[,1] 
#first call provides EV associated with first axis (the sqrt of this value = "canoncial correlation" of corresp())
#second call returns the site scores on the first axis
#third call provides the species scores on the first axis

### Plot CA
plot(bmiCA_dec)


#### Detrended Correspondence Analysis==============================================================
### Run analysis
BMIenvcountWideDF_trans %>%
  #select taxonomic cols only
  select(Amphipoda_Gammaridae:last_col()) %>%
  #rename for easy plotting
  rename_with(.cols=everything(),
              .fn=~paste0(str_sub(.x,1,3),str_extract(.x,"_[:alpha:]{5}")))  %>% 
  #ira=0 for detrended version
  decorana(ira=0)-> bmiDCA_dec


### Check out output
bmiDCA_dec
summary(bmiDCA_dec)
#note that this also provides axis lengths

bmiDCA_dec$evals 
#vector of total variance accounted for by each axis
bmiDCA_dec$rproj
#matrix of site scores on each axis
bmiDCA_dec$cproj
#matrix of species scores on each axis

### Plot results
plot(bmiDCA_dec) #plot DCA1 & 2
plot(bmiDCA_dec,choices=c(1,3)) #plot DCA1 & 3 (because EV of DCA 2 & 3 are similar)
plot(bmiDCA_dec,choices=c(2,3))
#this clearly shows how Eph_Baeti (and to a lesser extent, Tri_Apata) is brought closer to rest of 
  #the groups (when compared to CA)

#overall impression: DCA1 clearly discriminates SITES better than DCA2 & 3; however, species are
  #discriminated better along DCA 2 & 3 compared to 1


#### Interpretation=================================================================================
### Bivariate correlations between each env variable and site scores on first two DCA axes
#transformed vars (except sat) from PCA (but including nitrate)
#using full DF to retain order
BMIenvWideDF_trans %>%
  select(project_site,sat,elevation_trans:nitrate_trans) %>% 
  bind_cols(
    bmiDCA_dec$rproj[,1:3] %>%
      as_tibble()
  ) %>%
  pivot_longer(cols=sat:nitrate_trans,
               names_to="env",
               values_to="value") %>%
  relocate(project_site,env,value) %>%
  group_by(env) %>%
  cor_test(value,method="spearman") %>%
  select(-c(var1,statistic,method)) %>% 
  pivot_wider(names_from="var2",values_from=c("cor","p"),
              names_glue="{var2}_{.value}",names_vary="slowest") -> BMIenvDCAcorTab
#significant: elevation_trans-DCA2, ph-DCA2
  


### Correlations between each env var and all chosen ordination axes
#notes: 1) directions of arrows determined by signs of regression coefficients and 2) numerical
  #values of the regression coefficients are proportional to their lengths
BMIenvWideDF_trans %>%
  #select env vars only
  select(local_site,location,sat:last_col()) %>%
  envfit(ord=bmiDCA_dec,
         choices=1:3) -> BMIenvfit_all


## Plot results
# With factors
plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_all,choices=c(1,2))
#very cluttered because of factors

# Only 0-1 factors & num vars
BMIenvWideDF_trans %>%
  #select env vars only
  select(sat:nitrate_trans) %>%
  envfit(ord=bmiDCA_dec,
         choices=c(1:3)) -> BMIenvfit_nocat

plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_nocat,choices=c(1,2))

# Only numerical vars
#DCA1 & 2
BMIenvWideDF_trans %>%
  #select env vars only
  select(sat,elevation_trans:nitrate_trans) %>%
  envfit(ord=bmiDCA_dec,
         choices=1:3) -> BMIenvfit_numonly

plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_numonly,choices=c(1,2))

#recall that 1) length of each arrow projects 90o to each axis proportional to the strength of
  #the partial regression coefficient b/t each env var and the chosen DCA axes; direction indicates
  #sign of correlation with each axis
#interpretation: regardless of how many 0-1 or categorical vars included, elevation and temperature
  #clearly are the most important env vars--elevation strongly, positively associated with DCA2 
  #and temp strongly, negatively associated with DCA2; pH is also moderately, positively associated
  #with DCA2


#DCA1 & 3
plot(bmiDCA_dec,choices=c(1,3),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_numonly,choices=c(1,3))

#elevation is  strongly, positively correlated with DCA3, and temp, nitrate, and pH are strongly, 
  #negatively correlated with DCA3


#DCA2 & 3
plot(bmiDCA_dec,choices=c(2,3),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_numonly,choices=c(2,3))

#elevation: strongly, positively associated with DCA 2&3;
#temp: strongly, negatively associated with DCA 2&3;
#nitrate & pH: strongly, negatively associated with DCA 3



### Correlations between each ordination axis and all env vars
#in this scenario, the partial regression coefficient reflects the amount by which a unit change in
  #each env var would change the position of a site along that axis if all other variables didn't
  #change
plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
ord.on.env.arrows(ordination.site.scores=bmiDCA_dec$rproj[,1:2],
                  env.matrix=BMIenvWideDF_trans %>%
                    select(sat,elevation_trans:nitrate_trans),
                  arrow.col="blue",arrow.scale=5,
                  choices=c(1,2))

#notice that temp and sat are highly negatively correlated with DCA 1 but multicollinearity could be at play

## Assess multicollinearity
# Look at bivariate scatter plots and correlations of env data
BMIenvWideDF_trans %>%
  select(sat,elevation_trans:nitrate_trans) %>%
  pairs()

BMIenvWideDF_trans %>%
  select(sat,elevation_trans:nitrate_trans) %>% 
  ggpairs(lower=list(continuous="smooth"))

#strong correlations: DO-sat (-.959), temp-elevation (-0.714), sat-temp (-0.599) and DO-temp (.553) 
#only very strong correlation is the first pair: DO-sat,so PCA will be run on these
#note that it is a linear relationship


# Run PCA on these three variables
#pca
BMIenvWideDF_trans %>%
  #select sites + env vars of interest
  select(project_site,sat,DO_trans) %>%
  #formula input using scaled and centered variables
  prcomp(~sat + DO_trans,
         data=.,scale.=TRUE,center=TRUE) -> satDO_PCA_pr

satDO_PCA_pr
summary(satDO_PCA_pr)
#PCA1 contains ~ =98% of total variance

screeplot(satDO_PCA_pr,bstick=TRUE,type="barplot",
          main="PCA of alpine lakes environmental variables")
abline(h=mean(satDO_PCA_pr$sdev^2))
#clearly only PCA1 matters (i.e., EV drops significantly from PCA1-2 and EV of PCA2 is below broken stick
  #and avg EV)

#combine PCA1 scores with env vars and re-plot without multicollinearity
satDO_PCA_pr[["x"]] %>%
  as_tibble() %>%
  select(oxygen="PC1") %>%
  bind_cols(
    BMIenvcountWideDF_trans %>% 
      select(-c(DO_trans,sat)) 
  ) %>%
  relocate(oxygen,.after="nitrate_trans") -> BMIenvcountWideDF_compvar


### Write new file
# saveRDS(BMIenvcountWideDF_compvar,
#         here("data","tidy_data",paste0("alpine_bmi_env_n_trans_compVar_noMiss_",Sys.Date(),".rds")))

#check multicollinearity
BMIenvcountWideDF_compvar %>% 
  select(elevation_trans:oxygen) %>%
  ggpairs(lower=list(continuous="smooth"))
#now highest cor is -.714


# Re-plot correlations 
#between each env var and all ordination axes
#DCA1 & 2
BMIenvcountWideDF_compvar %>%
  #select env vars only
  select(elevation_trans:oxygen) %>%
  envfit(ord=bmiDCA_dec,
         choices=1:3) -> BMIenvfit_compvar_numonly

#DCA1 & 2
plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_compvar_numonly,choices=c(1,2))


#DCA1 & 3
plot(bmiDCA_dec,choices=c(1,3),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_compvar_numonly,choices=c(1,3))

#DCA2 & 3
plot(bmiDCA_dec,choices=c(2,3),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_compvar_numonly,choices=c(2,3))


#between each ordination axis and all env vars (with comp variable)
plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
ord.on.env.arrows(ordination.site.scores=bmiDCA_dec$rproj[,1:2],
                  env.matrix=BMIenvcountWideDF_compvar %>%
                    select(elevation_trans:oxygen),
                  arrow.col="blue",arrow.scale=5,
                  choices=c(1,2))


#plot DCA1&2 side-by-side
par(mfrow=c(1,2))

plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_compvar_numonly,choices=c(1,2))
title("environmental variable~axes")

plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
ord.on.env.arrows(ordination.site.scores=bmiDCA_dec$rproj[,1:2],
                  env.matrix=BMIenvcountWideDF_compvar %>%
                    select(elevation_trans:oxygen),
                  arrow.col="blue",arrow.scale=4,
                  choices=c(1,2))
title("axis~environmental variables")

#explanation:
#left plot: each env var and all chosen ordination axes (i.e., how does the combination of ordination
  #axes predict each env var)
  #show correlations between env vars and ord axes


#right plot: each ordination axis and all env vars (i.e., how does the combination of env vars
#predict each ordination axis)
  #partial regression coefs = amount by which 1 unit change in each env var would change pos of a site
    #along that axis if all of the other vars didn't change
#show *partial* correlations between env vars and ord axes


#interpretation:
#left plot: the strongest organizing variables are elevation (~0 DCA1, + DCA2) and temp (~0 DCA1, 
  #- DCA2); no env var is strongly + or -
#right plot: greater separation of variables along DCA1 & 2
  #sites with + DCA1 and 0 DCA2 associated with elevation, nitrate, and oxygen (sat & DO)
  #sites with - DCA1 and moderate - DCA2 associated with temp
  #sites with ~0 DCA1 and + DCA2 = pH

#given that axis ~ env vars is more in line with how DCA is viewed (ord axis represents a composite
  #of env vars) and that it takes into account other variables, it's preferred
  #thus


### Plot DCA scores colored by factor variable (location, fish_presence, lotic, shore)
## Create list of inputs for plots
factor_vec<-c("local_site","location","fish_presence","lotic","shore")

factor_vec %>%
  map(function(x){
    fac<-BMIenvcountWideDF_compvar %>% pull(!!sym(x)) %>% as.factor()
    text_legend<-levels(fac)

    levels(fac)<-viridis(length(levels(fac)),end=.85)
    col_vec<-as.character(fac)
    col_legend<-levels(fac)
    
    list(x,col_vec,text_legend,col_legend)
  }) -> fac_list


## Make plots
par(mfrow=c(1,1))
# local_site
plot(bmiDCA_dec,choices=c(1,2),type="n",xlim=c(-1,2),ylim=c(-0.5,0.5))
title(fac_list[[1]][[1]])
points(bmiDCA_dec,display="sites",pch=16,cex=1.5,col=fac_list[[1]][[2]])
legend("bottomright",
       legend=fac_list[[1]][[3]],
       pch=16,
       col=fac_list[[1]][[4]])
#no clear pattern...ALB sites generaly moderately negative for both DC axes


# location
plot(bmiDCA_dec,choices=c(1,2),type="n",xlim=c(-1,2),ylim=c(-0.5,0.5))
title(fac_list[[2]][[1]])
points(bmiDCA_dec,display="sites",pch=16,cex=1.5,col=fac_list[[2]][[2]])
legend("bottomright",
       legend=fac_list[[2]][[3]],
       pch=16,
       col=fac_list[[2]][[4]])
#no clear pattern...creek sites typically are near origin and moderately - for both DC axes


# fish_presence
plot(bmiDCA_dec,choices=c(1,2),type="n",xlim=c(-1,2),ylim=c(-0.5,0.5))
title(fac_list[[3]][[1]])
points(bmiDCA_dec,display="sites",pch=16,cex=1.5,col=fac_list[[3]][[2]])
legend("bottomright",
       legend=fac_list[[3]][[3]],
       pch=16,
       col=fac_list[[3]][[4]])
#no clear pattern
#generally, 1 = close to origin but 1 = spread out


# lotic
plot(bmiDCA_dec,choices=c(1,2),type="n",xlim=c(-1,2),ylim=c(-0.5,0.5))
title(fac_list[[4]][[1]])
points(bmiDCA_dec,display="sites",pch=16,cex=1.5,col=fac_list[[4]][[2]])
legend("bottomright",
       legend=fac_list[[4]][[3]],
       pch=16,
       col=fac_list[[4]][[4]])
#no clear pattern
#generally, lotic = 1 has slightly negative DCA1, while lotic = 0 has DCA2 > 0 and DCA1 ~ 0 +


# shore
plot(bmiDCA_dec,choices=c(1,2),type="n",xlim=c(-1,2),ylim=c(-0.5,0.5))
title(fac_list[[5]][[1]])
points(bmiDCA_dec,display="sites",pch=16,cex=1.5,col=fac_list[[5]][[2]])
legend("bottomright",
       legend=fac_list[[5]][[3]],
       pch=16,
       col=fac_list[[5]][[4]])
#no real pattern


## Look at species associated with DCA scores
## Species scores
summary(bmiDCA_dec)
bmiDCA_dec$cproj

## Plot species scores
# Sort species by abundance
BMIenvcountWideDF_compvar %>% 
  select(starts_with("Amphipoda"):last_col()) %>%
  colSums() %>%
  sort(decreasing=TRUE) -> tax_n

#grab names
tax_n %>%
  names() %>% 
  as_tibble() %>%
  mutate(value=paste0(str_sub(value,1,3),str_extract(value,"_[:alpha:]{5}"))) %>%
  pull() -> tax_n_abbrv

# Subset most abundant species
comm_bmiDCA_dec<-bmiDCA_dec %>%
       .[["cproj"]] %>%
       as.data.frame() %>%
       .[tax_n_abbrv[1:3],]

# Subset rarest species (that are present)
rare_bmiDCA_dec<-bmiDCA_dec %>%
       .[["cproj"]] %>%
       as.data.frame() %>%
       .[tax_n_abbrv[10:13],]

# All species
plot(bmiDCA_dec,choices=c(1,2),type="n",xlim=c(-3,3),ylim=c(-1,1))
points(bmiDCA_dec,display="species",pch=16,col="gray50")
points(x=comm_bmiDCA_dec$DCA1,y=comm_bmiDCA_dec$DCA2,pch=16,cex=1.5,col="black")
points(x=rare_bmiDCA_dec$DCA1,y=rare_bmiDCA_dec$DCA2,pch=16,cex=1.5,col="red")
#most common genera in black and rarest genera in red
#interpretation: 3/4 rarest species near edges of DCA2 

#---------------------------------------------------------------------------------------------------
## DONE





#### LAST COMMIT
# colored 'species' scores on DCA biplot by generic abundance

