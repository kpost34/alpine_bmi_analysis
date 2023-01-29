#Created by Keith Post on 12/23/22
#Correspondence Analysis and Detrended Correspondence Analysis on Taxa-Site Data


#### Source DFs (and load packages)=================================================================
pacman::p_load(here,MASS,vegan)

select<-dplyr::select


source(here::here("code","01_DataSetup.R"))
source(here::here("code","DCA_Env_Arrows_function.R"))


#### Correspondence Analysis========================================================================
### Start with site x species matrix with sites as rows, species (taxa) as cols, and cells as counts, 
  #biomass, etc
BMIenvcountWideDF %>%
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
BMIenvcountWideDF %>%
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
BMIenvcountWideDF %>%
  #select numerical env vars
  select(project_site,elevation:nitrate) %>%
  # rename_with(.cols=!project_site,.fn=~paste("env",.x,sep="_")) %>%
  bind_cols(
    bmiDCA_dec$rproj[,1:3] %>%
      as_tibble()
  ) %>%
  pivot_longer(cols=elevation:nitrate,
               names_to="env",
               values_to="value") %>%
  relocate(project_site,env,value) %>%
  group_by(env) %>%
  cor_test(value,method="spearman") %>%
  select(-c(var1,statistic,method)) %>% 
  pivot_wider(names_from="var2",values_from=c("cor","p"),
              names_glue="{var2}_{.value}",names_vary="slowest") -> BMIenvDCAcorTab
#significant: elevation-DCA2, ph-DCA2






### Correlations between each env var and all chosen ordination axes
#notes: 1) directions of arrows determined by signs of regression coefficients and 2) numerical
  #values of the regression coefficients are proportional to their lengths
BMIenvcountWideDF %>%
  #select env vars only
  select(local_site,location,elevation:shore) %>%
  envfit(ord=bmiDCA_dec,
         choices=1:3) -> BMIenvfit_all

## Plot results
# With factors
plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit,choices=c(1,2))
#very cluttered because of factors

# Only 0-1 factors
BMIenvcountWideDF %>%
  #select env vars only
  select(elevation:lotic) %>%
  envfit(ord=bmiDCA_dec,
         choices=c(1:3)) -> BMIenvfit_nocat

plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_nocat,choices=c(1,2))

# Only numerical vars
#DCA1 & 2
BMIenvcountWideDF %>%
  #select env vars only
  select(elevation:nitrate) %>%
  envfit(ord=bmiDCA_dec,
         choices=1:3) -> BMIenvfit_numonly

plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_numonly,choices=c(1,2))

#recall that 1) length of each arrow project 90o to each axis proportional to the strength of
  #the partial regression coefficient b/t each env var and the chosen DCA axes; direction indicates
  #sign of correlation with each axis
#interpretation: regardless of how many 0-1 or categorical vars included, elevation and temperature
  #clearly are the most important env vars--elevation strongly, positively associated with DCA2 
  #and temp strongly, negatively associated with DCA2 


#DCA1 & 3
plot(bmiDCA_dec,choices=c(1,3),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_numonly,choices=c(1,3))

#elevation is also strongly, positively correlated with DCA3 (so is nitrate), and temp and pH
  #are strongly, negatively correlated with DCA3


#DCA2 & 3
plot(bmiDCA_dec,choices=c(2,3),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
plot(BMIenvfit_numonly,choices=c(2,3))



### Correlations between each ordination axis and all env vars
#in this scenario, the partial regression coefficient reflects the amount by which a unit change in
  #each env var would change the position of a site along that axis if all other variables didn't
  #change
plot(bmiDCA_dec,choices=c(1,2),type="n")
text(bmiDCA_dec,display="sites",col="black",cex=0.5)
points(bmiDCA_dec,display="species",pch=16,cex=0.5,col="red")
ord.on.env.arrows(ordination.site.scores=bmiDCA_dec$rproj[,1:2],
                  env.matrix=BMIenvcountWideDF %>%
                    select(elevation:nitrate),
                  arrow.col="blue",arrow.scale=5,
                  choices=c(1,2))

## DONE
# cleaned up code on CA and DCA to ensure appropriate comparisons among bmi and env data
# added an analysis to PCA code
# performed spearman corrs of env vars & DCA axes 1-3 and plotted results
# added ord.on.env.arrows() function script




#### LAST COMMIT
# created this script and began performing CA & DCA
# added Skalski ANoD function script
