#Created by Keith Post on 4/3/23
#Constrained Ordination Techniques


#### Source DF & functions (and load packages)======================================================
### Read in packages
pacman::p_load(here,tidyverse,vegan)

### Data import
## No composite vars
# Full data frame 
bmi_n_envDF<-readRDS(here::here("data","tidy_data",
                                         "alpine_bmi_env_n_trans_2023-04-09.rds"))

# Taxa only
bmi_nDF <- bmi_n_envDF %>%
  select(starts_with("Amphipoda"):last_col())

## With composite vars
# Full data frame 
bmi_n_envDF_comp<-readRDS(here::here("data","tidy_data",
                                     "alpine_bmi_env_n_trans_compVar_2023-04-09.rds"))

# Taxa only (with shortened names)
bmi_nDF_comp<-bmi_n_envDF %>%
  select(starts_with("Amphipoda"):last_col()) %>%
  rename_with(.cols=everything(),.fn=~paste0(str_sub(.x,1,2),"_",str_extract(.x,"(?<=_)[:alpha:]{1,5}")))


##### Canonical Correspondence Analysis (CCA)=======================================================
#### Run a CCA
bmi_cca<-cca(bmi_nDF~elevation_trans+temp_trans+ph_trans+nitrate_trans+sat+DO_trans,data=bmi_n_envDF)
bmi_cca            
#like RDA, CCA has total and partitioned inertia/variance
#constrained inertia = env vars = ~31.25 % variance
#unconstrained inertia = ~ 68.75 % variance

#### Check for highly collinear predictors
vif.cca(bmi_cca)
#sat & DO_trans very high
#elevation_trans and temp_trans high


#### Use envDF with composite variables oxygen (sat + DO) and elevTemp (elevation + temperature)
### Run a CCA
bmi_cca_comp<-cca(bmi_nDF_comp~ph_trans+nitrate_trans+oxygen+elevTemp,data=bmi_n_envDF_comp)


### Check for highly collinear predictors
vif.cca(bmi_cca_comp)
#none left


## Look at new model
bmi_cca_comp
#constrained inertia = env vars = ~ 20.71%
#unconstrained inertia = ~ 79.29%
#--> constrained axes of new model account for ~10.5 % less of total variance compared to the new
  #model, but VIFs are all less than 2.5, making the model more interpretable


## Run permutational ANOVA on new model to ensure
anova(bmi_cca_comp) #NS
anova(bmi_cca_comp,by="axis")
#no axis is significant...but will finish out the analysis


## Interpret summary
summary(bmi_cca_comp)
#Partitioning of scaled Chi-square:
  #constrained: predictor vars
  #unconstrained: all other variation
#EVs and their contribution to scaled Chi-square
  #CCA1 ~ 14.5% and CCA2 ~ 3.8%
  #CA1 ~ 45.6% and CA2 ~ 23.8%
  #indicates that key env vars have not been measured
#Accumulated constrained EVs
  #CCA1 & CCA2 represet ~88.4% of constrained inertia--meaning that they reasonably represent
    #constrianed variance
#Species scores
  #roughly represent where spp are most abundant along each axis
  #e.g., Plecoptera:Perlidae are most abundant family along CCA1
#Site scores
  #weighted averages of species scores
#Site constraints
  #site scores based on predicted species abundances rather than observed abundances
#Biplot scores for predictors
  #loadings of each predictor on each constrained axis (similar to PCA loadings)
  #e.g., elevTemp strongly + associated with CCA1 & pH strongly - associated with CCA1


## Plot result
# Fish presence
#plot observed species scores (not predicted ones) by display arg
plot(bmi_cca_comp,type="n",display="sp",xlim=c(-3,3),ylim=c(-3,3))

#create vectors of fish_presence var and fish_presence colors and env fit object
fp<-bmi_n_envDF[["fish_presence"]]
fp_colors<-c("black","green")
fit<-envfit(bmi_cca_comp~ph_trans+nitrate_trans+oxygen+elevTemp,data=bmi_n_envDF_comp)


#plot site scores on first two contrained axes and add the legend
points(summary(bmi_cca_comp)$sites[,2]~summary(bmi_cca_comp)$sites[,1],col=fp_colors[fp],pch=16)
text(bmi_cca_comp,display="sp",scaling="species",offset=0,cex=0.8)
plot(fit)
legend("topright",title="fish presence",legend=levels(fp),col=fp_colors,pch=16)


# Lotic
plot(bmi_cca_comp,type="n",display="sp",xlim=c(-3,3),ylim=c(-3,3))

#create vectors of lotic var and lotic colors and env fit object
lotic<-bmi_n_envDF[["lotic"]]
lotic_colors<-c("black","green")
fit<-envfit(bmi_cca_comp~ph_trans+nitrate_trans+oxygen+elevTemp,data=bmi_n_envDF_comp)


#plot site scores on first two contrained axes and add the legend
points(summary(bmi_cca_comp)$sites[,2]~summary(bmi_cca_comp)$sites[,1],col=lotic_colors[lotic],pch=16)
text(bmi_cca_comp,display="sp",scaling="species",offset=0,cex=0.8)
plot(fit)
legend("topright",title="lotic",legend=levels(fp),col=lotic_colors,pch=16)


#Interpretation
#a perpendicular line from a site/point to an arrow indicates the env value at that site
#a perpendicular line from a species/name to an arrow indicates env value in those sites in which
  #that species reaches max abundance
#lentic sites positively associated with oxygen (DO & sat) and negatively associated with nitrate
#lotic sites negatively associated with pH
#no clear pattern for taxa




##### Partial Canonical Correspondence Analysis (pCCA)===============================================
### Given reasonable separation of points by lentic/lotic, this variable is used to remove overall
  #data trend in ordination space (by using it as a conditioning variable)

### Conduct a CCA using lotic as a conditioning variable
bmi_cca_comp_lotic<-cca(bmi_nDF_comp~ph_trans+nitrate_trans+oxygen+elevTemp+Condition(lotic),
                        data=bmi_n_envDF_comp)
anova(bmi_cca_comp_lotic,by="axis")
#not significant: no effect of env vars after removing effect of lotic factor


### Look at ordination object
bmi_cca_comp_lotic


### Plot results
plot(bmi_cca_comp_lotic,type="n",display="sp",xlim=c(-3,3),ylim=c(-3,3))

#create vectors of lotic var and lotic colors and env fit object
lotic<-bmi_n_envDF[["lotic"]]
lotic_colors<-c("black","green")
fit<-envfit(bmi_cca_comp_lotic~ph_trans+nitrate_trans+oxygen+elevTemp,data=bmi_n_envDF_comp)


#plot site scores on first two contrained axes and add the legend
points(summary(bmi_cca_comp_lotic)$sites[,2]~summary(bmi_cca_comp_lotic)$sites[,1],col=lotic_colors[lotic],pch=16)
text(bmi_cca_comp_lotic,display="sp",scaling="species",offset=0,cex=0.8)
plot(fit)
legend("topright",title="lotic",legend=levels(fp),col=lotic_colors,pch=16)

#interpretation
#colors slightly more mixed...but have definitely shifted around
#conditional variance explains ~11% of total variance, whereas constrained explains ~24% (from ~21% before)
#seems as though the lotic factor pulls more variance from unconstrained portion, preventing mixing of colors






