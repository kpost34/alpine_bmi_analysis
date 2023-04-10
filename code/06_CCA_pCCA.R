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
# Fll data frame 
bmi_n_envDF_comp<-readRDS(here::here("data","tidy_data",
                                     "alpine_bmi_env_n_trans_compVar_2023-04-09.rds"))

# Taxa only
bmi_nDF_comp<-bmi_n_envDF %>%
  select(starts_with("Amphipoda"):last_col())


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
#no axis is significant


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
# Plot observed species scores (not predicted ones) by display arg
plot(bmi_cca_comp,display="sp",xlim=c(-3,3),ylim=c(-3,3))

#create vector containing five vegetation types and specifying each color
fp<-bmi_n_envDF[["fish_presence"]]
fp_colors<-c("black","green")


#plot site scores on first two contrained axes and add the legend
points(summary(bmi_cca_comp)$sites[,2]~summary(bmi_cca_comp)$sites[,1],col=fp_colors[fp],pch=16)
text(bmi_cca_comp,display="sp",scaling="species")
#legend("topright",legend=unique.communities,col=veg.type.colors,pch=16)






