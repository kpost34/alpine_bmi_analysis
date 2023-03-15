#Created by Keith Post on 3/5/23
#Principal Coordinates and Multidimensional Scaling


#### Source DF (and load packages)=================================================================
pacman::p_load(here,tidyverse,vegan,smacof)

select<-dplyr::select
filter<-dplyr::filter


#full data frame
bmi_envDF<-readRDS(here::here("data","tidy_data",
                                         "alpine_bmi_env_n_trans_compVar_noMiss_2023-03-13.rds"))

#site identifier + tax data
bmiDF <- bmi_envDF %>%
  select(c(project_site,starts_with("Amphipoda"):last_col()))
  


#### Compute distances==============================================================================
### Manhattan
vegdist(bmiDF[,-1],method="manhattan",diag=TRUE)

### Euclidean
vegdist(bmiDF[,-1],method="euclidean",diag=TRUE)

### Chord
#transform (normalize) data before calculating Euclidean distances to calculate chord distances
vegdist(decostand(bmiDF[,-1],"norm"),method="euclidean",diag=TRUE)

### Species profiles
vegdist(decostand(bmiDF[,-1],"total"),method="euclidean",diag=TRUE)

### Hellinger
vegdist(decostand(bmiDF[,-1],"hellinger"),method="euclidean",diag=TRUE)

### Chi-squared
vegdist(decostand(bmiDF[,-1],"chi.square"),method="euclidean",diag=TRUE)

### Bray-Curtis
vegdist(bmiDF[,-1],method="bray",diag=TRUE)


#### PCoA===========================================================================================
### Create dissimilarity matrix
#using square-roots of B-C distances because non-metric & can produce negative EVs
bmi_bray<-sqrt(vegdist(bmiDF[,-1],method="bray"))


### Fit PCoA
## Using a weighted PCoA
bmi_pcoa<-wcmdscale(bmi_bray,k=2,eig=TRUE)


## Percent of total inertia captured by (k=) 2 axes
bmi_pcoa$GOF

# Percent inertia captured by first and second axes
100*bmi_pcoa$eig[1:2]/sum(bmi_pcoa$eig)



#### NMDS===========================================================================================
### Choose number of dimensions
#fit a series of NMDS models on site scores of the first two PCoA axes while successively increasing
  #the number of dimensions
#look at how stress values decrease as number of dimensions (axes) increases using a scree plot
  #type="ordinal" uses ordinal regression and ties="primary" allows tied Bray-Curtis dissimilarity
  #values to result in untied Euclidean distances
bmi_nmds_stress<-rep(NA,10)
for(i in 1:10) {
  bmi_nmds_stress[i]<-smacof::mds(bmi_bray,ndim=i,type="ordinal",ties="primary")$stress
}
plot(bmi_nmds_stress,type="b")
#large decrease from 1 to 2 dim before more gradual declines

bmi_nmds_stress[2]
#stress = 0.08069043


### Iteratively find lowest stress value
bmi_nmds_default<-smacof::mds(bmi_bray,ndim=2,type="ordinal",ties="primary")
bmi_explore<-smacof::icExplore(bmi_bray,ndim=2,type="ordinal",ties="primary",returnfit=TRUE,
                               verbose=TRUE)
bmi_explore
plot(bmi_explore,main="icExplore BMI data")
which(bmi_explore$stressvec==min(bmi_explore$stressvec))
#75
bmi_explore$stressvec[75]
#this value (.08067371) is nearly identical to the default NMDS solution


### Compare default and run #75 solutions using Procrustes analysis
smacof::Procrustes(Y=bmi_explore$mdsfit[[75]]$conf,
                   X=bmi_nmds_default$conf)
#congruence coefficient (R value) = 1
#alienation coefficient (1-R^2) = .02
#thus, solutions are nearly identical





















