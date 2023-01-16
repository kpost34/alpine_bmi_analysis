#Created by Keith Post on 12/23/22
#Correspondence Analysis and Detrended Correspondence Analysis on Taxa-Site Data


#### Source DFs (and load packages)=================================================================
pacman::p_load(here,MASS,vegan)

select<-dplyr::select


source(here::here("code","01_DataSetup.R"))


#### Correspondence Analysis========================================================================
### Start with site x species matrix with sites as rows, species (taxa) as cols, and cells as counts, 
  #biomass, etc
BMIcountWideDF %>% 
  select(-c(local_site:date)) %>% 
  #select(-c(local_site,location,date)) %>%
  decorana(ira=1)-> bmiCA_dec

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


plot(bmiCA_dec)


#### Detrended Correspondence Analysis==============================================================
BMIcountWideDF %>% 
  select(-c(local_site:date)) %>%
  decorana(ira=0)-> bmiDCA_dec

bmiDCA_dec
summary(bmiDCA_dec)

bmiDCA_dec$evals 
#vector of total variance accounted for by each axis
bmiDCA_dec$rproj
#matrix of site scores on each axis
bmiDCA_dec$cproj
#matrix of species scores on each axis


plot(bmiDCA_dec)



####








## DONE




#### LAST COMMIT
# created this script and began performing CA & DCA
# added Skalski ANoD function script

