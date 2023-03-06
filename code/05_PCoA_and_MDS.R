#Created by Keith Post on 3/5/23
#Principal Coordinates and Multidimensional Scaling


#### Source DFs (and load packages)=================================================================
pacman::p_load(here,tidyverse,vegan)

select<-dplyr::select
filter<-dplyr::filter


BMIenvWideDF_trans<-readRDS(here::here("data","tidy_data","alpine_bmi_env_trans_2023-02-12.rds"))
BMIenvcountWideDF_trans0<-readRDS(here::here("data","tidy_data","alpine_bmi_env_n_trans_2023-02-12.rds"))



#### Correspondence Analysis========================================================================






