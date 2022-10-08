#Created by Keith Post on 10/1/22
#PCA on environmental variables


#### Source DFs (and load packages)=================================================================
pacman::p_load(rstatix,MASS)
source(here("code","01_DataSetup.R"))


#### Test Assumptions of PCA========================================================================
### Assumption 1: Distributions of all variables follow a normal distribution (or are at least 
  #symmetrical)

## Test assumption
## Histogram
BMIenvTidyDF %>% 
  filter(!variable %in% c("lotic","fish_presence")) %>%
  ggplot() +
  geom_histogram(aes(value),bins=10) +
  facet_wrap(~variable,scales="free") +
  theme_bw() 


## Q-q plots
BMIenvTidyDF %>% 
  filter(!variable %in% c("lotic","fish_presence")) %>%
  ggplot() +
  geom_qq(aes(sample=value)) +
  geom_qq_line(aes(sample=value)) +
  facet_wrap(~variable,scales="free") +
  theme_bw()

### Shapiro tests
BMIenvWideDF %>%
  shapiro_test(elevation,temp,sat,DO,ph,nitrate)
#significant (non-normal): DO, nitrate, ph, and temp
#normal: elevation (barely) and sat


## Transformation
DOvar<-BMIenvWideDF$DO
outDO<-boxcox(DOvar~1)
lambdaDO<-outDO$x[outDO$y==max(outDO$y)]
lambdaDO
DOy<-DOvar^lambdaDO
hist(DOy)
