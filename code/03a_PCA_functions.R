#Created by Keith Post on 10/8/22
#PCA functions




#### Assumption 1: Symmetrical Distribution/Normality===============================================
### Transformations
## Boxcox transformation
boxcoxer<-function(x){
  out<-boxcox(x~1,plotit=FALSE)
  lambda<-out$x[out$y==max(out$y)]
  x^lambda
}


## Inverse transformation
inverse<-function(x){
  1/x
}


### Visualizations
## Histogram plotter
histogrammer<-function(dat,var,meas,nbin=8,cols=NULL){
  dat %>%
    ggplot() +
      geom_histogram(aes({{meas}},fill={{var}}),color="black",bins=nbin) +
      facet_wrap(vars({{var}}),scales="free",ncol=cols) +
      scale_fill_viridis_d(guide="none") +
      theme_bw(base_size=12)
}

## Q-Q Plots
qqplotter<-function(dat,var,meas,cols=NULL){
  dat %>% 
    ggplot() +
    geom_qq(aes(sample={{meas}}),color="darkblue") +
    geom_qq_line(aes(sample={{meas}})) +
    facet_wrap(vars({{var}}),scales="free",ncol=cols) +
    labs(x="Theoretical Quantiles",
         y="Sample Quantiles") +
    theme_bw(base_size=12)
}



#### ANOVA==========================================================================================
### Run omnibus test
run_omnibus_anova<-function(data,cat){
  envPCA2_pr_anovaDF %>%
    select(site,!!sym(cat),PC,scores) %>%
    pivot_wider(id_cols=c(site,!!sym(cat)),names_from="PC",values_from="scores") %>%
    select(-site) %>%
    as.data.frame() %>%
    Skalski.adonis(PC.axes=c(2,3),Groups=1) %>%
    as_tibble() %>%
    mutate(categorical.variable=cat,.before="Group.SS")
}






