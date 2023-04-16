#Created by Keith Post on 9/22/22
#EDA functions


#### Bar plots======================================================================================
### Total counts
barplotter_tot<-function(dat,ind,dep,angled=FALSE,col){
  dat %>%
    group_by({{ind}}) %>%
    summarize({{dep}} := sum({{dep}})) %>% 
    mutate({{ind}} := fct_reorder({{ind}},{{dep}},.fun=sum,.desc=TRUE)) %>%
    ggplot(aes(x={{ind}},y={{dep}})) +
    geom_col(color="black",fill=col) +
    labs(x=str_to_title(quo_name(enquo(ind))),
         y=paste("Total",quo_name(enquo(dep)))) +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    theme_bw() -> p
  
  if(angled==TRUE) {
      p + theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5))
  }
  else{p}
}



### Mean counts
## One ind
barplotter_avg<-function(dat,ind,dep,angled=FALSE,col){
  dat %>%
    group_by({{ind}}) %>%
    summarize(mean_y := mean({{dep}}),
              se=sd({{dep}})/sqrt(n()),
              lower=mean_y-se,
              upper=mean_y+se) %>% 
    mutate("{{ind}}" := fct_reorder({{ind}},mean_y,.fun=sum,.desc=TRUE)) %>%
    ggplot(aes(x={{ind}},y=mean_y)) +
    geom_col(color="black",fill=col) +
    geom_errorbar(aes(ymin=lower,ymax=upper),width=0.4) +
    labs(x=str_to_title(quo_name(enquo(ind))),
         y=paste("Mean",quo_name(enquo(dep)))) +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    theme_bw() -> p
  
  if(angled==TRUE) {
    p + theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5))
  }
  else{p}
}

## Two ind
barplotter_avg2<-function(dat,ind1,ind2,dep,pos="stack",angled=FALSE){
  dat %>%
    group_by({{ind1}},{{ind2}}) %>%
    summarize(mean_y := mean({{dep}})) %>% 
    ungroup() %>%
    mutate("{{ind1}}" := fct_reorder({{ind1}},mean_y,.fun=sum,.desc=TRUE),
           "{{ind2}}" := fct_reorder({{ind2}},mean_y,.fun=sum)) %>%
    ggplot(aes(x={{ind1}},y=mean_y)) +
    #pos arg determines stacked vs grouped plot
    geom_col(aes(fill={{ind2}}),color="black",position=pos) +
    labs(x=str_to_title(quo_name(enquo(ind1))), y=paste("Mean",quo_name(enquo(dep)))) +
    scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
    scale_fill_viridis_d(end=0.92) +
    theme_bw() -> p
  
  #display x axis text at 45o angle when angled==TRUE
  if(angled==TRUE) {
    p + theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5))
  }
  else{p}
}


### Two ind--mean values-faceted
bar_faceter<-function(dat,ind1,ind2,dep,angled=FALSE){
  dat %>%
    group_by({{ind1}},{{ind2}}) %>%
    summarize(mean_y = mean({{dep}})) %>% 
    ungroup() %>%
    ggplot(aes(x={{ind1}},y=mean_y,fill={{ind2}})) +
    geom_col(color="black") +
    facet_wrap(vars({{ind2}}),scales="free") +
    scale_fill_viridis_d() +
    labs(x=str_to_title(quo_name(enquo(ind1))), y=paste("Mean",quo_name(enquo(dep)))) +
    theme_bw() +
    theme(legend.position="none") -> p
  
  if(angled==TRUE){
    p + theme(axis.text.x=element_text(angle=45,vjust=0.5,hjust=0.5)) 
  }
  else{p}
}



##### Boxplots======================================================================================
boxplotter<-function(dat,ind,dep){
  dat %>%
    mutate("{{ind}}"=fct_reorder({{ind}},{{dep}},.fun=median,.desc=TRUE)) %>%
    ggplot(aes(x={{ind}},y={{dep}},color={{ind}})) +
    geom_boxplot(color="black",outlier.shape=NA) +
    geom_jitter() +
    scale_y_continuous(trans = pseudo_log_trans()) +
    scale_color_viridis_d(end=0.92) +
    theme_bw() 
}


##### Regression Lines for Scatterplots=============================================================
smoother<-function(data,mapping,method="lm",...){
  p<-ggplot(data = data, mapping=mapping) +
    geom_point() +
    geom_smooth(method=method,...)
  p
}











