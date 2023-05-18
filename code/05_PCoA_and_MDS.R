#Created by Keith Post on 3/5/23
#Principal Coordinates and Multidimensional Scaling


##### Source DF & functions (and load packages)=====================================================
pacman::p_load(here,tidyverse,viridis,vegan,smacof,cowplot)

source(here::here("code","DCA_Env_Arrows_function.R"))

select<-dplyr::select
filter<-dplyr::filter


#full data frame
bmi_envDF<-readRDS(here::here("data",
                              "tidy_data",
                              "alpine_bmi_env_n_trans_compVar_2023-04-09.rds"))

#site identifier + tax data
bmiDF <- bmi_envDF %>%
  select(c(project_site,starts_with("Amphipoda"):last_col()))
  


##### Compute distances=============================================================================
#### Manhattan
vegdist(bmiDF[,-1],method="manhattan",diag=TRUE)

#### Euclidean
vegdist(bmiDF[,-1],method="euclidean",diag=TRUE)

#### Chord
#transform (normalize) data before calculating Euclidean distances to calculate chord distances
vegdist(decostand(bmiDF[,-1],"norm"),method="euclidean",diag=TRUE)

#### Species profiles
vegdist(decostand(bmiDF[,-1],"total"),method="euclidean",diag=TRUE)

#### Hellinger
vegdist(decostand(bmiDF[,-1],"hellinger"),method="euclidean",diag=TRUE)

#### Chi-squared
vegdist(decostand(bmiDF[,-1],"chi.square"),method="euclidean",diag=TRUE)

#### Bray-Curtis
vegdist(bmiDF[,-1],method="bray",diag=TRUE)


##### PCoA==========================================================================================
#### Create dissimilarity matrix
#using square-roots of B-C distances because non-metric & can produce negative EVs
bmi_bray<-sqrt(vegdist(bmiDF[,-1],method="bray"))


#### Fit PCoA
### Using a weighted PCoA
bmi_pcoa<-wcmdscale(bmi_bray,k=2,eig=TRUE)


### Percent of total inertia captured by (k=) 2 axes
bmi_pcoa$GOF[1]

## Percent inertia captured by first and second axes
100*bmi_pcoa$eig[1:2]/sum(bmi_pcoa$eig)


#### Plot PCoA
### Combine PCoA points with env data
bmi_pcoa_envDF<-bmi_pcoa$points %>%
  bind_cols(
    bmi_envDF %>%
      select(local_site,location,fish_presence:oxygen)
  )

### Create list of inputs for plots
factor_vec<-c("local_site","location","fish_presence","lotic","shore")

factor_vec %>%
  purrr::map(function(x){
    fac<-bmi_pcoa_envDF %>% pull(x) %>% as.factor()
    
    text_legend<-levels(fac)

    levels(fac)<-viridis(length(levels(fac)),end=.85)
    col_vec<-as.character(fac)
    col_legend<-levels(fac)
    
    list(x,col_vec,text_legend,col_legend)
  }) -> fac_list


### Construct plots
par(mfrow=c(1,1))
#local_site
plot(bmi_pcoa,xlab="PCoA axis 1",ylab="PCoA axis 2",type="n")
title(fac_list[[1]][[1]])
points(bmi_pcoa_envDF$Dim1,bmi_pcoa_envDF$Dim2,col=fac_list[[1]][[2]],pch=16,cex=1.25)
legend("bottomright",
       legend=fac_list[[1]][[3]],
       pch=16,
       col=fac_list[[1]][[4]])
#no clear pattern

#location
plot(bmi_pcoa,xlab="PCoA axis 1",ylab="PCoA axis 2",type="n")
title(fac_list[[2]][[1]])
points(bmi_pcoa_envDF$Dim1,bmi_pcoa_envDF$Dim2,col=fac_list[[2]][[2]],pch=16,cex=1.25)
legend("bottomright",
       legend=fac_list[[2]][[3]],
       pch=16,
       col=fac_list[[2]][[4]])
#no clear pattern

#fish_presence
plot(bmi_pcoa,xlab="PCoA axis 1",ylab="PCoA axis 2",type="n")
title(fac_list[[3]][[1]])
points(bmi_pcoa_envDF$Dim1,bmi_pcoa_envDF$Dim2,col=fac_list[[3]][[2]],pch=16,cex=1.25)
legend("bottomright",
       legend=fac_list[[3]][[3]],
       pch=16,
       col=fac_list[[3]][[4]])
#not much of a pattern--fish tend to cluster at PCoA1 from ~ -0.2 to 0.2 and PCoA2 ~ 0.25 but there
  #are a couple 'stray' points

#lotic
plot(bmi_pcoa,xlab="PCoA axis 1",ylab="PCoA axis 2",type="n")
title(fac_list[[4]][[1]])
points(bmi_pcoa_envDF$Dim1,bmi_pcoa_envDF$Dim2,col=fac_list[[4]][[2]],pch=16,cex=1.25)
legend("bottomright",
       legend=fac_list[[4]][[3]],
       pch=16,
       col=fac_list[[4]][[4]])
#again, not much of a pattern...lotic = 1 tends to be in the interior of lotic = 0






##### NMDS==========================================================================================
#### Choose number of dimensions
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


#### Iteratively find lowest stress value
bmi_nmds_default<-smacof::mds(bmi_bray,ndim=2,type="ordinal",ties="primary")
bmi_explore<-smacof::icExplore(bmi_bray,ndim=2,type="ordinal",ties="primary",returnfit=TRUE,
                               verbose=TRUE)
bmi_explore
plot(bmi_explore,main="icExplore BMI data")
which(bmi_explore$stressvec==min(bmi_explore$stressvec))
#36
bmi_explore$stressvec[36]
#this value (.08067672) is nearly identical to the default NMDS solution


#### Compare default and run #36 solutions using Procrustes analysis
smacof::Procrustes(Y=bmi_explore$mdsfit[[36]]$conf,
                   X=bmi_nmds_default$conf)
#congruence coefficient (R value) = 1
#alienation coefficient (1-R^2) = .007
#thus, solutions are nearly identical...will go with default



#### Determine sensitivity of solution to each site value using jackknife (i.e., how different is
  #the final configuration if we leave out each site in turn?)
bmi_jack<-jackmds(bmi_nmds_default)
#stability measure = 0.9966
#cross validity = 0.9999
#both close to 1 --> results do not differ to within rounding error between jackknifed runs
plot(bmi_jack,main="Jackknife plot of bmi data",col.p="black",col.l="red")
#only two sites (1, 14) show significant change when jackknifing

#*these results indicate that the NMDS solution obtained when using the site scores on the first
  #two PCoA axes as starting values is close to or at the best NMDS solution, which is highly
  #stable

#### Shepard diagram
#look at Shepard diagram to see how Bray-Curtis dissimilarities have been converted into Euclidean
  #distances in our 2-dim NMDS ordination
plot(bmi_nmds_default,plot.type="Shepard",xlab="Bray-Curtis dissimilarities",
     ylab="Euclidean distances (NMDS)",pch=16,main="Shepard diagram NMDS of bmi data")
#pattern: roughly a linear relationship from origin until ~0.7 B-C dissim then ~ exponential
#interpretation: pairs of sites with similar genera composition (small B-C dissim) are placed
  #closer together in the NMDS solution while pairs of sites with more dissimilar genera
  #compositions are placed further apart in their NMDS solutions


##### Plot environmental variables in NMDS space
### Combine NMDS points with env data
bmi_nmds_envDF<-bmi_nmds_default$conf %>%
  bind_cols(
    bmi_envDF %>% 
      select(local_site,location,fish_presence:elevTemp)
  )


### Construct plots (using ggplot2)
## As grid
factor_vec %>%
  map(function(fac){
    bmi_nmds_envDF %>%
      ggplot() +
      ggtitle(sym(fac)) +
      geom_point(aes(x=D1,y=D2,color=!!sym(fac)),size=2) +
      scale_color_viridis_d(end=0.8) +
      labs(x="NMDS axis 1",
           y="NMDS axis2") +
      theme_bw(base_size=12) +
      theme(legend.position="bottom") +
      guides(color=guide_legend(title.position="top",
                                title.hjust=0.5,
                                nrow=ifelse(n_distinct(bmi_nmds_envDF[fac])>2,
                                            2,
                                            1)))
  }) %>%
  set_names(factor_vec) %>% 
  plot_grid(plotlist=.,labels=LETTERS[1:5],align="hv") -> bmi_nmds_cat


## Overlay (numerical) env vars for NMDS plots that show "clustering"
# fish_presence
#predict env var given site scores on ordination axes
ef<-envfit(env=bmi_nmds_envDF,ord=bmi_nmds_default$conf)

#create table of ef for report
ef$vectors$arrows %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  bind_cols(
    tibble(r2=ef$vectors$r,
           `Pr(>r)`=ef$vectors$pvals)
  ) %>%
  filter(!str_detect(variable,"^D")) -> ef_table

ef_vars<-as.data.frame(ef$vectors$arrows*sqrt(ef$vectors$r)) %>%
  rownames_to_column(var="variable") %>%
  filter(!variable %in% c("D1","D2")) %>%
  as_tibble()

fp_env_p<-bmi_nmds_envDF %>%
  ggplot() +
  ggtitle("fish_presence") +
  geom_point(aes(x=D1,y=D2,color=fish_presence),size=2) +
  geom_segment(data=ef_vars,aes(x=0,xend=D1,y=0,yend=D2),
               arrow=arrow(ends="last",type="closed",length=unit(0.05,"inches")),
               color="red") +
  geom_text(data=ef_vars,aes(x=D1,y=D2,label=variable),
            nudge_x=0.05,
            nudge_y=0.05,
            color="red") +
  scale_color_viridis_d(end=0.8) +
  labs(x="NMDS axis 1",
       y="NMDS axis2",
       subtitle="env var~ord axes") +
  theme_bw(base_size=12) +
  theme(legend.position="bottom")

fp_env_p

#interpretation:
#as NMDS1 values increase, so does pH and to a lesser extent, oxygen and nitrate, while
  #elevTemp decreases
#as NMDS2 values increase, so does pH and to a lesser degree, elevTemp and oxygen, while 
  #nitrate decreases
#note: only pH is significant per envfit() output
#site scores where fish are present (in upper left) are positively associated with elevTemp &
  #negatively associated with nitrate


#predict site scores given env var
#base plotting
plot(bmi_nmds_default)
ord.on.env.arrows(ordination.site.scores=bmi_nmds_default$conf,
                  env.matrix=bmi_nmds_envDF %>% select(ph_trans:last_col()))

#ggplot
ord_env<-ord.on.env.arrows(ordination.site.scores=bmi_nmds_default$conf,
                  env.matrix=bmi_nmds_envDF %>% select(ph_trans:last_col()))


#tables of test results for report
ord_env[-3] %>% 
  map(function(x){
  x$coefficients %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    mutate(variable=str_remove(variable,"^scale\\(env.matrix\\)"),
           across(!variable,~signif(.x,3)))
  }) %>%
  bind_rows(.id="axis") %>%
  mutate(axis=str_replace(axis,"^axis","NMDS ")) -> ord_env_table


ord_vars<-list(ord_env$axis1$coefficients,ord_env$axis2$coefficients) %>%
  set_names(c("D1","D2")) %>%
  imap(function(x,y){
    x %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      select(variable="rowname",Estimate) %>%
      filter(variable!="(Intercept)") %>%
      mutate(variable=str_remove(variable,"^scale\\(env.matrix\\)")) %>%
      dplyr::rename(!!sym(y):="Estimate")
  }) %>%
  reduce(inner_join)



fp_ord_p<-bmi_nmds_envDF %>%
  ggplot() +
  ggtitle("fish_presence") +
  geom_point(aes(x=D1,y=D2,color=fish_presence),size=2) +
  geom_segment(data=ord_vars,aes(x=0,xend=D1,y=0,yend=D2),
               arrow=arrow(ends="last",type="closed",length=unit(0.05,"inches")),
               color="red") +
  geom_text(data=ord_vars,aes(x=D1,y=D2,label=variable),
            nudge_x=0.04,
            nudge_y=0.04,
            color="red") +
  scale_color_viridis_d(end=0.8) +
  labs(x="NMDS axis 1",
       y="NMDS axis2",
       subtitle="ord axis~env vars") +
  theme_bw(base_size=12) +
  theme(legend.position="bottom")

fp_ord_p

#interpretation: 
#as NMDS1 values increase, so does oxygen, and to a lesser extent, pH and nitrate, while
  #elevTemp decreases
#as NMDS2 values increase, so does pH & elevTemp, while oxygen and nitrate decrease
#note: for NMDS1, pH, oxygen, and elevTemp are significant; no env vars significantly related to
  #NMDS2 but pH is marginally significant
#site scores where fish are present (~ 0, 0.5) are positively associated with elevTemp and negatively
  #associated with oxygen


#plot both
plot_grid(fp_env_p,fp_ord_p)

#interpretation of differences:
#strengths of relationships differed between two types of regressions:
  #elevTemp: grew from 1 to 2
  #pH: decreased from 1 to 2
#nature of relationship:
  #oxygen: no relationship (near origin) to more negatively associated with NMDS1 & 2
#no change: nitrate


# lotic
#predict env var given site scores on ordination axes
l_env_p<-bmi_nmds_envDF %>%
  ggplot() +
  ggtitle("lotic") +
  geom_point(aes(x=D1,y=D2,color=lotic),size=2) +
  geom_segment(data=ef_vars,aes(x=0,xend=D1,y=0,yend=D2),
               arrow=arrow(ends="last",type="closed",length=unit(0.05,"inches")),
               color="red") +
  geom_text(data=ef_vars,aes(x=D1,y=D2,label=variable),
            nudge_x=0.05,
            nudge_y=0.05,
            color="red") +
  scale_color_viridis_d(end=0.8) +
  labs(x="NMDS axis 1",
       y="NMDS axis 2",
       subtitle="env var~ord axes") +
  theme_bw(base_size=12) +
  theme(legend.position="bottom")

l_env_p
  
#interpretation:
#as NMDS1 values increase, so does pH and to a lesser extent oxygen and nitrate, while
  #elevTemp decreases
#as NMDS2 values increase, so does pH and to a lesser extent elevTemp and oxygen, while
  #nitrate decreases
#note: only pH is significant per envfit() output
#sites scores where lotic = 0 (lentic areas) and are ~ 0-0.5, 0 - -0.5 are positively associated
  #with elevTemp and negatively associated with nitrate


#predict site scores given env var
l_ord_p<-bmi_nmds_envDF %>%
  ggplot() +
  ggtitle("lotic") +
  geom_point(aes(x=D1,y=D2,color=lotic),size=2) +
  geom_segment(data=ord_vars,aes(x=0,xend=D1,y=0,yend=D2),
               arrow=arrow(ends="last",type="closed",length=unit(0.05,"inches")),
               color="red") +
  geom_text(data=ord_vars,aes(x=D1,y=D2,label=variable),
            nudge_x=0.04,
            nudge_y=0.04,
            color="red") +
  scale_color_viridis_d(end=0.8) +
  labs(x="NMDS axis 1",
       y="NMDS axis 2",
       subtitle="ord axis~env vars") +
  theme_bw(base_size=12) +
  theme(legend.position="bottom")

l_ord_p

#interpretation:
#as NMDS1 values increase, so does oxygen, and to a lesser extent, pH and nitrate, while
  #elevTemp decreases
#as NMDS2 values increase, so does pH & elevTemp, while oxygen and nitrate decrease
#note: for NMDS1, pH, oxygen, and elevTemp are significant; no env vars significantly related to
  #NMDS2 but pH is marginally significant
#site scores where lotic = 0 (lentic areas) ~ 0-0.5, 0 - -0.5 are positively associated with
  #elevTemp and negatively associated with nitrate


plot_grid(l_env_p,l_ord_p)

#interpretation of differences:
#same as fish_presence








