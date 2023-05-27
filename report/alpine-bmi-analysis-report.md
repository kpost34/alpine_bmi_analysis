---
title: "Alpine Benthic Macroinvertebrate Analysis"
author: "Keith Post"
date: "2023-05-27"
output: 
  html_document:
    fig_caption: true
    keep_md: yes
---




<br>

# **Disclaimer**
***The purpose of this report is to demonstrate exploratory data analysis and multivariate statistical techniques and their associated R tools. It is not intended to make any conclusions about the study and should not be cited for that purpose.***  
<br>
<br>

## I. Introduction
The data used in this report were obtained from the [Environmental Data Initiative repository](www.https://edirepository.org/). These data were collected as part of a biweekly sampling campaign of four alpine lakes (i.e., Green Lakes 1, 4, and 5 and Lake Albion) within Niwot Ridge, CO (Loria and Yevak, 2019). Each lake was sampled at shoreline, inlet, and outlet sites for benthic macroinvertebrates (BMIs), which were identified to order, family, and, when possible, genus. Six continuous environmental variables were measured: 

* *DO*: dissolved oxygen (mg/l) 
* *elevation*: elevation (m) 
* *nitrate*: nitrate (mg/l) 
* *ph*: pH 
* *sat*: dissolved oxygen saturation (%) 
* *temp*: water temperature (^o^C) 

Five categorical environmental variables were also tracked:

* *fish_presence*: absent (0) or present (1)
* *local_site*: local site within the study area (ALB, GL1, GL4, or GL5)
* *location*: location within water body (LAKE, INLET, OUTLET, WATERFALL, CREEK)
* *lotic*: lentic (0) or present (1)
* *shore*: shore characteristics (grass, grass rock, talus, willow)

The BMI and environmental data were wrangled, explored, and visualized before analysis using two multivariate techniques: principal components analysis (PCA) and non-metric multidimensional scaling (NMDS). 
<br>
<br>
<br>

## II. Data munging
* Data sources 
  + glv_macroinvert_abund18.pj.data.csv (abundance data)
  + glv_macroinvert_trait18.pj.data.csv (trait data that also contains abundance data)
  + glv_macroinvert_sitequality18.pj.data.csv (environmental data)
* Data discrepancies & usage
  + Abundance data file does not have ALB creek-belowroad data and other abundances differ between that file and the trait file
  + Abundances extracted from trait data were used for this analysis
* Environmental data problems
  + GL4 waterfall high and low were not distinguished and *project_site* "ALB_5IN" was classified as location = "LAKE"
  + Corrected both errors
* Data reduction
  + Filtered abundance and environmental data to include samples from only 7/31/18 - 8/9/18
  + Removed genus information: taxon information to order and family retained
  + Removed sites with missing environmental data
  + Removed taxa (order-family) with no abundance data
  + Removed all specimens not classified to order (= 3 specimens)
* Data set used in multivariate analysis
  + 22 sites (*project_site*s)
  + 2353 BMIs
  + 6 BMI orders
  + 13 BMI families
<br>
<br>
<br>

## III. EDA
### A. Taxonomic data
The study area was dipteran dominant; this order (primarily chironomids) made up over 81% of all BMIs (Fig. 1). Caddisflies composed just over 9% of individuals, and the third most abundant order were freshwater leeches (order Rhynchobdellida) with nearly 5% of all specimens and a total abundance of 117 individuals.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/ordinal counts-1.png" alt="*Figure 1. Total numbers of benthic macroinvertebrates (BMIs) collected by order.*"  />
<p class="caption">*Figure 1. Total numbers of benthic macroinvertebrates (BMIs) collected by order.*</p>
</div>
<br>

BMIs were most commonly found in creek sites followed by inlet and waterfall locations; each *location* averaged more than 200 individuals (Fig. 2A). Lake and outlet sites had far fewer BMIs with neither location averaging more than 60 specimens. BMI abundance varied by *local_site* with ALB as the most abundant (> 140 individuals/site) and GL1, the least abundant (Fig. 2B).

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/location and local_site counts-1.png" alt="*Figure 2. Average numbers of BMIs collected by* project_site *and either* location *or* local_site."  />
<p class="caption">*Figure 2. Average numbers of BMIs collected by* project_site *and either* location *or* local_site.</p>
</div>
<br>
<br>

### B. Environmental data
#### 1. Categorical-numerical
Figure 3 illustrates how the various numerical environmental variables (including the binary *fish_presence* and *lotic* variables) varied among *local_site*s. 

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/mean env var by local_site-1.png" alt="*Figure 3. Average value of environmental variable by* local_site *(lake).*"  />
<p class="caption">*Figure 3. Average value of environmental variable by* local_site *(lake).*</p>
</div>
<br>

Clearly no fish were found in GL4 and GL5, and GL1 comprised only lentic sites. *DO* and *sat* were much greater, on average, in GL5 sites compared to the other *local_site*s. GL5 sites were located at the highest elevations and unsurprisingly had the lowest temperatures (*temp*), *nitrate*, on average, was much greater in ALB and GL5 sites relative to GL1 and GL4 sites.
<br>

#### 2. Numerical-numerical
The multi-panel graph shows the pairwise relationships among all numerical (i.e., non-binary, non-categorical) environmental variables with linear regression lines, correlations, and indicators of significance. As expected, *DO* and *sat* were strongly, positively correlated per their tight regression line and *r* > 0.96 (Fig. 4). *temp* was significantly, negatively correlated with *elevation*, *sat*, *DO*, and *nitrate*. *elevation* was significantly, positively correlated with *sat* and *DO*.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/scatterplots with reg-1.png" alt="*Figure 4. Pairwise relationship for all six continuous environmental variables are drawn. The lower left half of the matrix contains all pairwise scatterplots with regression lines. The upper right half comprises correlations and, if applicable, significance stars. Density plots of each individual variable are constructed along the diagonal.*"  />
<p class="caption">*Figure 4. Pairwise relationship for all six continuous environmental variables are drawn. The lower left half of the matrix contains all pairwise scatterplots with regression lines. The upper right half comprises correlations and, if applicable, significance stars. Density plots of each individual variable are constructed along the diagonal.*</p>
</div>
<br>

Scatter plots conditioned on lotic status (i.e., 0 = lentic and 1 = lotic) were visualized (Fig. 5). A few of the significant correlations discussed in the previous figure are inconsistent between *lotic* categories. For instance, *temp*, *sat*, and *DO* were significantly correlated with *elevation* (*temp* negatively, and the other two variables positively); however, when conditioned by lotic status, these relationships strengthen when the locations were lentic but were non-significant when lotic. Other patterns occurred with lotic status as a conditioning variable. For example, *ph* and *sat* were weakly correlated (*r* = 0.149) but when lotic status was considered, these relationships strengthened and became significant for both lotic (*r* = 0.722) and lentic (*r* = 0.588) locations.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/scatterplots by lotic with reg-1.png" alt="*Figure 5. Pairwise relationship for all six continuous environmental variables are drawn and distinguished according to lotic status: lentic (lotic = 0) is colored in red and lotic (lotic=1) is colored in green. The lower left half of the matrix contains all pairwise scatterplots with regression lines according to lotic status. The upper right half comprises correlations and, if applicable, significance stars for all points and by lotic status. Density plots of each individual variable according to lotic status are constructed along the diagonal.*"  />
<p class="caption">*Figure 5. Pairwise relationship for all six continuous environmental variables are drawn and distinguished according to lotic status: lentic (lotic = 0) is colored in red and lotic (lotic=1) is colored in green. The lower left half of the matrix contains all pairwise scatterplots with regression lines according to lotic status. The upper right half comprises correlations and, if applicable, significance stars for all points and by lotic status. Density plots of each individual variable according to lotic status are constructed along the diagonal.*</p>
</div>
<br>

Pairwise relationships between BMI abundances and the six numeric environmental variables were visualized as part of the exploratory data analysis (Fig. 6). 

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/scatterplots env count with reg-1.png" alt="*Figure 6. Scatterplots with regression lines of each numeric environmental variable regressed against total BMI abudnance.*"  />
<p class="caption">*Figure 6. Scatterplots with regression lines of each numeric environmental variable regressed against total BMI abudnance.*</p>
</div>
<br>

Only *ph* was found to be significant, with an *r* = -0.58.
<br>
<br>
<br>

## IV. Principal Components Analysis (PCA)
### A. Test Assumptions
There are two assumptions of PCA per Shipley (2021): 1) normally (or at least symmetrically) distributed variables and 2) linear relationships among variables.
<br>

#### 1. Distribution
Distributions of variables were assessed visually and statistically. First, a series of Q-Q plots were constructed (Fig. 7), which showed that variables *ph* and *nitrate* were clearly not normally distributed. 

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/qqplots untransformed-1.png" alt="*Figure 7. Q-Q plots of each untransformed numeric enviornmental variable are presented.*"  />
<p class="caption">*Figure 7. Q-Q plots of each untransformed numeric enviornmental variable are presented.*</p>
</div>

These results were supported by Shapiro tests.
<br>


```{=html}
<div id="htmlwidget-b7a91b6a885d5a5eb9c8" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b7a91b6a885d5a5eb9c8">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 1. Shapiro test results for untransformed numeric environmental variables.<\/caption>","data":[["DO","elevation","nitrate","ph","sat","temp"],[0.889,0.911,0.867,0.899,0.955,0.836],[0.0181,0.0504,0.00698,0.0281,0.397,0.00197],["Yes","No","Yes","Yes","No","Yes"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>variable<\/th>\n      <th>statistic<\/th>\n      <th>p<\/th>\n      <th>signif<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[1,2]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
<br>

Only the Q-Q plot for *sat* displayed normality, which was supported by the Shapiro test result. The Q-Q plot for *elevation* was borderline non-signifcant, and the remaining four variables were clearly non-normally distributed, particularly *nitrate* and *temp*, which had highly significant (*p* < .01) Shapiro test results.

Five of the six environmental variables underwent a Box-Cox transformation (i.e., only *sat* was left untransformed), and the Q-Q plots and Shapiro tests were re-run.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/qqplots box-cox transform-1.png" alt="*Figure 8. Q-Q plots of six numeric environmental variables are presented. All variables but* sat *were Box-Cox transformed.*"  />
<p class="caption">*Figure 8. Q-Q plots of six numeric environmental variables are presented. All variables but* sat *were Box-Cox transformed.*</p>
</div>


```{=html}
<div id="htmlwidget-b30844a9d023c524292f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-b30844a9d023c524292f">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 2. Shapiro test results for five Box-Cox transformed environmental \n                                                variables and dissolved oxygen saturation<\/caption>","data":[["DO_trans","elevation_trans","nitrate_trans","ph_trans","sat","temp_trans"],[0.974,0.912,0.883,0.901,0.955,0.917],[0.801,0.0526,0.014,0.0313,0.397,0.0666],["No","No","Yes","Yes","No","No"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>variable<\/th>\n      <th>statistic<\/th>\n      <th>p<\/th>\n      <th>signif<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[1,2]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

These results (Fig. 8; Table 2) indicate considerable improvement in the distributions of *DO_trans* and *temp_trans* and a slight improvement for *elevation_trans* (note: transformed variables will be indicated with "[*variable name*]*_trans*). However, *ph_trans* and *nitrate_trans* were still non-normal following Box-Cox transformations. Additional transformations (i.e., log, square-root, inverse) were calculated for these two variables, and none could achieve normality. Given that only a symmetrical distribution is needed for a PCA, symmetry was assessed by measuring skewness and running a Jarque-Bera Test, which measures whether data have skewness and kurtosis similar to a normal distribution.


```
##      ph_trans nitrate_trans 
##   -0.03295884   -0.05704655
```

```{=html}
<div id="htmlwidget-5d6aafa621af701577c5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-5d6aafa621af701577c5">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 3. Jarque-Bera test results of Box-Cox-transformed pH and nitrate<\/caption>","data":[["ph_trans","nitrate_trans"],[0.345,2.35],[0.842,0.309]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>variable<\/th>\n      <th>statistic<\/th>\n      <th>p.value<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[1,2]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
<br>

These results (Table 3) indicate that both *ph_trans* and *nitrate_trans* that underwent Box-Cox transformations had symmetrical distributions as neither test result was significant. 

All numerical environmental variables were retained. All variables but *sat* were Box-Cox transformed.
<br>

#### 2. Linear relationships
The second assumption of PCA is that the variables have linear relationships (Shipley, 2021). This was assessed visually using scatter plots and regression lines and quantitatively via correlations.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/scatter box-cox transform-1.png" alt="*Figure 9. Pairwise relationship for all six continuous environmental variables are drawn. All variables but* sat *have undergone Box-Cox transformation. The lower left half of the matrix contains all pairwise scatterplots with regression lines. The upper right half comprises correlations and, if applicable, significance stars. Density plots of each individual variable are constructed along the diagonal.*"  />
<p class="caption">*Figure 9. Pairwise relationship for all six continuous environmental variables are drawn. All variables but* sat *have undergone Box-Cox transformation. The lower left half of the matrix contains all pairwise scatterplots with regression lines. The upper right half comprises correlations and, if applicable, significance stars. Density plots of each individual variable are constructed along the diagonal.*</p>
</div>
<br>

Aside from a few variable pairs involving *nitrate_trans* (e.g., *sat*-*nitrate_trans*, *elevation_trans*-*nitrate_trans*, *DO_trans*-*nitrate_trans*), there tended to be linear relationships among pairs of variables. In the *nitrate_trans* scatter plots, there did not appear to be a linear (or even non-linear) relationship with *sat*, *elevation_trans*, or *DO_trans*, which was supported by weak correlations (-0.15 < *r* < 0.15).

Given that *nitrate_trans* does not have linear relationships with all other environmental variables, it was dropped from the PCA.
<br>
<br>

### B. Analysis
#### 1. Initial results
A PCA was run on the five remaining continuous environmental variables (i.e., *sat_trans*, *elevation_trans*, *temp_trans*, *DO_trans* and *ph_trans*), and all variables but *sat* were transformed.


```{=html}
<div id="htmlwidget-be8dde439032f978cdc0" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-be8dde439032f978cdc0">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 4. Summary of principal components analysis (PCA) of numeric environmental variables.<\/caption>","data":[["Standard deviation","Proportion of Variance","Cumulative Proportion"],[1.72,0.589,0.589],[1.06,0.224,0.813],[0.868,0.151,0.964],[0.39,0.0305,0.994],[0.17,0.00579,1]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>variable<\/th>\n      <th>PC1<\/th>\n      <th>PC2<\/th>\n      <th>PC3<\/th>\n      <th>PC4<\/th>\n      <th>PC5<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

```{=html}
<div id="htmlwidget-764825337fb617dc73b2" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-764825337fb617dc73b2">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 5. PCA loadings of enviromental variables on five principal components<\/caption>","data":[["sat","elevation_trans","temp_trans","DO_trans","ph_trans"],[-0.53,-0.463,0.474,0.517,-0.114],[-0.085,0.285,0.254,0.136,0.91],[0.443,-0.536,0.522,-0.474,0.135],[-0.149,0.626,0.634,-0.25,-0.349],[-0.702,-0.157,-0.193,-0.653,0.135]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>variable<\/th>\n      <th>PC1<\/th>\n      <th>PC2<\/th>\n      <th>PC3<\/th>\n      <th>PC4<\/th>\n      <th>PC5<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
<br>

The PCA summary indicates that that the first two principal components (PCs) accounted for ~81% of the total variance. The loadings show that *sat* and *elevation_trans* were moderately negatively correlated and that *temp_trans* and *DO_trans* were moderately positively correlated with PC1, while *ph_trans* was strongly positively correlated with PC2.
<br>

#### 2. Scree plot
A scree plot was drawn to determine which PCs to plot.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/pca scree plot-1.png" alt="*Figure 10. The scree plot of the PCA is presented. The five principal components are on the x-axis and their inertia values (i.e., eigenvalues) are on the y-axis. A broken stick model is overlaid on top of the scree plot.*"  />
<p class="caption">*Figure 10. The scree plot of the PCA is presented. The five principal components are on the x-axis and their inertia values (i.e., eigenvalues) are on the y-axis. A broken stick model is overlaid on top of the scree plot.*</p>
</div>
<br>

This plot shows that 1) the largest drop in eigenvalues occurred from PC1-PC2; 2) PC1 and PC2 were above the mean eigenvalue; and 3) only PC1 was above the broken stick model. 
<br>

#### 3. Biplot
Given that PC2 was above the mean eigenvalue and that it accounted for ~22% of the total variance, a distance-preserving biplot of PC1 and PC2 from the alpine lakes BMI data was constructed.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/pca biplot ungrouped-1.png" alt="*Figure 11. A biplot of the alpine lake sites is drawn on the first two principal components. The environmental variables are overlaid.*"  />
<p class="caption">*Figure 11. A biplot of the alpine lake sites is drawn on the first two principal components. The environmental variables are overlaid.*</p>
</div>
<br>

The biplot of sites on PC1 and PC2 axes reflect the loadings described above. *sat* and *elevation_trans* were moderately negatively correlated and *temp_trans* and *DO_trans* were moderately positively correlated with PC1, while *ph_trans* was strongly positively correlated with PC2. There appear to be four groups of sites: 1) low elevation and high pH, 2) near the origin, 3) high temperature and pH, and 4) low pH.
<br>

#### 4. Categorical variables
Categorical variables were used to distinguish sites on biplots. *fish_presence*, *lotic*, and *local_site* are displayed in Figure 12.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/pca biplot grouped-1.png" alt="*Figure 12. Biplots of the alpine lake sites are drawn on the first two principal components with environmental variables overlaid. Site locations are colored by A)* fish_presence, *B)* lotic, or *C)* local_site *status.*"  />
<p class="caption">*Figure 12. Biplots of the alpine lake sites are drawn on the first two principal components with environmental variables overlaid. Site locations are colored by A)* fish_presence, *B)* lotic, or *C)* local_site *status.*</p>
</div>
<br>

These biplots indicate strong discrimination among sites according to categorical variables. For example, *fish_presence* tended to diverge along PC1. Sites with fish appear to have higher temperatures and dissolved oxygen levels, while sites without fish tend to have higher dissolved oxygen saturation and occur at greater elevation.
<br>

#### 5. Statistical analysis
The PC scores were tested for differences among groups of categorical variables using ANOVAs. First, an omnibus test was run to determine whether differences occurred regardless of PC axis. If a significant test resulted, a by-axis test was conducted to ascertain where the differences occurred with respect to each PC axis.


```{=html}
<div id="htmlwidget-0becbfeae246e6d7d1d6" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-0becbfeae246e6d7d1d6">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 6. Omnibus ANOVA results for all categorical variables<\/caption>","data":[["local_site","fish_presence","lotic","location","shore"],[11.3,17.3,8.61,2.82,1.75],[0.481,1.27,1.7,1.85,2.08],[23.6,13.6,5.05,1.53,0.839],[6,2,2,8,6],[36,40,40,34,36],[4.2e-11,3.05e-05,0.0111,0.185,0.548]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>variable<\/th>\n      <th>Group.SS<\/th>\n      <th>Residual.SS<\/th>\n      <th>F.stat<\/th>\n      <th>df1<\/th>\n      <th>df2<\/th>\n      <th>null.prob<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
<br>

The omnibus test results indicated that there were significant differences among sites according to categorical variables *local_site*, *fish_presence*, and *lotic* but not for *location* or *shore* (Table 6). Thus, by-axis tests were conducted on the former three variables.


```{=html}
<div id="htmlwidget-35357be9b789536917fd" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-35357be9b789536917fd">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 7. By-axis ANOVA results for significant categorical variables \n                                               from omnibus test<\/caption>","data":[["PC1","PC2","PC1","PC2","PC1","PC2"],["local_site","local_site","fish_presence","fish_presence","lotic","lotic"],[55.2,12.9,34.4,0.262,4.9,12.3],[6.63,10.7,27.5,23.3,56.9,11.2],[3,3,1,1,1,1],[18,18,20,20,20,20],[49.9,7.22,25,0.225,1.72,22],[6.3e-09,0.002,6.87e-05,0.64,0.204,0.000142],["*","*","*","","","*"],[0.893,0.546,0.556,0.011,0.079,0.523]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>PC<\/th>\n      <th>Effect<\/th>\n      <th>SSn<\/th>\n      <th>SSd<\/th>\n      <th>DFn<\/th>\n      <th>DFd<\/th>\n      <th>F<\/th>\n      <th>p<\/th>\n      <th>p&lt;.05<\/th>\n      <th>ges<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,9]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
<br>

The by-axis test results indicated that site scores differed significantly among *local_site*s along both PC1 and PC2 (Table 7). This result was supported by the biplot, which shows clear clusters that form according to *local_site* groups along both PC axes. Sites differed significantly between *fish_presence* levels along PC1 only, which was corroborated by the biplot. Specifically, sites without fish (i.e., fish_presence = 0) tended to have negative PC1 values, while sites with fish had positive values. This suggested that sites without fish tended to occur at greater elevation and have higher dissolved oxygen saturation levels, while sites with fish were driven by warmer temperatures and higher dissolved oxygen levels. Furthermore, site scores along PC2 encompass a wide range of values regardless of *fish_presence* category, suggesting that *ph_trans* was a less important driver. Finally, site score patterns according to *lotic* class appeared to have an opposite pattern relative to *fish_presence* status. Here discrimination among sites by *lotic* group was more apparent along PC2 with lentic sites tending to have positive PC2 scores and lotic sites with more negative PC2 scores. Separation along PC1 was not apparent. This suggested that, on average, lentic sites had a greater pH than lotic sites.
<br>
<br>
<br>

## V. Non-metric Multidimensional Scaling (NMDS)
### A. Highly correlated predictors
The purpose of an NMDS analysis is to ordinate the sites in rank order of their taxonomic composition. Secondarily, environmental information can be added to NMDS biplots to further elucidate relationships among sites with respect to, in this case, their BMI communities. This is achieved through regressions between environmental variables and ordination axes and vice versa. However, highly collinear environmental variables (identified via pairwise correlations) can cause unstable regression coefficients when developing these models (Shipley, 2021). In these cases, PCA is performed on the subset of (i.e., two or more) variables that are strongly correlated. 


```{=html}
<div id="htmlwidget-a281b91ce88bdd496b2a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-a281b91ce88bdd496b2a">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 8. Pearson correlations of all numeric environmental variables<\/caption>","data":[["sat","elevation_trans","temp_trans","DO_trans","ph_trans","nitrate_trans"],[1,0.505,-0.599,-0.959,0.141,0.109],[0.505,1,-0.714,-0.491,0.358,-0.0892],[-0.599,-0.714,1,0.553,0.119,0.532],[-0.959,-0.491,0.553,1,-0.0719,-0.143],[0.141,0.358,0.119,-0.0719,1,0.194],[0.109,-0.0892,0.532,-0.143,0.194,1]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>sat<\/th>\n      <th>elevation_trans<\/th>\n      <th>temp_trans<\/th>\n      <th>DO_trans<\/th>\n      <th>ph_trans<\/th>\n      <th>nitrate_trans<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
<br>

This correlation matrix (Table 10) shows two pairs of variables that are highly correlated: *sat*-*DO* and *elevation*-*temp*. PCAs were run on each of these variable pairs [*analysis not shown*]. The variable pairs were renamed to *oxygen*, and *elevTemp*, respectively, and their site scores from PC1 were used as their values for NMDS analysis.
<br>
<br>

### B. Distance metric
The first step of an NMDS analysis is to choose the type of distance metric. The square-roots of Bray-Curtis dissimilarities were chosen. Bray-Curtis dissimilarities are non-metric but can produce negative eigenvalues; thus, their square roots were used for this analysis (Shipley, 2021). 
<br>
<br>

### C. Number of dimensions
The second step of an NMDS analysis is to choose the number of dimensions. A series of NMDS models are fit onto the site scores of the first two principal coordinates analysis (PCoA) axes while successively increasing the number of dimensions. Stress levels are observed as the number of dimensions (axes) increase using a scree plot.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/nmds stress plot-1.png" alt="*Figure 13. The scree plot for a non-metric multidimensional scaling (NMDS) analysis of the alpine lake BMI data has the number of dimensions on the x-axis and the corresponding stress levels on the y-axis.*"  />
<p class="caption">*Figure 13. The scree plot for a non-metric multidimensional scaling (NMDS) analysis of the alpine lake BMI data has the number of dimensions on the x-axis and the corresponding stress levels on the y-axis.*</p>
</div>
<br>

This plot (Fig. 13) shows that there was a large decrease in stress from one to two dimensions before more gradual decline in stress levels. The stress level at two dimensions was 0.0807. This pattern suggests using two dimensions for the NMDS analysis.
<br>
<br>

### D. Sensitivity of solution
The sensitivity of solutions for each site value was determined using a jackknife approach (i.e., assess differences in final configuration if each site was left out).


```
## 
## Call: jackmds.smacofB(object = bmi_nmds_default)
## 
## SMACOF Jackknife
## Number of objects: 22 
## Value loss function: 1.2808 
## Number of iterations: 17 
## 
## Stability measure: 0.9966 
## Cross validity: 0.9999 
## Dispersion: 0.0035
```

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/nmds sensitivity plot-1.png" alt="*Figure 14. A jacknife plot of BMI data is presented along the first two NMDS axes.*"  />
<p class="caption">*Figure 14. A jacknife plot of BMI data is presented along the first two NMDS axes.*</p>
</div>
<br>

The stability of measure and cross validity values were very close to 1, indicating that they do not differ within rounding error between jackknifed runs. Only two sites (i.e., 1, 14) show significant change when jackknifing (Fig. 14). These results indicate that the NMDS solution obtained when using the site scores on the first two PCoA axes as starting values is close to or at the best NMDS solution, which is highly stable (Shipley, 2021). 
<br>
<br>

### E. Shepard diagram
A Shepard diagram was constructed to see how Bray-Curtis dissimilarities have been converted into Euclidean distances in the two-dimensional NMDS ordination.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/nmds shepard-1.png" alt="*Figure 15. A Shepard plot of Bray-Curtis dissimilarities (x-axis) and Euclidean distances (y-axis) of alpine lake BMI data is shown above.*"  />
<p class="caption">*Figure 15. A Shepard plot of Bray-Curtis dissimilarities (x-axis) and Euclidean distances (y-axis) of alpine lake BMI data is shown above.*</p>
</div>
<br>

The Shepard diagram shows that there was roughly a linear relationship from the origin until ~0.7 Bray-Curtis dissimilarities before transitioning to an approximately exponential relationship (Fig. 15). This pattern indicated that pairs of sites with similar familial composition of BMI (i.e., small Bray-Curtis dissimilarities) were placed closer together in the NMDS solution, while pairs of sites with more dissimilar familial composition were placed further apart.
<br>
<br>

### F. NMDS Plots
#### 1. Plot categorical environmental variables
Biplots of sites were plotted on NMDS axes 1 and 2, and points were colored by groups within the five categorical variables: *local_site*, *location*, *fish_presence*, *lotic*, and *shore*.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/nmds biplots cat-1.png" alt="*Figure 16. These five NMDS biplots contain the alpine lake sites plotted on the first two NMDS axes. Site locations are colored by different categorical environmental variables per the five panels: A)* local_site, *B)* location, *C)* fish_presence, *D)* lotic, *and E)* shore."  />
<p class="caption">*Figure 16. These five NMDS biplots contain the alpine lake sites plotted on the first two NMDS axes. Site locations are colored by different categorical environmental variables per the five panels: A)* local_site, *B)* location, *C)* fish_presence, *D)* lotic, *and E)* shore.</p>
</div>
<br>

Out of the five categorical variables, only the two binary variables--*fish_presence* and *lotic*--showed discrimination of sites along NMDS axes 1 and 2. Thus, numerical environmental variables were drawn onto their biplots to improve analysis and interpretation. 
<br>

#### 2. Relationships between ordination axes and environmental variables
Numerical variables can be related to ordination axes in two ways: 1) predict environmental variables given their site scores on ordination axes and 2) predict site scores on ordination axes given their environmental variables. 
<br>

##### a. fish_presence
The NMDS plot was constructed with sites color coded by *fish_presence* category and environmental variables overlain either as dependent variables or predictors.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/nmds biplots fp cat num-1.png" alt="*Figure 17. NMDS biplots of the alpine lake sites with points colored by* fish_presence *status are drawn. Environmental variables individually regressed on the first two NMDS axes are presented in (A), while each NMDS axis regressed on environmental variables are presented in (B).*"  />
<p class="caption">*Figure 17. NMDS biplots of the alpine lake sites with points colored by* fish_presence *status are drawn. Environmental variables individually regressed on the first two NMDS axes are presented in (A), while each NMDS axis regressed on environmental variables are presented in (B).*</p>
</div>
<br>

Fig. 17A shows that as NMDS1 values increase, so does *ph_trans* and to a lesser extent, *oxygen* and *nitrate_trans*, while *elevTemp* decreases. As NMDS2 values increase, so does *ph_trans* and to a smaller degree, *elevTemp* and *oxygen*, while *nitrate_trans* decreases. Site scores where fish were present (in upper left) were positively associated with *elevTemp* and negatively associated with *nitrate_trans*. 



```{=html}
<div id="htmlwidget-d8ab710b363271d4aabc" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-d8ab710b363271d4aabc">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 9. Significance test results of multiple regression models where \n                                                each environmental variable is regressed on both NMDS axes<\/caption>","data":[["ph_trans","nitrate_trans","oxygen","elevTemp"],[0.739,0.415,0.975,-0.988],[0.674,-0.91,0.221,0.154],[0.357,0.0301,0.00799,0.117],[0.016,0.743,0.927,0.302]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>variable<\/th>\n      <th>D1<\/th>\n      <th>D2<\/th>\n      <th>r2<\/th>\n      <th>Pr(&gt;r)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[1,2,3,4]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
<br>

Statistical analysis of these relationships shows that only *ph* was significant (Table 9).

Fig. 17B shows that as NMDS1 values increase, so does *oxygen*, and to a lesser extent, *ph_trans* and *nitrate_trans*, while *elevTemp* decreases. As NMDS2 values increase, so does *ph_trans* and *elevTemp*, while *oxygen* and *nitrate_trans* decrease. Site scores where fish were present (~ 0, 0.5) were positively associated with *elevTemp* and negatively associated with *oxygen*.

Statistical analyses were run on the regressions of NMDS axes 1 and 2 on environmental variables. 


```{=html}
<div id="htmlwidget-6f76e1df23193844c723" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-6f76e1df23193844c723">{"x":{"filter":"none","vertical":false,"caption":"<caption style=\"color:black; font-style: italic\">Table 10. Significance test results of multiple regression models where each \n                                                NMDS axis is regressed on all numeric environmental variables<\/caption>","data":[["NMDS 1","NMDS 1","NMDS 1","NMDS 1","NMDS 1","NMDS 2","NMDS 2","NMDS 2","NMDS 2","NMDS 2"],["(Intercept)","ph_trans","nitrate_trans","oxygen","elevTemp","(Intercept)","ph_trans","nitrate_trans","oxygen","elevTemp"],[7.26e-17,0.214,0.204,0.357,-0.441,-1.23e-16,0.181,-0.16,-0.083,0.149],[0.0898,0.0961,0.112,0.126,0.135,0.0899,0.0962,0.112,0.126,0.135],[8.09e-16,2.23,1.83,2.84,-3.27,-1.36e-15,1.88,-1.43,-0.66,1.1],[1,0.0396,0.085,0.0113,0.00453,1,0.0771,0.171,0.518,0.286]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>axis<\/th>\n      <th>variable<\/th>\n      <th>Estimate<\/th>\n      <th>Std. Error<\/th>\n      <th>t value<\/th>\n      <th>Pr(&gt;|t|)<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"dom":"t","columnDefs":[{"className":"dt-right","targets":[2,3,4,5]}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```
<br>

These results indicated that for NMDS1, *ph_trans*, *oxygen*, and *elevTemp* were significant predictors (Table 10). Regarding NMDS2, no partial regression coefficients of environmental variables were significant, although *ph_trans* was marginally significant (*p* < .078).
<br>

##### b. lotic
A second set of biplots with numerical environmental variables was constructed for site scores colored by lotic category.

<div class="figure" style="text-align: center">
<img src="alpine-bmi-analysis-report_files/figure-html/nmds biplots lotic cat num-1.png" alt="*Figure 18. NMDS biplots of the alpine lake sites with points colored by* fish_presence *status are drawn. Environmental variables individually regressed on the first two NMDS axes are presented in (A), while each NMDS axis regressed on environmental variables are presented in (B).*"  />
<p class="caption">*Figure 18. NMDS biplots of the alpine lake sites with points colored by* fish_presence *status are drawn. Environmental variables individually regressed on the first two NMDS axes are presented in (A), while each NMDS axis regressed on environmental variables are presented in (B).*</p>
</div>
<br>

Interpretation of arrows and their statistical significance is the same for these plots as they were for Fig. 17. However, Fig. 18 shows that a cluster of lentic sites (i.e., where lotic = 0) were positively associated with *elevTemp* and negatively associated with *nitrate_trans* and that a cluster of *lotic* sites were positively associated with *elevTemp* and negatively associated with *ph_trans*.
<br>
<br>
<br>

## VI. Summary 
### A. Exploratory data analysis
The sample analyzed for this report comprised 2,353 BMIs and associated environmental data collected from 22 sites in four alpine lakes in Niwot Ridge, CO, from 7/31 - 8/19/18. Dipterans were by far the most populous order; they composed more than 80% of all individuals collected and no other order exceeded 10% of total BMI abundance. Green Lake 5 had, on average, the highest *DO* and *sat* relative to the other four *local_site*s. Fish were present only in Lake Albion and Green Lake 1, and the latter was the only *local_site* without a lotic location. *temp* was greatest, on average, in GL1 sites and lowest in GL5 sites. Unsurprisingly, *sat* and *DO* were strongly positively correlated. Both of these variables, *elevation*, and *nitrate* were each significantly negatively correlated with *temp*. *DO* and *sat* were each significantly positively correlated with *elevation*. When correlation analyses were performed separately on BMI abundances and the six continuous environmental variables, only *ph* was a significant correlate.
<br>
<br>

### B. PCA
*nitrate* was dropped, and all remaining continuous environmental variables but *sat* underwent Box-Cox transformations to fulfill the assumptions of PCA. A summary of the PCA indicated that PC1 and PC2 explained roughly 59% and 22%, respectively, of the total variance. *sat* and *elevation_trans* were negatively associated with PC1, while *temp_trans* and *DO_trans* were positively associated with this principal component. *ph_trans* was strongly positively associate with PC2. PC1 and PC2 were selected for visualization and further analysis following interpretation of the Scree plot. The distance-preserving biplot of these data suggested four groups of sites: 1) low elevation and high pH, 2) near the origin (no associations), 3) high temperature and pH, and 4) low pH. After incorporating categorical environmental variables, PC scores significantly varied among *local_site*s and *lotic* and *fish_presence* categories overall. By-axis tests indicated that PC scores varied by *local_site* for both PC1 and PC2, by *fish_presence* category for PC1 only, and by *lotic* category for PC2 only.
<br>
<br>

### C. NMDS
Pearson correlations of all pairs of continuous environmental variables were performed to consolidate highly correlated variable pairs into composite variables; this was necessary for *sat*-*DO*, which became *oxygen*, and *elevation*-*temp*, which became *elevTemp*, as each pair was highly correlated. An NMDS analysis was conducted on square-roots of Bray-Curtis dissimilarities of these data. Biplots of sites were plotted on NMDS axes 1 and 2. After coloring sites by each categorical environmental variable (in separate plots), only *fish_presence* and *lotic* showed discrimination of sites along these two axes. Thus, a second set of plots (two per variable) were constructed where continuous environmental variables were added as arrows, either showing variables regressed on ordination axes or the converse, for both *fish_presence* and *lotic*. When ordination axes were regressed on environmental variables, sites scores with fish present were positively associated with *elevTemp* and negatively associated with *oxygen*. Furthermore, a group of lentic sites was positively correlated with *elevTemp* and negatively associated with *nitrate_trans*, whereas lotic sites were also positively associated with *elevTemp* and were negatively associated with *ph_trans*.
<br>
<br>
<br>

### References

Loria, K. and S. Yevak. 2019. Macroinvertebrate trait, abundance and site quality data from the Green Lakes Valley, 2018 ver 1. Environmental Data Initiative. https://doi.org/10.6073/pasta/038a8ab20a6ffa2044b885c6a999112e (Accessed 2023-04-16).

Shipley, B. 2021. *Ordination methods for biologists: a non-mathematical introduction using R*. Quebec: BS Publishing.

