ord.on.env.arrows<-  function(ordination.site.scores,env.matrix,choices=c(1,2),arrow.scale=NA,arrow.col="red"){  
  #adds arrows to 2D plots of ordination scores representing the  regression weights  
  #of multiple regressions of additional variables on each ordination  axis.  #  
  #ordination.site.scores: matrix or data frame of numerical  ordination scores  
  #env.matrix: matrix or data frame of environmental variables  
  #choices: the axes for which the regressions are sought  
  #arrow.scale: the scaling factor to fit arrows onto the ordination  plot  
  #centre the ordination site scores, in case they have not already  been centered  
  ordination.site.scores1<-ordination.site.scores[,choices[1]]-  
    mean(ordination.site.scores[,choices[1]])  
  ordination.site.scores2<-ordination.site.scores[,choices[2]]-  
    mean(ordination.site.scores[,choices[2]])  
  #Now, do multiple regressions and extract regression coefficients  
  lm.axis1<-lm(ordination.site.scores1~scale(env.matrix))  
  lm.axis2<-lm(ordination.site.scores2~scale(env.matrix))  
  axis1<-coefficients(lm.axis1)[-1]  
  axis2<-coefficients(lm.axis2)[-1]  
  var.names<-colnames(env.matrix)  
  n.vars<-dim(env.matrix)[2]  #get range of axis scores and range of regression coefficients  
  range1<-range(ordination.site.scores1)  
  range2<-range(ordination.site.scores2)  
  min.range<-  min(range1[2],abs(range1[1]),range2[2],abs(range2[1])) 
  a1<-max(abs(axis1))  
  a2<-max(abs(axis2))  
  arrow.range<-max(a1,a2)  
  #Try and get a good scaling factor if not specified  
  if(is.na(arrow.scale))arrow.scale<-min.range/arrow.range  
  #add arrows to the pre-existing ordination plot  
  for(i in 1:n.vars){  
    x<-axis1[i]*arrow.scale  
    y<-axis2[i]*arrow.scale  
    arrows(x0=0,y0=0,x1=x,y1=y,col=arrow.col,length=0.1)  
    text(x=x,y=y,labels=var.names[i],col=arrow.col)  }  
  #return the partial regression coefficients and the scaling factor  
  list(axis1=summary(lm.axis1),axis2=summary(lm.axis2),arrow.scale=arrow.scale)  
  } 
   