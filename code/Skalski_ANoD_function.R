Skalski.adonis<-function(dat,PC.axes,Groups){  
  #dat is the data frame containing the PC scores in  columns  
  #given in PC.axes and groups given in columns given in  
  Groups  
    n.axes<-length(PC.axes)  
    ssTreat.x<-ssError.x<-dfTreat.x<-dfError.x<-  
      rep(NA,n.axes)  
    for(i in 1:n.axes){  
      my.mod<-aov(dat[,PC.axes[i]]~dat[,Groups],data=dat)  
      test<-summary(my.mod)  
      ssTreat.x[i] = test[[1]][1,2]  
      ssError.x[i] = test[[1]][2,2]  
      dfTreat.x[i] = test[[1]][1,1]  
      dfError.x[i] = test[[1]][2,1]  }  
    #Add Sums of Square for overall test  
    sumSS.trtmnt<-sum(ssTreat.x)/sum(dfTreat.x)  
    sumSS.error<-sum(ssError.x)/sum(dfError.x)  
    #F-test  
    F.all =sumSS.trtmnt/sumSS.error  
    F.df1 = sum(dfTreat.x)  
    F.df2 = sum(dfError.x)  
    #null probability  
    null.p<-1-pf(F.all,F.df1,F.df2)  
    list(Group.SS=sumSS.trtmnt,Residual.SS=sumSS.error,  
         F.stat=F.all,df1=F.df1,df2=F.df2,null.prob=null.p)  
    } 
  
 





