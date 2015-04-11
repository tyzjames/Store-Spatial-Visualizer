setSensColors<-function(inTable, type){
  #Sets the sensitivity color column
  if (type=="traffic") {
    df.temp<-data.frame(trsens=c("Not Sensitive","Insufficient","Sensitive"), trCol=c("#00FF00","#BEBEBE","#FF0000"), Freq=0)
    df.temp$Freq[df.temp$trsens=="Not Sensitive"]<-inTable$Freq[inTable$Var1=="not sensitive"]
    df.temp$Freq[df.temp$trsens=="Sensitive"]<-inTable$Freq[inTable$Var1=="sensitive"]
    df.temp$Freq[df.temp$trsens=="Insufficient"]<-inTable$Freq[inTable$Var1=="insufficient"]    
  } else if (type=="gp"){
    df.temp<-data.frame(gpsens=c("Low","Right","High","Insufficient"), gpCol=c("#00FF00","#0000FF","#FF0000","#BEBEBE"), Freq=0)
    df.temp$Freq[df.temp$gpsens=="Low"]<-inTable$Freq[inTable$Var1=="low"]
    df.temp$Freq[df.temp$gpsens=="Right"]<-inTable$Freq[inTable$Var1=="right"]
    df.temp$Freq[df.temp$gpsens=="High"]<-inTable$Freq[inTable$Var1=="high"]
    df.temp$Freq[df.temp$gpsens=="Insufficient"]<-inTable$Freq[inTable$Var1=="insufficient"]
  }
 
  return (df.temp)
}