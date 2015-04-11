library(zoo)
library(RColorBrewer)
library(plotrix)
library(scales)
source("helper.R")


processData<-function(inFile){
  #Process Date
  inFile$period<-as.yearmon(inFile$period)
  
  return (inFile)
}

processAggData<-function(inFile) {

  colPalette<-colorRampPalette(c("red", "yellow", "green"))(n = nrow(inFile))
  Opposite_colPalette<-colorRampPalette(c("green", "yellow", "red"))(n = nrow(inFile))
  
  #Process sensitivity colors
  inFile$smix<-rescale(inFile$sales, c(0,1))
  inFile$apmix<-rescale(inFile$avgprice, c(0,1))
  inFile$gpmix<-rescale(inFile$gp, c(0,1))
  inFile$tmix<-rescale(inFile$traffic, c(0,1))

  inFile$gpCol<-ifelse(inFile$gpsens=="low", "#00FF00", ifelse(inFile$gpsens=="right","#0000FF",ifelse(inFile$gpsens=="high","#FF0000","#BEBEBE")))
  inFile$trCol<-ifelse(inFile$trsens=="not sensitive", "#00FF00", ifelse(inFile$trsens=="sensitive","#FF0000","#BEBEBE"))
  inFile$apCol[order(-inFile$apmix)]<-colPalette
  inFile$tmixCol[order(-inFile$tmix)]<-Opposite_colPalette
  inFile$gpmixCol[order(-inFile$gpmix)]<-Opposite_colPalette
  inFile$smixCol[order(-inFile$smix)]<-Opposite_colPalette
  inFile$yoygcCol[order(-inFile$yoygc)]<-Opposite_colPalette
  inFile$yoygpCol[order(-inFile$yoygp)]<-Opposite_colPalette
  inFile$yoyavgpCol[order(-inFile$yoyavgp)]<-Opposite_colPalette
  inFile$yoysalesCol[order(-inFile$yoysales)]<-Opposite_colPalette
  
  return (inFile)
}

df.latLong<<- processAggData(read.csv(file="data/data.csv", header=TRUE, sep=","))

