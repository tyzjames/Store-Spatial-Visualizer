library(leaflet)
library(maps)
library(shiny)
library(scales)
library(DT)
source("helper.R")
source("dataload.R")

options(shiny.trace=TRUE)
options("scipen" = 20)

shinyServer(function(input, output, session) {
  
  map<-createLeafletMap(session,"map")
  
  #Get stores that are within the current map boundaries
  storesInBound<-reactive({
    if(is.null(input$map_bounds)){
      return (df.latLong[FALSE,])
    } else{
      bounds<-input$map_bounds
      latRng<-range(bounds$north, bounds$south)
      lngRng<-range(bounds$east, bounds$west)
      ib<-subset(df.latLong, lat>=latRng[1] & lat<=latRng[2] & long >= lngRng[1] & long <= lngRng[2])
    }
  })
  
  output$myChart<- renderPlot({
    stores<-storesInBound()
    #     stores<-df.latLong
    if(nrow(stores)==0) {
      return (NULL)
    } 

    trsens<-data.frame(table(stores$trsens))
    trPlot<-setSensColors(trsens, "traffic")
    
    gpsens<-data.frame(table(stores$gpsens))
    gpPlot<-setSensColors(gpsens, "gp")
    
    par(bg=NA, mfrow=c(2,2),mar=c(4,4,2,3), new=F)
    barplot(trPlot[order(trPlot$trsens),c("Freq")], col=as.character(trPlot[order(trPlot$trsens),c("trCol")]), main="Traffic Sens", axes=F)
    barplot(gpPlot[order(gpPlot$gpsens),c("Freq")], col=as.character(gpPlot[order(gpPlot$gpsens),c("gpCol")]), main="GP Sens", axes=F)
    plot(x=stores$yoygc, y=stores$yoyavgp, col=stores$trCol, axes=T, pch=1, xlab="YOY GC", ylab="YOY Avg Price")
    plot(x=stores$yoygc, y=stores$yoygp,col=stores$gpCol, axes=T, xlab="YOY GC", ylab="YOY GP")
    
  })
  

  output$myTable<- renderDataTable({
    stores<-storesInBound()
    
    if(nrow(stores)==0) {
      return ()
    } 
    x<-data.frame(Stores=stores$store, Sales=dollar(stores$sales), GP=dollar(stores$gp), Traffic=comma(stores$traffic))  
    return(datatable(x, options=list(autoWidth=F, searching=F, paging=F, ordering=F, info=F)))
  })
  
  session$onFlushed(once=TRUE, function() {
    paintObs<-observe({
      add.sens.markers()    
    })
    session$onSessionEnded(paintObs$suspend)
  })
  
  add.sens.markers<- reactive ({
    map$clearShapes()
    
    circle.op<-0.3
    rad<-input$set.size
    #Add Sensitivity Colors
    if (input$select.color == "Traffic Sens") {
      col<-df.latLong$trCol
    } else if (input$select.color == "Gross Profit Sens") {
      col<-df.latLong$gpCol
    } else if (input$select.color == "Average Price") {
      col<-df.latLong$apCol
    } else if (input$select.color == "Traffic") {
      col<-df.latLong$tmixCol
    } else if (input$select.color == "YOY Traffic") {
      col<-df.latLong$yoygcCol
    } else if (input$select.color == "Gross Profit") {
      col<-df.latLong$gpmixCol
    } else if (input$select.color == "YOY GP") {
      col<-df.latLong$yoygpCol
    }
    
    if (input$select.size == "Sales Mix") {
      ssize<-df.latLong$smix*rad
    } else if (input$select.size == "Gross Profit Mix") {
      ssize<-df.latLong$gpmix*rad
    } else if (input$select.size == "Traffic Mix") {
      ssize<-df.latLong$tmix*rad
    } else {
      ssize=0.3*rad
    }
    
    map$addCircle(
      df.latLong$lat,
      df.latLong$long,
      ssize,
      NULL,
      list(stroke=T, fill=TRUE, fillOpacity=circle.op, fillColor=col, color=col, weight=3, clickable=F)
    )
    
    #Pinpoint shop location
    map$addCircle(
      df.latLong$lat,
      df.latLong$long,
      15,
      df.latLong$store,
      list(stroke=F, fill=TRUE, fillOpacity=0.5, fillColor='#000000', clickable=F)
    )  
    
    #Pinpoint clickable shop location 
    map$addCircleMarker(
      df.latLong$lat,
      df.latLong$long,
      15,
      df.latLong$store,
      list(stroke=F, fill=TRUE, fillOpacity=0, fillColor='#000000')
    )  
    
  })
  
  map.showPopUp<-function(eventType){
    temp.data<-df.latLong[eventType$id==df.latLong$store,]
    content <- as.character(tagList(
      tags$strong(eventType$id),
      tags$br(),
      sprintf("Sales: %s", dollar(temp.data[1,"sales"])),
      tags$br(),
      sprintf("Gross Profit: %s", dollar(temp.data[1,"gp"])),
      tags$br(),
      sprintf("Traffic: %s", comma(temp.data[1,"traffic"])),
      tags$br(),
      sprintf("Avg Price: %s", dollar(temp.data[1,"avgprice"])),
      tags$br(),
      sprintf("YOY Sales: %s", paste(round(temp.data[1,"yoysales"]*100,2),"%",sep="")),
      tags$br(),
      sprintf("YOY GP: %s", paste(round(temp.data[1,"yoygp"]*100,2),"%",sep="")),
      tags$br(),
      sprintf("YOY Traffic: %s", paste(round(temp.data[1,"yoygc"]*100,2),"%",sep=""))
    ))     
    map$showPopup(eventType$lat, eventType$lng, content, layerId=eventType$id)
  }
  
  #Map shape click event
  observe({
    event_mClick<-input$map_marker_click
    if (is.null(event_mClick) || is.null(event_mClick$id)) {
      return ()
    }
    isolate({
      map$clearPopups()
      map.showPopUp(event_mClick)
    })   
  })
  
})
