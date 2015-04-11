library(leaflet)
library(shiny)
library(ShinyDash)

shinyUI(
  fluidPage(
    div(class="outer",
        tags$head(
          includeCSS("styles.css")
        ),
        leafletMap(
          "map", width="100%", height="100%",
          initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
          options=list(
            center = c(-29.05, 141.8),
            zoom = 4,
            maxBounds = list(list(10,95), list(-60,170))
          )
        ),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                      width = 300, height = "auto",
                      h3("Map Explorer", align="center"),
                      selectInput(inputId="select.color", label="Select Color Indicator",
                                  choices=c("Traffic Sens","Gross Profit Sens","Average Price","Traffic","YOY Traffic","Gross Profit","YOY GP")),
                      selectInput(inputId="select.size", label="Select Size Indicator",
                                  choices=c("Constant","Sales Mix","Gross Profit Mix","Traffic Mix")),
                      numericInput(inputId="set.size", label="Set Size",min=100,max=10000,step=100, value=3000)
        ),
        absolutePanel(id = "controls2", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 60,
                      width = 400, height = 400,
                      plotOutput("myChart")
        )
    )
  )
)