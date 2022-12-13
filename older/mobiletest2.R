#if(interactive()){
library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinyBS)
library(gtools)
library(ggplot2)
library(shinyWidgets)
library(shinyMobile)
library(htmlwidgets)

shinyApp(
  
  ui = f7Page(
    title = "My Neighbourhood",
    f7TabLayout(
      tags$head(
        tags$script(
          "$(function(){
               $('#tapHold').on('taphold', function () {
                 app.dialog.alert('Tap hold fired!');
               });
             });
             "
        )
      ),
      panels = tagList(
        f7Panel(title = "Left Panel", side = "left", theme = "light", effect = "cover"),
      ),
      navbar = f7Navbar(
        title = "My Neighbourhood",
        hairline = FALSE,
        shadow = TRUE,
        leftPanel = T,
        rightPanel = F
      ),
      # tab 1
      f7Tabs(
        animated = FALSE,
        swipeable = TRUE,
        f7Tab(
          tabName = "Tab1",
          icon = f7Icon("map"),
          active = TRUE,
          f7Card(
            title = "Where?",
            leafletOutput("mappy", height = "100%")
          )
        ),
        # tab 2
        f7Tab(
          tabName = "Tab2",
          icon = f7Icon("info"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "How many?",
              htmlOutput("stats")
            )
          )
        ),
        # tab 3
        f7Tab(
          tabName = "Tab3",
          icon = f7Icon("timelapse"),
          active = FALSE,
          f7Shadow(
            intensity = 10,
            hover = TRUE,
            f7Card(
              title = "How far?"
            )
          )
        )
      )
    )
  ),
  
  
  server = function(input, output, session) {
    
    # data --- change this to local authority and nearby ones
    Accesspoint<-readRDS("data/OSgreenspace/data/AccessPoint.rds")
    Site<-readRDS("data/OSgreenspace/data/Site.rds")
    ScotlandComp<-read.csv("data/OSgreenspace/ScotlandFreq.csv")
    
    
    
#observe({
      
    # Create the map
    output$mappy <- renderLeaflet({
        
        leaflet() %>%
          addProviderTiles(providers$Stamen.TonerLite)        %>%
          addScaleBar(position = c("bottomleft"))         %>%
          setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
          onRender(
            "function(el, x) {
          L.control.zoom({
            position:'topright'
          }).addTo(this);
        }")
        
    #  })
      
    })
  })
    