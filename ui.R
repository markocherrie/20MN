library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinyBS)
library(gtools)
library(ggplot2)
library(shinyWidgets)
library(shinycssloaders)
library(shinyMobile)


shinyUI(f7Page(
  # header font
  tags$style(type = "text/css", "#map {height: calc(100vh - 160px) !important;}"),
  tags$style(type = "text/css", "#Plot {height: calc(100vh - 160px) !important;}"),
  tags$head(
    tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Roboto+Slab');
                  "))
  ),

  # geolocation
  tags$script('
              $(document).ready(function () {
              navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
              function onError (err) {
              Shiny.onInputChange("geolocation", false);
              }
              
              function onSuccess (position) {
              setTimeout(function () {
              var coords = position.coords;
              console.log(coords.latitude + ", " + coords.longitude);
              Shiny.onInputChange("geolocation", true);
              Shiny.onInputChange("lat", coords.latitude);
              Shiny.onInputChange("long", coords.longitude);
              }, 1100)
              }
              });
              '),
  title = "My Neighbourhood",
  f7TabLayout(
    panels = tagList(
      f7Panel(title = "About", side = "right", theme = "dark", 
              "", 
              effect = "reveal")
    ),
    navbar = f7Navbar(
      title = "My Neighbourhood",
      hairline = TRUE,
      shadow = TRUE,
      leftPanel = F,
      rightPanel = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      #swipeable = TRUE,
      f7Tab(
        tabName = "Tab1",
       # f7Picker(
      #    inputId = "mypicker",
      #    placeholder = "",
      #    label = "",
      #    choices = c('Greenspaces', 'Trees', 'Wild swimming')
      #  ),
        icon = f7Icon("map"),
        active = FALSE,
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "Where are they?",
            withSpinner(leafletOutput("map"))
          )
        )
      ),
      f7Tab(
        tabName = "Tab2",
        icon = f7Icon("info"),
        active = FALSE,
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            title = "How many are there?",
            htmlOutput("stats")
          )
        )
      ),
      f7Tab(
        tabName = "Tab3",
        icon = f7Icon("timelapse"),
        active = FALSE,
      f7Shadow(
          intensity = 10,
          hover = TRUE,
      f7Card(
            title = "How far are they?", 
            imageOutput("Plot")        
          )
        )
      )
    )
  )
)
)
