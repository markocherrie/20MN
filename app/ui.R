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


shinyUI(

f7Page(
    # so you can put in the address using enter
    tags$script(src = "www/enter_button.js"),
    # google analytics
    tags$head(includeHTML(("google-analytics.html"))),
    tags$style(type = "text/css", "#map {height: calc(100vh - 160px) !important;}"),
    tags$style(type = "text/css", "#Plot {height: calc(100vh - 160px) !important;}"),
    tags$head(
    tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Roboto+Slab');
                  "))),
    tags$style(type="text/css", ".item-input {width:200px;
                                background-color: none;}"),
    tags$style(type="text/css", ".button {
                    border: none;
                    color: white;
                    text-align: center;
                    text-decoration: none;
                    display: inline-block;
                    font-size: 16px;
                    width:60px;
                    height:30px;}"),
    tags$style(type="text/css", ".panel-content{
                    padding-left: 15px;
                    padding-right: 15px;}"),
    # geolocation
    # get geolocation from user #
    # tags$script('
    #  $(document).ready(function () {
    #    navigator.geolocation.getCurrentPosition(onSuccess, onError);
    
    #   function onError (err) {
    #    Shiny.onInputChange("geolocation", false);
    #    }
    
    #   function onSuccess (position) {
    #      setTimeout(function () {
    #          var coords = position.coords;
    #          console.log(coords.latitude + ", " + coords.longitude);
    #          Shiny.onInputChange("geolocation", true);
    #          Shiny.onInputChange("lat", coords.latitude);
    #          Shiny.onInputChange("long", coords.longitude);
    #      }, 1100)
    # }
    #  });
    #           '),
    title = "My Neighbourhood",
    
f7TabLayout(
      panels = tagList(

f7Panel(title = "About", side = "right", theme = "dark", 
                "I made this so that people can find out more about the good stuff that is near where they live. 
              
              
              The app uses data from Ordnance Survey Greenspace Open.", 
                effect = "reveal")
      ),
      navbar = 
f7Navbar(
        title = "My Neighbourhood",
        hairline = TRUE,
        shadow = TRUE,
        leftPanel = F,
        rightPanel = TRUE
      ),
f7Tabs(
        animated = TRUE,
    f7Tab(
          tabName = "Tab1",
          icon = f7Icon("map"),
          active = FALSE,
        f7Card(
          list(
            f7Text(inputId = "str",
                      label = "",
                      value = "",
                      placeholder = "Your Address here"), 
            f7SmartSelect(
              "feature",
              "Select Features",
              c("Greenspace" = "gre",
                "Bluespaces" = "blu",
                "Transport" = "tra"),
              openIn = "sheet",
              multiple = FALSE
            ),
            f7Button(inputId="goButton",color = "blue", label = "Go", size="small"
                     #f7Picker(
                     #   inputId = "mypicker",
                     #   placeholder = "",
                     #   label = "",
                     #   choices = c('Greenspaces', 'Trees', 'Bluespaces')
                     #     )
            
            )),
            f7Shadow(intensity = 10,
                     hover = TRUE,
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
              title = "How does your neighbourhood compare?",
              plotOutput("stats")
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