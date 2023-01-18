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

# this isn't working - it's so the person can click enter rarther than
# having to click on the go button
jscode <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#goButton").click();
}});'

shinyUI(f7Page(
  # header font
  tags$style(type = "text/css", "#map {height: calc(100vh - 160px) !important;}"),
  tags$style(type = "text/css", "#Plot {height: calc(100vh - 160px) !important;}"),
  tags$head(
    tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Roboto+Slab');
                  "))
  ),
  tags$style(type="text/css", ".item-input {width:400px;
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
              "I made this so that people can find out more about the neighbourhood where
              they live. ", 
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
      f7Tab(
        tabName = "Tab1",
        icon = f7Icon("map"),
        active = FALSE,
        f7Shadow(
          intensity = 10,
          hover = TRUE,
        f7Card(
          title=
              tagList(
                tagAppendAttributes(
                  f7Text(
                    inputId = "str",
                    label = "",
                    value = "",
                    placeholder = "Your Address here"), `data-proxy-click` = "goButton"),
                f7Button(inputId="goButton",color = "blue", label = "Go", size="small"
               #f7Picker(
              #   inputId = "mypicker",
              #   placeholder = "",
              #   label = "",
              #   choices = c('Greenspaces', 'Trees', 'Bluespaces')
          #     )
          )),
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
