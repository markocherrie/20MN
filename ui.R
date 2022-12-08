###### PACKAGES
library(shiny)
library(shinyjs)
library(leaflet)
library(rgdal)
library(shinyBS)
library(gtools)
library(ggplot2)
library(shinyWidgets)
###updates--- to get datazones that have been clicked
### http://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny


####### NOT SURE
textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}

####### Shiny fluid page set sup
shinyUI(fluidPage(
  useShinyjs(),  # Include shinyjs
  
  # Set the fonts and css for map window
  tags$style(type = "text/css", "#map {height: calc(100vh - 160px) !important;}"),
  tags$head(
    tags$style(HTML("
                  @import url('//fonts.googleapis.com/css?family=Roboto+Slab');
                  "))
  ),
  
  headerPanel(
    fluidRow(
      column(11, h1("My Neighbourhood", 
                    style = "font-family: 'Roboto Slab', cursive;
     font-weight: bold; font-size: 39px")))
    , windowTitle = "What's my neighbourhood like?"),
  
  
  ### CRESH favicon
  tags$head(tags$style(
    HTML('
        #sidebar {
            background-color: #dec4de;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  tags$head(
    tags$style(HTML("
      .leaflet-left .leaflet-control{
        visibility: hidden;
      }
    "))),
  tags$head(tags$link(rel = "shortcut icon", href="http://www.iconj.com/ico/g/g/ggtzbwew2b.ico", type="image/x-icon")),
  tags$head(tags$style("#summary{
                      position: relative;
                      display: inline-block;
                       width: 20%;
                       height: 10%;
                       top: 10px;
                       padding: 10% 0;
                       border-radius:50%;
                       line-height:0;
                       /* further display options */
                       @shadow: rgba(0, 0, 0, .1);
                       @shadow-length: 4px;
                       -webkit-box-shadow: 0 @shadow-length 0 0 @shadow;
                       box-shadow: 0 @shadow-length 0 0 @shadow;
                       text-shadow: 0 @shadow-length 0 @shadow;
                       background: #428bca;
                       color: white;
                       font-family: Helvetica, Arial Black, sans;
                       font-size: 24px;
                       text-align: center;
                       }",
              
                       
  )),
  column(1,
    #helpText(
    #  tags$div(
    #    "Ever wondered what's on your doorstep?",
    #    tags$br(),
    #    "Fill in the features you'd like to see and your address then click enter",
    #    tags$br(),
    #    "Results will be over here 👉")),
    dropdownButton(
    selectInput("category", "Feature", choices = c("Greenspaces")),
    div(style="display:inline-block", textInput("str", label =("Address"), value = "")),
    div(style="display:inline-block",actionButton("goButton", "Enter 👉")),
    circle = TRUE,
    status = "danger", 
    icon = icon("gear"), width = "320px"
    #tooltip = tooltipOptions(title = "Click to see inputs !")
    
    )
    # adding the new div tag to the sidebsar            
    ),
    #div(style="display:inline-block", textInput("str", label =("Enter an Area of Interest"), value = "")),
    #bsTooltip("str", "Tip: Type in an address, postcode or point of interest and click Enter. The location will be shown as a blue marker with data for the surrounding area local area presented on the map.", "top"
    #),
  tags$br(),
  column(11,
    tabsetPanel(type = "tabs",                
                tabPanel("Where?", shinycssloaders::withSpinner(leafletOutput("map", height = "100%"))),
                tabPanel("How many?", htmlOutput("stats")),
                #tabPanel("How far?", plotOutput("graph"), width= "100%"),
                tabPanel("How far?", imageOutput("Plot",height = "100%", width = "100%"))
                #tabPanel("How to use", includeHTML("howtouse.html")),
                #tabPanel("Change over time", leafletOutput("change")),
                #tabPanel("Acknowledgements", includeHTML("acknowledgements.html"))
    ))
))


