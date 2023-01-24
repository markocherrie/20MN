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
            title = "How far?",
                tableOutput("data")
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
  
  
  
  #  EDITTT!"!!
  lat<-55.9533
  long<--3.1883
  
  
  coords<-data.frame(lat,long)
  coords_wgs <- st_as_sf(coords, coords = c("long", "lat"),
                         crs = 4326, dim = "XY")
  coords_BNG <- st_transform(coords_wgs, 27700)
  #coords_BNG_2km <- st_buffer(coords_BNG, 2000)
  
  isolines <- isoline(
    coords_wgs,
    datetime = Sys.time(),
    arrival = FALSE,
    range = seq(10, 30, 10) * 60,
    range_type = "time",
    routing_mode = "fast",
    transport_mode = "pedestrian",
    traffic = TRUE,
    optimize = "balanced",
    consumption_model = NULL,
    aggregate = FALSE,
    url_only = FALSE
  )%>% 
    #mutate(name = paste0((range - 600) / 60," to ", range / 60, " mins"))
    mutate(name = paste0("0 to ", range / 60, " mins"))
  
  isolines_20<-isolines[2,]
  isolines_20_BNG <- st_transform(isolines_20, 27700)
  isolines_BNG <- st_transform(isolines, 27700)
  
  
  #### for the graph
  
  df<-data.frame(GStypes=unique(Site$function.))
  
  Siteswithinbuffer1<-st_intersection(isolines_BNG[1,], Site)
  Siteswithinbuffer2<-st_intersection(isolines_BNG[2,], Site)
  Siteswithinbuffer3<-st_intersection(isolines_BNG[3,], Site)
  Siteswithinbuffer2 <- Siteswithinbuffer2[!Siteswithinbuffer2$id.1 %in% unique(Siteswithinbuffer1$id.1),]
  Siteswithinbuffer3 <- Siteswithinbuffer3[!Siteswithinbuffer3$id.1 %in% unique(Siteswithinbuffer2$id.1),]
  
  
  
  Siteswithinbuffer1$geometry<-NULL
  Siteswithinbuffer2$geometry<-NULL
  Siteswithinbuffer3$geometry<-NULL
  
  
  if(nrow(Siteswithinbuffer1)>0){
    n<-Siteswithinbuffer1 %>%
      count(function.)
    Siteswithinbuffer1<-as.data.frame(unique(Siteswithinbuffer1[,c("function.")]))
    Siteswithinbuffer1$time<-10
    colnames(Siteswithinbuffer1)[1]<-"GStypes"
  }else{
    colnames(Siteswithinbuffer1)[8]<-"GStypes"
    n<- data.frame("function."=character(),
                   "n"=numeric())
  }
  
  if(nrow(Siteswithinbuffer2)>0){
    n2<-Siteswithinbuffer2 %>%
      count(function.)
    Siteswithinbuffer2<-as.data.frame(unique(Siteswithinbuffer2[,c("function.")]))
    Siteswithinbuffer2$time<-20
    colnames(Siteswithinbuffer2)[1]<-"GStypes"
  }else{
    colnames(Siteswithinbuffer2)[8]<-"GStypes"
    n2<- data.frame("function."=character(),
                    "n"=numeric())
  }
  
  if(nrow(Siteswithinbuffer3)>0){
    n3<-Siteswithinbuffer3 %>%
      count(function.)
    Siteswithinbuffer3<-as.data.frame(unique(Siteswithinbuffer3[,c("function.")]))
    Siteswithinbuffer3$time<-30
    colnames(Siteswithinbuffer3)[1]<-"GStypes"
  }else{
    colnames(Siteswithinbuffer3)[8]<-"GStypes"
    n3<- data.frame("function."=character(),
                    "n"=numeric())
  }
  
  Siteswithinbuffer2 <- Siteswithinbuffer2[!Siteswithinbuffer2$GStypes %in% unique(Siteswithinbuffer1$GStypes),]
  Siteswithinbuffer3 <- Siteswithinbuffer3[!Siteswithinbuffer3$GStypes %in% unique(Siteswithinbuffer2$GStypes),]
  Siteswithinbuffer3 <- Siteswithinbuffer3[!Siteswithinbuffer3$GStypes %in% unique(Siteswithinbuffer1$GStypes),]
  
  Siteswithinbuffer1<-merge(Siteswithinbuffer1, n, by.x="GStypes", by.y="function.", all.x=T)
  Siteswithinbuffer2<-merge(Siteswithinbuffer2, n2, by.x="GStypes", by.y="function.", all.x=T)
  Siteswithinbuffer3<-merge(Siteswithinbuffer3, n3, by.x="GStypes", by.y="function.", all.x=T)
  
  
  dfadd<-rbind(Siteswithinbuffer1, Siteswithinbuffer2, Siteswithinbuffer3)
  df<-merge(df, dfadd, by="GStypes", all.x=T)
  df$time[is.na(df$time)]<-"40"
  # could change this to within 5km....
  df$n[is.na(df$n)]<-"1"
  df$n<-as.numeric(df$n)
  df$GStypes[df$GStypes=="Allotments Or Community Growing Spaces"]<-"Allotments"
  df$GStypes[df$GStypes=="Public Park Or Garden"]<-"Public Park/Garden"
  rowadd<-c("Your location", "0", "1")
  df<-rbind(df, rowadd)
  df$time<-as.numeric(df$time)
  
  # For the map
  Siteswithinbuffer_20<-st_intersection(isolines_20_BNG, Site)
  Accesspoint$geometry<-NULL
  Siteswithinbuffer_20<-Siteswithinbuffer_20 %>% st_transform(., 4326)
  Siteswithinbufferandaccesspoints_20<-merge(Siteswithinbuffer_20, Accesspoint, by="id", all.x=T)
  Siteswithinbufferandaccesspoints_20 <- st_zm(Siteswithinbufferandaccesspoints_20, drop = T, what = "ZM")
  
  # Create a color palette 
  iso_1.pal <- colorFactor("Reds", isolines$range)
  
  
  # arrange so closest is top
  isolines<-isolines %>% dplyr::arrange(-range)
  isolines_line = st_cast(isolines,"LINESTRING")
  
  ###################
  pal <- colorFactor(palette = 'Set1', domain = Siteswithinbufferandaccesspoints_20$function.) 
  
  # calculate area of spatial polygons sf object
  Siteswithinbufferandaccesspoints_20$area <- st_area(Siteswithinbufferandaccesspoints_20)
  Siteswithinbufferandaccesspoints_20 <- arrange(Siteswithinbufferandaccesspoints_20, -area)
  
  ### superscript in leaflet
  popup <- 
    (paste0(Siteswithinbufferandaccesspoints_20$function.))
    
    # Create the map

output$mappy<- renderLeaflet({
  
    
    #################
  mapit <- leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite)        %>%
      addScaleBar(position = c("bottomleft")) %>%
      setView(lng =  long, lat = lat, zoom = 14)  %>%
      addMarkers(lng=as.numeric(coords$long), lat=as.numeric(coords$lat)) %>% 
      addPolylines(data = isolines_line[2,],
                   color = "#100c08",
                   opacity=0.5,
                   weight=2,
                   popup = isolines$range, 
                   group="20 minute walking distance") %>%
      addPolygons(data=Siteswithinbufferandaccesspoints_20,
                  stroke=T,
                  weight=0.3,
                  smoothFactor = 0.2,
                  fillOpacity = 0.5,
                  popup=popup,
                  #color="green",
                  color= ~pal(function.),
                  group = "Greenspace") %>%
      # Layers control allows the user to turn layers on and off
      addLayersControl(options = layersControlOptions(collapsed = T),
                       overlayGroups = c("Greenspace","20 minute walking distance")
      ) 
    mapit
    
  })

      
      
      
      })
#}