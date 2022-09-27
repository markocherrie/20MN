## load packages
library(shiny)
library(leaflet)
require(RCurl)
require(RJSONIO)
require(plyr)
library(rgdal)
library(rgeos)
library(dplyr)
library(gtools)
library(jsonlite)
library(mongolite)
library(rgdal)
library(DT)
library(leaflet.extras)
library(sf)

# load functions
BING <- function(str){
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=Apo4HssxpmYvVbDEUA464pmX5Y30xsQNlJ4pES6Z6D056puS63D90MLZlQ1yVeTG"))
  d <- getURL(u)
  j <- RJSONIO::fromJSON(d,simplify = FALSE) 
  if (j$resourceSets[[1]]$estimatedTotal > 0) {
    lat <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
    lng <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
  }
  else {    
    lat <- lng <- NA
  }
  data<-c(lat,lng)
  data[3]<-"BING"
  return(data)
}  
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
sp.na.omit <- function(x, margin=2) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}


# server file
shinyServer(function(input, output) {
  
observe({

      # Create the map
      output$map <- renderLeaflet({
        
        # data --- change this to local authority and nearby ones
        Accesspoint<-readRDS("data/OSgreenspace/data/AccessPoint.rds")
        Site<-readRDS("data/OSgreenspace/data/Site.rds")
        
        
          leaflet() %>%
          addProviderTiles(providers$Stamen.Toner)        %>%
          addScaleBar(position = c("bottomleft"))         %>%
          addFullscreenControl()                          %>%
          setView(lng =-4.2026, lat = 56.4907, zoom = 7) 
        
      })
        
})
    
    
    ###################################################################################################################    
observeEvent(input$goButton,{
   
  # load up the basemap
    mapit <- leafletProxy("map") 
    mapit %>% clearShapes() %>% clearMarkers()
   
  # geocode the person's string
    str   <- as.character(paste0(input$str, ", Scotland"))
    map   <- BING(str)
    
  # If the search is not null then set the lat and long coordinates
if (!is.null(str)){
      lat<-map[1]
      long<-map[2]
      
  # This is important... it sets the zoom to the geocoded location
if (!is.na(lat)){
        mapit %>% 
        setView(lng =  long, lat = lat, zoom = 14)
} else {
        mapit  %>% setView(lng =-4.2026, lat = 56.4907, zoom = 7) 
} 
      
      coords<-data.frame(lat,long)
      coords_wgs <- st_as_sf(coords, coords = c("long", "lat"),
                             crs = 4326, dim = "XY")
      coords_BNG <- st_transform(coords_wgs, 27700)
      coords_BNG_2km <- st_buffer(coords_BNG, 2000)
      Siteswithinbuffer<-st_intersection(coords_BNG_2km,Site)
      
      Accesspoint$geometry<-NULL
      Siteswithinbuffer<-Siteswithinbuffer %>% st_transform(., 4326)
      Siteswithinbufferandaccesspoints<-merge(Siteswithinbuffer, Accesspoint, by="id", all.x=T)
      Siteswithinbufferandaccesspoints <- st_zm(Siteswithinbufferandaccesspoints, drop = T, what = "ZM")
      
      ## map 2 is the red dot location
      #location<-as.data.frame(cbind(long=as.numeric(long), lat=as.numeric(lat)))
      #location<-SpatialPoints(cbind(as.numeric(long),as.numeric(lat)))
      #proj4string(location) <- CRS("+proj=longlat +datum=WGS84")
      #map<-spTransform(map, proj4string(geog))
      
      # Here we are getting the local authority location
      #location<-over(map, geog , fn = NULL)
      #location2<-location[1,2]
      
      ###################################################################################################################    
      
observe({    
        if(!is.na(coords_BNG)){
          mapit  %>% clearShapes() %>% clearMarkers()
          
          ###################
          pal <- colorFactor(palette = 'Set1', domain =Siteswithinbufferandaccesspoints$function.) 
          
          # calculate area of spatial polygons sf object
          Siteswithinbufferandaccesspoints$area <- st_area(Siteswithinbufferandaccesspoints)
          Siteswithinbufferandaccesspoints <- arrange(Siteswithinbufferandaccesspoints, -area)
          
          ### superscript in leaflet
          popup <- 
            (paste0(Siteswithinbufferandaccesspoints$function.))
          
          #################
          mapit  %>%
          addPolygons(data=Siteswithinbufferandaccesspoints,
                      stroke=T,
                      weight=0.3,
                      smoothFactor = 0.2,
                      fillOpacity = 0.7,
                      popup=popup,
                      color= ~pal(function.)) 
          
}
})
      
# If there is no proper geocode then clear the map 
}else{
      mapit <- leafletProxy("map") %>% clearShapes() %>% clearMarkers()  
}
    
    #This ones for the observe 2 - geocoded location    
})  
  
  ###################################################################################################################    
  
  # this ones for the Shinyserver  
})
