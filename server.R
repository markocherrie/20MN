## load packages
library(shiny)
library(leaflet)
require(RCurl)
#require(RJSONIO)
require(plyr)
library(rgdal)
library(rgeos)
library(dplyr)
library(gtools)
#library(jsonlite)
#library(mongolite)
library(rgdal)
library(DT)
library(leaflet.extras)
library(sf)
# install.packages("hereR")
library(hereR)
library(geomtextpath)
set_key(Sys.getenv("HEREAPIKEY"))
Sys.getenv("R_TEST")

# load functions
BING <- function(str){
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=",Sys.getenv(c("BINGKEY"))))
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
  
  # data --- change this to local authority and nearby ones
  Accesspoint<-readRDS("data/OSgreenspace/data/AccessPoint.rds")
  Site<-readRDS("data/OSgreenspace/data/Site.rds")
  ScotlandComp<-read.csv("data/OSgreenspace/ScotlandFreq.csv")
  

observe({

      # Create the map
      output$map <- renderLeaflet({
        
          leaflet() %>%
          addProviderTiles(providers$Stamen.TonerLite)        %>%
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
      
      isolines <- isoline(
        coords_wgs,
        datetime = Sys.time(),
        arrival = FALSE,
        range = seq(20, 20, 20) * 60,
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
      
      
      isolines_BNG <- st_transform(isolines, 27700)
      Siteswithinbuffer<-st_intersection(isolines_BNG, Site)
      Accesspoint$geometry<-NULL
      Siteswithinbuffer<-Siteswithinbuffer %>% st_transform(., 4326)
      Siteswithinbufferandaccesspoints<-merge(Siteswithinbuffer, Accesspoint, by="id", all.x=T)
      Siteswithinbufferandaccesspoints <- st_zm(Siteswithinbufferandaccesspoints, drop = T, what = "ZM")

      # Create a color palette 
      iso_1.colors <- c("#CCCCCC", "#000000")
      iso_1.pal <- colorFactor(iso_1.colors, isolines$range)
      
      # arrange so closest is top
      isolines<-isolines %>% dplyr::arrange(range)
      
      
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
            addMarkers(lng=as.numeric(coords$long), lat=as.numeric(coords$lat)) %>% 
            addPolygons(data = isolines,
                        fill=TRUE,
                        fillColor = ~iso_1.pal(isolines$name),
                        fillOpacity=0.8,
                        stroke=TRUE,
                        color = "black",
                        weight=0.5,
                        popup = isolines$range, 
                        group="20 minute walking distance")       %>%
            addPolygons(data=Siteswithinbufferandaccesspoints,
                        stroke=T,
                        weight=0.3,
                        smoothFactor = 0.2,
                        fillOpacity = 0.5,
                        popup=popup,
                        color="green",
                        #color= ~pal(function.),
                        group = "Greenspace") %>%
            # Layers control allows the user to turn layers on and off
            addLayersControl(options = layersControlOptions(collapsed = T),
                             overlayGroups = c("20 minute walking distance", "Greenspace")
          ) 
          
        }
  
  
  tb<-as.data.frame(table(Siteswithinbufferandaccesspoints$function.))
  if(nrow(tb)==0){
    tb<-data.frame (Type  = c(""),
                    Freq = c(""))
    
  }
  colnames(tb)<-c("Type", "Freq")
  tb<-merge(tb, ScotlandComp, by=c("Type"), all.y=T)
  tb$Freq.x[is.na(tb$Freq.x)]<-0
  tb$Type<-as.character(tb$Type)
  tb$Type[tb$Type=="Allotments Or Community Growing Spaces"]<-"Allotments"
  tb$Type[tb$Type=="Public Park Or Garden"]<-"Public Park/Garden"
  
  # Percentage .... change to area????
  # rather than scotland - compare it to scottish datazones
  tb$Perc.y<-round(tb$Freq.x/sum(tb$Freq.x)*100)
  tb$Perccomp<-tb$Perc.y/tb$Perc
  benchdown<-floor(max(tb$Perccomp)*0.75)
  benchup<-ceiling(max(tb$Perccomp)*1.25)
  
output$stats <- renderUI({ 
    
    title<-"<h4>Greenspace</h4>"
    str1<-paste0(tb$Freq.x[1]," ", tb$Type[1])
    str2<-paste0(tb$Freq.x[2]," ", tb$Type[2])
    str3<-paste0(tb$Freq.x[3]," ", tb$Type[3])
    str4<-paste0(tb$Freq.x[4]," ", tb$Type[4])
    str5<-paste0(tb$Freq.x[5]," ", tb$Type[5])
    str6<-paste0(tb$Freq.x[6]," ", tb$Type[6])
    str7<-paste0(tb$Freq.x[7]," ", tb$Type[7])
    str8<-paste0(tb$Freq.x[8]," ", tb$Type[8])
    str9<-paste0(tb$Freq.x[9]," ", tb$Type[9])
    str10<-paste0(tb$Freq.x[10]," ", tb$Type[10])
    
    HTML(paste(title, str1, str2, str3,str4, str5, str6,str7, str8, str9, str10, sep = '<br/>'))
    })

output$graph <- renderPlot({
  
  tb %>%
    dplyr::arrange(Type) %>%
    ggplot(., aes(x = Type, y = Perccomp, group=1)) +
    geom_polygon(fill="grey", alpha = 0.6)+
    geom_point(color="black")+
    coord_curvedpolar()+ 
    geom_texthline(yintercept = benchdown, label = ifelse(benchdown>1, paste0(benchdown, " times higher"), "Similar"), 
                   hjust = 0, vjust = -0.2, color = "grey")+
    geom_texthline(yintercept = benchup, label = paste0(benchup, " times higher"), 
                   hjust = 0, vjust = -0.2, color = "grey")+
    #geom_texthline(yintercept = 3, label = "3", 
    #               hjust = 0, vjust = -0.2, color = "grey") +
    theme_bw() +
    #facet_wrap(~ mode)+
    theme(legend.position = "none",
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks = element_blank(),
          panel.grid  = element_blank(),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_blank(),
          strip.text.x = element_text(size = 16),
          #axis.text.x = element_text(colour = mycolors)
    )+
    labs(title="Greenspace (compared to Scottish average)")
  
  
})


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
