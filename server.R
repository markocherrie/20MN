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
library(Cairo)
library(htmlwidgets)

#readRenviron("~/.Renviron")

options(shiny.usecairo=T)
set_key(Sys.getenv("HEREAPIKEY"))
Sys.getenv("R_TEST")


shinyServer(function(input, output) {

  # data --- change this to local authority and nearby ones
  Accesspoint<-readRDS("data/OSgreenspace/data/AccessPoint.rds")
  Site<-readRDS("data/OSgreenspace/data/Site.rds")
  Site <- sf::st_transform(Site, 27700)
  ScotlandComp<-read.csv("data/OSgreenspace/ScotlandFreq.csv")
  #Trees<-readRDS("data/EdinburghCouncil/Trees/trees.rds")
  
  
observe({
    
  if(is.null(input$lat)){
    
      print("wait")
      
} else{
  
    lat<-input$lat
    long<-input$long
    
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
    df$GStypes[df$GStypes=="Public Park Or Garden"]<-"Park/Gardens"
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
    
    
    if(nrow(Siteswithinbufferandaccesspoints_20)>0){
      tb<-as.data.frame(table(Siteswithinbufferandaccesspoints_20$function.))
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
      tb$Perc.y<-round(tb$Freq.x/sum(tb$Freq.x)*100)
      tb$Perccomp<-tb$Perc.y/tb$Perc
      benchdown<-floor(max(tb$Perccomp)*0.75)
      benchup<-ceiling(max(tb$Perccomp)*1.25)
    } else{
      return(Siteswithinbufferandaccesspoints_20)
    }
    
    
    ###################
    pal <- colorFactor(palette = 'Set1', domain = Siteswithinbufferandaccesspoints_20$function.) 
    
    # calculate area of spatial polygons sf object
    Siteswithinbufferandaccesspoints_20$area <- st_area(Siteswithinbufferandaccesspoints_20)
    Siteswithinbufferandaccesspoints_20 <- arrange(Siteswithinbufferandaccesspoints_20, -area)
    
    ### superscript in leaflet
    popup <- 
      (paste0(Siteswithinbufferandaccesspoints_20$function.))
    
    
    output$map <- renderLeaflet({
      
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite)        %>%
        addScaleBar(position = c("bottomleft"))         %>%
        setView(lng =  long, lat = lat, zoom = 15) %>% 
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
                         overlayGroups = c("Greenspace","20 minute walking distance"))
      
    })    
    
    
    output$stats <- renderUI({ 
      
      if(nrow(Siteswithinbufferandaccesspoints_20)>0){
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
        
        HTML(paste('<br/>',str1, str2, str3,str4, str5, str6,str7, str8, str9, str10, sep = '<br/>'))
      } else{
        HTML(paste("No features within 20 minutes"))
      }
    })
    
    output$Plot <- renderImage({
      
      Plot <- df %>%
        dplyr::arrange(GStypes) %>%
        ggplot(., aes(x = GStypes, y = time, group=1)) +
        # take out size=n if want the original
        geom_polygon(fill="#008000", alpha = 0.6)+
        coord_curvedpolar()+ 
        geom_texthline(yintercept = 10, label = "10 minutes", 
                       hjust = 0, vjust = -0.2, color = "white")+
        geom_texthline(yintercept = 20, label = "20 minutes", 
                       hjust = 0, vjust = -0.2, color = "white")+
        geom_texthline(yintercept = 30, label = "30 minutes", 
                       hjust = 0, vjust = -0.2, color = "white") +
        geom_texthline(yintercept = 40, label = "Over 30 minutes", 
                       hjust = 0, vjust = -0.2, color = "white") +
        geom_point(aes(size = n), color="black")+
        labs(title=" ") +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.y=element_blank(),
              axis.text.x = element_text(colour= "white", vjust = -0.5, size =10, face ="bold"),
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank(),
              plot.background = element_blank(),
              panel.background = element_rect(fill = "#1c1c1d", colour = "#1c1c1d"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              panel.border = element_blank(),
              #strip.text.x = element_text(size = 16),
              plot.title = element_text(size=18))
      
      outfile <- tempfile(fileext = '.png')
      
      # Generate the PNG
      png(outfile, 
          width = 360*10, 
          height = 360*10,
          res = 72*10,
          bg = "#1c1c1d")
      print(Plot)
      dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = 550,
           height = 550,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    }
  }) # observe
}) # shinyserver