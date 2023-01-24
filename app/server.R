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

library(ggpubr)

#readRenviron("~/.Renviron")

options(shiny.usecairo=T)
set_key(Sys.getenv("HEREAPIKEY"))

# data --- change this to local authority and nearby ones
Accesspoint<-readRDS("data/OSgreenspace/data/AccessPoint.rds")
#Site<-readRDS("data/OSgreenspace/data/Site.rds")
# pre-converteed Site but it's not working so keeping this in for now
LA<-read_sf("data/Local_Authority_Boundaries_-_Scotland/pub_las.shp")
ScotlandComp<-read.csv("data/OSgreenspace/ScotlandFreq.csv")
#Trees<-readRDS("data/EdinburghCouncil/Trees/trees.rds")
  

##### NOT WORKING
icons <- awesomeIcons(
  icon        = "house",
  iconColor   = "white",
  library     = 'fa',
  markerColor = "white"
)


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



shinyServer(function(input, output) {

output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite)        %>%
      addScaleBar(position = c("bottomleft"))         %>%
      setView(lng =-4.2026, lat = 56.4907, zoom = 7) %>%
      addScaleBar(position = c("bottomleft"))%>%
      addFullscreenControl() 
  })
  
  
  
observeEvent(input$goButton,{
  
    mapit <- leafletProxy("map") 
    mapit  %>% clearShapes() %>% clearMarkers() #%>%
    str   <- as.character(paste0(input$str, ", UK"))
    map   <- BING(str)
    lat<-map[1]
    long<-map[2]
    mapit %>% 
      setView(lng =  long, lat = lat, zoom = 14)
    coords<-data.frame(lat,long)
    coords_wgs <- st_as_sf(coords, coords = c("long", "lat"),
                           crs = 4326, dim = "XY")
    coords_BNG <- st_transform(coords_wgs, 27700)
    
    # cut site to size
    BNGbuffer<-st_buffer(coords_BNG, 5000)

    
    
  ### greenspace
if(input$feature=="gre"){
    Site<-do.call(rbind, lapply(paste0("data/OSgreenspace/LAdata/", LA[BNGbuffer,]$local_auth,".rds"), readRDS))
   
    # Site<-readRDS(paste0("data/OSgreenspace/LAdata/", LA[BNGbuffer,]$local_auth[1],".rds"))
    Site <- sf::st_transform(Site, 27700)
    Site<-Site[BNGbuffer,]


    #coords_BNG_2km <- st_buffer(coords_BNG, 2000)
    
    # create the isolines
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
    isolines_BNG
    
    
  ### greenspace
if(input$feature=="gre"){
    Site<-do.call(rbind, lapply(paste0("data/OSgreenspace/LAdata/", LA[BNGbuffer,]$local_auth,".rds"), readRDS))
   
    # Site<-readRDS(paste0("data/OSgreenspace/LAdata/", LA[BNGbuffer,]$local_auth[1],".rds"))
    Site <- sf::st_transform(Site, 27700)
    Site <- Site[BNGbuffer,]

    #coords_BNG_2km <- st_buffer(coords_BNG, 2000)
    
    #### for the graph
    df<-data.frame(GStypes=unique(Site$function.))
    Siteswithinbuffer1<-Site[(unlist(st_intersects(isolines_BNG[1,], Site))),]
    Siteswithinbuffer1$name<-"0 to 10 mins"
    Siteswithinbuffer2<-Site[(unlist(st_intersects(isolines_BNG[2,], Site))),]
    Siteswithinbuffer2$name<-"0 to 20 mins"
    Siteswithinbuffer3<-Site[(unlist(st_intersects(isolines_BNG[3,], Site))),]
    Siteswithinbuffer3$name<-"0 to 30 mins"
    Siteswithinbuffer2 <- Siteswithinbuffer2[!Siteswithinbuffer2$id %in% unique(Siteswithinbuffer1$id),]
    Siteswithinbuffer3 <- Siteswithinbuffer3[!Siteswithinbuffer3$id %in% unique(Siteswithinbuffer2$id),]
    
    Siteswithinbuffer1$geometry<-NULL
    Siteswithinbuffer2$geometry<-NULL
    Siteswithinbuffer3$geometry<-NULL
    
    # making sure that we don't have empty dataframe
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
    
    if(nrow(Siteswithinbuffer1)>0){
    Siteswithinbuffer1<-merge(Siteswithinbuffer1, n, by.x="GStypes", by.y="function.", all.x=T)}
    if(nrow(Siteswithinbuffer2)>0){
    Siteswithinbuffer2<-merge(Siteswithinbuffer2, n2, by.x="GStypes", by.y="function.", all.x=T)}
    if(nrow(Siteswithinbuffer3)>0){
    Siteswithinbuffer3<-merge(Siteswithinbuffer3, n3, by.x="GStypes", by.y="function.", all.x=T)}
    
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
      
      # greenspace formatting
      ###################
      pal <- colorFactor(palette = 'Set1', domain = Siteswithinbufferandaccesspoints_20$function.) 
      
      # calculate area of spatial polygons sf object
      Siteswithinbufferandaccesspoints_20$area <- st_area(Siteswithinbufferandaccesspoints_20)
      Siteswithinbufferandaccesspoints_20 <- arrange(Siteswithinbufferandaccesspoints_20, -area)
      
      ### superscript in leaflet
      popup <- 
        (paste0(Siteswithinbufferandaccesspoints_20$function.))
      
      
      
    } else{
      return(Siteswithinbufferandaccesspoints_20)
      
      # greenspace formatting
      ###################
      pal <- colorFactor(palette = 'Set1', domain = Siteswithinbufferandaccesspoints_20$function.) 
      
      # calculate area of spatial polygons sf object
      Siteswithinbufferandaccesspoints_20$area <- st_area(Siteswithinbufferandaccesspoints_20)
      Siteswithinbufferandaccesspoints_20 <- arrange(Siteswithinbufferandaccesspoints_20, -area)
      
      ### superscript in leaflet
      popup <- 
        (paste0(Siteswithinbufferandaccesspoints_20$function.))
    }


    
}else if(input$feature=="blu"){
  
  Site<-do.call(rbind, lapply(paste0("data/bluespaces/lakes/LAdata/", LA[BNGbuffer,]$local_auth,".rds"), readRDS))
  colnames(Site)[1]<-"id"
  #Site$function.<-ifelse(Site$POLY_AREA_>5000, "Smaller lake", "Larger lake")

}else if(input$feature=="blu"){
  


}else if(input$feature=="blu"){
  
  
  

  
  
  
  
  
} else if(input$feature=="tra"){
  
  
  
}
  
}
    

    
    
observe({
  
    
      mapit  %>%  
        setView(lng =  long, lat = lat, zoom = 14) %>% 
        addMarkers(lng=as.numeric(coords$long), lat=as.numeric(coords$lat), icon = icons) %>% 
        addPolylines(data = isolines_line[2,],
                     color = "black",
                     opacity=0.5,
                     weight=3,
                     popup = isolines$range, 
                     group="20 minute walking distance") %>%
        addPolygons(data=Siteswithinbufferandaccesspoints_20,
                    stroke=T,
                    weight=0.3,
                    smoothFactor = 0.2,
                    fillOpacity = 0.65,
                    popup=popup,
                    #color="green",
                    color= "#008000",
                    group = "Greenspace") %>%
        # Layers control allows the user to turn layers on and off
        addLayersControl(options = layersControlOptions(collapsed = T),
                         overlayGroups = c("Greenspace","20 minute walking distance"))
      
    })    
    
    
    output$stats <- renderPlot({ 
      
      if(nrow(Siteswithinbufferandaccesspoints_20)>0){
        
        
        
        # create base
        base<-ggplot(tb, aes(x = Type))+
          coord_flip()+ theme_minimal() + theme(panel.border = element_blank(), 
                                                panel.grid.major = element_blank(),
                                                panel.grid.minor = element_blank(),
                                                axis.ticks.x=element_blank(),
                                                axis.text.y=element_text(size=15, color="#1c1c1d"),
                                                plot.background = element_rect(fill = "white"), 
                                                panel.background = element_rect(fill = "white", colour="white"),
                                                plot.margin = unit(c(1.85,0.5,0.3,0),"cm")
          )+
          xlab("")
        
        p2 <- ggplot(tb, aes(x = Type, y = Perc.y))+
          geom_col(fill="#008000", alpha=0.6)+ coord_flip()+ylim(0,100)+
          #scale_fill_manual(breaks = c("MANAGERS, DIRECTORS AND SENIOR OFFICIALS",
          #                            "PROFESSIONAL OCCUPATIONS",
          #                             "ASSOCIATE PROFESSIONAL AND TECHNICAL OCCUPATIONS",
          #                            "ADMINISTRATIVE AND SECRETARIAL OCCUPATIONS",
          #                           "SKILLED TRADES OCCUPATIONS",
          #                          "CARING, LEISURE AND OTHER SERVICE OCCUPATIONS",
          #                         "SALES AND CUSTOMER SERVICE OCCUPATIONS",
          #                        "PROCESS, PLANT AND MACHINE OPERATIVES",
          #                       "ELEMENTARY OCCUPATIONS"), 
          #           values=c("#88CCEE", "#CC6677", "#DDCC77", "#117733", 
          #                   "#332288", "#AA4499", "#44AA99", "#999933", "#661100"))+
        geom_text(aes(label=paste0(Perc.y,"%")),hjust=-0.25, vjust=0.5, color="#1c1c1d", size=6)+
          theme_minimal()+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(size = 15, face = "bold", color="#1c1c1d"),
                plot.background = element_rect(fill = "white"), 
                panel.background = element_rect(fill = "white", colour="white"),
                panel.border = element_blank(),
                plot.margin = unit(c(1,0,0,0),"cm")
          )+
          #geom_hline(yintercept=c(48.6), linetype="dashed")+
          theme(legend.position = "none")+
          ggtitle("   Your neighbourhood")
        
        
        
        p3 <- ggplot(tb, aes(x = Type, y = Perc))+
          geom_col(fill="#008000", alpha=0.6)+ coord_flip()+ylim(0,100)+
          # put in custom colours here!!!
          #scale_fill_manual(breaks = c("MANAGERS, DIRECTORS AND SENIOR OFFICIALS",
          #                            "PROFESSIONAL OCCUPATIONS",
          #                             "ASSOCIATE PROFESSIONAL AND TECHNICAL OCCUPATIONS",
          #                            "ADMINISTRATIVE AND SECRETARIAL OCCUPATIONS",
          #                           "SKILLED TRADES OCCUPATIONS",
          #                          "CARING, LEISURE AND OTHER SERVICE OCCUPATIONS",
          #                         "SALES AND CUSTOMER SERVICE OCCUPATIONS",
          #                        "PROCESS, PLANT AND MACHINE OPERATIVES",
          #                       "ELEMENTARY OCCUPATIONS"), 
          #           values=c("#88CCEE", "#CC6677", "#DDCC77", "#117733", 
        #                   "#332288", "#AA4499", "#44AA99", "#999933", "#661100"))+
        geom_text(aes(label=paste0(Perc, "%")),hjust=-0.25, vjust=0.5, color="#1c1c1d", size=6)+
          theme_minimal()+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(size = 15, face = "bold", color="#1c1c1d"),
                plot.background = element_rect(fill = "white"), 
                panel.background = element_rect(fill = "white", colour="white"),
                panel.border = element_blank(),
                plot.margin = unit(c(1,0,0,0),"cm"))+
          #geom_hline(yintercept=c(48.6), linetype="dashed")+
          theme(legend.position = "none")+
          ggtitle("   Scotland")
        
        
        # edit this when we have more data
        figure <- ggarrange(base, p2,p3,
                            #base, p2, p3,
                            ncol = 3, 
                            # add a row with new data
                            nrow = 1, 
                            widths = c(1, 1.5, 1.5))
        
        figure
        
        #str1<-paste0(tb$Freq.x[1]," ", tb$Type[1])
        #str2<-paste0(tb$Freq.x[2]," ", tb$Type[2])
        #str3<-paste0(tb$Freq.x[3]," ", tb$Type[3])
        #str4<-paste0(tb$Freq.x[4]," ", tb$Type[4])
        #str5<-paste0(tb$Freq.x[5]," ", tb$Type[5])
        #str6<-paste0(tb$Freq.x[6]," ", tb$Type[6])
        #str7<-paste0(tb$Freq.x[7]," ", tb$Type[7])
        #str8<-paste0(tb$Freq.x[8]," ", tb$Type[8])
        #str9<-paste0(tb$Freq.x[9]," ", tb$Type[9])
        #str10<-paste0(tb$Freq.x[10]," ", tb$Type[10])
        
        #HTML(paste('<br/>',str1, str2, str3,str4, str5, str6,str7, str8, str9, str10, sep = '<br/>'))
      } else{
        
        #HTML(paste("No features within 20 minutes"))
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
                       hjust = 0, vjust = -0.2, color = "black")+
        geom_texthline(yintercept = 20, label = "20 minutes", 
                       hjust = 0, vjust = -0.2, color = "black")+
        geom_texthline(yintercept = 30, label = "30 minutes", 
                       hjust = 0, vjust = -0.2, color = "black") +
        geom_texthline(yintercept = 40, label = "Over 30 minutes", 
                       hjust = 0, vjust = -0.2, color = "black") +
        geom_point(aes(size = n), color="grey22", alpha=0.7)+
        labs(title=" ") +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.y=element_blank(),
              axis.text.x = element_text(colour= "black", vjust = -0.5, size =10, face ="bold"),
              axis.title.y=element_blank(),
              axis.title.x=element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank(),
              plot.background = element_blank(),
              panel.background = element_rect(fill = "white", colour = "white"),
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
          bg = "white")
      print(Plot)
      dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = 550,
           height = 550,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
  }) # observe
}) # shinyserver