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
# install.packages("hereR")
library(hereR)
library(geomtextpath)

Site<-readRDS("data/OSgreenspace/data/Site.rds")
Accesspoint<-readRDS("data/OSgreenspace/data/AccessPoint.rds")
ScotlandComp<-read.csv("data/OSgreenspace/ScotlandFreq.csv")

lat = 55.9030598
long = -3.1952914

coords<-data.frame(lat,long)
coords_wgs <- st_as_sf(coords, coords = c("long", "lat"),
                       crs = 4326, dim = "XY")
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

isolines_BNG <- st_transform(isolines, 27700)

Siteswithinbuffer<-st_intersection(isolines_BNG, Site)
Accesspoint$geometry<-NULL
Siteswithinbuffer<-Siteswithinbuffer %>% st_transform(., 4326)
Siteswithinbufferandaccesspoints<-merge(Siteswithinbuffer, Accesspoint, by="id", all.x=T)
Siteswithinbufferandaccesspoints <- st_zm(Siteswithinbufferandaccesspoints, drop = T, what = "ZM")

iso_1.colors <- c("#CCCCCC", "#000000")
iso_1.pal <- colorFactor(iso_1.colors, isolines$range)

# arrange so closest is top
isolines<-isolines %>% dplyr::arrange(range)

pal <- colorFactor(palette = 'Set1', domain =Siteswithinbufferandaccesspoints$function.) 

# calculate area of spatial polygons sf object
Siteswithinbufferandaccesspoints$area <- st_area(Siteswithinbufferandaccesspoints)
Siteswithinbufferandaccesspoints <- arrange(Siteswithinbufferandaccesspoints, -area)


################ ADD TO SERVER!!!

tb<-as.data.frame(table(Siteswithinbufferandaccesspoints$function.))
colnames(tb)<-c("Type", "Freq")
tb<-merge(tb, ScotlandComp, by=c("Type"), all.y=T)
tb$Freq.x[is.na(tb$Freq.x)]<-0
tb$Type<-as.character(tb$Type)
tb$Type[tb$Type=="Allotments Or Community Growing Spaces"]<-"Allotments"
tb$Type[tb$Type=="Public Park Or Garden"]<-"Public Park/Garden"
tb$Perc.y<-round(tb$Freq.x/sum(tb$Freq.x)*100)
tb$Perccomp<-tb$Perc.y/tb$Perc
benchdown<-ceiling(max(tb$Perccomp)*0.75)
benchup<-ceiling(max(tb$Perccomp)*1.25)

#tb[tb$Freq.x=0]<-0.001
# plot
library(dplyr)

tb %>%
  dplyr::arrange(Type) %>%
  ggplot(., aes(x = Type, y = Perccomp, group=1)) +
  geom_polygon(fill="grey", alpha = 0.6)+
  geom_point(color="black")+
  coord_curvedpolar()+ 
  geom_texthline(yintercept = benchdown, label = paste0(benchdown), 
                 hjust = 0, vjust = -0.2, color = "grey")+
  geom_texthline(yintercept = benchup, label = paste0(benchup), 
                 hjust = 0, vjust = -0.2, color = "grey")+
  geom_texthline(yintercept = 3, label = "3", 
                hjust = 0, vjust = -0.2, color = "grey") +
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
  )


