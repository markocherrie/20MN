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

set_key(Sys.getenv("HEREAPIKEY"))

Site<-readRDS("data/OSgreenspace/data/Site.rds")
Accesspoint<-readRDS("data/OSgreenspace/data/AccessPoint.rds")
#ScotlandComp<-read.csv("data/OSgreenspace/ScotlandFreq.csv")

# centre of town
#55.9533
#-3.1883

# home
#55.893863
#-3.316051

# old home
#55.935408, 
#-3.226440

greennearme<- function(lat, long){
lat = lat
long = long


coords<-data.frame(lat,long)
coords_wgs <- st_as_sf(coords, coords = c("long", "lat"),
                       crs = 4326, dim = "XY")
coords_BNG <- st_transform(coords_wgs, 27700)

# cut site to size
BNGbuffer<-st_buffer(coords_BNG, 10000)
Site<-Site[BNGbuffer,]

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
isolines_BNG

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



#### VIz

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
        panel.border = element_blank())
        #strip.text.x = element_text(size = 16),
        #plot.title = element_text(size=18))

Plot


# Generate the PNG
png(paste0("output/greenspacenearme",lat, long, ".png"), 
    width = 360*10, 
    height = 360*10,
    res = 72*10)
print(Plot)
dev.off()
#+
 # ggtitle(paste0(lat,",", long))
}


greennearme(55.893608, -3.317020)
#greennearme(55.935408, -3.226440)


