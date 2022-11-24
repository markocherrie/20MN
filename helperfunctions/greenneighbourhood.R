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
ScotlandComp<-read.csv("data/OSgreenspace/ScotlandFreq.csv")

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


df<-data.frame(GStypes=unique(Site$function.))

Siteswithinbuffer1<-st_intersection(isolines_BNG[1,], Site)
Siteswithinbuffer2<-st_intersection(isolines_BNG[2,], Site)
Siteswithinbuffer3<-st_intersection(isolines_BNG[3,], Site)
Siteswithinbuffer2 <- Siteswithinbuffer2[!Siteswithinbuffer2$id.1 %in% unique(Siteswithinbuffer1$id.1),]
Siteswithinbuffer3 <- Siteswithinbuffer3[!Siteswithinbuffer3$id.1 %in% unique(Siteswithinbuffer2$id.1),]

Siteswithinbuffer1$geometry<-NULL
Siteswithinbuffer2$geometry<-NULL
Siteswithinbuffer3$geometry<-NULL

n<-Siteswithinbuffer1 %>%
  count(function.)
Siteswithinbuffer1<-as.data.frame(unique(Siteswithinbuffer1[,c("function.")]))
Siteswithinbuffer1$time<-10
colnames(Siteswithinbuffer1)[1]<-"GStypes"

n2<-Siteswithinbuffer2 %>%
  count(function.)
Siteswithinbuffer2<-as.data.frame(unique(Siteswithinbuffer2[,c("function.")]))
Siteswithinbuffer2$time<-20
colnames(Siteswithinbuffer2)[1]<-"GStypes"

n3<-Siteswithinbuffer3 %>%
  count(function.)
Siteswithinbuffer3<-as.data.frame(unique(Siteswithinbuffer3[,c("function.")]))
Siteswithinbuffer3$time<-30
colnames(Siteswithinbuffer3)[1]<-"GStypes"


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


#Accesspoint$geometry<-NULL
#Siteswithinbuffer<-Siteswithinbuffer %>% st_transform(., 4326)
#Siteswithinbufferandaccesspoints<-merge(Siteswithinbuffer, Accesspoint, by="id", all.x=T)
#Siteswithinbufferandaccesspoints <- st_zm(Siteswithinbufferandaccesspoints, drop = T, what = "ZM")

#'iso_1.colors <- c("#CCCCCC", "#000000")
#iso_1.pal <- colorFactor(iso_1.colors, isolines$range)

# arrange so closest is top
#isolines<-isolines %>% dplyr::arrange(range)

#pal <- colorFactor(palette = 'Set1', domain =Siteswithinbufferandaccesspoints$function.) 

# calculate area of spatial polygons sf object
#Siteswithinbufferandaccesspoints$area <- st_area(Siteswithinbufferandaccesspoints)
#Siteswithinbufferandaccesspoints <- arrange(Siteswithinbufferandaccesspoints, -area)


################ ADD TO SERVER!!!

#tb<-as.data.frame(table(Siteswithinbufferandaccesspoints$function.))
#colnames(tb)<-c("Type", "Freq")
#tb<-merge(tb, ScotlandComp, by=c("Type"), all.y=T)
#tb$Freq.x[is.na(tb$Freq.x)]<-0
#tb$Type<-as.character(tb$Type)
df$GStypes[df$GStypes=="Allotments Or Community Growing Spaces"]<-"Allotments"
df$GStypes[df$GStypes=="Public Park Or Garden"]<-"Public Park/Garden"
#tb$Perc.y<-round(tb$Freq.x/sum(tb$Freq.x)*100)
#tb$Perccomp<-tb$Perc.y/tb$Perc
#benchdown<-ceiling(max(tb$Perccomp)*0.75)
#benchup<-ceiling(max(tb$Perccomp)*1.25)

#tb[tb$Freq.x=0]<-0.001
# plot
rowadd<-c("Your location", "0", "1")
df<-rbind(df, rowadd)
df$time<-as.numeric(df$time)


#### VIz
library(dplyr)
df %>%
  dplyr::arrange(GStypes) %>%
  ggplot(., aes(x = GStypes, y = time, group=1)) +
  # take out size=n if want the original
  geom_point(aes(size = n), color="black")+
  geom_polygon(fill="#008000", alpha = 0.6)+
  coord_curvedpolar()+ 
  geom_texthline(yintercept = 10, label = "10 minutes", 
                 hjust = 0, vjust = -0.2, color = "grey")+
  geom_texthline(yintercept = 20, label = "20 minutes", 
                 hjust = 0, vjust = -0.2, color = "grey")+
  geom_texthline(yintercept = 30, label = "30 minutes", 
                 hjust = 0, vjust = -0.2, color = "grey") +
  geom_texthline(yintercept = 40, label = "Over 30 minutes", 
                 hjust = 0, vjust = -0.2, color = "grey") +
  theme_bw() +
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
  ggtitle(paste0(lat,",", long))

ggsave(paste0("output/greenspacenearme",lat, long, ".png"))
}

greennearme(55.893863, -3.316051)
greennearme(55.935408, -3.226440)


