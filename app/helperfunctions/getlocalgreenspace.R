library(sf)

# load greenspace
Accesspoint<-read_sf("data/OSgreenspace/data/GB_AccessPoint.shp")
Site<-read_sf("data/OSgreenspace/data/GB_GreenspaceSite.shp")


# input location of person
lat <- c("55.893610")
long <- c("-3.317020")
coords<-data.frame(lat,long)
coords_wgs <- st_as_sf(coords, coords = c("long", "lat"),
                       crs = 4326, dim = "XY")
coords_BNG <- st_transform(coords_wgs, 27700)

# get buffer
coords_BNG_2km <- st_buffer(coords_BNG, 2000)

# get greenspace within buffer
Siteswithinbuffer<-st_intersection(coords_BNG_2km,Site)

Accesspoint$geometry<-NULL
Siteswithinbuffer<-Siteswithinbuffer %>% st_transform(., 4326)
Siteswithinbufferandaccesspoints<-merge(Siteswithinbuffer, Accesspoint, by="id", all.x=T)
Siteswithinbufferandaccesspoints <- st_zm(Siteswithinbufferandaccesspoints, drop = T, what = "ZM")

# 
library(leaflet)


leaflet() %>%
  addPolygons(data=Siteswithinbufferandaccesspoints)%>%
  addProviderTiles(providers$Stamen.Toner)        %>%
  addScaleBar(position = c("bottomleft"))                    




