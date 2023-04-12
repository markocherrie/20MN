lat=55.953251
long=-3.188267

set_key(Sys.getenv("HEREAPIKEY"))




# your greenspaces in your neighbourhood
subsetthegreen<-function(BNGbuffer){
Site<-do.call(rbind, lapply(paste0("data/OSgreenspace/LAdata/", LA[BNGbuffer,]$local_auth,".rds"), readRDS))
# Site<-readRDS(paste0("data/OSgreenspace/LAdata/", LA[BNGbuffer,]$local_auth[1],".rds"))
Site <- sf::st_transform(Site, 27700)
Site<-Site[BNGbuffer,]
LA<-read_sf("data/Local_Authority_Boundaries_-_Scotland/pub_las.shp")
Site<-do.call(rbind, lapply(paste0("data/OSgreenspace/LAdata/", LA[BNGbuffer,]$local_auth,".rds"), readRDS))
Site <- st_set_crs(Site, 27700)
Site<-Site[BNGbuffer,]
}

# Your isoline
library(hereR)
library(dplyr)

isoliner<-function(coods_wgs){
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
}


