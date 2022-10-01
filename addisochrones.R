# help from https://rpubs.com/djn34/here_isochrones

DT = data.frame(
  lat=c(55.893608),
  long=c(-3.317020)
)

library(sf)
DT = st_as_sf(DT, coords = c("long","lat"), remove = FALSE) %>% st_set_crs(4326)

# install.packages("hereR")
library(hereR)
set_key(Sys.getenv("HEREAPIKEY"))

isolines <- isoline(
  DT,
  datetime = Sys.time(),
  arrival = FALSE,
  range = seq(5, 20, 5) * 60,
  range_type = "time",
  routing_mode = "fast",
  transport_mode = "pedestrian",
  traffic = TRUE,
  optimize = "balanced",
  consumption_model = NULL,
  aggregate = FALSE,
  url_only = FALSE
)

# Create a color palette 
iso_1.colors <- c("#CCCCCC", "#000000")
iso_1.pal <- colorFactor(iso_1.colors, isolines$range)

# arrnage so closest is top
isolines<-isolines %>% dplyr::arrange(-range)


library(leaflet)
leaflet() %>% 
  
  # Add background map
  addProviderTiles("CartoDB.Positron") %>% 
  
  # 
  addPolygons(data = isolines,
              fillColor = ~iso_1.pal(isolines$range),
              fillOpacity=0.35,
              stroke=TRUE,
              color = "black",
              weight=0.5,
              popup = isolines$range
  ) 