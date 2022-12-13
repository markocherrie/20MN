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
  range = seq(20, 20, 20) * 60,
  range_type = "time",
  routing_mode = "fast",
  transport_mode = "pedestrian",
  traffic = TRUE,
  optimize = "balanced",
  consumption_model = NULL,
  aggregate = FALSE,
  url_only = FALSE
) %>%
  #mutate(name = paste0((range - 600) / 60," to ", range / 60, " mins"))
  mutate(name = paste0("0 to ", range / 60, " mins"))

# Convert to shapefile
isolines <- as(isolines, "Spatial")

# Create a color palette 
iso_1.colors <- c("#9e9ac8", "#006d2c")
iso_1.pal <- colorFactor(iso_1.colors, isolines$name)

# arrnage so closest is top
#isolines<-isolines %>% dplyr::arrange(-range)


library(leaflet)
leaflet() %>% 
  
  # Add background map
  addProviderTiles("CartoDB.Positron") %>% 
  
  # 
  addPolygons(data = isolines,
              #fill=TRUE,
              fillColor = ~iso_1.pal(isolines$name),
              fillOpacity=0.35,
              stroke=TRUE,
              color = "black",
              weight=0.5,
              popup = isolines$range, 
              group="20MN") %>%
  addLegend(position="bottomleft",
            values = isolines$name,
            pal = iso_1.pal, 
            opacity = 0.35,
            title = "Your 20 minute neighbourhood",
            group = "20MN") %>%
  
  # Layers control allows the user to turn layers on and off
  addLayersControl(options = layersControlOptions(collapsed = F),
                   overlayGroups = c("20MN"))