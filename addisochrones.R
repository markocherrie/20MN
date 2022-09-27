library(leaflet)
library(leaflet.extras2)

Sys.setenv("OPRS" = '')

leaflet() %>%
  addTiles() %>%
  setView(8, 50, 10) %>%
  addReachability()


# maybe this: https://xang1234.github.io/isochrone/