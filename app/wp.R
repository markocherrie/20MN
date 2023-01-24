wpuprn<-read.csv("data/Workplaces/workplaces_uprn.csv")


library(sf)
st_layers("data/Workplaces/osopenuprn_202211_gpkg/osopenuprn_202210.gpkg")

p = st_read("data/Workplaces/osopenuprn_202211_gpkg/osopenuprn_202210.gpkg",
            query="select * from osopenuprn_address where UPRN > 1000000000", 
            quiet=TRUE)

p = st_read("data/Workplaces/osopenuprn_202211_gpkg/osopenuprn_202210.gpkg",
             query="select * from osopenuprn_address where UPRN>")


wp_test <- st_read("data/Workplaces/osopenuprn_202211_gpkg/osopenuprn_202210.gpkg",
                    layer = "osopenuprn_address",
                    query="select UPRN,geom from osopenuprn_address")
