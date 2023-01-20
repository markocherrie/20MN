#### 

Site<-readRDS("data/OSgreenspace/data/Site.rds")

library(sf)
LA<-read_sf("data/Local_Authority_Boundaries_-_Scotland/pub_las.shp")

for(i in unique(LA$local_auth)){
LA1<-LA[LA$local_auth==i,]
LAsites <- Site[LA1,]
saveRDS(LAsites, paste0("data/OSgreenspace/LAdata/", i,".rds"))
}


#Site<-readRDS("data/OSgreenspace/LAdata/Clackmannanshire.rds")
<<<<<<< HEAD


Site<-read_sf("data/bluespaces/lakes/UK_Lakes_-_Lakes_Portal_(UKCEH).shp")

library(sf)
LA<-read_sf("data/Local_Authority_Boundaries_-_Scotland/pub_las.shp")

for(i in unique(LA$local_auth)){
  LA1<-LA[LA$local_auth==i,]
  LAsites <- Site[LA1,]
  saveRDS(LAsites, paste0("data/bluespaces/lakes/LAdata/", i,".rds"))
}

done<-gsub(".rds", "", list.files("data/bluespaces/lakes/LAdata/"))

s<-as.data.frame(LA$local_auth)
s$status<-unique(LA$local_auth) %in% done
s<-s[s$status=="FALSE",]
newlist<-dput(s$`LA$local_auth`)

# still got 4 to do!!!!

for(i in newlist){
  LA1<-LA[LA$local_auth==i,]
  LAsites <- Site[LA1,]
  saveRDS(LAsites, paste0("data/bluespaces/lakes/LAdata/", i,".rds"))
}



