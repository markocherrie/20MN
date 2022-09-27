# viz

# load packages
library(ggplot2)
library(geomtextpath)

# create dataframe
time<-as.numeric(sample(1:40, 42, replace=TRUE))
domain<-as.factor(rep(c("moving\n around", "public\n transport", "traffic and\n parking",
                        "streets\n and spaces", "natural\n space", "play and\n recreation",
                        "facilities\n and amenities", "work and\n economy", 
                        "housing\n and community", "social\n contact", 
                        "identity and\n belonging", "feeling\n safe", 
                        "care and\n maintenance", "influence and \nsense of control"),3))
mode<-as.factor(rep(c("Walk", "Cycle", "Transit"),14))
df <- data.frame(time, domain, mode)

# for colouring the text
#mycolors<- c("red","blue","green","red","blue","green","red","blue","green",
#             "red","blue","green","red","blue")

# test
ggplot(df, aes(x = domain, y = time, color = domain, group=mode)) +
  geom_path(fill=NA)
  
# plot
library(dplyr)
df %>%
  dplyr::arrange(domain) %>%
  ggplot(., aes(x = domain, y = time, group=mode)) +
  geom_polygon(fill="grey", alpha = 0.6)+
  geom_point(color="black")+
  coord_curvedpolar()+ 
  geom_texthline(yintercept = 10, label = "10 minutes", 
                 hjust = 0, vjust = -0.2, color = "grey")+
  geom_texthline(yintercept = 20, label = "20 minutes", 
                 hjust = 0, vjust = -0.2, color = "grey")+
  geom_texthline(yintercept = 30, label = "30 minutes", 
                 hjust = 0, vjust = -0.2, color = "grey") +
  theme_bw() +
  facet_wrap(~ mode)+
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

