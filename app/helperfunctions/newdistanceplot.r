df<-structure(list(GStypes = c("Allotments", "Bowling Green", "Cemetery", 
                           "Golf Course", "Other Sports\n Facility", "Play Space", "Playing Field", 
                           "Public Park\nGarden", "Religious Grounds", "Tennis Court"
), time = c(20, 20, 20, 40, 20, 20, 30, 10, 20, 30), 
n = c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2), 
degree = c(0, 40, 50, 70, 90, 130, 220, 100, 150, 190)), 
class = "data.frame", 
row.names = c(NA, -10L))



Plot <- df %>%
  dplyr::arrange(GStypes) %>%
  ggplot(., aes(x = degree, y = time, group=1)) +
  # take out size=n if want the original
  #geom_polygon(fill="#008000", alpha = 0.6)+#
  geom_segment(aes( y=0, xend=degree, yend=time),fill=NA,
               arrow=arrow(angle=30,type="closed",length=unit(0.3,"cm")))+
  expand_limits(x=c(0,360))+
  coord_polar(start = 0)+
  geom_texthline(yintercept = 10, label = "10 minutes", 
                 hjust = 0, vjust = -0.2, color = "#2EB62C", linetype = 2)+
  geom_texthline(yintercept = 20, label = "20 minutes", 
                 hjust = 0, vjust = -0.2, color = "#57C84D", linetype = 2)+
  geom_texthline(yintercept = 30, label = "30 minutes", 
                 hjust = 0, vjust = -0.2, color = "#83D475", linetype = 2) +
  geom_texthline(yintercept = 40, label = "Over 30 minutes", 
                 hjust = 0, vjust = -0.2, color = "#ABE098", linetype = 2) +
  #geom_point(aes(size = n), color="grey22", alpha=0.7)+
  labs(title=" ") +
  scale_x_continuous(breaks = df$degree,
                     labels = df$GStypes)  +
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
        panel.border = element_blank(),
        #strip.text.x = element_text(size = 16),
        plot.title = element_text(size=18))
