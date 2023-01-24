
  # create base
  base<-ggplot(tb, aes(x = Type))+
    coord_flip()+ theme_minimal() + theme(panel.border = element_blank(), 
                                     panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     axis.ticks.x=element_blank(),
                                     axis.text.y=element_text(size=15, color="white"),
                                     plot.background = element_rect(fill = "#1c1c1d"), 
                                     panel.background = element_rect(fill = "#1c1c1d", colour="#1c1c1d")
    )+
    xlab("")+
    theme(plot.margin = unit(c(0.85,0.5,0.3,0),"cm"))
  
  p2 <- ggplot(tb, aes(x = Type, y = Perc.y))+
    geom_col(aes(fill = Type))+ coord_flip()+ylim(0,100)+
    #scale_fill_manual(breaks = c("MANAGERS, DIRECTORS AND SENIOR OFFICIALS",
    #                            "PROFESSIONAL OCCUPATIONS",
    #                             "ASSOCIATE PROFESSIONAL AND TECHNICAL OCCUPATIONS",
    #                            "ADMINISTRATIVE AND SECRETARIAL OCCUPATIONS",
    #                           "SKILLED TRADES OCCUPATIONS",
    #                          "CARING, LEISURE AND OTHER SERVICE OCCUPATIONS",
    #                         "SALES AND CUSTOMER SERVICE OCCUPATIONS",
    #                        "PROCESS, PLANT AND MACHINE OPERATIVES",
    #                       "ELEMENTARY OCCUPATIONS"), 
    #           values=c("#88CCEE", "#CC6677", "#DDCC77", "#117733", 
    #                   "#332288", "#AA4499", "#44AA99", "#999933", "#661100"))+
  geom_text(aes(label=paste0(Perc.y,"%")),hjust=-0.25, vjust=0.5, color="white", size=6)+
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 15, face = "bold", color="white"),
          plot.background = element_rect(fill = "#1c1c1d"), 
          panel.background = element_rect(fill = "#1c1c1d", colour="#1c1c1d"),
          panel.border = element_blank()
          )+
    #geom_hline(yintercept=c(48.6), linetype="dashed")+
    theme(legend.position = "none")+
    ggtitle("   Your neighbourhood")

  
  
  p3 <- ggplot(tb, aes(x = Type, y = Perc))+
    geom_col(aes(fill = Type))+ coord_flip()+ylim(0,100)+
    # put in custom colours here!!!
    #scale_fill_manual(breaks = c("MANAGERS, DIRECTORS AND SENIOR OFFICIALS",
    #                            "PROFESSIONAL OCCUPATIONS",
    #                             "ASSOCIATE PROFESSIONAL AND TECHNICAL OCCUPATIONS",
    #                            "ADMINISTRATIVE AND SECRETARIAL OCCUPATIONS",
    #                           "SKILLED TRADES OCCUPATIONS",
    #                          "CARING, LEISURE AND OTHER SERVICE OCCUPATIONS",
    #                         "SALES AND CUSTOMER SERVICE OCCUPATIONS",
    #                        "PROCESS, PLANT AND MACHINE OPERATIVES",
    #                       "ELEMENTARY OCCUPATIONS"), 
    #           values=c("#88CCEE", "#CC6677", "#DDCC77", "#117733", 
  #                   "#332288", "#AA4499", "#44AA99", "#999933", "#661100"))+
  geom_text(aes(label=paste0(Perc, "%")),hjust=-0.25, vjust=0.5, color="white", size=6)+
    theme_minimal()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 15, face = "bold", color="white"),
          plot.background = element_rect(fill = "#1c1c1d"), 
          panel.background = element_rect(fill = "#1c1c1d", colour="#1c1c1d"),
          panel.border = element_blank())+
    #geom_hline(yintercept=c(48.6), linetype="dashed")+
    theme(legend.position = "none")+
    ggtitle("   Scotland")
  
  
  
  figure <- ggarrange(base, p2,p3,
                      ncol = 3, nrow = 1, widths = c(1, 1.5, 1.5))
  
  figure