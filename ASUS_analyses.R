#need file asus3 which is the matrix of species
#need file sites with a column of site names and region names

asus3<-read.table("C:/Users/Abigail/Desktop/asus3.txt",header=TRUE)
sites<-read.table("C:/Users/Abigail/Desktop/asusites.txt",header=TRUE)

#load vegan
library(vegan)

#compute NMDS
asus3nmds<-metaMDS(asus3)

#plot NMDS
#ordiplot(asus3nmds,type="n")
#ordiellipse(asus3nmds,groups=sites$Sea,draw="polygon",col="grey90",label=F)
#orditorp(asus3nmds,display="species",col="black",air=0.01)
#orditorp(asus3nmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
anosim(asus3,sites$site)
anosim(asus3,sites$Sea)

#nested anosim
adonis(formula=asus3~sites$Sea+sites$site)

#moving plot to ggplot

data.scores <- as.data.frame(scores(asus3nmds))
datascores<-cbind(data.scores,sites)
head(datascores)
species.scores <- as.data.frame(scores(asus3nmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)

library(ggplot2)

#make hulls

grp.a <- data.scores[datascores$Sea == "Adriatic", ][chull(datascores[datascores$Sea == 
                                                                        "Adriatic", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Sea == "Baltic", ][chull(datascores[datascores$Sea == 
                                                                      "Baltic", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Sea == "Biscay", ][chull(datascores[datascores$Sea == 
                                                                      "Biscay", c("NMDS1", "NMDS2")]), ]
grp.d <- data.scores[datascores$Sea == "Black", ][chull(datascores[datascores$Sea == 
                                                                     "Black", c("NMDS1", "NMDS2")]), ]
grp.e <- data.scores[datascores$Sea == "Channel", ][chull(datascores[datascores$Sea == 
                                                                       "Channel", c("NMDS1", "NMDS2")]), ]
grp.f <- data.scores[datascores$Sea == "Gulf_of_Lions", ][chull(datascores[datascores$Sea == 
                                                                             "Gulf_of_Lions", c("NMDS1", "NMDS2")]), ]
grp.g <- data.scores[datascores$Sea == "Red", ][chull(datascores[datascores$Sea == 
                                                                   "Red", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(grp.a, grp.b, grp.c, grp.d, grp.e, grp.f, grp.g) 
hull.sea<-c("Adriatic","Adriatic","Adriatic","Adriatic","Baltic","Baltic","Baltic","Biscay","Biscay","Biscay","Biscay","Biscay","Black","Black","Black","Black","Channel","Channel","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Red","Red","Red")
hull.data<-cbind(hull.data,hull.sea)

#plot in ggplot

ggplot() +
geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Sea),size=5) + # add the point markers
scale_colour_manual(values=c("green","darkorange2","gold","black","blue","purple","red")) +
coord_equal() +
theme_bw()+
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.sea),alpha=0.20)


ggplot(asusdiv, aes(x=Sea, y=SIMPSONS,color=Sea))+ geom_jitter(position=position_jitter(0.2), cex=6)+theme_bw()+theme(panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank())+xlab("\nSea")+ylab("Simpsons\n")+scale_colour_manual(values=c("green","darkorange2","gold","black","blue","purple","red"))+theme(axis.text.x= element_text(size=16))+theme(axis.text.y= element_text(size=16))

TukeyHSD(aov(asusdiv$SIMPSONS~asusdiv$Sea))
summary(aov(asusdiv$SIMPSONS~asusdiv$Sea))

