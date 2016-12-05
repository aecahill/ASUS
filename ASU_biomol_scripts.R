#need file asus3 which is the matrix of species
#need file sites with a column of site names and region names
#This is for biomol data, raw OTUs

asus3<-read.table("C:/Users/acahill/Desktop/asus_oct.txt",header=TRUE)
sites<-read.table("C:/Users/acahill/Desktop/asu_biomol_sites.txt",header=TRUE)
rich<-read.table("C:/Users/acahill/Desktop/otu_rich_oct.txt",header=TRUE)

#load vegan
library(vegan)
library(pracma)

#fourth-root transform the data
vec<-1:1606
asus4 = NULL

for (i in vec) {
  
  b<-nthroot(asus3[,i],4)
  asus4<-cbind(asus4,b)
}


#compute NMDS
#asus3nmds<-metaMDS(asus3)
asus4nmds<-metaMDS(asus4,autotransform=FALSE)

#plot NMDS in base R
#ordiplot(asus3nmds,type="n")
#ordiellipse(asus3nmds,groups=sites$Sea,draw="polygon",col="grey90",label=F)
#orditorp(asus3nmds,display="species",col="black",air=0.01)
#orditorp(asus3nmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
#anosim(asus3,sites$site)
#anosim(asus3,sites$Sea)

#nested anosim
adonis(formula=asus4~sites$Sea+sites$site)
#adonis2(formula=asus3~sites$Sea+sites$site,add=TRUE,by="terms")

#moving plot to ggplot

data.scores <- as.data.frame(scores(asus4nmds))
datascores<-cbind(data.scores,sites)
head(datascores)
species.scores <- as.data.frame(scores(asus4nmds, "species"))
species.scores$species <- rownames(species.scores)
head(species.scores)



library(ggplot2)

#make hulls, one for each sea
#this needs to be checked with each run because depends on data

grp.a <- data.scores[datascores$Sea == "Adriatic", ][chull(datascores[datascores$Sea == 
                                                                        "Adriatic", c("NMDS1", "NMDS2")]), ]
grp.b <- data.scores[datascores$Sea == "Baltic", ][chull(datascores[datascores$Sea == 
                                                                      "Baltic", c("NMDS1", "NMDS2")]), ]
grp.c <- data.scores[datascores$Sea == "Biscay", ][chull(datascores[datascores$Sea == 
                                                                      "Biscay", c("NMDS1", "NMDS2")]), ]
grp.d <- data.scores[datascores$Sea == "Black", ][chull(datascores[datascores$Sea == 
                                                                     "Black", c("NMDS1", "NMDS2")]), ]
grp.f <- data.scores[datascores$Sea == "Gulf_of_Lions", ][chull(datascores[datascores$Sea == 
                                                                             "Gulf_of_Lions", c("NMDS1", "NMDS2")]), ]
grp.g <- data.scores[datascores$Sea == "Red", ][chull(datascores[datascores$Sea == 
                                                                   "Red", c("NMDS1", "NMDS2")]), ]

hull.data <- rbind(grp.a, grp.b, grp.c, grp.d, grp.f, grp.g) #turn the hulls into a single dataframe
hull.sea<-c("Adriatic","Adriatic","Adriatic","Adriatic","Adriatic","Baltic","Baltic","Baltic","Biscay","Biscay","Biscay","Biscay","Biscay","Black","Black","Black","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Gulf_of_Lions","Red","Red","Red","Red") #add column for groups (these are based on this data only)
hull.data<-cbind(hull.data,hull.sea) #attach group names to hull dataframe

#plot in ggplot

ggplot() +
  geom_point(data=datascores,aes(x=NMDS1,y=NMDS2,colour=Sea),size=5) + # add the point markers
  scale_colour_manual(values=c("green","darkorange2","gold","black","purple","red")) +
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
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,group=hull.sea),alpha=0.20) #add polygon based on the hulls calculated


#diversity statistics

asusdiv<-cbind(diversity(asus3,index="simpson"),rich) #calculate simpsons index, bind to site information

colnames(asusdiv)<-c("simpsons","margalef","Sea") #rename columns


summary(aov(asusdiv$simpsons~asusdiv$Sea)) #anova among regions
TukeyHSD(aov(asusdiv$simpsons~asusdiv$Sea)) #post-hoc tests among regions

#Plot of diversity stats

ggplot(asusdiv, aes(x=Sea, y=simpsons,color=Sea))+ 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2), cex=6)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  xlab("\nLocation")+ylab("Simpsons\n")+
  scale_colour_manual(values=c("darkorange2","gold","purple","green","black","red"))+
  scale_x_discrete(labels=c("Baltic","Biscay","Gulf of Lions","Adriatic","Black","Red"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  theme(legend.position="none")+
  ylim(0,1)+
  annotate("text", x = 1, y = 0.9, label = "ab", size = 6)+
  annotate("text", x = 2, y = 0.95, label = "c", size = 6)+
  annotate("text", x = 3, y = 0.99, label = "c", size = 6)+
  annotate("text", x = 4, y = 0.92, label = "c", size = 6)+
  annotate("text", x = 5, y = 0.57, label = "a", size = 6)+
  annotate("text", x = 6, y = 0.87, label = "bc", size = 6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#margalef richness

summary(aov(rich$margalef~rich$region))
TukeyHSD(aov(rich$margalef~rich$region))

ggplot(rich, aes(x=region, y=margalef,color=region))+ 
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.2), cex=6)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  xlab("\nLocation")+ylab("Margalef\n")+
  scale_colour_manual(values=c("darkorange2","gold","purple","green","black","red"))+
  scale_x_discrete(labels=c("Baltic","Biscay","Gulf of Lions","Adriatic","Black","Red"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  theme(legend.position="none")+
  ylim(0,20)+
  annotate("text", x = 1, y = 2, label = "a", size = 6)+
  annotate("text", x = 2, y = 2, label = "bc", size = 6)+
  annotate("text", x = 3, y = 2, label = "c", size = 6)+
  annotate("text", x = 4, y = 2, label = "ab", size = 6)+
  annotate("text", x = 5, y = 2, label = "a", size = 6)+
  annotate("text", x = 6, y = 2, label = "ab", size = 6)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


