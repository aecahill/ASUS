##Script to make an x-y plot correlating mass and read number

#read in data
readmass<-read.table("C:/Users/acahill/Desktop/readmass.txt",header=TRUE)
colnames(readmass)<-c("Location","asu","read","taxon","weight")


#correlate the two metrics
cor.test(readmass$read,readmass$weight)

library(ggplot2)

ggplot(readmass,aes(read,weight))+
  geom_point(aes(color=Location), size = 4)+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
  scale_colour_manual(values=c("green","darkorange2","gold","black","purple","red"))+
  theme(axis.text.x= element_text(size=16))+
  theme(axis.text.y= element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(axis.title.y=element_text(size=16))+
  xlab("\nNumber of reads")+ylab("Mass (g)\n")