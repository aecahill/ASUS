##Script to make an x-y plot correlating diversity metrics with morpho and mol data

#read in asu OTU data
asuotu<-read.table("C:/Users/acahill/Desktop/asus_oct.txt",header=TRUE)

#read in file with morphological simpson's indices
morphodiv<-read.table("C:/Users/acahill/Desktop/morphodiv.txt",header=TRUE)

#calculate diversity with OTUs
otudiv<-diversity(asuotu,index="simpson")

#correlate the two metrics
cor.test(morphodiv$SIMPSONS,otudiv)

#put the metrics in a single dataframe so I can use ggplot
div<-cbind(morphodiv,otudiv)
colnames(div)<-c("SIMPSONS","Location","asu","otudiv")

library(ggplot2)

ggplot(div,aes(SIMPSONS,otudiv))+
  geom_abline(slope=1,intercept=0)+
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
  ylim(0,1)+
  xlim(0,1)+
  xlab("\nMorphological diversity")+ylab("Molecular diversity\n")

#correlation after removing outlying AZTI points
noAZTI<-morphodiv[3:33,]
noAZTIotu<-otudiv[3:33]
cor.test(noAZTI$SIMPSONS,noAZTIotu)