
samples<-c(1:33)
otu = NULL

for (i in samples) {
  collapsed<-tapply(asuotu[,i],asuotu$Class,sum)
  otu<-cbind(otu,collapsed)
  
}

otu
colnames(otu)<-names$V1
otu2<-t(otu)
regions<-read.table("C:/Users/Abigail/Desktop/regions.txt")


#need file asus3 which is the matrix of species
#need file sites with a column of site names and region names


#load vegan
library(vegan)

#compute NMDS
asusotunmds<-metaMDS(otu2)

#plot NMDS in base R
#ordiplot(asusotunmds,type="n")
#ordiellipse(asusotunmds,groups=sites$regions$V1,draw="polygon",col="grey90",label=F)
#orditorp(asusotunmds,display="species",col="black",air=0.01)
#orditorp(asusotunmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
anosim(otu2,names$V1)
anosim(otu2,regions$V1)

#nested anosim

ordiplot(asusotunmds,type="n") #initiate plot
ordihull(asusotunmds,groups=regions$V1,draw="polygon",col="grey90",label=F) #make polygons
orditorp(asusotunmds,display="sites",col="red",air=0.01)#add site labels
orditorp(asusotunmds,display="species",col="black",air=0.01) #add species labels