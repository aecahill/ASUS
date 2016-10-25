asus3<-read.table("C:/Users/Abigail/Desktop/asus3.txt",header=TRUE)
morphosites<-read.table("C:/Users/Abigail/Desktop/asus3sites.txt",header=TRUE)
biomol<-read.table("C:/Users/Abigail/Desktop/asu_biomol.txt",header=TRUE)
biomolsites<-read.table("C:/Users/Abigail/Desktop/asu_biomol_sites.txt",header=TRUE)

library(vegan)

#make collapsed list of taxa across regions
morpho<-cbind(asus3,morphosites)

seas<-c(1:26)
region = NULL

for (i in seas) {
  collapsed<-tapply(morpho[,i],morpho$Sea,sum)
  region<-cbind(region,collapsed)
  
}

region
colnames(region)<-colnames(morpho[1:26])
#remove Channel from dataset
region<-rbind(region[1:4,],region[6:7,])


#repeat for molecular data
biomol<-cbind(biomol,biomolsites)

biomolseas<-c(1:1608)
biomolregion = NULL

for (i in biomolseas) {
  collapsed<-tapply(biomol[,i],biomol$Sea,sum)
  biomolregion<-cbind(biomolregion,collapsed)
  
}

colnames(biomolregion)<-colnames(biomol[1:1608])

#Make distance matrices
braymorpho<-vegdist(region,method="bray")
braybiomol<-vegdist(biomolregion,method="bray")

#mantel test
mantel(braymorpho,braybiomol)


#collapsing the otus and redoing with collapsed
asuotu<-read.table("C:/Users/Abigail/Desktop/asuoct.txt",header=TRUE)

samples<-c(1:33)
otu = NULL

for (i in samples) {
  collapsed<-tapply(asuotu[,i],asuotu$Class,sum)
  otu<-cbind(otu,collapsed)
  
}

otu
colnames(otu)<-colnames(asuotu[1:33])
otu2<-t(otu)

coll<-cbind(otu2,biomolsites)

collseas<-c(1:33)
collregion = NULL

for (i in collseas) {
  collapsed<-tapply(coll[,i],coll$Sea,sum)
  collregion<-cbind(collregion,collapsed)
  
}

colnames(collregion)<-colnames(coll[1:33])

braycoll<-vegdist(collregion,method="bray")

mantel(braymorpho,braycoll)
mantel(braybiomol,braycoll)