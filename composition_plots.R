asus3<-read.table("C:/Users/Abigail/Desktop/asus3.txt",header=TRUE)
morphosites<-read.table("C:/Users/Abigail/Desktop/asus3sites.txt",header=TRUE)
biomol<-read.table("C:/Users/Abigail/Desktop/asu_biomol.txt",header=TRUE)
biomolsites<-read.table("C:/Users/Abigail/Desktop/asu_biomol_sites.txt",header=TRUE)

library(vegan)
library(ggplot2)
library(tidyr)

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

#Find taxa that represent at least 0.1% in the dataset

tots<-colSums(region)
grandtot<-sum(tots)
percs<-100*(tots/grandtot)

#Remove all taxa that do not represent 0.1% in the dataset

filtered = NULL
taxanames<-colnames(region)
namesvec = NULL

taxa<-c(1:26)

for (i in taxa) {
  if (percs[i]>= 0.1) {
    filtered<-cbind(filtered,region[,i])
    namesvec<-c(namesvec,taxanames[i])
  }
}

colnames(filtered)<-namesvec
regnames<-c("Adriatic","Baltic","Biscay","Black","Channel","Gulf_of_Lions","Red")
filtered<-as.data.frame(cbind(filtered,regnames))

morphofiltered<-gather(filtered,key=regnames,value=taxon,1:14)
abundance<-as.numeric(morphofiltered$taxon)
morphofiltered<-cbind(morphofiltered,abundance)
colnames(morphofiltered)<-c("region","taxon","V3","abundance")


#Graph

colors<-brewer.pal(9,"Set1")
pal<-colorRampPalette(colors)
good_palette<-pal(20)

compositionplot <-ggplot(morphofiltered, aes(x=region, y=abundance, fill=taxon)) +
  geom_bar(stat="identity") + 
  theme_bw()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  xlab("Population")+
  theme(axis.title.x = element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  scale_fill_manual(values = good_palette) +
  theme_bw() + ylab("Abundance\n") + 
  theme(axis.text.x= element_text(angle = 90))


##biomolplots for COLLAPSED TO CLASS LEVEL ANALYSIS

#make collapsed list of taxa across regions
biomol<-cbind(otu2,biomolsites)

seas<-c(1:33)
region = NULL

for (i in seas) {
  collapsed<-tapply(biomol[,i],biomol$Sea,sum)
  region<-cbind(region,collapsed)
  
}

#region
colnames(region)<-colnames(biomol[1:33])

#Find taxa that represent at least 1% in the dataset

tots<-colSums(region)
grandtot<-sum(tots)
percs<-100*(tots/grandtot)

#Remove all taxa that do not represent 0.1% in the dataset

filtered = NULL
taxanames<-colnames(region)
namesvec = NULL

taxa<-c(1:33)

for (i in taxa) {
  if (percs[i]>= 0.1) {
    filtered<-cbind(filtered,region[,i])
    namesvec<-c(namesvec,taxanames[i])
  }
}

colnames(filtered)<-namesvec
regnames<-c("Adriatic","Baltic","Biscay","Black","Gulf_of_Lions","Red")
filtered<-as.data.frame(cbind(filtered,regnames))

biomolfiltered<-gather(filtered,key=regnames,value=taxon,1:20)
abundance<-as.numeric(biomolfiltered$taxon)
biomolfiltered<-cbind(biomolfiltered,abundance)
colnames(biomolfiltered)<-c("region","taxon","V3","abundance")


#Graph

colors<-brewer.pal(9,"Set1")
pal<-colorRampPalette(colors)
good_palette<-pal(20)

#good_palette<-c("#71742d","#b657c5","#56c267","#d64585","#68a536","#676acd","#b6b43c","#964e83","#4ca47e","#ce4e33","#5c99cf","#d18837","#cf8ecd","#be9f63","#c36261")

compositioncollapsedplot <-ggplot(biomolfiltered, aes(x=region, y=abundance, fill=taxon)) +
  geom_bar(stat="identity") + 
  theme_bw()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  xlab("Population")+
  theme(axis.title.x = element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  scale_fill_manual(values = good_palette) +
  theme_bw() + ylab("Read\n") + 
  theme(axis.text.x= element_text(angle = 90))


##biomolplots 

biomol<-read.table("C:/Users/Abigail/Desktop/asu_biomol.txt",header=TRUE)
biomolsites<-read.table("C:/Users/Abigail/Desktop/asu_biomol_sites.txt",header=TRUE)

#make collapsed list of taxa across regions
biomol<-cbind(biomol,biomolsites)

seas<-c(1:1608)
region = NULL

for (i in seas) {
  collapsed<-tapply(biomol[,i],biomol$Sea,sum)
  region<-cbind(region,collapsed)
  
}

#region
colnames(region)<-colnames(biomol[1:1608])

#Find taxa that represent at least 1% in the dataset

tots<-colSums(region)
grandtot<-sum(tots)
percs<-100*(tots/grandtot)

#Remove all taxa that do not represent 1% in the dataset

filtered = NULL
taxanames<-colnames(region)
namesvec = NULL

taxa<-c(1:1608)

for (i in taxa) {
  if (percs[i]>= 1) {
    filtered<-cbind(filtered,region[,i])
    namesvec<-c(namesvec,taxanames[i])
  }
}

colnames(filtered)<-namesvec
regnames<-c("Adriatic","Baltic","Biscay","Black","Gulf_of_Lions","Red")
filtered<-as.data.frame(cbind(filtered,regnames))

biomolfiltered<-gather(filtered,key=regnames,value=taxon,1:24)
abundance<-as.numeric(biomolfiltered$taxon)
biomolfiltered<-cbind(biomolfiltered,abundance)
colnames(biomolfiltered)<-c("region","taxon","V3","abundance")


#Graph

colors<-brewer.pal(9,"Set1")
pal<-colorRampPalette(colors)
good_palette<-pal(24)

#good_palette<-c("#71742d","#b657c5","#56c267","#d64585","#68a536","#676acd","#b6b43c","#964e83","#4ca47e","#ce4e33","#5c99cf","#d18837","#cf8ecd","#be9f63","#c36261")

compositioncollapsedplot <-ggplot(biomolfiltered, aes(x=region, y=abundance, fill=taxon)) +
  geom_bar(stat="identity") + 
  theme_bw()+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  xlab("Population")+
  theme(axis.title.x = element_text(size=12))+
  theme(axis.title.y=element_text(size=12))+
  scale_fill_manual(values = good_palette) +
  theme_bw() + ylab("Read\n") + 
  theme(axis.text.x= element_text(angle = 90))