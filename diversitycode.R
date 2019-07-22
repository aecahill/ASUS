#Code for community analysis in Molecular Ecology course
#need file which is the matrix of species
#need file with a column of site names
#package vegan must already be downloaded

#load vegan
library(vegan)

species<-read.table("C:/Users/acahill/Desktop/asus.txt",header=TRUE)
sites<-read.table("C:/Users/acahill/Desktop/asussites.txt",header=TRUE)


#compute NMDS
nmds<-metaMDS(species)

#plot NMDS in base R
ordiplot(nmds,type="n") #plots blank axes

ordihull(nmds,groups=sites$Group,draw="polygon",col="grey90",label=F) #polygons connecting sites

orditorp(nmds,display="sites",col="red",air=0.01) #display sites on graph

orditorp(nmds,display="species",col="black",air=0.01) #display species on graph


#calculate diversity scores
shannon<-diversity(species, index="shannon")  #calculates the Shannon Index for each replicate
shannon #view the results

#statistically test the Shannon Index between your two groups
t.test(shannon~sites$Group)