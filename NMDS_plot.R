#Code to make a plot based on an nmds result
#requires data matrix of taxa and their counts (here, asus2)
#requires vector of sites (here, sites)

library(vegan) #load vegan

asus2nmds<-metaMDS(asus2) #compute nmds

ordiplot(asus2nmds,type="n") #initiate plot
ordihull(asus2nmds,groups=sites,draw="polygon",col="grey90",label=F) #make polygons
orditorp(asus2nmds,display="sites",col="red",air=0.01)#add site labels
orditorp(asus2nmds,display="species",col="black",air=0.01) #add species labels

