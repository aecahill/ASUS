#need file asus3 which is the matrix of species
#need file sites with a column of site names and region names

#load vegan
library(vegan)

#compute NMDS
asus3nmds<-metaMDS(asus3)

#plot NMDS
ordiplot(asus3nmds,type="n")
ordihull(asus3nmds,groups=sites$region,draw="polygon",col="grey90",label=F)
orditorp(asus3nmds,display="species",col="black",air=0.01)
orditorp(asus3nmds,display="sites",col="red",air=0.01)

#analysis of similarity for sites and regions
anosim(asus3,sites$site)
anosim(asus3,sites$region)

#nested anosim
adonis(formula=asus3~sites$region+sites$site)
