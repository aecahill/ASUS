library(dplyr)

asus3<-read.table("C:/Users/Abigail/Desktop/asus3.txt",header=TRUE)
morphosites<-read.table("C:/Users/Abigail/Desktop/asus3sites.txt",header=TRUE)
asuotu<-read.table("C:/Users/Abigail/Desktop/asuoct.txt",header=TRUE)
biomolsites<-read.table("C:/Users/Abigail/Desktop/asu_biomol_sites.txt",header=TRUE)

#make collapsed list of morpho taxa across regions
morpho<-cbind(asus3,morphosites)

seas<-c(1:26)
taxalist = NULL

for (i in seas) {
  collapsed<-tapply(morpho[,i],morpho$Sea,sum)
  taxalist<-cbind(taxalist,collapsed)
  
}

#taxalist
colnames(taxalist)<-colnames(morpho[1:26])

#sum for each region
regionsums<-rowSums(taxalist)

#matrix of percentages
percents<-taxalist/regionsums


#for each row,check each element of the row, 
#compare it to 5%, make vector of col names that are true
#need 7 lists of names at the end

Adriatic<-percents[1,]
Adriatic<-as.data.frame(Adriatic)
Baltic<-percents[2,]
Baltic<-as.data.frame(Baltic)
Biscay<-percents[3,]
Biscay<-as.data.frame(Biscay)
Black<-percents[4,]
Black<-as.data.frame(Black)
Channel<-percents[5,]
Channel<-as.data.frame(Channel)
Gulf_of_Lions<-percents[6,]
Gulf_of_Lions<-as.data.frame(Gulf_of_Lions)
Red<-percents[7,]
Red<-as.data.frame(Red)

for (i in Adriatic){
  namesAd<-rownames(Adriatic)[i>0.05]
}

for (i in Baltic){
  namesBa<-rownames(Baltic)[i>0.05]
}

for (i in Biscay){
  namesBi<-rownames(Biscay)[i>0.05]
}

for (i in Black){
  namesBl<-rownames(Black)[i>0.05]
}

for (i in Channel){
  namesCh<-rownames(Channel)[i>0.05]
}

for (i in Gulf_of_Lions){
  namesGu<-rownames(Gulf_of_Lions)[i>0.05]
}

for (i in Red){
  namesRe<-rownames(Red)[i>0.05]
}




##ok now with the molecular data
#collapse molecular data

samples<-c(1:33)
otu = NULL

for (i in samples) {
  collapsed<-tapply(asuotu[,i],asuotu$Class,sum)
  otu<-cbind(otu,collapsed)
  
}

#otu
colnames(otu)<-colnames(asuotu[1:33])
otu2<-t(otu)
moldata<-cbind(otu2,biomolsites)


molseas<-c(1:33)
moltaxalist = NULL

for (i in molseas) {
  collapsed<-tapply(moldata[,i],moldata$Sea,sum)
  moltaxalist<-cbind(moltaxalist,collapsed)
  
}

#taxalist
colnames(moltaxalist)<-colnames(moldata[1:33])

#sum for each region
molregionsums<-rowSums(moltaxalist)

#matrix of percentages
molpercents<-moltaxalist/molregionsums

#for each row,check each element of the row, 
#compare it to 5%, make vector of col names that are true
#need 7 lists of names at the end

Adriaticmol<-molpercents[1,]
Adriaticmol<-as.data.frame(Adriaticmol)
Balticmol<-molpercents[2,]
Balticmol<-as.data.frame(Balticmol)
Biscaymol<-molpercents[3,]
Biscaymol<-as.data.frame(Biscaymol)
Blackmol<-molpercents[4,]
Blackmol<-as.data.frame(Blackmol)
Gulf_of_Lionsmol<-molpercents[5,]
Gulf_of_Lionsmol<-as.data.frame(Gulf_of_Lionsmol)
Redmol<-molpercents[6,]
Redmol<-as.data.frame(Redmol)

for (i in Adriaticmol){
  molnamesAd<-rownames(Adriaticmol)[i>0.05]
}

for (i in Balticmol){
  molnamesBa<-rownames(Balticmol)[i>0.05]
}

for (i in Biscaymol){
  molnamesBi<-rownames(Biscaymol)[i>0.05]
}

for (i in Blackmol){
  molnamesBl<-rownames(Blackmol)[i>0.05]
}

for (i in Gulf_of_Lionsmol){
  molnamesGu<-rownames(Gulf_of_Lionsmol)[i>0.05]
}

for (i in Redmol){
  molnamesRe<-rownames(Redmol)[i>0.05]
}


#list out the vectors of names
namesAd
namesBa
namesBi
namesBl
namesCh
namesGu
namesRe

#list out the vectors of names
molnamesAd
molnamesBa
molnamesBi
molnamesBl
molnamesGu
molnamesRe
