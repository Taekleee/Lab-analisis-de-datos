library(tidyverse)

data <- read.csv("house-votes-84.data", header = TRUE, sep = ",",quote = "\"",fill=T)
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
data2 <- lapply(X=data,FUN = function(m) df <- summary(m))
class.name<-data2[1]
data2[1]<-NULL
data2<-do.call(rbind.data.frame, data2)
row.names(data2) <- c( "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
colnames(data2)<-c("?","n","y")

names<-c("democrat","republican")
type.vote<- c("y","n","?")
col.names <- c( "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")

a<-filter(data,classname == "democrat" & handicappedinfants == "y")
b<-filter(data,classname == "democrat" & handicappedinfants == "n")
c<-filter(data,classname == "democrat" & handicappedinfants == "?")

data3 <- lapply(X=col.names,FUN = function(m) df <- print(filter(data,classname == "democrat" & data$m == "y"))) #funciona
d<-lapply(X=col.names,FUN = function(m) df <- as.numeric(summary(unlist(select(as.data.frame(data3),m)))))

x<-as.numeric(summary(unlist(select(a,handicappedinfants))))
y<-as.numeric(summary(unlist(select(b,handicappedinfants))))
z<-as.numeric(summary(unlist(select(c,handicappedinfants))))

w<-as.numeric(summary(unlist(select(d,handicappedinfants))))
