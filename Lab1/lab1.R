library(tidyverse)
#setwd("/Users/cata/Desktop/Lab-analisis-de-datos/Lab1")
#DATA.FILTER obtiene la tabla de frecuencia de los votos totales, de democratas y republicanos par cada
#votación

data.filter<-function(
  data,
  col.names
){
  data2<-filter(data, data$classname == "democrat")
  data2 <- lapply(X=data,FUN = function(m) df <- summary(m))
  data2[1]<-NULL
  data2<-do.call(rbind.data.frame, data2)
  colnames(data2)<-col.names
  data2
}


medidas<-function(
  data,
){
  names <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
  
}

#DATA: DATOS ORIGINALES DE LA BASE DE DATOS, CON LOS POLÍTICOS Y SUS VOTOS
data <- read.csv("house-votes-84.data", header = TRUE, sep = ",",quote = "\"",fill=T)
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
democrat<-filter(data, data$classname == "democrat")
republican<-filter(data, data$classname == "republican")
data1<-data.filter(data,c("?","n","y"))
data2 <- data.filter(democrat,c("Democrat ?","Democrat n","Democrat y"))
data3 <- data.filter(republican,c("Republican ?","Republican n","Republican y"))

final <- c(data1,data2,data3)
final<-do.call(cbind.data.frame, final)
rownames(final) <- c( "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")

