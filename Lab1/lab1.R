library(tidyverse)
library(ggplot2)
library(ggpubr)
#setwd("/Users/cata/Desktop/Lab-analisis-de-datos/Lab1")
#DATA.FILTER obtiene la tabla de frecuencia de los votos totales, de democratas y republicanos par cada
#votación

data.filter<-function(
  data,
  col.names
){
  data2 <- lapply(X=data,FUN = function(m) df <- summary(m))
  data2[1]<-NULL
  data2<-do.call(rbind.data.frame, data2)
  colnames(data2)<-col.names
  data2
}


#Cálculo de las medidas como varianza, desviación, media y mediana
#Salida: Una tabla en donde las filas son los casos ( NV, Y , N, ETC) y las columnas las medidas
medidas<-function(
  data,
  mean =NULL,
  sd = NULL,
  median = NULL,
  var = NULL
){
  for(i in 1:length(data)){
    mean[i]<-mean(data[,i])
    sd[i]<-sd(data[,i])
    median[i]<-median(data[,i])
    var[i]<-var(data[,i])
  }
  
  result<- data.frame("mean" = mean, "sd" = sd, "median" = median,"var" = var)
  row.names(result)<-c("NV","n","y","DemocratNV","DemocratN","DemocratY","RepublicanNV","RepublicanN","RepublicY")
  result
}

#DATA: DATOS ORIGINALES DE LA BASE DE DATOS, CON LOS POLÍTICOS Y SUS VOTOS
data <- read.csv("house-votes-84.data", header = TRUE, sep = ",",quote = "\"",fill=T)
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
democrat<-filter(data, data$classname == "democrat")
republican<-filter(data, data$classname == "republican")
data1<-data.filter(data,c("NV","n","y"))
data2 <- data.filter(democrat,c("DemocratNV","DemocratN","DemocratY"))
data3 <- data.filter(republican,c("RepublicanNV","RepublicanN","RepublicanY"))

final <- c(data1,data2,data3)
final<-do.call(rbind.data.frame, final)
colnames(final) <- c("handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
row.names(final)<-c("NV","n","y","DemocratNV","DemocratN","DemocratY","RepublicanNV","RepublicanN","RepublicY")


ggplot(final[1:2,], aes(x = rownames(final[1:2,]), y = handicappedinfants))+
  geom_bar(
    aes(fill =rownames(final[1:2,]) ), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )
