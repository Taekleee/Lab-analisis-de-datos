library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
#setwd("/Users/cata/Desktop/Lab-analisis-de-datos/Lab1")
#setwd("/home/d3f4ult/Escritorio/Lab-analisis-de-datos/Lab1")
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
#Salida: Una tabla en donde las columnas son los casos ( NV, Y , N, ETC) y las filas las medidas
statistical.measures<-function(
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


#HISTOGRAMA DE LOS VOTOS
plot.hist<-function(
  data,
  x,
  y,
  xlab,
  ylab,
  tittle
){
  ggplot(data, aes(x = x, y = y))+
    geom_bar(
      aes(fill =x ), stat = "identity", color = "black",
      position = position_dodge(0.9)
    )+
    labs(fill = "Votos")+
    ggtitle(tittle)+
    labs(x=xlab,y=ylab)
  
}

#DATA: DATOS ORIGINALES DE LA BASE DE DATOS, CON LOS POLÍTICOS Y SUS VOTOS
data <- read.csv("house-votes-84.data", header = TRUE, sep = ",",quote = "\"",fill=T)
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
democrat<-filter(data, data$classname == "democrat")
republican<-filter(data, data$classname == "republican")
data1<-data.filter(data,c("NV","n","y"))
data2 <- data.filter(democrat,c("DemocratNV","DemocratN","DemocratY"))
data3 <- data.filter(republican,c("RepublicanNV","RepublicanN","RepublicanY"))

#FINAL TIENE EN SUS COLUMNAS LOS NOMBRES DE LAS VOTACIONES Y EN SUS FILAS LOS TIPOS DE VOTOS (TAMBIÉN SEPARADOS EN 
#DEMOCRATAS Y REPUBLICANOS)
final <- c(data1,data2,data3)
final<-do.call(rbind.data.frame, final)
colnames(final) <- c("handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
row.names(final)<-c("No vota","No","Si","Dem. No vota","Dem. No","Dem. Si","Rep. No vota","Rep. No","Rep. Si")


#################################################GRÁFICOS#####################################################################
d.range<-final[4:6,]
r.range<-final[7:9,]

grid.arrange(
  plot.hist(d.range,rownames(d.range),d.range$"handicappedinfants","Tipo de voto","Cantidad de votos","Votos demócratas handicapped infants"),
  plot.hist(d.range,rownames(d.range),d.range$"waterprojectcostsharing","Tipo de voto","Cantidad de votos","Votos demócratas water project cost sharing"),
  plot.hist(d.range,rownames(d.range),d.range$"adoptionofthebudgetresolution","Tipo de voto","Cantidad de votos","Votos demócratas adoption of the budget resolution"),
  plot.hist(d.range,rownames(d.range),d.range$"physicianfeefreeze","Tipo de voto","Cantidad de votos","Votos demócratas physician fee freeze"),
  plot.hist(d.range,rownames(d.range),d.range$"elsalvadoraid","Tipo de voto","Cantidad de votos","Votos demócratas el salvador aid"),
  plot.hist(d.range,rownames(d.range),d.range$"religiousgroupsinschools","Tipo de voto","Cantidad de votos","Votos demócratas religious groups in schools"),
  plot.hist(d.range,rownames(d.range),d.range$"antisatellitetestban","Tipo de voto","Cantidad de votos","Votos demócratas antisatellite test ban"),
  plot.hist(d.range,rownames(d.range),d.range$"aidtonicaraguancontras","Tipo de voto","Cantidad de votos","Votos demócratas aid to nicaraguan contras"),
  plot.hist(d.range,rownames(d.range),d.range$"mxmissile","Tipo de voto","Cantidad de votos","Votos demócratas mx missile"),
  plot.hist(d.range,rownames(d.range),d.range$"immigration","Tipo de voto","Cantidad de votos","Votos demócratas immigration"),
  plot.hist(d.range,rownames(d.range),d.range$"synfuelscorporationcutback","Tipo de voto","Cantidad de votos","Votos demócratas synfuels corporation cutback"),
  plot.hist(d.range,rownames(d.range),d.range$"educationspending","Tipo de voto","Cantidad de votos","Votos demócratas education spending"),
  plot.hist(d.range,rownames(d.range),d.range$"superfundrighttosue","Tipo de voto","Cantidad de votos","Votos demócratas superfun dright to sue"),
  plot.hist(d.range,rownames(d.range),d.range$"crime","Tipo de voto","Cantidad de votos","Votos demócratas crime"),
  plot.hist(d.range,rownames(d.range),d.range$"dutyfreeexports","Tipo de voto","Cantidad de votos","Votos demócratas duty free exports"),
  plot.hist(d.range,rownames(d.range),d.range$"exportadministrationactsouthafrica","Tipo de voto","Cantidad de votos","Votos demócratas export administration act south africa"),
  ncol = 4
)

grid.arrange(
  plot.hist(r.range,rownames(r.range),r.range$"handicappedinfants","Tipo de voto","Cantidad de votos","Votos republicanos handicapped infants"),
  plot.hist(r.range,rownames(r.range),r.range$"waterprojectcostsharing","Tipo de voto","Cantidad de votos","Votos republicanos water project cost sharing"),
  plot.hist(r.range,rownames(r.range),r.range$"adoptionofthebudgetresolution","Tipo de voto","Cantidad de votos","Votos republicanos adoption of the budget resolution"),
  plot.hist(r.range,rownames(r.range),r.range$"physicianfeefreeze","Tipo de voto","Cantidad de votos","Votos republicanos physician fee freeze"),
  plot.hist(r.range,rownames(r.range),r.range$"elsalvadoraid","Tipo de voto","Cantidad de votos","Votos republicanos el salvador aid"),
  plot.hist(r.range,rownames(r.range),r.range$"religiousgroupsinschools","Tipo de voto","Cantidad de votos","Votos republicanos religious groups in schools"),
  plot.hist(r.range,rownames(r.range),r.range$"antisatellitetestban","Tipo de voto","Cantidad de votos","Votos republicanos antisatellite test ban"),
  plot.hist(r.range,rownames(r.range),r.range$"aidtonicaraguancontras","Tipo de voto","Cantidad de votos","Votos republicanos aid to nicaraguan contras"),
  plot.hist(r.range,rownames(r.range),r.range$"mxmissile","Tipo de voto","Cantidad de votos","Votos republicanos mx missile"),
  plot.hist(r.range,rownames(r.range),r.range$"immigration","Tipo de voto","Cantidad de votos","Votos republicanos immigration"),
  plot.hist(r.range,rownames(r.range),r.range$"synfuelscorporationcutback","Tipo de voto","Cantidad de votos","Votos republicanos synfuels corporation cutback"),
  plot.hist(r.range,rownames(r.range),r.range$"educationspending","Tipo de voto","Cantidad de votos","Votos republicanos education spending"),
  plot.hist(r.range,rownames(r.range),r.range$"superfundrighttosue","Tipo de voto","Cantidad de votos","Votos republicanos superfun dright to sue"),
  plot.hist(r.range,rownames(r.range),r.range$"crime","Tipo de voto","Cantidad de votos","Votos republicanos crime"),
  plot.hist(r.range,rownames(r.range),r.range$"dutyfreeexports","Tipo de voto","Cantidad de votos","Votos republicanos duty free exports"),
  plot.hist(r.range,rownames(r.range),r.range$"exportadministrationactsouthafrica","Tipo de voto","Cantidad de votos","Votos republicanos export administration act south africa"),
  ncol = 4)

