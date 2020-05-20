library(tidyverse)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(caret) #-> install.packages("e1071")

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
plot.hist1<-function(
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



plot.hist2<-function(
  data,
  tittle,
  xlab,
  color
){
  hist(democrat.contingency[,1], 
       main=tittle, 
       xlab=xlab, 
       ylab = "Frecuencia",
       border="black", 
       col=color,
       las=1, 
       breaks=5)
}

#############################GRÁFICO DE CAJA DE LOS VOTOS (DEMÓCRATAS VS REPUBLICANOS)###############################

plot.box<-function(
  table,
  x,
  y,
  xlab,
  ylab,
  tittle
  
){
  ggboxplot(
    table, x = x, y = y,
    color = x, palette = c("#00AFBB", "#E7B800", "#FC4E07"),
    xlab = xlab,
    add = "jitter",
    ylab = ylab
  ) 
}


#DATA: DATOS ORIGINALES DE LA BASE DE DATOS, CON LOS POLÍTICOS Y SUS VOTOS
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"
data <- read.csv(url, header = TRUE, sep = ",",quote = "\"",fill=T)
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
  plot.hist1(d.range,rownames(d.range),d.range$"handicappedinfants","Tipo de voto","Cantidad de votos","Votos demócratas handicapped infants"),
  plot.hist1(d.range,rownames(d.range),d.range$"waterprojectcostsharing","Tipo de voto","Cantidad de votos","Votos demócratas water project cost sharing"),
  plot.hist1(d.range,rownames(d.range),d.range$"adoptionofthebudgetresolution","Tipo de voto","Cantidad de votos","Votos demócratas adoption of the budget resolution"),
  plot.hist1(d.range,rownames(d.range),d.range$"physicianfeefreeze","Tipo de voto","Cantidad de votos","Votos demócratas physician fee freeze"),
  plot.hist1(d.range,rownames(d.range),d.range$"elsalvadoraid","Tipo de voto","Cantidad de votos","Votos demócratas el salvador aid"),
  plot.hist1(d.range,rownames(d.range),d.range$"religiousgroupsinschools","Tipo de voto","Cantidad de votos","Votos demócratas religious groups in schools"),
  plot.hist1(d.range,rownames(d.range),d.range$"antisatellitetestban","Tipo de voto","Cantidad de votos","Votos demócratas antisatellite test ban"),
  plot.hist1(d.range,rownames(d.range),d.range$"aidtonicaraguancontras","Tipo de voto","Cantidad de votos","Votos demócratas aid to nicaraguan contras"),
  plot.hist1(d.range,rownames(d.range),d.range$"mxmissile","Tipo de voto","Cantidad de votos","Votos demócratas mx missile"),
  plot.hist1(d.range,rownames(d.range),d.range$"immigration","Tipo de voto","Cantidad de votos","Votos demócratas immigration"),
  plot.hist1(d.range,rownames(d.range),d.range$"synfuelscorporationcutback","Tipo de voto","Cantidad de votos","Votos demócratas synfuels corporation cutback"),
  plot.hist1(d.range,rownames(d.range),d.range$"educationspending","Tipo de voto","Cantidad de votos","Votos demócratas education spending"),
  plot.hist1(d.range,rownames(d.range),d.range$"superfundrighttosue","Tipo de voto","Cantidad de votos","Votos demócratas superfun dright to sue"),
  plot.hist1(d.range,rownames(d.range),d.range$"crime","Tipo de voto","Cantidad de votos","Votos demócratas crime"),
  plot.hist1(d.range,rownames(d.range),d.range$"dutyfreeexports","Tipo de voto","Cantidad de votos","Votos demócratas duty free exports"),
  plot.hist1(d.range,rownames(d.range),d.range$"exportadministrationactsouthafrica","Tipo de voto","Cantidad de votos","Votos demócratas export administration act south africa"),
  ncol = 4
)

grid.arrange(
  plot.hist1(r.range,rownames(r.range),r.range$"handicappedinfants","Tipo de voto","Cantidad de votos","Votos republicanos handicapped infants"),
  plot.hist1(r.range,rownames(r.range),r.range$"waterprojectcostsharing","Tipo de voto","Cantidad de votos","Votos republicanos water project cost sharing"),
  plot.hist1(r.range,rownames(r.range),r.range$"adoptionofthebudgetresolution","Tipo de voto","Cantidad de votos","Votos republicanos adoption of the budget resolution"),
  plot.hist1(r.range,rownames(r.range),r.range$"physicianfeefreeze","Tipo de voto","Cantidad de votos","Votos republicanos physician fee freeze"),
  plot.hist1(r.range,rownames(r.range),r.range$"elsalvadoraid","Tipo de voto","Cantidad de votos","Votos republicanos el salvador aid"),
  plot.hist1(r.range,rownames(r.range),r.range$"religiousgroupsinschools","Tipo de voto","Cantidad de votos","Votos republicanos religious groups in schools"),
  plot.hist1(r.range,rownames(r.range),r.range$"antisatellitetestban","Tipo de voto","Cantidad de votos","Votos republicanos antisatellite test ban"),
  plot.hist1(r.range,rownames(r.range),r.range$"aidtonicaraguancontras","Tipo de voto","Cantidad de votos","Votos republicanos aid to nicaraguan contras"),
  plot.hist1(r.range,rownames(r.range),r.range$"mxmissile","Tipo de voto","Cantidad de votos","Votos republicanos mx missile"),
  plot.hist1(r.range,rownames(r.range),r.range$"immigration","Tipo de voto","Cantidad de votos","Votos republicanos immigration"),
  plot.hist1(r.range,rownames(r.range),r.range$"synfuelscorporationcutback","Tipo de voto","Cantidad de votos","Votos republicanos synfuels corporation cutback"),
  plot.hist1(r.range,rownames(r.range),r.range$"educationspending","Tipo de voto","Cantidad de votos","Votos republicanos education spending"),
  plot.hist1(r.range,rownames(r.range),r.range$"superfundrighttosue","Tipo de voto","Cantidad de votos","Votos republicanos superfun dright to sue"),
  plot.hist1(r.range,rownames(r.range),r.range$"crime","Tipo de voto","Cantidad de votos","Votos republicanos crime"),
  plot.hist1(r.range,rownames(r.range),r.range$"dutyfreeexports","Tipo de voto","Cantidad de votos","Votos republicanos duty free exports"),
  plot.hist1(r.range,rownames(r.range),r.range$"exportadministrationactsouthafrica","Tipo de voto","Cantidad de votos","Votos republicanos export administration act south africa"),
  ncol = 4)


#############################################TABLA DE CONTINGENCIA######################################################
names <- c('y','n','?')
democrat.contingency <- as.data.frame(sapply(names, function(x) rowSums(democrat[,2:17]==x)))
colnames(democrat.contingency) <- names
class<-rep("democrat",267)
democrat.contingency$class<-class

republican.contingency <- as.data.frame(sapply(names, function(x) rowSums(republican[,2:17]==x)))
colnames(republican.contingency) <- names
class<-rep("republican",167)
republican.contingency$class<-class

table <-rbind(democrat.contingency,republican.contingency)
colnames(table)<- c('y','n','nv','class')


grid.arrange(plot.box(table,"class","y","Clases","Votos si","Votos si demócratas vs republicanos"),
             plot.box(table,"class","n","Clases","Votos no","Votos si demócratas vs republicanos"),
             plot.box(table,"class","nv","Clases","No vota","Votos si demócratas vs republicanos"),
             ncol = 3)

#Se indica como factor la clase que incluye las opciones "democrat" y "republican
table$class <- as.factor(table$class)





############################t-test###################################################
plot.hist2(democrat.contingency[,1],"Histograma demócratas votos si","Votos si","steelblue2")
plot.hist2(republican.contingency[,1],"Histograma republicanos votos si","Votos si","darkslategray3")
plot.hist2(democrat.contingency[,2],"Histograma demócratas votos no","Votos no","steelblue2")
plot.hist2(republican.contingency[,2],"Histograma republicanos votos no","Votos no","darkslategray3")
t.test(x = democrat.contingency[,1], y = republican.contingency[,1], alternative = "two.sided", mu = 0, paired = FALSE, conf.level = 0.99)
t.test(x = democrat.contingency[,2], y = republican.contingency[,2], alternative = "two.sided", mu = 0, paired = FALSE, conf.level = 0.99)

#####################################Correlación###########################################################

#Correlación con los tipos de relaciones, de esta forma es posible saber si votar por 
#una categoría implica votar por la otra
cor(final)
dem.cor<-cor(d.range)
rep.cor<-cor(r.range)

################################## REGRESIÓN LOGÍSTICA #################################


data[,2:17] <- data.frame(lapply(data[,2:17], as.character), stringsAsFactors=FALSE)
data[data=="y"] <- "1"
data[data=="n"] <- "2"
data[data=="?"] <- "0"
data[,2:17] <- data.frame(lapply(data[,2:17], as.numeric))


# Se genera el modelo de regresión logística con el factor de 2 niveles
# classname respecto a todas las votaciones realizadas, para saber si una
# persona puede ser clasificada como demócrata o republicano según
# como vota en las distintas instancias
rlog <- glm(classname ~ ., data, family="binomial")
summary(rlog)
# Al hacer la función summary(), se obtienen 5 votaciones que son 
# las más significativas:
#                        - adoption of the budget resolution
#                        - physician free freeze
#                        - immigration
#                        - synfuels corporation cutback
#                        - education spending
# las cuales serán usadas para hacer la regresión, ya que las demás
# al ser menos significantes, solo le aportan complejidad al modelo

#Se filtra el dataset por las votaciones más significativas
significative.data <- data[,c("classname","adoptionofthebudgetresolution", "physicianfeefreeze",
                              "immigration", "synfuelscorporationcutback",
                              "educationspending")]

rlog <- glm(classname ~ ., significative.data, family="binomial")

summary(rlog)
coefficients(rlog)

prediction <- predict(rlog, significative.data, type = "response")
classes.pred <- rep("republican", length(prediction))
classes.pred[prediction < 0.5] <- "democrat"
classes.pred <- factor(classes.pred, levels = c("republican", "democrat"))
confusion.matrix <- confusionMatrix(
  data = classes.pred,
  reference = data[["classname"]],
  positive = "republican"
)


library(pROC)

m1.roc1 <- roc(data[["classname"]], prediction)
# En porciento
m1.roc2 <- roc(data[["classname"]], prediction, percent = TRUE)
# Suavisado
m1.roc3 <- roc(data[["classname"]], prediction, percent = TRUE,
               smooth = TRUE)
ggroc(m1.roc1)
ggroc(m1.roc2)
plot(m1.roc3)
