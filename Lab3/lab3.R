library(arules)
library(dplyr)
library(magrittr)
library(tidyverse)
#Filter puede ser "y", "n" o "?"
create_transaction<-function(
  filter
){
  url = "http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"
  data <- (read.csv(url, header = TRUE, sep = ",",quote = "\"",fill=T))
  colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
  data[,2:17] <- data.frame(lapply(data[,2:17], as.character), stringsAsFactors=FALSE)
  if(filter == "y"){
    data[data=="y"] <- "1"
    data[data=="n"] <- "0"
    data[data=="?"] <- "0"  
  }
  else if(filter == "n"){
    data[data=="y"] <- "0"
    data[data=="n"] <- "1"
    data[data=="?"] <- "0"
  }
  else{
    data[data=="y"] <- "0"
    data[data=="n"] <- "0"
    data[data=="?"] <- "1"
  }
  
  data[,2:17] <- data.frame(lapply(data[,2:17], as.numeric))
  data$handicappedinfants[data$handicappedinfants == 1] = "handicappedinfants"
  data$waterprojectcostsharing[data$waterprojectcostsharing == 1] = "waterprojectcostsharing"
  data$adoptionofthebudgetresolution[data$adoptionofthebudgetresolution == 1] = "adoptionofthebudgetresolution"
  data$physicianfeefreeze[data$physicianfeefreeze == 1] = "physicianfeefreeze"
  data$elsalvadoraid[data$elsalvadoraid == 1] = "elsalvadoraid"
  data$religiousgroupsinschools[data$religiousgroupsinschools == 1] = "religiousgroupsinschools"
  data$antisatellitetestban[data$antisatellitetestban == 1] = "antisatellitetestban"
  data$aidtonicaraguancontras[data$aidtonicaraguancontras == 1] = "aidtonicaraguancontras"
  data$mxmissile[data$mxmissile == 1] = "mxmissile"
  data$immigration[data$immigration == 1] = "immigration"
  data$synfuelscorporationcutback[data$synfuelscorporationcutback == 1] = "synfuelscorporationcutback"
  data$educationspending[data$educationspending == 1] = "educationspending"
  data$superfundrighttosue[data$superfundrighttosue == 1] = "superfundrighttosue"
  data$crime[data$crime == 1] = "crime"
  data$dutyfreeexports[data$dutyfreeexports == 1] = "dutyfreeexports"
  data$exportadministrationactsouthafrica[data$exportadministrationactsouthafrica == 1] = "exportadministrationactsouthafrica"
  
  data <- data[,2:17] 
  new_data = NULL
  new_data2 = NULL
  l = 1
  for(i in 1:nrow(data)){
    k = 1
    for(j in 1:ncol(data)){
      if(data[i,j]!= 0){
        new_data[k] = data[i,j]
        k = k + 1
      }
      
    }
    new_data2[l] = list(new_data)
    l = l + 1
 
  }  
  new_data2
}

#Transacciones: cada fila contiene una fila, en donde se indica los votos emitidos por cada político

transacciones <- as(new_data2, Class = "transactions")
number_transactions <- size(transacciones)
#Frecuencia de las transacciones (cantidad de elementos presentes en cada una de ellas)
data.frame(number_transactions) %>%
  ggplot(aes(x = number_transactions)) +
  geom_histogram() +
  labs(title = "Distribución del tamaño de las transacciones",
       x = "Tamaño") +
  theme_bw()

#Soporte de cada item, es decir, la cantidad de veces que aparece cada item en las transacciones respecto
#al total 
#type <- relative = soporte
#     <- absolute = frecuencia con la que aparecen los items en las transacciones

soporte <- itemFrequency(x = transacciones, type = "relative")
soporte %>% sort(decreasing = TRUE) %>% head(16)




#Algoritmo apriori
rules <- apriori(data = transacciones, parameter = list(support = 0.3, minlen = 2,maxlen = 16,target = "rules",confidence = 0.8))
summary(rules)
top_20_itemsets <- sort(rules, by = "support", decreasing = TRUE)[1:20]
inspect(top_20_itemsets)

#Se pueden variar las métricas 
metricas <- interestMeasure(rules, measure = c("coverage", "fishersExactTest"),
                            transactions = transacciones)
