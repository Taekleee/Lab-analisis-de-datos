library(arules)
library(dplyr)
library(magrittr)
library(tidyverse)
library(arulesViz)

#######################################################################################################################
#####################################Obtención de datos###############################################################
#######################################################################################################################
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"
data <- (read.csv(url, header = TRUE, sep = ",",quote = "\"",fill=T))
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
votes<-as.tibble(data)




#######################################################################################################################
############################################Algoritmo Apriori##########################################################
#######################################################################################################################
support <- 0.2
confidence <- 0.5


#La regla se fuerza a un clasificador, debido a que el consecuente es la clase
rules <- apriori(votes,
                 parameter=list(support = support, confidence = confidence,  minlen = 3, maxlen = 17, target="rules"),
                 appearance=list(rhs = c("classname=democrat", "classname=republican")))
all_rules<- sort(x = rules, decreasing = TRUE, by = "confidence")
#Mejores 10 reglas según el lift
result<-inspect(head(all_rules, n= 10, by = "lift"))

#Item sets más frecuentes dentro de las reglas según soporte
itemsets <- apriori(votes,
                    parameter=list(support = support,  confidence = confidence,minlen = 3, maxlen = 17,target = "frequent itemset"),
                    appearance=list(rhs = c("classname=democrat", "classname=republican")))
summary(itemsets)
top_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:20]



#######################################################################################################################
#####################################Gráficos#########################################################################
#######################################################################################################################


#Gráfico del soporte vs confianza de las reglas
plot(rules, measure = c("support", "confidence"), shading = "lift", jitter = 0)

#Gráfico reglas vs posición
plot(rules, method = "paracoord")

#Gráfico de los 20 sets de items más frecuentes según el soporte
as(top_itemsets, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Itemsets más frecuentes", x = "itemsets") +
  theme_bw()

