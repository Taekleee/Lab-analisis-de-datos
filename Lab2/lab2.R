library(cluster)
library(purrr) 
library(factoextra)
library(plyr)
library(tclust)


url = "http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"
data <- read.csv(url, header = TRUE, sep = ",",quote = "\"",fill=T)
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
names <- data$classname
democrat<-dplyr::filter(data, data$classname == "democrat")
republican<-dplyr::filter(data, data$classname == "republican")
data <- rbind(democrat,republican)
data[,2:17] <- data.frame(lapply(data[,2:17], as.character), stringsAsFactors=FALSE)
data[data=="y"] <- "1"
data[data=="n"] <- "2"
data[data=="?"] <- "0"
data[,2:17] <- data.frame(lapply(data[,2:17], as.numeric))
#data <- scale(data[,2:17])
data <- data[,2:17]
#Se calcula la distancia euclideana, la matriz de similitud y se aplica k-means
distance <- get_dist(data, method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),show_labels=TRUE)
k2 <- kmeans(data, centers = 3, nstart = 25)
k2.plot <- fviz_cluster(k2, data = data)
print(k2.plot)

######################## Cluster con datos significativos en reg.log ###########################

#Se seleccionan las columnas más significativas para el análisis de la regresión logística
reglog <- data[,c("adoptionofthebudgetresolution", "physicianfeefreeze",
                              "immigration", "synfuelscorporationcutback",
                              "educationspending")]

distance <- get_dist(reglog, method = "euclidean")
distance <- mahalanobis(reglog, center = FALSE, cov = (var(reglog)) )

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),show_labels=TRUE)

#Kmeans clustering con 2 y 3 nodos
k2 <- kmeans(reglog, centers = 2, nstart = 25)
k2.plot <- fviz_cluster(k2, data = reglog)
print(k2.plot)
k3 <- kmeans(reglog, centers = 3, nstart = 25)
k3.plot <- fviz_cluster(k3, data = reglog)
print(k3.plot)

#pam clustering con 2 y 3 nodos
pam_clusters2 <- pam(x = reglog, k = 2, metric = "manhattan")
pam_clusters3 <- pam(x = reglog, k = 3, metric = "manhattan")
pamk2.plot <- fviz_cluster(object = pam_clusters2, data = reglog, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

pamk3.plot <- fviz_cluster(object = pam_clusters3, data = reglog, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")


#################################################################################################

#hclust y agnes se ocupan para la agrupación jerárquica
#agnes también retorna el coeficiente de aglomeración (ac), el cual indica la cantidad de estructura
#de agrupamiento encontrada (si es más cercano a 1 es más fuerte).
hc1 <- hclust(distance, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
hc2 <- agnes(data, method = "complete")
hc2$ac

#Se genera el dendograma
pltree(hc2, hang=-1, cex = 0.6)
plot(hc2, cex = 0.6, hang = -1, main = "Dendrogram of diana")
rect.hclust(hc2, k = 3, border = 1:5)

#Número de elementos por cluster
cantidad<-table(data$cluster)


#Se utiliza pam para generar un agrupamiento k-medoids, en el cual cada cluster se encuentra 
#representado por su medoids, a diferencia de k-means en donde se representan por el centroide.
#Recibe como entrada los datos, la cantidad de clúster y la medida de similaridad.
#Medoids: elemento dentro de un cluster cuya distancia (diferencia) promedio entre él y 
#todos los demás elementos del mismo cluster es lo menor posible.
#Medoids se ve menos afectado por el ruido o outliers que k-means


pam_clusters <- pam(x = data, k = 2, metric = "manhattan")
fviz_cluster(object = pam_clusters, data = data, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")
#Con pam se pueden ver los outliers en el gráfico, para k-means se calcula
#la distancia entre cada elemento y su centroide.

centers <- k2$centers[k2$cluster, ]
distances <- sqrt(rowSums((data - centers)^2))
outliers <- order(distances, decreasing=T)[1:15]
print("Votaciones de los outliers:")
print(data[outliers,])

new.data<-data[outliers*-1,]
pam_clusters <- pam(x = new.data, k = 3, metric = "manhattan")
fviz_cluster(object = pam_clusters, data = data, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")
distance <- get_dist(new.data, method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),show_labels=TRUE)
k2 <- kmeans(new.data, centers = 3, nstart = 25)
k2.plot <- fviz_cluster(k2, data = new.data)
print(k2.plot)