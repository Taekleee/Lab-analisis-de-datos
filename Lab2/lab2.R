library(cluster)
library(purrr) 
library(factoextra)
library(plyr)
library(tclust)
library(gridExtra)
library(grid)
library(ggpubr)
library(NbClust)

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


############################### K-MEANS con todos los datos ####################################

#Se calcula el número óptimo de cluster, la distancia euclideana, la matriz de similitud y se aplica k-means

index.optimal.cluster<- NbClust(data = data, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)        

distance <- get_dist(data, method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),show_labels=TRUE)
k2 <- kmeans(data, centers = 2, nstart = 25)
k2.plot <- fviz_cluster(k2, data = data)

k3 <- kmeans(data, centers = 3, nstart = 25)
k3.plot <- fviz_cluster(k3, data = data)

k4 <- kmeans(data, centers = 4, nstart = 25)
k4.plot <- fviz_cluster(k4, data = data)
k2$centers
k3$centers
k4$centers


ggarrange(k2.plot,k3.plot,k4.plot)

######################## Cluster con datos significativos en reg.log ###########################

#Se seleccionan las columnas más significativas para el análisis de la regresión logística
reglog <- data[,c("adoptionofthebudgetresolution", "physicianfeefreeze",
                              "immigration", "synfuelscorporationcutback",
                              "educationspending")]

distance <- get_dist(reglog, method = "euclidean")

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),show_labels=TRUE)

#Kmeans clustering con 2 y 3 nodos
index.optimal.cluster.log<- NbClust(data = reglog, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)        

k2 <- kmeans(reglog, centers = 2, nstart = 25)
k2.plot <- fviz_cluster(k2, data = reglog)
k3 <- kmeans(reglog, centers = 3, nstart = 25)
k3.plot <- fviz_cluster(k3, data = reglog)
k8 <- kmeans(reglog, centers = 8, nstart = 25)
k8.plot <- fviz_cluster(k8, data = reglog)

k2$centers
k3$centers
k8$centers

ggarrange(k2.plot, k3.plot, k8.plot)


######################################DENDOGRAMA###########################################################

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


#############################Outliers###############################
k2 <- kmeans(data, centers = 2, nstart = 25)
centers <- k2$centers[k2$cluster, ]
distances <- sqrt(rowSums((data - centers)^2))
outliers1 <- order(distances, decreasing=T)[1:10]
outliers2 <- order(distances, decreasing=T)[1:50]
outliers3 <- order(distances, decreasing=T)[1:100]

print("Votaciones de los outliers:")
print(outliers1)
print(outliers2)
print(outliers3)

new.data1<-data[outliers1*-1,]
new.data2<-data[outliers2*-1,]
new.data3<-data[outliers3*-1,]

index.optimal.cluster.out1<- NbClust(data = new.data1, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)        
index.optimal.cluster.out2<- NbClust(data = new.data2, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)        
index.optimal.cluster.out3<- NbClust(data = new.data3, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all", alphaBeale = 0.1)        


k3 <- kmeans(new.data1, centers = 2, nstart = 25)
k3.plot <- fviz_cluster(k3, data = new.data1)
k4 <- kmeans(new.data2, centers = 2, nstart = 25)
k4.plot <- fviz_cluster(k4, data = new.data2)
k7 <- kmeans(new.data3, centers = 2, nstart = 25)
k7.plot <- fviz_cluster(k7, data = new.data3)

k5 <- kmeans(new.data1, centers = 3, nstart = 25)
k5.plot <- fviz_cluster(k5, data = new.data1)
k6 <- kmeans(new.data2, centers = 3, nstart = 25)
k6.plot <- fviz_cluster(k6, data = new.data2)
k8 <- kmeans(new.data3, centers = 3, nstart = 25)
k8.plot <- fviz_cluster(k8, data = new.data3)

k3$centers
k4$centers
k7$centers
k5$centers
k6$centers
k8$centers


ggarrange(k3.plot, k4.plot, k7.plot, k5.plot,k6.plot,k8.plot)



















#Se utiliza pam para generar un agrupamiento k-medoids, en el cual cada cluster se encuentra 
#representado por su medoids, a diferencia de k-means en donde se representan por el centroide.
#Recibe como entrada los datos, la cantidad de clúster y la medida de similaridad.
#Medoids: elemento dentro de un cluster cuya distancia (diferencia) promedio entre él y 
#todos los demás elementos del mismo cluster es lo menor posible.
#Medoids se ve menos afectado por el ruido o outliers que k-means

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
ggarrange(pamk2.plot,pamk3.plot)


pam_clusters <- pam(x = data, k = 2, metric = "euclidean")
fviz_cluster(object = pam_clusters, data = data, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")
pam_clusters <- pam(x = data, k = 3, metric = "euclidean")
fviz_cluster(object = pam_clusters, data = data, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")
#Con pam se pueden ver los outliers en el gráfico, para k-means se calcula
#la distancia entre cada elemento y su centroide.

centers <- k2$centers[k2$cluster, ]
distances <- sqrt(rowSums((data - centers)^2))
outliers <- order(distances, decreasing=T)[1:10]
outliers2 <- order(distances, decreasing=T)[1:70]

print("Votaciones de los outliers:")
print(data[outliers,])
print(data[outliers2,])

new.data<-data[outliers*-1,]
new.data2<-data[outliers2*-1,]
############################################## 2 ###################################################
pam_clusters <- pam(x = new.data, k = 2, metric = "euclidean")
fviz_cluster(object = pam_clusters, data = new.data, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

pam_clusters2 <- pam(x = new.data2, k = 2, metric = "euclidean")
fviz_cluster(object = pam_clusters2, data = new.data2, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")


############################################## 2 ###################################################


############################################## 3 ###################################################

pam_clusters <- pam(x = new.data, k = 3, metric = "euclidean")
fviz_cluster(object = pam_clusters, data = new.data, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")


pam_clusters2 <- pam(x = new.data2, k = 3, metric = "euclidean")
fviz_cluster(object = pam_clusters2, data = new.data2, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")
fviz_nbclust(new.data2,pam, method = "silhouette")
############################################## 3 ###################################################
