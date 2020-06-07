library(cluster)
library(purrr) 
library(factoextra)
library(plyr)
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
distance <- get_dist(data, method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(data, centers = 3, nstart = 25)
a <- fviz_cluster(k2, data = data)
print(a)








hc1 <- hclust(distance, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
hc2 <- agnes(data, method = "complete")

# Agglomerative coefficient
hc2$ac


pltree(hc2, hang=-1, cex = 0.6)
plot(hc2, cex = 0.6, hang = -1, main = "Dendrogram of diana")
rect.hclust(hc2, k = 3, border = 1:5)