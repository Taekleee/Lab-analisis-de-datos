library("C50")
library("caret")

url = "http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"
data <- (read.csv(url, header = TRUE, sep = ",",quote = "\"",fill=T))
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")


data[,2:17] <- data.frame(lapply(data[,2:17], as.character), stringsAsFactors=FALSE)
#data[data=="y"] <- "1"
#data[data=="n"] <- "2"
#data[data=="?"] <- "0"
data[,2:17] <- data.frame(lapply(data[,2:17], as.character))

training.index = createDataPartition(data$classname, p=0.7)$Resample1
training.set = data[training.index, ]



test.set = data[-training.index, ]
tree = C5.0(classname ~ ., training.set)
tree.rules = C5.0(x = training.set[, -1], y = training.set$classname, rules = T)
tree.pred.class = predict(tree, test.set[,-1], type = "class")
tree.pred.prob = predict(tree, test.set[,-1], type = "prob")

tree.pred.class
tree.pred.prob

plot(tree)

summary(tree.rules)

conf.matrix.tree = confusionMatrix(table(test.set$class, tree.pred.class))