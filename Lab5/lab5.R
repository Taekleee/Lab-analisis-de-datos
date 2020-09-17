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

training.set <- read.csv("training.csv")
test.set <- read.csv("test.csv")
training.set$X <- NULL
test.set$X <- NULL

colnames(training.set) <- c("classname", "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")
colnames(test.set) <- c("classname", "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")
training.set$classname <- as.character(training.set$classname)
training.set[training.set=="democrat"] <- "d"
training.set[training.set=="republican"] <- "r"
training.set$classname <- as.factor(training.set$classname)

test.set$classname <- as.character(test.set$classname)
test.set[test.set=="democrat"] <- "d"
test.set[test.set=="republican"] <- "r"
test.set$classname <- as.factor(test.set$classname)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Se genera un árbol de decisión con todas las votaciones.
# No se le entrega la clase de cada votante, como la clase
# se encuentra en la columna 1, se realiza training.set[, -1]
# indicando que la columna 1 se omite.

tree = C5.0(classname ~ ., training.set)
tree.rules = C5.0(x = training.set[, -1], y = training.set$classname, rules = T)
tree.pred.class = predict(tree, test.set[,-1], type = "class")
tree.pred.prob = predict(tree, test.set[,-1], type = "prob")



plot(tree)

summary(tree.rules)

conf.matrix.tree = confusionMatrix(table(test.set$classname, tree.pred.class))
conf.matrix.tree

# Ahora se realiza el mismo procedimiento anterior para generar un 
# árbol de decisión, pero esta vez eliminando la raíz del árbol generado
# que corresponde a la votación physicianfeefreeze que corresponde a la
# columna con el número 5.
#

tree = C5.0(classname ~ ., training.set[, -5])
tree.rules = C5.0(x = training.set[, c(-1, -5)], y = training.set$classname, rules = T)
tree.pred.class = predict(tree, test.set[,c(-1, -5)], type = "class")
tree.pred.prob = predict(tree, test.set[,c(-1, -5)], type = "prob")



plot(tree)

summary(tree.rules)

conf.matrix.tree = confusionMatrix(table(test.set$classname, tree.pred.class))
conf.matrix.tree

# Ahora se realiza el mismo procedimiento anterior para generar un 
# árbol de decisión, pero esta vez eliminando la raíz del árbol generado
# que corresponde a la votación adoptionofthebudgetresolution que corresponde a la
# columna con el número 3.
#

tree = C5.0(classname ~ ., training.set[, c(-5,-4)])
tree.rules = C5.0(x = training.set[, c(-1, -5, -4)], y = training.set$classname, rules = T)
tree.pred.class = predict(tree, test.set[,c(-1, -5, -4)], type = "class")
tree.pred.prob = predict(tree, test.set[,c(-1, -5, -4)], type = "prob")



plot(tree)

summary(tree.rules)

conf.matrix.tree = confusionMatrix(table(test.set$classname, tree.pred.class))
conf.matrix.tree

# Ahora se realiza el mismo procedimiento anterior para generar un 
# árbol de decisión, pero esta vez eliminando la raíz del árbol generado
# que corresponde a la votación elsalvadoraid que corresponde a la
# columna con el número 5.
#

tree = C5.0(classname ~ ., training.set[, c(-5,-4, -6)])
tree.rules = C5.0(x = training.set[, c(-1, -5, -4, -6)], y = training.set$classname, rules = T)
tree.pred.class = predict(tree, test.set[,c(-1, -5, -4, -6)], type = "class")
tree.pred.prob = predict(tree, test.set[,c(-1, -5, -4, -6)], type = "prob")



plot(tree)

summary(tree.rules)

conf.matrix.tree = confusionMatrix(table(test.set$classname, tree.pred.class))
conf.matrix.tree

# Ahora se realiza el mismo procedimiento anterior para generar un 
# árbol de decisión, utilizando solo aquellas votaciones
# que son significativas para una regresión logística


significative.data <- data[,c("classname","adoptionofthebudgetresolution", "physicianfeefreeze",
                              "immigration", "synfuelscorporationcutback",
                              "educationspending")]

significative.training.set <- training.set[, c(1,4,5,11,12,13)]
significative.test.set <- test.set[, c(1,4,5,11,12,13)]

index <- c(4, 5, 11, 12, 13)
tree = C5.0(classname ~ ., significative.training.set)
tree.rules = C5.0(x = significative.training.set[, -1], y = significative.training.set$classname, rules = T)
tree.pred.class = predict(tree, significative.test.set[,-1], type = "class")
tree.pred.prob = predict(tree, significative.test.set[,-1], type = "prob")



plot(tree)

summary(tree.rules)

conf.matrix.tree = confusionMatrix(table(significative.test.set$classname, tree.pred.class))
conf.matrix.tree




