library(e1071) 
library(caret)
library(pROC)
library(ROCR)
#######################################################################################################################
#####################################Obtención de datos###############################################################
#######################################################################################################################
url = "http://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"
data <- (read.csv(url, header = TRUE, sep = ",",quote = "\"",fill=T))
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")


#El set de entrenamiento concentra el 70% de los datos y el de prueba el 30% restante
training.index = createDataPartition(data$classname, p=0.7)$Resample1
training.set = data[training.index, ]
test.set = data[-training.index, ]


#Bayesiano, entrega las probabilidades a priori y condicionales (verosimilitud)
bayesiano <- naiveBayes(classname ~ ., data = training.set)

#Prueba del modelo
pred <- predict(bayesiano, test.set)
tabla <- table(test.set$classname, pred, dnn = c("Actual", "Predicha"))
confusionMatrix(tabla)
m1.roc1 <- roc(test.set$classname, as.numeric(pred))