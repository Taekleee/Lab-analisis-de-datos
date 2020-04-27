library(tidyverse)

data <- read.csv("house-votes-84.data", header = TRUE, sep = ",",quote = "\"",fill=T)
colnames(data) <- c("classname", "handicappedinfants","water-project-cost-sharing","adoption-of-the-budget-resolution","physician-fee-freeze","el-salvador-aid","religious-groups-in-schools","anti-satellite-test-ban","aid-to-nicaraguan-contras","mx-missile","immigration","synfuels-corporation-cutback","education-spending","superfund-right-to-sue","crime","duty-free-exports","export-administration-act-south-africa")
data2 <- lapply(X=data,FUN = function(m) df <- summary(m))
class.name<-data2[1]
data2[1]<-NULL
data2<-do.call(rbind.data.frame, data2)
row.names(data2) <- c( "handicapped-infants","water-project-cost-sharing","adoption-of-the-budget-resolution","physician-fee-freeze","el-salvador-aid","religious-groups-in-schools","anti-satellite-test-ban","aid-to-nicaraguan-contras","mx-missile","immigration","synfuels-corporation-cutback","education-spending","superfund-right-to-sue","crime","duty-free-exports","export-administration-act-south-africa")
colnames(data2)<-c("?","n","y")

