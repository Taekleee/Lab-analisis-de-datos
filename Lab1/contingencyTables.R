data <- read.csv("house-votes-84.data", header = TRUE, sep = ",",quote = "\"",fill=T)
colnames(data) <- c("classname", "handicappedinfants","waterprojectcostsharing","adoptionofthebudgetresolution","physicianfeefreeze","elsalvadoraid","religiousgroupsinschools","antisatellitetestban","aidtonicaraguancontras","mxmissile","immigration","synfuelscorporationcutback","educationspending","superfundrighttosue","crime","dutyfreeexports","exportadministrationactsouthafrica")
democrat<-filter(data, data$classname == "democrat")
republican<-filter(data, data$classname == "republican")

names <- c('y','n','?')
democrat.contingency <- sapply(names, function(x) rowSums(democrat[,2:17]==x))
colnames(democrat.contingency) <- names

republican.contingency <- sapply(names, function(x) rowSums(democrat[,2:17]==x))
colnames(republican.contingency) <- names