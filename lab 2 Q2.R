library(dplyr)

data(iris)
library(knitr)
kable(iris[round(seq(1,nrow(iris),length.out=12)),],row.names=FALSE,booktabs=TRUE)

setosa <- filter(iris,Species=="setosa")
hist(setosa$Sepal.Width, main="", xlab="Sepal Width",freq=FALSE)
lines(density(setosa$Sepal.Width))

boxplot(Sepal.Width~Species, data=iris, notch=TRUE, ylab="Sepal Width")

library(Hmisc)
Hmisc::bpplot(with(iris, split(Sepal.Width, Species)))





data(anscombe)
par(mfrow=c(2,2),mar=c(5,5,0,0)+.05)
for(i in 1:4){
  x<-anscombe[[i]]; y<-anscombe[[i+4]]
  plot(x,y, xlab=paste0("x",i),ylab=paste0("y",i))
  abline(lsfit(x,y))
} 




pairs(iris[,-5],col=as.numeric(iris$Species)+1,pch=".")
PLPW.pca <- prcomp(iris[,-5], center = TRUE, scale. = TRUE)
biplot(PLPW.pca)
plot(PLPW.pca,main="")
