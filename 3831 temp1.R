data<- read.csv(file='3831assignment.csv',header=TRUE)
summary(data)
disdata <- dist(data[,7:10],"manhattan")#calculate distance matrix
data.clst <- hclust(disdata,"average")# hie

#rarchical clustering, complete linkage
plot
data.hclust(data.clst, k=5, border="red")
plot(data.clst,hang=-1)#plot a dendrogram
rect.hclust(data.clst,k=5,border = "red")
rect.hclust(data.clst,k=6,border = "blue")
disdata
sqrt(var(data[,9]))
cor(data$What.is.your.attitude.towards.online.learning.,data$I.have.access.to.stable.internet.connection.while.accessing.online.learning.)
cor(data$What.is.your.attitude.towards.online.learning.,data$I.have.access.to.suitable.devices.to.meet.the.requirements.for.online.learning)
cor(data$What.is.your.attitude.towards.online.learning.,data$I.am.able.to.stay.focused.and.concentrated.throughout.all.my.online.courses.)
cor(data$What.is.your.attitude.towards.online.learning.,data$I.feel.supported.by.my.course.coordinators..lecturers.and.tutors.through.online.platforms.)
cor(data$What.is.your.attitude.towards.online.learning.,data$The.courses.I.am.undertaking.have.provided.me.resources.to.help.me.learn.online)
barplot(data$What.is.your.current.employment.status.)
library(ggplot2)
barplot(data$What.is.your.current.living.situation.)

c1 <- cutree(data.clst,5)
c2 <- 1:92
temp1<- data.frame(obs=c2,cluster=temp2)
temp1(order(temp1$cluster))



rect.hclust(data.clst,k=5,border = "red")
rect.hclust(data.clst,k=6,border = "blue")

cutree(data.clst,k=6)
