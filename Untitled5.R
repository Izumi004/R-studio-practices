par(mfrow=c(2,5),mar=c(2, 2, 3, 2))
boxplot(data$I.have.access.to.stable.internet.connection.while.accessing.online.learning.,main='Stable internet',col='tomato')
boxplot(data$I.have.access.to.suitable.devices.to.meet.the.requirements.for.online.learning,main='Suitable devices',col='khaki')
boxplot(data$I.am.able.to.stay.focused.and.concentrated.throughout.all.my.online.courses.,main='Concentration',col='darkolivegreen2')
boxplot(data$I.feel.supported.by.my.course.coordinators..lecturers.and.tutors.through.online.platforms.,main='Supportive course coordinators',col='deepskyblue')
boxplot(data$The.courses.I.am.undertaking.have.provided.me.resources.to.help.me.learn.online,main='Sufficient recource',col='slateblue1')
hist(data$I.have.access.to.stable.internet.connection.while.accessing.online.learning.,main='Stable internet',col='tomato')
hist(data$I.have.access.to.suitable.devices.to.meet.the.requirements.for.online.learning,main='Suitable devices',col='khaki')
hist(data$I.am.able.to.stay.focused.and.concentrated.throughout.all.my.online.courses.,main='Concentration',col='darkolivegreen2')
hist(data$I.feel.supported.by.my.course.coordinators..lecturers.and.tutors.through.online.platforms.,main='Supportive course coordinators',col='deepskyblue')
hist(data$The.courses.I.am.undertaking.have.provided.me.resources.to.help.me.learn.online,main='Sufficient recource',col='slateblue1')

lm(data[,2]~data[,7]+data[,8]+data[,9]+data[,10]+data[,11])

datatemp <- data[,7:11]
covmat <- as.matrix(datatemp)
covmat.eig <-eigen(covmat)
plot(covmat.eig$values,,type="b",ylab="Variances",xlab="Principal Component")

#Clustering
disdata <- dist(data[,7:10],"manhattan")#calculate distance matrix
data.clst <- hclust(disdata,"average")
plot(data.clst,hang=-1)#plot a dendrogram
rect.hclust(data.clst,k=5,border = "red")
rect.hclust(data.clst,k=6,border = "blue")
cutree(data.clst,k=6)


#Cluster
cluster1 <- c(1,	2,	20,	44,	45,	65,	68,	71,	73,	78,	86,	91)
cluster2 <- c(3,	6,	7,	9,	11,	12,	14,	15,	18,	19,	24,	26,	27,	31,	33,	51,	52,	59,	62,	70,	72,	76,	80,	83,	85,	87,	88,	89,	92)
cluster3 <- c(4,	8,	10,	21,	22,	23,	37,	38,	39,	43,	56,	64,	66	,74	,75	,79	,81,	84)
cluster4 <- c(5,	13,	17,	36,	42,	50,	67)
cluster5 <- c(16,	25,	28,	29,	30,	32,	34,	35,	40,	41,	46	,47,	48,	49,	53,	54,	55,	57,	58,	60,	61	,63,	69,	82,	90)
clu1 <- data[cluster1,]
clu2 <- data[cluster2,]
clu3 <- data[cluster3,]
clu4 <- data[cluster4,]
clu5 <- data[cluster5,]
attitudemean <- mean(data$What.is.your.attitude.towards.online.learning.)
xtemp1  <- 1:5
ytemp1  <- c(mean(clu1[,2]),mean(clu2[,2]),mean(clu3[,2]),mean(clu4[,2]),mean(clu5[,2]))
xtemp2  <- 1:5
ytemp2  <- c(mean(clu1[,3]),mean(clu2[,3]),mean(clu3[,3]),mean(clu4[,3]),mean(clu5[,3]))

#cluster mean diagram
par(mfrow(2,2),mar=c(1, 1, 3, 2))
plot
plot(xtemp1,ytemp1,"b",xlab="Cluster",ylab="Attitude mean")
abline(h=attitudemean,col="tomato")
plot(xtemp2,ytemp2,"b",xlab="Cluster",ylab="Effectiveness mean")
abline(h=mean(data$Online.learning.has.proven.to.be.effective.towards.my.academic.performance.),col="tomato")

#cluster factor mean comparison
ttemp <- 7:11
for(i in ttemp){print(mean(clu2[,i]))}
for(i in ttemp){print(mean(clu4[,i]))}

mean(data[,11])
