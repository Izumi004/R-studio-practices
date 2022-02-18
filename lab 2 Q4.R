library(latentnet) 

data(sampson)
samp.fit2 <- suppressWarnings(ergmm(samplike ~ euclidean(d=2, G=3)+rreceiver))
plot(samp.fit2, pie=TRUE, rand.eff="receiver", main="", print.formula=FALSE,xlab="",ylab="",suppress.axes=TRUE,suppress.center=TRUE)

