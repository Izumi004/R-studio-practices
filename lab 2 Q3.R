library(knitr)
data(HairEyeColor) 
kable(HairEyeColor[,,1], booktabs=TRUE)

data(HairEyeColor)
kable(HairEyeColor[,,2], booktabs=TRUE)

hair_freq <- apply(HairEyeColor, 1, sum)
hair_freq
hair_col <- c("black", "burlywood4", "darkorange1", "yellow")
barplot(hair_freq, col=hair_col)

barplot(sort(hair_freq, decreasing=TRUE)/sum(hair_freq), col=hair_col[order(hair_freq, decreasing=TRUE)])

par(mfrow=c(1,2), mar=c(4,3,0,0)+.1) # mfrow=c(1,2) = two plots side-by-side (but only for base R graphics)
# We are now collapsing across dimension 3 (Sex) to create counts for each combination of hair and eye colour.
barplot(apply(HairEyeColor, 1:2, sum), beside=TRUE, col=c("black", "burlywood4", "darkorange1", "yellow"), xlab="Eye colour")
barplot(apply(HairEyeColor, 1:2, sum), beside=FALSE, col=c("black", "burlywood4", "darkorange1", "yellow"), xlab="Eye colour")

mosaicplot(apply(HairEyeColor, 2:1, sum), main="", color=c("black", "burlywood4", "darkorange1", "yellow"))

mosaicplot(HairEyeColor, main="", color=TRUE, xlab="", ylab="")
