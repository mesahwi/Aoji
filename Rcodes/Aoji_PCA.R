rm(list = ls())
#install.packages("mclust")
#install.packages("psych")
library(mclust)
library(psych)
data(banknote)

# load swiss bank notes
y=banknote[,1]
x=banknote[,-1]
round(cor(x),3)
cortest.bartlett(cor(x),nrow(banknote))

# PCA
spr=princomp(x)
U=spr$loadings
L=(spr$sdev)^2
Z=spr$scores

# scatterplot of input x
idx = which(banknote$Status=='genuine')
pairs(x,col=c('green', 'blue')[factor(y)])

# scatterplot of principal components
pairs(Z,col=c('green', 'blue')[factor(y)])

# variances of each principal component
par(mfrow=c(1,2))
plot(L,type="b",xlab="component",ylab="lambda",main="Scree plot")
plot(cumsum(L)/sum(L)*100,ylim=c(0,100),type="b",xlab="component",ylab="cumulative propotion (%)",main="Cum. Scree plot")
lines(c(0,10),c(90,90),col="blue",lty=2)

