## kNN classification 

library(MASS)
library(car)
library(class)

rm(list=ls())

data(iris)
iris$Species=factor(iris$Species)

n=nrow(iris)
set.seed(1234)
sp=sample(c("r","t","v"),n,replace=T,prob=c(.5,.25,.25))

iris$sample=factor(sp)
train=subset(iris,sample=="r") #training set
test=subset(iris,sample=="t")  #test set
valid=subset(iris,sample=="v") #validation set

nn=seq(1,50,by=2) #sequence of k
misrate=rep(0,length(nn))
for(k in 1:length(nn)){
  fit=knn(train[,1:4],test[,1:4],train[,5],k=nn[k],prob=TRUE)
  tl=table(test[,5],fit)
  misrate[k]=1-sum(diag(tl))/sum(tl)
}

plot(misrate~nn,type='o',ylim=c(0,0.3),xlab='k',ylab="Misclassification rates")

## test step
fit.t=knn(train[,1:4],test[,1:4],train[,5],k=5,prob=TRUE)
tl=table(test[,5],fit.t)
sum(diag(tl))/sum(tl) #correct classification rate

## validation step
fit.v=knn(train[,1:4],valid[,1:4],train[,5],k=5,prob=TRUE)
tl=table(valid[,5],fit.v)
sum(diag(tl))/sum(tl)

