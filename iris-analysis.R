# PCA
pr.out = prcomp(iris[,1:4], scale=T)

pr.out$sdev
pr.var=pr.out$sdev^2; pr.var
pve=pr.var/sum(pr.var); pve

plot(pve, xlab="Principal Component",
     ylab="Proportion of Variance Explained",
     ylim=c(0,1),type='b')

pr.out$x #두 개의 주성분을 선택할 것.

# SVM
g1 = which(iris$Species=="setosa")
g2 = which(iris$Species=="versicolor")
g3 = which(iris$Species=="virginica")

## Fisrt SVM
x = pr.out$x[c(g1,g2),1:2]
y = rep(0, length(c(g1,g2)))
y[g1] = 1
y[g2] = -1
dat1=data.frame(x=x, y=as.factor(y))

plot(dat1[,1:2], col=(3-y))

install.packages("e1071")
library(e1071)
set.seed(1)
tune.out=tune(svm, y~.,data=dat1,
              kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))

summary(tune.out)

bestmod1=tune.out$best.model
summary(bestmod1)

plot(bestmod1, dat1)

## Second SVM
x = pr.out$x[c(g2,g3),1:2]
y = rep(0, length(c(g2,g3)))
y[g2-length(g1)] = 1
y[g3-length(g1)] = -1
dat2=data.frame(x=x, y=as.factor(y))

plot(dat2[,1:2], col=(3-y))

set.seed(1)
tune.out=tune(svm, y~.,data=dat2,
              kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))

summary(tune.out)

bestmod2=tune.out$best.model
summary(bestmod2)

plot(bestmod2, dat2)

## Third SVM
x = pr.out$x[c(g1,g3),1:2]
y = rep(0, length(c(g1,g3)))
y[g1] = 1
y[g3-length(g2)] = -1
dat3=data.frame(x=x, y=as.factor(y))

plot(dat3[,1:2], col=(3-y))

set.seed(1)
tune.out=tune(svm, y~.,data=dat3,
              kernel="linear",
              ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))

summary(tune.out)

bestmod3=tune.out$best.model
summary(bestmod3)

plot(bestmod3, dat3)
