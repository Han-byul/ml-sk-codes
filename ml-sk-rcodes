# Clustering
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

plot(x)

set.seed(2)
km.out=kmeans(x,2,nstart=20)

plot(x, col=(km.out$cluster+1), 
     main="K-Means Clustering Results with K=2", xlab="", ylab="")

km.out

set.seed(4)
km.out=kmeans(x,3,nstart=20); km.out

plot(x, col=(km.out$cluster+1), 
     main="K-Means Clustering Results with K=3", xlab="", ylab="")

set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

summary(USArrests)

pairs(~Murder+Assault+UrbanPop+Rape,data=USArrests)

clustering.K3 = kmeans(USArrests,3)
kmeans(USArrests, 3)

clustering.K3 <- kmeans(USArrests, 3)
plot(USArrests, col=clustering.K3$cluster)

set.seed(1)
clustering1 <- kmeans(USArrests,4)
set.seed(2)
clustering2 <- kmeans(USArrests,4)
table(clustering1$cluster, clustering2$cluster)

clustering1

clustering2

## Hierarchical clustering
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

x[1:4,]
dist(x[1:4,])

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

dev.off()
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"),
     main="Hierarchical Clustering with Scaled Features")
# plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)

x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"),
     main="Complete Linkage with Correlation-Based Distance")

dist(USArrests)
clustering.EC = hclust(dist(USArrests))

plot(clustering.EC)

clustering.EA = hclust(dist(USArrests),"average")
plot(clustering.EA)

clustering.ES = hclust(dist(USArrests),"single")
plot(clustering.ES)

cutree(clustering.EC,k=2:5)

cutree(clustering.EC,h=100)

plot(clustering.EC)
rect.hclust(clustering.EC, k=4, border="red")

clustering.EA = hclust(dist(USArrests),"ave")
summary(USArrests[cutree(clustering.EA,3)==1,])

summary(USArrests[cutree(clustering.EA,3)==2,])
summary(USArrests[cutree(clustering.EA,3)==3,])

complete = cutree(clustering.EC , 4)
average = cutree(clustering.EA , 4)
single = cutree(clustering.ES , 4)

table(complete , average)
table(complete , single)

# Support vector machine and Kernel machine
set.seed(3)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
dat=data.frame(x=x, y=as.factor(y))

plot(x, col=(3-y))

library(e1071)
svmfit=svm(y~., data=dat, kernel="linear", cost=10,scale=FALSE)

plot(svmfit, dat); svmfit$index

svmfit=svm(y~., data=dat, kernel="linear", cost=0.1,scale=FALSE)
plot(svmfit, dat); svmfit$index

set.seed(1)
tune.out=tune(svm,y~.,data=dat,
kernel="linear",
ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))

summary(tune.out)

bestmod=tune.out$best.model
summary(bestmod)

xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred, truth=testdat$y)

svmfit=svm(y~., data=dat, kernel="linear", cost=.01,scale=FALSE)
ypred=predict(svmfit,testdat)
table(predict=ypred, truth=testdat$y)

x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)

plot(svmfit, dat)

svmfit=svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)

plot(svmfit,dat)

set.seed(1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y)); plot(x, col=y)

train=sample(200,100)
svmfit=svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train,])

summary(svmfit)

svmfit=svm(y~., data=dat[train,], kernel="radial",gamma=1,cost=1e5)
plot(svmfit,dat[train,])

set.seed(1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",
ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.1,1,10,100,1000)))
summary(tune.out)
table(true=dat[-train,"y"],
pred=predict(tune.out$best.model,newx=dat[-train,]))


# PCA : Dimension reduction
str(USArrests)
head(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, sd)
summary(USArrests)

pr.out=prcomp(USArrests, scale=TRUE)
str(pr.out)

pr.out$center
pr.out$scale

pr.out$rotation

dim(pr.out$x)
str(pr.out$x)

biplot(pr.out, scale=0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev
pr.var=pr.out$sdev^2; pr.var
pve=pr.var/sum(pr.var); pve

plot(pve, xlab="Principal Component",
ylab="Proportion of Variance Explained",
ylim=c(0,1),type='b')

plot(cumsum(pve), xlab="Principal Component",
ylab="Cumulative Proportion of Variance Explained",
ylim=c(0,1), type='b')


# PCA regression
library(ISLR)
str(Hitters)

head(Hitters)

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
Hitters=na.omit(Hitters)

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

pcr.fit=pcr(Salary~.,data=Hitters,
    subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type = "MSEP")

summary(pcr.fit)

pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)



