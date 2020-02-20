require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data=swiss)

require(party)

treeSwiss <- ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert <- ctree(Fertility ~ Agriculture + Education + Catholic, data=swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data=swiss, controls=cforest_control(mtry=2, mincriterion=0))

library(tree)
tr <- tree(Species ~., data=iris)
tr
tr$frame
plot(tr)
text(tr)

fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
plot(fit2M, use.n=TRUE, all=TRUE, cex=.8)

fitk <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis", type="simple")


require(kknn)
data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size=round(m/3), replace=FALSE, prob=rep(1/m,m))
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
iris.kknn <- train.kknn(Species~., iris.learn, distance=1, kernel=c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))
summary(iris.kknn)
table(predict(iris.kknn, iris.valid), iris.valid$Species)

head(iris.kknn$W)
head(iris.kknn$D)
head(iris.kknn$C)
head(iris.kknn$fitted.values)


require(randomForest)
fitKF <- randomForest(Kyphosis ~ Age + Number + Start, data=kyphosis)
print(fitKF)
importance(fitKF)

fitSwiss <- randomForest(Fertility ~ Agriculture + Education + Catholic, data=swiss)
print(fitSwiss)
importance(fitSwiss)
varImpPlot(fitSwiss)

plot(fitSwiss)

getTree(fitSwiss, 1, labelVar=TRUE)
