data('USArrests')

states <- row.names(USArrests)
states
names(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pr.out <- prcomp(USArrests, scale=TRUE)

pr.out$center
pr.out$scale

pr.out$rotation

dim(pr.out$x)

biplot(pr.out, scale=0)

pr.out$sdev

pr.var <-  pr.out$sdev^2
pr.var

pve <- pr.var/sum(pr.var)
pve

data('iris')
head(iris)

irisdata1 <- iris[,1:4]
irisdata1

principal_components <- princomp(irisdata1, cor=TRUE, score=TRUE)
summary(principal_components)
plot(principal_components)
plot(principal_components, type='l')
biplot(principal_components)

data(Boston, package='MASS')

pca_out <- prcomp(Boston, scale=T)
pca_out
plot(pca_out)

biplot(pca_out, scale=0)
boston_pc <- pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)