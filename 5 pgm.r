library(dplyr) # for Iris dataset
library(ggplot2)
#Loading iris dataset
data(iris)
#Display first few rows in a iris dataset
head(iris)

summary(iris)


iris_2<-iris[-5]
head(iris_2)

iris_3<-as.data.frame(scale(iris_2))
head(iris_3)

sapply(iris_2,mean)

sapply(iris_2,sd)

sapply(iris_3,mean)

sapply(iris_3,sd)

#install.packages("NbClust")
library(NbClust)
# creating function wssplot
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(iris_3,nc=30,seed=1234)

iris_kmeans<-kmeans(iris_3,7)
iris_kmeans$centers

iris_kmeans$size

iris$clstr<-iris_kmeans$cluster

iris$clstr<-iris_kmeans$cluster
table(iris$Species,iris$clstr)

iris$clstr <- as.factor(iris$clstr)
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$clstr)) + geom_point()