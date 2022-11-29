library(e1071) # for navieBayes
library(caret) #for Classification And REgression Training
library(dplyr)
data(iris)
#Display first few rows in a iris dataset
head(iris)

summary(iris)

set.seed(123) # Here seed is set to ensure reproducibility;
indexes = createDataPartition(iris$Species, p = .8, list = F)
train = iris[indexes, ]
test = iris[-indexes, ]
#Building the Model using training data
nb_class = naiveBayes(Species~., data = train)
print(nb_class)


pred = predict(nb_class, test, type="class")
#Confusion Matrix
cm = confusionMatrix(test$Species, pred)
print(cm)
