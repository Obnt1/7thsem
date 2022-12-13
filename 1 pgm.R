library(dplyr) 
library(ggplot2)
library(corrplot)
cars_multi <- read.csv("C:/Users/acer/Desktop/Datasets/Cars/Cars/cars_multi.csv")
cars_price <- read.csv("/Users/acer/Desktop/Datasets/Cars/Cars/cars_price.csv")
head(cars_multi)
head(cars_price)
dim(cars_multi)
dim(cars_price)
cars <- left_join(cars_multi, cars_price, by = "ID")
colnames(cars)
sum(!complete.cases(cars))
summary(cars)
str(cars)


ggplot(cars, aes(mpg)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of MPG", y = "Count") +
  theme_classic()

ggplot(cars, aes(cylinders)) +
  geom_bar() +
  labs(title = "Cylinders", y = "Count") +
  theme_classic()


boxplot(cars$displacement, data=cars$displacement, main="Box Plot Displacement",
        xlab="", ylab="Displacement")

count(cars[as.character(cars$horsepower) == "?",])


ggplot(cars, aes(weight)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Weight", y = "Count") +
  theme_classic()


ggplot(cars, aes(acceleration)) +
  geom_density() +
  labs(title = "Density of Weight") +
  theme_classic()

to_Plot <- as.data.frame(table(cars$model))
colnames(to_Plot) <- c("Model", "Frequency")
ggplot(to_Plot, aes(x = Model, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Model") +
  theme_classic()

ggplot(cars, aes(origin)) +
  geom_bar() +
  labs(title = "Origin", y = "Count") +
  theme_classic()


cars$price <- as.integer(cars$price)

boxplot(cars$price, data=cars$price, main="Price BoxPLot",
        xlab="", ylab="Price")

to_Plot <- as.data.frame(table(cars$price))
colnames(to_Plot) <- c("Price", "Frequency")

ggplot(head(to_Plot[ order(-to_Plot[,2]), ]), aes(x = reorder(Price, Frequency), y
                                                  = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Common Price", x = "Price") +
  theme_classic() +
  coord_flip()

cars$horsepower <- as.numeric(as.character(cars$horsepower))

cars <- cars[complete.cases(cars),]
cars <- cars[,-1]

nums <- sapply(cars, is.numeric)
correlations <- cor(cars[,nums])
corrplot(correlations, order = "hclust")
