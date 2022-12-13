library(dplyr) 
library(ggplot2) 
library(caret) 
library(datarium) 
data("marketing", package = "datarium")
head(marketing)
dim(marketing)
summary(marketing)
str(marketing)
sum(!complete.cases(marketing))
ggplot(marketing, aes(sales)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of Sales", y = "Count") +
  theme_classic()
ggplot(marketing, aes(x = youtube, y = sales)) +
  geom_point() +
  stat_smooth()
cor(marketing$sales, marketing$youtube)
set.seed(123)
training.samples <- marketing$sales %>% createDataPartition(p = 0.6, list = FALSE)
train.data <- marketing[training.samples,]
test.data <- marketing[-training.samples,]
model <- lm(sales ~ youtube, data = train.data)
summary(model)$coef
predictions <- model %>% predict(test.data)

rmse(predictions, test.data$sales)
R2(predictions, test.data$sales)

newdata <- data.frame(youtube=c(0,1000))
predictions <- model %>% predict(newdata)
predictions
