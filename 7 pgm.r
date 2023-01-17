library(caret)
library(rpart.plot)
#paste car.data in documents folder download via https://shorturl.at/bdzPY 
car_df <- read.csv("~/car.data", sep = ',', header = FALSE)
str(car_df)
head(car_df)
set.seed(123)
intrain <- createDataPartition(y = car_df$V7, p= 0.7, list = FALSE)
training <- car_df[intrain,]
testing <- car_df[-intrain,]
dim(training); dim(testing);
anyNA(car_df)
summary(car_df)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
dtree_fit <- train(V7 ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
testing[1,]
predict(dtree_fit, newdata = testing[1,])
test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred,as.factor(testing$V7))