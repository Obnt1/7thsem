
loan <- read.csv("credit_data.csv")

str(loan)

loan.subset <- loan[c('Creditability','Age..years.','Sex...Marital.Status','Occupation','Account.Balance','Credit.Amount','Length.of.current.employment','Purpose')]

str(loan.subset)

head(loan.subset)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

loan.subset.n <- as.data.frame(lapply(loan.subset[,2:8], normalize))

head(loan.subset.n)

set.seed(123)
dat.d <- sample(1:nrow(loan.subset.n),size=nrow(loan.subset.n)*0.7,replace=FALSE)

train.loan <- loan.subset[dat.d,] # 70% training data
test.loan <- loan.subset[-dat.d,]

train.loan_labels <- loan.subset[dat.d,1]
test.loan_labels <-loan.subset[-dat.d,1]

library(class) 

NROW(train.loan_labels) 
knn.26 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=26)
knn.27 <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=27)

ACC.26 <- 100 * sum(test.loan_labels == knn.26)/NROW(test.loan_labels)
ACC.27 <- 100 * sum(test.loan_labels == knn.27)/NROW(test.loan_labels)
ACC.26

ACC.27

table(knn.26 ,test.loan_labels)

knn.26

table(knn.27 ,test.loan_labels)

knn.27

library(caret)

confusionMatrix(table(knn.26 ,test.loan_labels))

i=1
k.optm=1
for (i in 1:28){
  knn.mod <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=i)
  k.optm[i] <- 100 * sum(test.loan_labels == knn.mod)/NROW(test.loan_labels)
  k=i
  cat(k,'=',k.optm[i],'') }

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")