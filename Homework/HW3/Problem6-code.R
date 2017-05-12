#Problem 6
#Load the data
library(ISLR)
library(MASS)
library(caret)
data("Boston")
attach(Boston)
#Get the information about variables
help(Boston)

crim01 <- rep(0, length(Boston$crim))
crim01[Boston$crim > median(Boston$crim)] <- 1
Boston <- data.frame(Boston, crim01)
summary(Boston)

set.seed(1234)
train <- sample(1:dim(Boston)[1], dim(Boston)[1]*.7, rep=FALSE)
test <- -train
Boston.train <- Boston[train, ]
Boston.test <- Boston[test, ]
crim01.test <- crim01[test]
#Logistic regression
fit.glm1 <- glm(crim01 ~ . - crim01 - crim, data = Boston, family = binomial)
fit.glm1

library(corrplot)
corrplot::corrplot.mixed(cor(Boston[, -1]), upper="circle")
fit.glm <- glm(crim01 ~ nox + indus + age + rad, data = Boston, family = binomial)
probs <- predict(fit.glm, Boston.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, crim01.test)
mean(pred.glm != crim01.test)

#LDA
fit.lda <- lda(crim01 ~ nox + indus + age + rad , data = Boston)
pred.lda <- predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)
mean(pred.lda$class != crim01.test)
#QDA
fit.qda = qda(crim01 ~ nox + indus + age + rad , data = Boston)
pred.qda = predict(fit.qda, Boston.test)
table(pred.qda$class, crim01.test)
mean(pred.qda$class != crim01.test)
#KNN
data = scale(Boston[,-c(1,15)])
set.seed(1234)
train = sample(1:dim(Boston)[1], dim(Boston)[1]*.7, rep=FALSE)
test = -train
#In KNN, we get training_y and testing_y seperately
training_data = data[train, c("nox" , "indus" , "age" , "rad")]
testing_data = data[test, c("nox" , "indus" , "age" , "rad")]
train.crime01 = Boston$crim01[train]
test.crime01= Boston$crim01[test]
library(class)
set.seed(1234)
knn_pred_y = knn(training_data, testing_data, train.crime01, k = 1)
table(knn_pred_y, test.crime01)
mean(knn_pred_y != test.crime01)

knn_pred_y = NULL
error_rate = NULL
for(i in 1:dim(testing_data)[1]){
  set.seed(1234)
  knn_pred_y = knn(training_data,testing_data,train.crime01,k=i)
  error_rate[i] = mean(test.crime01 != knn_pred_y)
}

### find the minimum error rate and corresponding k value
min_error_rate = min(error_rate)
print(min_error_rate)
K = which(error_rate == min_error_rate)
print(K)