# EXERCISE SET 1

# Task 1
x <- c(1, 2, 3, 4, 5)
y <- c(1.88, 4.54, 10.12, 9.14, 11.26)

df <- data.frame(x, y)

alpha1 <- function(x,y) {
  
  n <- length(x)
  
  ret <- (n*sum(x*y) - (sum(y) * sum(x))) / (n * sum(x^2) - (sum(x))^2)
  
  return(ret)

}

alpha1(x, y)

# a)
alpha0 <- mean(y) - alpha1(x, y) * mean(x)
pred_x3 <- alpha0 + 2.336*3

# Or:
x <- 1:5
y <- c(1.88, 4.54, 10.12, 9.14, 11.26)
n <- length(x)
# Fit model
a1 <- (n * sum(x * y) - sum(y) * sum(x) / (n * sum(x^2) - sum(x)^2))
a0 <- mean(y) - a1 * mean(x)
# Prediction for x = 3
a0 + a1 * 3

# b) 
model <- lm(y ~ x, df)
model

predict <- predict(model)
predict[3]

# Can also call the value we want to predict directly 
predict(model, data.frame(x = 3))

# c)
mean(df$y[order(abs(df$x-3))[1:3]])

# KNN for K = 3
(4.54 + 10.12 + 9.14) / 3

# KNN for K = 1
10.12

# KNN for K = 5
(1.88 + 4.54 + 10.12 + 9.14 + 11.26) / 5

# d) 
knn <- function(x0, x, y, K = 20) {
  d <- abs(x - x0)
  o <- order(d)[1:K]
  ypred <- mean(y[o])
  return(ypred)
}

knn(3, x, y, K = 3)

d # Distances beteen x0 and the different x - values
o # Indicies of the three smallest distances
ypred # Average of the y's for the 3 observations with smallest d


# Task 2

# a)
library(ISLR)
summary(College)
help(College)
data("College")

# b)
set.seed(123)
n <- nrow(College)
train_indicator <- sample(1:n, size = floor(n / 2))

train <- College[train_indicator, ]
test <- College[-train_indicator, ]

# c) 
m1 <- lm(Apps ~ Private + Accept, data = train)
summary(m1)

# d)
pred_train <- predict(m1)
mse_train <- mean((train$Apps - pred_train)^2)
mse_train

# e)
m2 <- lm(Apps ~ Accept, data = train)
summary(m2)

pred_train_2 <- predict(m2)
mse_train_2 <- mean((train$Apps - pred_train_2)^2) 
mse_train_2 

mse_diff <- mse_train - mse_train_2

# f)
pred_test <- predict(m1, newdata = test)
mse_test <- mean((test$Apps - pred_test)^2)

pred_test_2 <- predict(m2, newdata = test)
mse_test_2 <- mean((test$Apps - pred_test_2)^2)

# g)
mse_train
mse_test
mse_train_2
mse_test_2

# h)
x0=test$Accept
x=train$Accept
y=train$Apps
pred_test_3 <- apply(as.matrix(x0), 1, knn, x = x, y = y, 3)
mse_test_3 <- mean((test$Apps - pred_test_3)^2)
mse_test_3
