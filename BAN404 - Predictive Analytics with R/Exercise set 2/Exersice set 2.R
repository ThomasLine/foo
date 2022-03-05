# EXERCISE SET 2
library(tidyverse)

# Task 1------------------------------------------------------------------------
x <- c(1, 2, 3, 4, 5, 6, 7)
y <- c(0, 1, 0, 1, 1, 1, 1)

# a) 
m1 = glm(y ~ x, family = "binomial")
summary(m1)

equation <- function(x0) {
    1 / (1 + exp(2.8126 - 1.1629 * x0))
}

equation(x0 = 4)

# Bigger probability that y=1 when x0=4, as equation(x0 = 4) = 0.86

# b)

# Method 1
df <- data.frame(x, y)
pred_y <- sapply(x, equation)
df$pred <- pred_y
df %>% ggplot(aes(x = x, y = y))+
  geom_point() + 
  geom_line(aes(x = x, y = pred_y))

# Method 2
plot(x, y)
prob = predict(m1,type = "response")
lines(x, prob, col = "blue")

# It is necessary that the type is a response. If not,
# values will follow a linear regression which does not limit
# itself to be between the values of 0 x|and 1, which is the case
# for logistic regression.

# Quote from ISLR:
# The type = "response" option tells R to output probabilities of the form 
# P(Y = 1|X), as opposed to other information such as the logit.

# c)
knn = function(x0,x,y,K) {
  d = abs(x0 - x)
  o = order(d)
  prob = mean(y[o[1:K]])
  return(prob)
}

# Visualize knn
d <- abs(4 - x)
o <- order(d)
prob <- mean(y[o[1:K]])

# Specific case
x0 <- 4
k_vec <- c(1, 3, 5)
k_val <- c()

for(k in k_vec) {
  knn_value <- knn(x0 = x0, x, y, K = k)
  k_val <- c(k_val, knn_value)
}

# d)
prob_knn = matrix(0,7,1)
for(i in 1:7) prob_knn[i] = knn(x0 = x[i], x, y, K = 3)
plot(x, y)
lines(x, prob_knn)

# Plotting it all together
prob_knn1 = matrix(0, 7, 1)
for(i in 1:7) prob_knn1[i] = knn(x0 = x[i], x, y, K = 1)
prob_knn5 = matrix(0, 7, 1)
for(i in 1:7) prob_knn5[i] = knn(x0 = x[i], x, y, K = 5)
plot(x, y)
lines(x, prob_knn1, col = "red")
lines(x, prob_knn5, col = "green")
lines(x, prob, col = "blue")
legend(4, 0.5, legend = c("KNN, K = 1", "KNN, K = 5", "Logistic Regression"), 
       col = c("red", "green", "blue"), lty = 1, cex = 0.8)

# Jonas's solution
prob_knn1 = matrix(0, 7, 1)
prob_knn3 = matrix(0, 7, 1)
prob_knn5 = matrix(0, 7, 1)
for(i in 1:7) {
  prob_knn1[i] = knn(x0 = x[i], x, y, K=1)
  prob_knn3[i] = knn(x0 = x[i], x, y, K = 3)
  prob_knn5[i] = knn(x0 = x[i], x, y, K=5)
}
plot(x, y)
lines(x, prob_knn1, col = 2, lty = 2)
lines(x, prob_knn3, col = 3, lty = 3)
lines(x, prob_knn5, col = 4, lty = 4)
lines(x, prob, col = 5, lty = 5)
legend("bottomright", 
       legend = c("K = 1", "K = 3", "K = 5", "logistic"),lty = 2:5,col = 2:5)

# e)
pred = prob > 0.5
table(y, pred)
prop.table(table(y, pred), margin = 1)

pred_knn = prob_knn > 0.5
table(y, pred_knn)
prop.table(table(y, pred_knn), margin = 1)

# Task 2------------------------------------------------------------------------
library(ISLR)
library(MASS)

# a)
n <- nrow(Default)
ind <- sample(1:n, size = floor(n / 2))
train <- Default[ind, ]
test <- Default[-ind, ]
logreg <- glm(default ~ ., data = train, family = "binomial")
prob_logreg <- predict(logreg, newdata = test, type = "response")
lda <- lda(default ~ balance + income, data = train)
prob_lda <- predict(lda, newdata = test)$posterior[ , 2]
head(prob_logreg)
head(prob_lda)

# b)
pred_logreg <- prob_logreg > 0.5
pred_lda <- prob_lda > 0.5
prop.table(table(test$default, pred_logreg), margin = 1)
prop.table(table(test$default, pred_lda), margin = 1)

# Task 3------------------------------------------------------------------------

# a)
library(ISLR)
Auto$y = "low"
Auto$y[Auto$mpg > median(Auto$mpg)] = "high"
Auto$y = as.factor(Auto$y)
Auto$age = 83 - Auto$year
Auto = Auto[ ,!(names(Auto) %in% c("mpg","name","year"))]

# b)
n = nrow(Auto)
draw = sample(1:n, size = floor(n / 2))
train = Auto[draw, ]
test = Auto[-draw, ]
m1 = glm(y ~ ., data = train, family = "binomial")
summary(m1)
