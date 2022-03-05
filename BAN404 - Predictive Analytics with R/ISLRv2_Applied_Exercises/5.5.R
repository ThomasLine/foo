# 5.5

# Packages / # Read data
library(ISLR)

# (a)
logreg = glm(default ~ balance, data = Default, family = "binomial")
summary(logreg)

# (b)
# Split into training and test data
n = nrow(Default)
ntrain = floor(n / 2)
ind = sample(1:n, size = ntrain)
train = Default[ind, ]
test = Default[-ind, ]
# Fit a logistic regression model to the training data
logreg2 = glm(default ~ balance, data = train, family = "binomial")
# Predict on the test observations
prob2 = predict(logreg2, newdata = test, type = "response")
pred2 = prob2 > 0.5
# Compute validation z error (test error)
conf_matrix = table(test$default, pred2)
conf_matrix 
sum(diag(conf_matrix)) / ntrain

# (d)
logreg3 = glm(default ~ balance + income + student, 
              data = train, family = "binomial")
prob3 = predict(logreg3, newdata = test, type = "response")
pred3 = prob3 > 0.5
conf_matrix3 = table(test$default, pred3)
conf_matrix3
