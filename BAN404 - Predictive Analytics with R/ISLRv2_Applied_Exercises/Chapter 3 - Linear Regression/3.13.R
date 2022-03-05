# Task 4.13

# READ DATA
library(ISLR)
library(MASS)

# (a) First look at the data
head(weekly)
plot.ts(Weekly$Today)
names(Weekly)
sapply(Weekly, class)
table(Weekly$Direction)
plot.ts(Weekly$Volume)
attach(Weekly)
by(Volume, Direction, mean)
by(Volume, Direction, sd)
plot(Volume ~ Direction)

# First try with a linear probability model
formula1 = Direction ~ Volume + Lag1 + Lag2 + Lag3 + Lag4 + Lag5
linreg = lm(formula1, data = Weekly) # Problem with factor-valued dependent var. 

# Solution to the problem:
y = as.numeric(Weekly$Direction)
y = y - 1

# Formula with the problem fixed
formula2 = y ~ Volume + Lag1 + Lag2 + Lag3 + Lag4 + Lag5
linreg2 = lm(formula2, data = Weekly)
summary(linreg2)
prob2 = predict(linreg2)
pred2 = prob2 > 0.5
conf_matrix = table(Weekly$Direction, pred2)
conf_matrix
accuracy = sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# (b)
logreg = glm(formula2, data = Weekly, family = binomial())
summary(logreg)

# (c)
pred2 = predict(logreg)
pred2 = predict(logreg, type = "response")
head(log(pred2 / (1 - pred2)))
conf_matrix = table(Weekly$Direction, pred2 > 0.5)
conf_matrix
prop.table(conf_matrix, margin = 1)

# (d)

# (e) Linear discriminant analysis
lda1 = lda(Direction ~ Lag2, data = Weekly)
pred3 = predict(lda1)
conf_matrix = table(Weekly$Direction, pred3$class)
conf_matrix
