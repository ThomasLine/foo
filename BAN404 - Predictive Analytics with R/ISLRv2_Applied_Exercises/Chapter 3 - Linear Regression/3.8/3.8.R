# 3.8

# Packages
library(ISLR)

# Data
Auto <- Auto

# (a)
lm <- lm(mpg ~ horsepower, data = Auto)
summary(lm)

# Use for general confidence intervals
confint(lm)

# Use for confidence intervals for given values of predictors
predict(lm, 
        data.frame(horsepower = 98), 
        interval = "confidence", 
        level = 0.95)

# (b)
plot(Auto$horsepower, Auto$mpg, 
     col = Auto$cylinders, 
     main = "Mpg vs. Horsepower",
     col.main = "red",
     xlab = "Horsepower", 
     ylab = "Mpg", 
     col.lab = "red")
abline(lm, col = "red")



