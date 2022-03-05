# (a)
college <- read.csv("college.csv")

#(b)
rownames(college) <- college[, 1]

college <- college[, -1]

# (c)
summary(college)

college[, 1] <- as.numeric(factor(college[,1]))
pairs(college[, 1:10])

plot(college$Private, college$Outstate)

Elite <- rep ("No", nrow (college))
Elite[college$Top10perc > 50] <- " Yes "
Elite <- as.factor(Elite)
college <- data.frame(college , Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate)
par(mfrow = c(2, 2))
hist(college$Books, col=2, xlab='Books', ylab= 'Count')
hist(college$PhD, col=4, xlab='PhD',ylab='Count')
hist(college$Grad.Rate, col=3, xlab='Graduation Rate', ylab='Count')
hist(college$perc.alumni,col=6,xlab = 'Alumni',ylab = 'Count')
hist(college$Books, breaks = 30)
