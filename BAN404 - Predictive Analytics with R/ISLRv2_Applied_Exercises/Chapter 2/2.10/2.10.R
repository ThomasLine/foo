# 2.10

# Packages 
library(MASS)
library(tidyverse)
library(gridExtra)

# Data
Boston <- Boston

# (a)
nrow(Boston)
ncol(Boston)

# (b)
vars <- c("nox", "medv", "rm", "ptratio")
varsplots <- c()
for(p in 1:length(vars)) {
print(
  ggplot(Boston, aes(x = !! sym(vars[p]), y = crim)) +
  geom_point() +
  ylab("")
  ) -> varsplots[[p]]
}

grid.arrange(grobs = varsplots, top = "Crime vs. varables")

# (c) 
vars <- names(Boston)
varsplots <- c()
for(p in 1:length(vars)) {
  print(
    ggplot(Boston, aes(x = !! sym(vars[p]), y = crim)) +
      geom_point() +
      ylab("")
  ) -> varsplots[[p]]
}

grid.arrange(grobs = varsplots, top = "Crime vs. varables")

# (d)
map(Boston, max)
map(Boston, which.max)
max <- sapply(Boston, max)
row <- sapply(Boston, which.max)
df <- round(data.frame(row, max), digits = 2)

# (e)
table(Boston$chas)
round(prop.table(table(Boston$chas)) * 100, digits = 2)

# (f)
median(Boston$ptratio)

# (g)
min(Boston$medv)
which.min(Boston$medv)
Boston[Boston$medv == 5, ]

Boston %>% 
  filter(row_number() == which.min(medv))

# (h)
Boston %>% 
  select(rm) %>% 
  filter(. > 7) %>% 
  nrow()

