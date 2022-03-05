# 2.9 

# Packages 
library(ISLR)
library(tidyverse)

# Data 
Auto <- Auto

# Explorative analysis
summary(Auto)
sapply(Auto, function(x)sum(is.na(x)))

# Exploring in general
Auto$name <- as.character(Auto$name)

# (a) 
classes <- as.data.frame(sapply(Auto, class))

# (b)
numeric_classes <- classes %>% 
  filter(. == "numeric") %>% 
  rownames()

Auto %>% 
  select(numeric_classes) %>% 
  sapply(., range)

# (c)
mean_and_sd <- 
  Auto %>% 
  select(numeric_classes) %>% 
  sapply(., function(x){list(mean(x), sd(x))}) %>% 
  as.data.frame() 

rownames(mean_and_sd) <- c("mean", "sd")

mean_and_sd <- pivot_longer(mean_and_sd, 
                            col = everything(), 
                            names_to = "names",
                            values_to = "value")
#mean_and_sd <- pivot_longer(mean_and_sd, col = mpg:origin, names_to = "name", values_to = "value")

# (d)
Auto %>% 
  select(numeric_classes) %>% 
  head()

Auto_reduced <- Auto[-(10:85),]


# Compare data frames
library(janitor)
compare_df_cols(Auto_reduced, Auto)
Auto[10,] == Auto_reduced[10,]


# Range, mean, sd
mean_sd_range <- 
  Auto_reduced %>% 
  select(numeric_classes) %>% 
  sapply(., function(x){
    list(mean(x), sd(x), range(x))
  }) %>% 
  as.data.frame() 



# (e)
Auto %>% 
  ggplot(aes(x = mpg, y = weight)) + 
  geom_point()


# (f)
plotFunc <- 
  function(input, df){
    df %>% 
      ggplot(aes(x = mpg, y = input)) + 
      geom_point()
  }

cols <- names(Auto)
cols <- cols[cols != "mpg"]


par(mfrow = c(2,4))
for(c in 1:length(cols)){
  #input = cols[c]
  #print(c)
  plot(Auto$mpg, Auto$input, ylab = cols[c])
}






