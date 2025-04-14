library(ISLR2)
library(tidyverse)
library(tidymodels)
library(e1071)
library(MASS)
library(pROC)
library(ggcorrplot)
library(patchwork)


corr <- round(cor(Boston),1)
ggcorrplot(corr,
           type = "upper",
           lab = TRUE) +
    labs(title = "Correlation Matrix")

p1 <- Boston |> 
    ggplot(mapping = aes(x = lstat, y=..density..)) +
    geom_histogram(bins = 50) +
    geom_density(color = "red")

p2 <- Boston |> 
    ggplot(mapping = aes(x = age, y = ..density..)) +
    geom_histogram(bins = 50) +
    geom_density(color = "red")

p3 <- Boston |> 
    ggplot(mapping = aes(x = medv, y = ..density..)) +
    geom_histogram(bins = 50) +
    geom_density(color = "red")

p1 | p2 | p3

cor(Boston$medv, Boston$lstat)
cor(Boston$lstat, Boston$age)

dim(Boston)
sum(is.na(Boston))


