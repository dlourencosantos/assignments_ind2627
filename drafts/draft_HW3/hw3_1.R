library(ISLR2)
library(tidyverse)
library(tidymodels)
library(boot)

attach(Wage)

set.seed(42)
wage_split <- initial_split(Wage, prop = 0.8)

wage_train <- training(wage_split)
wage_test <- testing(wage_split)
# Fitting polynomial regressions -------------------

poly_1 <- glm(wage ~ poly(age, 1), data = wage_train)
poly_2 <- glm(wage ~ poly(age, 2), data = wage_train)
poly_3 <- glm(wage ~ poly(age, 3), data = wage_train)
poly_4 <- glm(wage ~ poly(age, 4), data = wage_train)
poly_5 <- glm(wage ~ poly(age, 5), data = wage_train)

summary(poly_1)
summary(poly_2)
summary(poly_3)
summary(poly_4)
summary(poly_5)

cv.erro_1 <- cv.glm(wage_train, poly_1, K = 10)
cv.erro_2 <- cv.glm(wage_train, poly_2, K = 10)
cv.erro_3 <- cv.glm(wage_train, poly_3, K = 10)
cv.erro_4 <- cv.glm(wage_train, poly_4, K = 10)
cv.erro_5 <- cv.glm(wage_train, poly_5, K = 10)

cv.erro_1$delta
cv.erro_2$delta
cv.erro_3$delta
cv.erro_4$delta
cv.erro_5$delta
