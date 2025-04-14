library(ISLR2)
library(tidyverse)
library(tidymodels)
library(MASS)
library(e1071)
library(pROC)
library(ggcorrplot)
library(patchwork)
library(boot)

insurance <- Insurance
insurance$Age <- factor(insurance$Age, ordered = FALSE)

glimpse(insurance)

set.seed(42)
pr.fit <- glm(Claims ~ Age + Holders, data = insurance,
              family = poisson)

cv.err <- cv.glm(insurance, pr.fit, K=10)$delta[1]
cv.err

summary(pr.fit)

exp(pr.fit$coefficients[2])

ggplot(insurance, mapping = aes(x=Age, y= Claims)) +
    geom_boxplot(aes(fill = Age))

ggplot(insurance, mapping = aes(x=Holders, y= Claims)) +
    geom_point()

ggplot(insurance, mapping = aes(x=Age, y= Holders)) +
    geom_boxplot()

mean(insurance$Claims)

