library(ISLR2)
library(tidyverse)
library(tidymodels)
library(e1071)
library(MASS)
library(pROC)



# Loading and splitting Default dataset --------------
df.split <- initial_split(Default, prop = 0.8)

df.train <- training(df.split)
df.test <- testing(df.split)
# Logistic Regression -------------
lr.fit <- glm(default ~ balance + income, data = df.train, 
              family = binomial(link = "logit"))
summary(lr.fit)

lr.train.probs <- predict(lr.fit, type = "response")
lr.test.probs <- predict(lr.fit,newdata = df.test, type = "response")

lr.train.preds <- ifelse(predict(lr.fit, type = "response") > 0.5,
                         "Yes", "No")
lr.test.preds <- ifelse(predict(lr.fit, df.test, type = "response")>.5,
                        "Yes", "No")



table(lr.train.preds, df.train$default)
table(lr.test.preds, df.test$default)

accuracy.lr.train <- mean(lr.train.preds == df.train$default)
accuracy.lr.train

accuracy.lr.test <- mean(lr.test.preds == df.test$default)
accuracy.lr.test

# LDA Fit --------------------------------------------------

lda.fit <- lda(default ~ balance + income, data = df.train)
lda.fit

lda.train.preds <- predict(lda.fit)
lda.test.preds <- predict(lda.fit, df.test)

table(lda.train.preds$class, df.train$default)
table(lda.test.preds$class, df.test$default)

accuracy.lda.train <- mean(lda.train.preds$class == df.train$default)
accuracy.lda.train

accuracy.lda.test <- mean(lda.test.preds$class == df.test$default)
accuracy.lda.test

# Naive Bayes Fit ----------------------------------------

nb.fit <- naiveBayes(default ~ balance + income, data = df.train)
nb.train.predicts <- predict(nb.fit, df.train)
nb.test.predicts <- predict(nb.fit, df.test)

table(nb.train.predicts, df.train$default)
table(nb.test.predicts, df.test$default)

accuracy.nb.train <-  mean(nb.train.predicts == df.train$default)
accuracy.nb.test <-  mean(nb.test.predicts == df.test$default)

accuracy.nb.train
accuracy.nb.test

nb.train.probs <- predict(nb.fit, df.train, type = "raw")[,"Yes"]
nb.test.probs <- predict(nb.fit, df.test, type = "raw")[,"Yes"]

# Evaluating accuracy of the models ----------------

table(lr.train.preds, df.train$default)
table(lr.test.preds, df.test$default)

accuracy.lr.train
accuracy.lr.test

table(lda.train.preds$class, df.train$default)
table(lda.test.preds$class, df.test$default)
accuracy.lda.train
accuracy.lda.test

table(nb.train.predicts, df.train$default)
table(nb.test.predicts, df.test$default)

accuracy.nb.train
accuracy.nb.test

# Evaluating ROC curves --------------------------------
par(pty = "s",mfrow = c(1,3))

roc.lr <- roc(df.test$default,lr.test.probs)
plot.roc(roc.lr,
         print.auc = TRUE)

roc.lda <- roc(df.test$default, lda.test.preds$posterior[,2])
plot.roc(roc.lda,
         print.auc = TRUE,
         add = FALSE)


roc.nb <- roc(df.test$default, nb.test.probs)
plot.roc(roc.nb,
         print.auc = TRUE,
         add = FALSE)


auc(roc.lda)
auc(roc.lr)
auc(roc.nb)
