############################################################################
# This script trains a ridge model using 80% of the data from the US. Then 
# the model is tested on the remaining 20% of US data
############################################################################
library(caret)
library(glmnet)
library(tidyverse)

# Reading US data
load('names.RData')
USA <- read_csv('data/USA.csv')

y <- USA$PV1SCIE
w <- USA$W_FSTUWT
x <- USA %>% select(names_selected)

prop_NA <- function(x) {mean(is.na(x))}
x$NA_prop <- apply(x, 1, prop_NA)

x$ST011D17TA[str_ends(x$ST011D17TA, '1')] <- 1
x$ST011D17TA[str_ends(x$ST011D17TA, '2')] <- 2
x$ST011D18TA[str_ends(x$ST011D18TA, '1')] <- 1
x$ST011D18TA[str_ends(x$ST011D18TA, '2')] <- 2
x$ST011D19TA[str_ends(x$ST011D19TA, '1')] <- 1
x$ST011D19TA[str_ends(x$ST011D19TA, '2')] <- 2

x$ST095Q04NA[x$ST095Q04NA == 5] <- 0
x$ST095Q07NA[x$ST095Q07NA == 5] <- 0
x$ST095Q08NA[x$ST095Q08NA == 5] <- 0
x$ST095Q13NA[x$ST095Q13NA == 5] <- 0
x$ST095Q15NA[x$ST095Q15NA == 5] <- 0

x$OCOD1 <- str_extract(x$OCOD1, '^.{1}')
x$OCOD2 <- str_extract(x$OCOD2, '^.{1}')
x$OCOD3 <- str_extract(x$OCOD3, '^.{1}')

x[names_categorical][is.na(x[names_categorical])] <- -99 

set.seed(111)
train_index <- createDataPartition(y, p = 0.8, list = F)
x_train <- x[train_index,]
x_test <- x[-train_index,]
y_train <- y[train_index]
y_test <- y[-train_index]
w_train <- w[train_index]
w_test <- w[-train_index]

x_train[names_categorical] <- 
  as.data.frame(lapply(x_train[names_categorical], function(x) factor(x)))

nzv <- nearZeroVar(x_train)
if (length(nzv)) x_train <- x_train[-nzv]

knnImpute <- preProcess(x_train, method = 'knnImpute')
x_train <- predict(knnImpute, x_train)

dummy <- dummyVars(~ ., x_train)
x_train <- as.data.frame(predict(dummy, x_train))

# corr <- preProcess(x_train, method = c('zv', 'corr'), cutoff = 0.90)
# x_train <- predict(corr, x_train)

x_test[names_categorical] <- 
  as.data.frame(lapply(x_test[names_categorical], function(x) factor(x)))
if (length(nzv)) x_test <- x_test[-nzv]

x_test <- predict(knnImpute, x_test)

dummy_test <- dummyVars(~ ., x_test)
x_test <- as.data.frame(predict(dummy_test, x_test))

names_xtrain <- names(x_train)
train_minus_test <- setdiff(names_xtrain, names(x_test))
if (length(train_minus_test)) x_test[train_minus_test] <- 0
x_test <- select(x_test, names_xtrain)

set.seed(111)
ridge <- cv.glmnet(as.matrix(x_train), y_train,
                   weights = w_train, alpha = 0, nfolds = 10)

predicted <- predict(ridge, as.matrix(x_test), s = 'lambda.min')

metrics <- c(RMSE(predicted, y_test), MAE(predicted, y_test), 
             cor(predicted, y_test, method = 'pearson')[1],
             cor(predicted, y_test, method = 'kendall')[1],
             original_sd <- sd(y_test), origninal_mad <- mad(y_test))
             
save(nzv, knnImpute, names_xtrain, ridge, metrics, file = 'ridge_US.RData')

