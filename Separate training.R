############################################################################
# This script partitions the data from each country/region into train set 
# and test set, then fits and tests a ridge model separately for every 
# country/region
############################################################################
library(caret)
library(glmnet)
library(haven)
library(tidyverse)

# Read in the complete data
Part1 <- read_sas('data/cy6_ms_cmb_stu_qqq.sas7bdat')
Part2 <- read_sas('data/cy6_ms_cmb_stu_qq2.sas7bdat')
Full_data <- left_join(Part1, Part2, by = 'CNTSTUID', suffix = c('', '.y'))
load('names.RData')

############################################################################
# Tibble to store the test metrics
l <- length(countries_selected)
results <- tibble(id = countries_selected, 
                  RMSE = numeric(l), MAE = numeric(l),
                  corr = numeric(l), kendall = numeric(l),
                  median_RMSE = numeric(l), median_MAE = numeric(l))
# Tibble to store the coefficients
coef <- tibble(id = countries_selected)

for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == countries_selected[i])
  # NEW[NEW == ''] <-  NA
  
  y <- NEW$PV1SCIE
  w <- NEW$W_FSTUWT
  x <- NEW %>% select(names_selected)
  
  x$NA_prop <- rowMeans(is.na(x))
  
  x$ST011D17TA[str_ends(x$ST011D17TA, '1')] <- 1
  x$ST011D17TA[str_ends(x$ST011D17TA, '2')] <- 2
  x$ST011D18TA[str_ends(x$ST011D18TA, '1')] <- 1
  x$ST011D18TA[str_ends(x$ST011D18TA, '2')] <- 2
  x$ST011D19TA[str_ends(x$ST011D19TA, '1')] <- 1
  x$ST011D19TA[str_ends(x$ST011D19TA, '2')] <- 2
  
  x$ST021Q01TA[is.na(x$ST021Q01TA)] <- 0
  
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
  
  x_train <- x_train %>% add_row(); x_train[nrow(x_train),] <- -100 
  x_train[names_categorical] <- 
    as.data.frame(lapply(x_train[names_categorical], function(x) factor(x)))
  
  knnImpute <- preProcess(x_train, method = 'knnImpute')
  x_train <- predict(knnImpute, x_train)
  
  dummy <- dummyVars(~ ., x_train)
  x_train <- as.data.frame(predict(dummy, x_train))
  x_train <- x_train[-nrow(x_train),] 
  
  zv <- preProcess(x_train, method = c('zv'))
  x_train <- predict(zv, x_train)
  
  x_test <- x_test %>% add_row(); x_test[nrow(x_test),] <- -100
  x_test[names_categorical] <- 
    as.data.frame(lapply(x_test[names_categorical], function(x) factor(x)))
  
  x_test <- predict(knnImpute, x_test)
  
  dummy_test <- dummyVars(~ ., x_test)
  x_test <- as.data.frame(predict(dummy_test, x_test))
  x_test <- x_test[-nrow(x_test),]  
  
  names_xtrain <- names(x_train)
  train_minus_test <- setdiff(names_xtrain, names(x_test))
  if (length(train_minus_test)) x_test[train_minus_test] <- 0
  x_test <- select(x_test, names_xtrain)
  
  set.seed(111)
  ridge <- cv.glmnet(as.matrix(x_train), y_train,
                     weights = w_train, alpha = 0, nfolds = 10)
  
  predicted <- predict(ridge, as.matrix(x_test), s = 'lambda.min')
  
  results[i, 2] <- RMSE(predicted, y_test)
  results[i, 3] <- MAE(predicted, y_test)
  results[i, 4] <- cor(predicted, y_test, method = 'pearson')[1]
  results[i, 5] <- cor(predicted, y_test, method = 'kendall')[1]
  results[i, 6] <- RMSE(rep(median(y_test), length(y_test)), y_test)
  results[i, 7] <- MAE(rep(median(y_test), length(y_test)), y_test)
  
  name_set <- dimnames(coef.cv.glmnet(ridge, s = 'lambda.min'))[[1]]
  coef[setdiff(name_set, names(coef))] <- NA
  coef[i, match(name_set, names(coef))] <- 
    matrix(coef.cv.glmnet(ridge, s = 'lambda.min'))[, 1]
}

country_data <- read_csv('data/HDI.csv') %>%
  right_join(results, by = c('CNTRYID' = 'id')) %>%
  left_join(coef, by = c('CNTRYID' = 'id'))

write_csv(country_data, 'output/results_separate.csv')

# country_data <- read_csv('output/results_separate.csv')
# GDP <- read_csv('data/Country-level Analysis Passion and Science Mar 30 AL EDT.csv') %>% 
#   select(CNTRYID, ZlnCHINAGDP)
# country_data <- left_join(country_data, GDP, by = 'CNTRYID') %>% 
#   select(1:2, 354, 3:353)
# write_csv(country_data, 'output/results_separate.csv')
