############################################################################
# This script partitions the data from each country/region into train set 
# and test set, then fits and tests a ridge model separately for every 
# country/region
############################################################################
library(caret)
library(doMC)
library(glmnet)
library(haven)
library(tidyverse)

registerDoMC(cores = 8)

# Read in the complete data
Part1 <- read_sas('data/cy6_ms_cmb_stu_qqq.sas7bdat')
Part2 <- read_sas('data/cy6_ms_cmb_stu_qq2.sas7bdat')
Full_data <- left_join(Part1, Part2, by = 'CNTSTUID', suffix = c('', '.y'))
load('names.RData')

prop_NA <- function(x) {mean(is.na(x))}
############################################################################
# Tibble to store the test metrics
list_of_id <- unique(Full_data$CNTRYID)
l <- length(list_of_id)
results <- tibble(id = list_of_id, RMSE = numeric(l), MAE = numeric(l), 
                  corr = numeric(l), kendall = numeric(l),
                  median_RMSE = numeric(l), median_MAE = numeric(l))
# Tibble to store the coefficients
coef <- tibble(id = list_of_id)

for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i])
  NEW[NEW == ''] <-  NA
  
  y <- NEW$PV1SCIE
  w <- NEW$W_FSTUWT
  x <- NEW %>% select(names_selected)
  
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
  
  medianImpute <- preProcess(x_train, method = c('center', 'scale', 'medianImpute'))
  x_train <- predict(medianImpute, x_train)
  
  dummy <- dummyVars(~ ., x_train)
  x_train <- as.data.frame(predict(dummy, x_train))
  
  # corr <- preProcess(x_train, method = c('zv', 'corr'), cutoff = 0.90)
  # x_train <- predict(corr, x_train)
  
  x_test[names_categorical] <- 
    as.data.frame(lapply(x_test[names_categorical], function(x) factor(x)))
  if (length(nzv)) x_test <- x_test[-nzv]
  
  x_test <- predict(medianImpute, x_test)
  
  dummy_test <- dummyVars(~ ., x_test)
  x_test <- as.data.frame(predict(dummy_test, x_test))
  
  train_minus_test <- setdiff(names(x_train), names(x_test))
  if (length(train_minus_test)) x_test[train_minus_test] <- 0
  x_test <- select(x_test, names(x_train))
  
  set.seed(111)
  ridge <- cv.glmnet(as.matrix(x_train), y_train,
                     weights = w_train, alpha = 0, nfolds = 10)
  
  predicted <- predict(ridge, as.matrix(x_test), s = 'lambda.min')
  
  results[i, 2] <- RMSE(predicted, y_test)
  results[i, 3] <- MAE(predicted, y_test)
  results[i, 4] <- cor(predicted, y_test, method = 'pearson')[1]
  results[i, 4] <- cor(predicted, y_test, method = 'kendall')[1]
  results[i, 6] <- RMSE(rep(median(y_test), length(y_test)), y_test)
  results[i, 7] <- MAE(rep(median(y_test), length(y_test)), y_test)
  
  name_set <- dimnames(coef.cv.glmnet(ridge, s = 'lambda.min'))[[1]]
  coef[setdiff(name_set, names(coef))] <- NA
  coef[i, match(name_set, names(coef))] <- 
    matrix(coef.cv.glmnet(ridge, s = 'lambda.min'))[, 1]
}

country_data <- read_csv('data/Country-level Analysis Passion and Science Mar 30 AL EDT.csv') %>%
  select(CNTRYID, ZlnCHINAGDP, HDI) %>%
  left_join(results, by = c('CNTRYID' = 'id')) %>%
  left_join(coef, by = c('CNTRYID' = 'id'))

write_csv(country_data, 'output/results_separate.csv')
