############################################################################
# This script tests the US model in all countries/regions contained in the dataset
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
# Training was completed in "US model.R"
load('ridge_US.RData')

############################################################################
# Testing
# Prepare a Tibble to store the test metrics
l <- length(countries_selected)
results <- tibble(id = countries_selected, 
                  RMSE = numeric(l), MAE = numeric(l),
                  corr = numeric(l), kendall = numeric(l),
                  median_RMSE = numeric(l), median_MAE = numeric(l))

# Loop through all the countries/regions
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
  
  x <- x %>% add_row(); x[nrow(x),] <- -100  # Extra row to avoid errors in dummyVars
  x[names_categorical] <- 
    as.data.frame(lapply(x[names_categorical], function(x) factor(x)))
  
  x <- predict(knnImpute, x)
  
  dummy_test <- dummyVars(~ ., x)
  x <- as.data.frame(predict(dummy_test, x))
  x <- x[-nrow(x),]  # remove the extra row
  
  train_minus_test <- setdiff(names_xtrain, names(x))
  if (length(train_minus_test)) x[train_minus_test] <- 0
  x <- select(x, names_xtrain)
  
  predicted <- predict(ridge, as.matrix(x), s = 'lambda.min')
  
  results[i, 2] <- RMSE(predicted, y)
  results[i, 3] <- MAE(predicted, y)
  results[i, 4] <- cor(predicted, y, method = 'pearson')[1]
  results[i, 5] <- cor(predicted, y, method = 'kendall')[1]
  results[i, 6] <- RMSE(rep(median(y), length(y)), y)
  results[i, 7] <- MAE(rep(median(y), length(y)), y)
}

results[which(countries_selected == 840), -1] <- metrics

country_data <- read_csv('data/HDI.csv') %>%
  right_join(results, by = c('CNTRYID' = 'id'))

write_csv(country_data, 'output/results_US.csv')
