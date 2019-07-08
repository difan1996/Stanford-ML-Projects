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

prop_NA <- function(x) {mean(is.na(x))}
############################################################################
# Training was completed in "US model.R"
load('ridge_US.RData')

############################################################################
# Testing
# Prepare a Tibble to store the test metrics
list_of_id <- unique(Full_data$CNTRYID)
l <- length(list_of_id)
results <- tibble(id = list_of_id, RMSE = numeric(l), MAE = numeric(l),
                  corr = numeric(l), kendall = numeric(l),
                  median_RMSE = numeric(l), median_MAE = numeric(l))

# Loop through all the countries/regions
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
  
  x <- x %>% add_row(); x[nrow(x),] <- -100  # Extra row to avoid errors in dummyVars
  x[names_categorical] <- 
    as.data.frame(lapply(x[names_categorical], function(x) factor(x)))
  if (length(nzv)) x <- x[-nzv]
  
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

results[70, -1] <- metrics

country_data <- read_csv('data/Country-level Analysis Passion and Science Mar 30 AL EDT.csv') %>%
  select(CNTRYID, ZlnCHINAGDP, HDI) %>%
  left_join(results, by = c('CNTRYID' = 'id'))

write_csv(country_data, 'output/results_US.csv')
