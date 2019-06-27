############################################################################
# This script trains a ridge model using data from the US. Then the model is 
# tested in all countries/regions contained in the dataset
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

############################################################################
# Training
# Getting train set from US data
USA <- read_csv('data/USA.csv')
y_train <- USA$PV1SCIE
w_train <- USA$W_FSTUWT
# Variables are selected after inspecting codebook
x_train <- USA %>% 
  select(ST001D01T:ST065Class, AGE:ADINST,
         unfairteacher:ESCS, ST016Q01NA:ST038Q08NA) %>% 
  select(-ST038Q01NA, -ST038Q02NA, -ST124Q01TA, -LANGN, 
         -PROGN, -ISCEDD, -ISCEDO, -HOMESCH, -ENTUSE, 
         -starts_with('COBN'), -starts_with('OCOD'))

# Dealing with some variables
# Function to calculate proportion of NA's
prop_NA <-  function(x) {sum(is.na(x))/length(x)}
x_train$NA_prop <- apply(x_train, 1, prop_NA)

# UH form
x_train$UH <- as.numeric(USA$CBASCI == 5)

# Country-specific wealth item, 1-Yes 2-No
x_train$ST011D17TA[str_ends(x_train$ST011D17TA, '1')] <- 1
x_train$ST011D17TA[str_ends(x_train$ST011D17TA, '2')] <- 2
x_train$ST011D18TA[str_ends(x_train$ST011D18TA, '1')] <- 1
x_train$ST011D18TA[str_ends(x_train$ST011D18TA, '2')] <- 2
x_train$ST011D19TA[str_ends(x_train$ST011D19TA, '1')] <- 1
x_train$ST011D19TA[str_ends(x_train$ST011D19TA, '2')] <- 2

# First digit of occupation indicates category
x_train$occupation_m <- str_extract(USA$OCOD1, '^.{1}')
x_train$occupation_f <- str_extract(USA$OCOD2, '^.{1}')
x_train$occupation_s <- str_extract(USA$OCOD3, '^.{1}')

# Divide variables into categorical/numerical
names_categorical <- 
  names(x_train %>% 
          select(ST003D02T, ST003D03T, ST004D01T, 
                 starts_with('ST006'), starts_with('ST008'),
                 starts_with('ST011'), starts_with('ST019'), 
                 ST022Q01TA, starts_with('ST063'),
                 starts_with('ST076'), starts_with('ST078'), 
                 ISCEDL, IMMIG, REPEAT, UH, starts_with('occupation')))
names_numerical <- names(select(x_train,-names_categorical))
save(names_categorical, names_numerical, file = 'names.RData')

# Encode all NA's of categorical variables as -99, so that they will be 
# treated as a new level
x_train[names_categorical][is.na(x_train[names_categorical])] <- -99 

# Convert categorical variables to factors
x_train[names_categorical] <- 
  as.data.frame(lapply(x_train[names_categorical], function(x) factor(x)))

# Impute missing values with using k-nearest neighbor
knnImpute <- preProcess(x_train, method = 'knnImpute')
x_train <- predict(knnImpute, x_train)

x_train <- as.data.frame(model.matrix(~., x_train))

# Remove highly correlated variables
corr <- preProcess(x_train, method = c('zv', 'corr'), cutoff = 0.90)
x_train <- predict(corr, x_train)

# Training
set.seed(111)  # ensure results can be replicated
# alpha is kept at 0 to obtain a ridge model
ridge <- cv.glmnet(as.matrix(x_train), y_train,
                   weights = w_train, alpha = 0, nfolds = 5)
saveRDS(ridge, 'ridge_US.rds')
############################################################################



############################################################################
# Testing
# Prepare a Tibble to store the test metrics
list_of_id <- unique(Full_data$CNTRYID)
l <- length(list_of_id)
results <- tibble(id = list_of_id, RMSE = numeric(l), 
                  MAE = numeric(l), corr = numeric(l))

# Loop through all the countries/regions
for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i])
  NEW[NEW == ''] <-  NA  # Empty values will not be imputed, resulting in problems
  
  # Exactly same steps as train set
  y_test <- NEW$PV1SCIE
  w_test <- NEW$W_FSTUWT
  x_test <- NEW %>% 
    select(ST001D01T:ST065Class, AGE:ADINST,
           unfairteacher:ESCS, ST016Q01NA:ST038Q08NA) %>% 
    select(-ST038Q01NA, -ST038Q02NA, -ST124Q01TA, -LANGN, 
           -PROGN, -ISCEDD, -ISCEDO, -HOMESCH, -ENTUSE, 
           -starts_with('COBN'), -starts_with('OCOD'))
  
  x_test$NA_prop <- apply(x_test, 1, prop_NA)
  
  x_test$UH <- as.numeric(NEW$CBASCI == 5)
  x_test$UH[is.na(x_test$UH)] <- 0
  
  x_test$ST011D17TA[str_ends(x_test$ST011D17TA, '1')] <- 1
  x_test$ST011D17TA[str_ends(x_test$ST011D17TA, '2')] <- 2
  x_test$ST011D18TA[str_ends(x_test$ST011D18TA, '1')] <- 1
  x_test$ST011D18TA[str_ends(x_test$ST011D18TA, '2')] <- 2
  x_test$ST011D19TA[str_ends(x_test$ST011D19TA, '1')] <- 1
  x_test$ST011D19TA[str_ends(x_test$ST011D19TA, '2')] <- 2
  
  x_test$occupation_m <- str_extract(NEW$OCOD1, '^.{1}')
  x_test$occupation_f <- str_extract(NEW$OCOD2, '^.{1}')
  x_test$occupation_s <- str_extract(NEW$OCOD3, '^.{1}')
  
  x_test[names_categorical][is.na(x_test[names_categorical])] <- -99
  # Adding a row with a new value so that model.matrix receives at least two
  # distinct levels
  x_test <- add_row(x_test)
  x_test[nrow(x_test),] <- -100
  x_test[names_categorical] <- 
    as.data.frame(lapply(x_test[names_categorical], function(x) factor(x)))
  
  x_test <- predict(knnImpute, x_test)
  x_test <- as.data.frame(model.matrix(~., x_test))
  x_test <- x_test[-nrow(x_test),]  # Remove added row
  
  train_minus_test <- setdiff(names(x_train), names(x_test))
  if (length(train_minus_test) > 0) x_test[train_minus_test] <- 0
  x_test <- select(x_test, names(x_train))
  
  x_test <- predict(corr, x_test)
  
  ridge <- readRDS('ridge_US.rds')
  predicted <- predict(ridge, as.matrix(x_test), s = 'lambda.min')
  
  results[i, 2] <- RMSE(predicted, y_test)
  results[i, 3] <- MAE(predicted, y_test)
  results[i, 4] <- cor(predicted, y_test, method = 'pearson')[1]
}

country_data <- read_csv('data/Country-level Analysis Passion and Science Mar 30 AL EDT.csv') %>%
  select(CNTRYID, ZlnCHINAGDP, HDI) %>%
  left_join(results, by = c('CNTRYID' = 'id'))

write_csv(country_data, 'output/results_US.csv')
