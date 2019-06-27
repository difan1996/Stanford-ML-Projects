############################################################################
# This script trains a ridge model using 80% of the data from the US. Then 
# the model is tested on the remaining 20% of US data
############################################################################
library(caret)
library(doMC)
library(glmnet)
library(tidyverse)

registerDoMC(cores = 8)

# Reading US data
USA <- read_csv('data/USA.csv')
y <- USA$PV1SCIE
w <- USA$W_FSTUWT
# Variables are selected after inspecting codebook
x <- USA %>% 
  select(ST001D01T:ST065Class, AGE:ADINST,
         unfairteacher:ESCS, ST016Q01NA:ST038Q08NA) %>% 
  select(-ST038Q01NA, -ST038Q02NA, -ST124Q01TA, -LANGN, 
         -PROGN, -ISCEDD, -ISCEDO, -HOMESCH, -ENTUSE, 
         -starts_with('COBN'), -starts_with('OCOD'))

# Dealing with some variables
# Function to calculate proportion of NA's
prop_NA <-  function(x) {sum(is.na(x))/length(x)}
x$NA_prop <- apply(x, 1, prop_NA)

# UH form
x$UH <- as.numeric(USA$CBASCI == 5)

# Country-specific wealth item, 1-Yes 2-No
x$ST011D17TA[str_ends(x$ST011D17TA, '1')] <- 1
x$ST011D17TA[str_ends(x$ST011D17TA, '2')] <- 2
x$ST011D18TA[str_ends(x$ST011D18TA, '1')] <- 1
x$ST011D18TA[str_ends(x$ST011D18TA, '2')] <- 2
x$ST011D19TA[str_ends(x$ST011D19TA, '1')] <- 1
x$ST011D19TA[str_ends(x$ST011D19TA, '2')] <- 2

# First digit of occupation indicates category
x$occupation_m <- str_extract(USA$OCOD1, '^.{1}')
x$occupation_f <- str_extract(USA$OCOD2, '^.{1}')
x$occupation_s <- str_extract(USA$OCOD3, '^.{1}')

# Divide variables into categorical/numerical
names_categorical <- 
  names(x %>% 
          select(ST003D02T, ST003D03T, ST004D01T, 
                 starts_with('ST006'), starts_with('ST008'),
                 starts_with('ST011'), starts_with('ST019'), 
                 ST022Q01TA, starts_with('ST063'),
                 starts_with('ST076'), starts_with('ST078'), 
                 ISCEDL, IMMIG, REPEAT, UH, starts_with('occupation')))
names_numerical <- names(select(x,-names_categorical))
save(names_categorical, names_numerical, file = 'names.RData')

# Encode all NA's of categorical variables as -99, so that they will be 
# treated as a new level
x[names_categorical][is.na(x[names_categorical])] <- -99 

# Split out train set and test set
set.seed(111)
train_index <- createDataPartition(y, p = 0.8, list = F)
x_train <- x[train_index,]
x_test <- x[-train_index,]
y_train <- y[train_index]
y_test <- y[-train_index]
w_train <- w[train_index]
w_test <- w[-train_index]

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
saveRDS(ridge, 'ridge.rds')

# Do the same transformations on the test set
x_test[names_categorical] <- 
  as.data.frame(lapply(x_test[names_categorical], function(x) factor(x)))

x_test <- predict(knnImpute, x_test)
x_test <- as.data.frame(model.matrix(~., x_test))

train_minus_test <- setdiff(names(x_train), names(x_test))
if (length(train_minus_test) > 0) x_test[train_minus_test] <- 0
x_test <- select(x_test, names(x_train))

x_test <- predict(corr, x_test)

ridge <- readRDS('ridge.rds')
predicted <- predict(ridge, as.matrix(x_test), s = 'lambda.min')

(RMSE <- RMSE(predicted, y_test))
(MAE <- MAE(predicted, y_test))
(rho <- cor(predicted, y_test, method = 'pearson')[1])
