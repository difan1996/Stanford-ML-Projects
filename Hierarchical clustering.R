library(caret)
library(circlize)
library(dendextend)
library(tidyverse)

country_names <- read.csv('data/country_names.csv')
US_model <- read.csv('output/results_US.csv')
Global_model <- read.csv('output/results_separate.csv')

prop_NA <- function(x) {mean(is.na(x))}
indicator <- apply(Global_model, 1, prop_NA) >= 0.3
Global_model$indicator <- indicator

tmp <- country_names %>% 
  right_join(Global_model, by = 'CNTRYID') %>% 
  # filter(!is.na(HDI)) %>% 
  filter(indicator == 0)
rownames(tmp) <- tmp$CNT
tmp <- tmp[-(1:10)]; tmp$indicator <- NULL; tmp$ST001D01T.13 <- NULL

imputation <- preProcess(tmp, method = c('scale', 'medianImpute'))
tmp <- predict(imputation, tmp)
hcd <- as.dendrogram(hclust(dist(tmp), method = 'ward.D'))
hcd <- hcd %>% 
  color_branches(k = i) %>% 
  color_labels(k = i)
circlize_dendrogram(hcd, labels_track_height = 0.4) 

tmp$group <- as.factor(cutree(hcd, 5))
write.csv(tmp, 'output/clustering.csv')

# tmp <- US_model[1:6] %>% 
#   left_join(Global_model[-c(2, 3, 7:9)], by = 'CNTRYID') %>% 
#   left_join(country_names, by = 'CNTRYID') %>% 
#   filter(indicator == 0)
# rownames(tmp) <- tmp$CNT
# tmp$CNT <- NULL; tmp$indicator <- NULL; tmp$ST001D01T.13 <- NULL
# tmp <- predict(imputation, tmp)
# tmp$CNTRYID <- as.factor(cutree(hcd, 5))
# tmp <- tmp %>% rename(group = CNTRYID)
# write.csv(tmp, 'output/clustering.csv')
