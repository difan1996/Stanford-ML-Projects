library(caret)
library(circlize)
library(dendextend)
library(tidyverse)

US_model <- read.csv('output/results_US.csv')
Global_model <- read.csv('output/results_separate.csv')

tmp <- Global_model[-(1:11)]
tmp <- tmp[colSums(!is.na(tmp)) >= 30]
rownames(tmp) <- Global_model$CNT

imputation <- preProcess(tmp, method = c('knnImpute'))
tmp <- predict(imputation, tmp)
hcd <- as.dendrogram(hclust(dist(tmp, method = 'euclidean'), method = 'ward.D'))
hcd <- hcd %>% 
  color_branches(k = 4) %>% 
  color_labels(k = 4)
circlize_dendrogram(hcd, labels_track_height = 0.4) 

Global_model$group <- as.factor(cutree(hcd, 4))
other_data <- read_csv('data/Country-level Analysis Passion and Science Mar 30 AL EDT.csv')
grouped <- US_model %>% 
  left_join(Global_model, by = c('CNTRYID', 'CNT')) %>% 
  left_join(other_data, by = 'CNTRYID') %>% 
  select(-CNTRYID, -CNT) %>% 
  mutate(RMSE_ratio.x = RMSE.x/median_RMSE.x, MAE_ratio.x = MAE.x/median_MAE.x,
         RMSE_ratio.y = RMSE.y/median_RMSE.y, MAE_ratio.y = MAE.y/median_MAE.y) %>% 
  group_by(group) %>% 
  summarise_all(funs(mean), na.rm = T) %>% 
  select(1:2, 612:615, 3:611)
write.csv(grouped, 'output/clustering.csv')

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
