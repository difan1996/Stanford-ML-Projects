library(circlize)
library(dendextend)

country_names <- read.csv('data/country_names.csv')
Global_model <- read.csv('output/results_separate.csv')

indicator <- apply(Global_model, 1, prop_NA) >= 0.3
Global_model$indicator <- indicator

tmp <- country_names %>% 
  right_join(Global_model, by = 'CNTRYID') %>% 
  # filter(!is.na(HDI)) %>% 
  filter(indicator == 0)
rownames(tmp) <- tmp$CNT
tmp <- tmp[-(1:10)]; tmp$indicator <- NULL; tmp$ST001D01T.13 <- NULL

imputation <- preProcess(tmp, method = c('center', 'scale', 'medianImpute'))
tmp <- predict(imputation, tmp)
hcd <- as.dendrogram(hclust(dist(tmp), method = 'ward.D'))
hcd <- hcd %>% 
  color_branches(k = 5) %>% 
  color_labels(k = 5)

circlize_dendrogram(hcd, facing = 'outside', labels_track_height = 0.4) 
