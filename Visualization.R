############################################################################
# Cleaning & visualization of previous results
############################################################################
library(tidyverse)
library(ggpubr)
library(latex2exp)

US_model <- read_csv('output/results_US.csv')
Global_model <- read_csv('output/results_separate.csv')
labels_table <- read_csv('data/labels.csv') %>%
  add_row(NAME = 'NA_prop', VARLABEL = 'Proportion of NA\'s') 

prop_NA <- function(x) {mean(is.na(x))}

# Treat countries with features missing systematically specially
indicator <- apply(Global_model, 1, prop_NA) >= 0.3
US_model$indicator <- indicator
Global_model$indicator <- indicator
############################################################################
# GDP 
tmp1 <- US_model %>% 
  filter(!is.na(ZlnCHINAGDP)) %>% 
  arrange(ZlnCHINAGDP)

tmp2 <- Global_model %>% 
  filter(!is.na(ZlnCHINAGDP)) %>% 
  arrange(ZlnCHINAGDP)

# 1. Quick plot of correlation between performance and GDP
ggplot(tmp1, aes(ZlnCHINAGDP, RMSE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), label.x.npc = 0.85, label.y.npc = 0.95) +
  labs(x = 'GDP')

ggplot(tmp1, aes(ZlnCHINAGDP, MAE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), label.x.npc = 0.85, label.y.npc = 0.95) +
  labs(x = 'GDP')

ggplot(tmp1, aes(ZlnCHINAGDP, corr)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'GDP')

tmp1 %>% 
  transmute(ZlnCHINAGDP, r2 = corr^2) %>% 
  ggplot(aes(ZlnCHINAGDP, r2)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'GDP', y = TeX('$R^2$'))

ggplot(tmp2, aes(ZlnCHINAGDP, RMSE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'GDP')

ggplot(tmp2, aes(ZlnCHINAGDP, MAE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'GDP')

ggplot(tmp2, aes(ZlnCHINAGDP, corr)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'GDP')

tmp2 %>% 
  transmute(ZlnCHINAGDP, r2 = corr^2) %>% 
  ggplot(aes(ZlnCHINAGDP, r2)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'GDP', y = TeX('$R^2$'))

# 2. Distribution of correlations
tmp2 <- tmp2[sapply(tmp2, function(x) {sum(!is.na(x)) >= 30})]

correlations <- as_tibble(
  cor(tmp2[-(1:7)], tmp2$ZlnCHINAGDP, use = 'pairwise.complete.obs'),
  rownames = 'variable') 

correlations$label <- NA
for (i in 1:nrow(labels_table)) {
  correlations$label[startsWith(correlations$variable, labels_table$NAME[i])] <- 
    labels_table$VARLABEL[i]
}
correlations <- correlations[c(1, 3, 2)]

ggplot(correlations) +
  geom_histogram(aes(x = V1), binwidth = 0.1) +
  labs(x = 'correlation')

filter(correlations, (abs(V1) >= 0.45) & (abs(V1) < 1)) %>% 
  print(n = Inf)

filtered <- rbind(filter(correlations, abs(V1) >= 0.45),
                  filter(correlations, (abs(V1) >= 0.35) & (abs(V1) < 0.45)),
                  filter(correlations, (abs(V1) >= 0.25) & (abs(V1) < 0.35)))

stats <- as_tibble(cbind(colMeans(tmp2[-(1:7)], na.rm = T), 
                   sapply(tmp2[-(1:7)], function(x) {var(x, na.rm = T)}),
                   sapply(tmp2[-(1:7)], function(x) {max(x, na.rm = T)}),
                   sapply(tmp2[-(1:7)], function(x) {min(x, na.rm = T)})),
                   rownames = 'variable')

filtered <- left_join(filtered, stats, by = 'variable')

names(filtered) <- c('variable', 'label', 'Correlation with GDP',
                     'Global mean', 'Global variance', 'Global max', 'Global min')

write_csv(filtered, 'output/correlations.csv')

# 3. Plotting most important features
US_coef <- tmp2[51, -(1:7)]
ordered_names <- names(US_coef)[order(-abs(as.matrix(US_coef)))]
tmp2 <- tmp2 %>% 
  select(CNTRYID:`(Intercept)`, ordered_names)

tmp3 <- tmp2 %>% 
  select(CNTRYID:`(Intercept)`, matches('\\d$'), -BMMJ1, -BFMJ2)

n_tmp3 <- names(tmp3)[-(1:7)]
tmp4 <- tmp2 %>% select(-n_tmp3)

tmp3 %>% 
  select(ZlnCHINAGDP, 8:16) %>% 
  gather('variable', 'value', -ZlnCHINAGDP) %>% 
  left_join(correlations, by = 'variable') %>%
  ggplot(aes(ZlnCHINAGDP, value, na.rm = T)) +
  geom_point(alpha = 0.3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5, 
              linetype = 1, color = 'darkblue') +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'GDP', y = 'coef') +
  facet_wrap(~ variable, nrow = 3, scales = 'free')

tmp4 %>% 
  select(ZlnCHINAGDP, 8:16) %>% 
  gather('variable', 'value', -ZlnCHINAGDP) %>% 
  left_join(correlations, by = 'variable') %>%
  ggplot(aes(ZlnCHINAGDP, value, na.rm = T)) +
  geom_point(alpha = 0.3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5, 
              linetype = 1, color = 'darkblue') +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'GDP', y = 'coef') +
  facet_wrap(~ variable, nrow = 3, scales = 'free')

# 4. Plotting in the same graph
tmp5 <- left_join(tmp1, tmp2, by = 'CNTRYID')

tmp5 %>% 
  select(ZlnCHINAGDP.x, starts_with('RMSE')) %>% 
  gather('key', 'value', starts_with('RMSE')) %>%
  ggplot(aes(ZlnCHINAGDP.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  annotate(geom = 'text', x = -2, y = 86, label = 'p = 0.0035') +
  labs(x = 'GDP', y = 'RMSE') +
  scale_colour_discrete(name = '',
                        breaks = c('RMSE.x', 'RMSE.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('RMSE.x', 'RMSE.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country')) 

tmp5 %>% 
  select(ZlnCHINAGDP.x, starts_with('MAE')) %>% 
  gather('key', 'value', starts_with('MAE')) %>%
  ggplot(aes(ZlnCHINAGDP.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  annotate(geom = 'text', x = -2, y = 69.5, label = 'p = 0.0035') +
  labs(x = 'GDP', y = 'MAE') +
  scale_colour_discrete(name = '',
                        breaks = c('MAE.x', 'MAE.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('MAE.x', 'MAE.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country')) 

tmp5 %>% 
  select(ZlnCHINAGDP.x, starts_with('corr')) %>% 
  gather('key', 'value', starts_with('corr')) %>%
  ggplot(aes(ZlnCHINAGDP.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  annotate(geom = 'text', x = -2, y = 0.79, label = 'p = 0.2420') +
  labs(x = 'GDP', y = 'corr') +
  scale_colour_discrete(name = '',
                        breaks = c('corr.x', 'corr.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('corr.x', 'corr.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country'))

tmp5 %>% 
  select(ZlnCHINAGDP.x, starts_with('corr')) %>% 
  transmute(ZlnCHINAGDP.x, r2.x = corr.x^2, r2.y = corr.y^2) %>% 
  gather('key', 'value', starts_with('r2')) %>%
  ggplot(aes(ZlnCHINAGDP.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  annotate(geom = 'text', x = -2, y = 0.64, label = 'p = 0.2150') +
  labs(x = 'GDP', y = TeX('$R^2$')) +
  scale_colour_discrete(name = '',
                        breaks = c('r2.x', 'r2.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('r2.x', 'r2.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country'))
############################################################################



############################################################################
# HDI
tmp1 <- US_model %>% 
  filter(!is.na(HDI)) %>% 
  filter(indicator == 0) %>%
  arrange(HDI) %>% 
  mutate(RMSE = RMSE/median_RMSE, MAE = MAE/median_MAE)
tmp1$indicator <- NULL

tmp2 <- Global_model %>% 
  filter(!is.na(HDI)) %>% 
  filter(indicator == 0) %>%
  arrange(HDI) %>% 
  mutate(RMSE = RMSE/median_RMSE, MAE = MAE/median_MAE)
tmp2$indicator <- NULL

# 1. Quick plot of correlation between performance and GDP
ggplot(tmp1, aes(HDI, RMSE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), label.x.npc = 0.85, label.y.npc = 0.95) +
  labs(x = 'HDI', y = 'RMSE/sd')

ggplot(tmp1, aes(HDI, MAE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), label.x.npc = 0.85, label.y.npc = 0.95) +
  labs(x = 'HDI', y = 'MAE/mad')

ggplot(tmp1, aes(HDI, corr)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI')

ggplot(tmp1, aes(HDI, kendall)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI', y = TeX('Kendall\'s $\\tau$'))

tmp1 %>% 
  transmute(HDI, r2 = corr^2) %>% 
  ggplot(aes(HDI, r2)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI', y = TeX('$R^2$'))

ggplot(tmp2, aes(HDI, RMSE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI')

ggplot(tmp2, aes(HDI, MAE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI')

ggplot(tmp2, aes(HDI, corr)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI')

tmp2 %>% 
  transmute(HDI, r2 = corr^2) %>% 
  ggplot(aes(HDI, r2)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI', y = TeX('$R^2$'))

# 2. Distribution of correlations
tmp2 <- tmp2[sapply(tmp2, function(x) {sum(!is.na(x)) >= 30})]

correlations <- as_tibble(
  cor(tmp2[-(1:9)], tmp2$HDI, use = 'pairwise.complete.obs'),
  rownames = 'variable') 

correlations$label <- NA
for (i in 1:nrow(labels_table)) {
  correlations$label[startsWith(correlations$variable, labels_table$NAME[i])] <- 
    labels_table$VARLABEL[i]
}
correlations <- correlations[c(1, 3, 2)]

ggplot(correlations) +
  geom_histogram(aes(x = V1), binwidth = 0.1) +
  labs(x = 'correlation')

filter(correlations, (abs(V1) >= 0.45) & (abs(V1) < 1)) %>% 
  print(n = Inf)

filtered <- rbind(filter(correlations, abs(V1) >= 0.45),
                  filter(correlations, (abs(V1) >= 0.35) & (abs(V1) < 0.45)),
                  filter(correlations, (abs(V1) >= 0.25) & (abs(V1) < 0.35)))

stats <- as_tibble(cbind(colMeans(tmp2[-(1:9)], na.rm = T), 
                         sapply(tmp2[-(1:9)], function(x) {var(x, na.rm = T)}),
                         sapply(tmp2[-(1:9)], function(x) {max(x, na.rm = T)}),
                         sapply(tmp2[-(1:9)], function(x) {min(x, na.rm = T)})),
                   rownames = 'variable')

filtered <- left_join(filtered, stats, by = 'variable')

names(filtered) <- c('variable', 'label', 'Correlation with HDI',
                     'Global mean', 'Global variance', 'Global max', 'Global min')

write_csv(filtered, 'output/correlations_HDI.csv')

# 3. Plotting most important features
US_coef <- tmp2[45, -(1:9)]
ordered_names <- names(US_coef)[order(-abs(as.matrix(US_coef)))]
tmp2 <- tmp2 %>% 
  select(CNTRYID:`(Intercept)`, ordered_names)

tmp3 <- tmp2 %>% 
  select(CNTRYID:`(Intercept)`, matches('\\d$'), -BMMJ1, -BFMJ2)

n_tmp3 <- names(tmp3)[-(1:9)]
tmp4 <- tmp2 %>% select(-n_tmp3)

tmp3 %>% 
  select(HDI, sort(filtered$variable)[c(70:73, 111:115)]) %>% 
  gather('variable', 'value', -HDI) %>% 
  left_join(correlations, by = 'variable') %>%
  mutate(label = str_wrap(label, width = 30)) %>% 
  unite('variable', variable, label, sep = '\n') %>% 
  ggplot(aes(HDI, value, na.rm = T)) +
  geom_point(alpha = 0.3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5, 
              linetype = 1, color = 'darkblue') +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI', y = 'coef') +
  facet_wrap(~ variable, nrow = 3, scales = 'free') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))

tmp4 %>% 
  select(HDI, sort(filtered$variable)[c(127:128)]) %>% 
  gather('variable', 'value', -HDI) %>% 
  left_join(correlations, by = 'variable') %>%
  mutate(label = str_wrap(label, width = 30)) %>% 
  unite('variable', variable, label, sep = '\n') %>% 
  ggplot(aes(HDI, value, na.rm = T)) +
  geom_point(alpha = 0.3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5, 
              linetype = 1, color = 'darkblue') +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI', y = 'coef') +
  facet_wrap(~ variable, nrow = 3, scales = 'free') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))

tmp2 %>% 
  select(HDI, sort(filtered$variable)) %>% 
  select(-contains('.')) %>% 
  select(HDI, 74:80) %>% 
  gather('variable', 'value', -HDI) %>% 
  left_join(correlations, by = 'variable') %>%
  mutate(label = str_wrap(label, width = 30)) %>% 
  unite('variable', variable, label, sep = '\n') %>% 
  ggplot(aes(HDI, value, na.rm = T)) +
  geom_point(alpha = 0.3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, size = 0.5, 
              linetype = 1, color = 'darkblue') +
  stat_cor(aes(label = ..r.label..)) +
  labs(x = 'HDI', y = 'coef') +
  facet_wrap(~ variable, nrow = 3, ncol = 3, scales = 'free') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))

# 4. Plotting in the same graph
tmp5 <- left_join(tmp1, tmp2, by = 'CNTRYID')

tmp5 %>% 
  select(HDI.x, starts_with('RMSE')) %>% 
  gather('key', 'value', starts_with('RMSE')) %>%
  ggplot(aes(HDI.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  annotate(geom = 'text', x = 0.743, y = 1.43, label = 'p = 0.0071') +
  labs(x = 'HDI', y = '', title = 'Prediction RMSE/RMSE of Naive Prediction (All Median)') +
  scale_colour_discrete(name = '',
                        breaks = c('RMSE.x', 'RMSE.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('RMSE.x', 'RMSE.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country')) 

tmp5 %>% 
  select(HDI.x, starts_with('MAE')) %>% 
  gather('key', 'value', starts_with('MAE')) %>%
  ggplot(aes(HDI.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  annotate(geom = 'text', x = 0.743, y = 1.58, label = 'p = 0.0114') +
  labs(x = 'HDI', y = '', title = 'Prediction MAE/MAE of Naive Prediction (All Median)') +
  scale_colour_discrete(name = '',
                        breaks = c('MAE.x', 'MAE.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('MAE.x', 'MAE.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country')) 

tmp5 %>% 
  select(HDI.x, starts_with('corr')) %>% 
  gather('key', 'value', starts_with('corr')) %>%
  ggplot(aes(HDI.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  annotate(geom = 'text', x = 0.743, y = 0.798, label = 'p = 0.0574') +
  labs(x = 'HDI', y = '', title = 'Correlation Between Prediction and True Value') +
  scale_colour_discrete(name = '',
                        breaks = c('corr.x', 'corr.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('corr.x', 'corr.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country'))  

tmp5 %>% 
  select(HDI.x, starts_with('corr')) %>% 
  transmute(HDI.x, r2.x = corr.x^2, r2.y = corr.y^2) %>% 
  gather('key', 'value', starts_with('r2')) %>%
  ggplot(aes(HDI.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  # annotate(geom = 'text', x = 0.7, y = 0.63, label = 'p = 0.1211') +
  labs(x = 'HDI', y = TeX('$R^2$')) +
  scale_colour_discrete(name = '',
                        breaks = c('r2.x', 'r2.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('r2.x', 'r2.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country'))

tmp5 %>% 
  select(HDI.x, starts_with('kendall')) %>% 
  gather('key', 'value', starts_with('kendall')) %>%
  ggplot(aes(HDI.x, value, group = key, shape = key, color = key)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), show.legend = F) +
  # annotate(geom = 'text', x = 0.7, y = 0.78, label = 'p = 0.1471') +
  labs(x = 'HDI', y = TeX('Kendall\'s $tau$')) +
  scale_colour_discrete(name = '',
                        breaks = c('kendall.x', 'kendall.y'),
                        labels = c('Trained in the U.S.,\n tested around the world',
                                   'Trained in each country,\n tested in each country')) +
  scale_shape_discrete(name = '',
                       breaks = c('kendall.x', 'kendall.y'),
                       labels = c('Trained in the U.S.,\n tested around the world',
                                  'Trained in each country,\n tested in each country')) 