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
weights <- read_csv('data/weights.csv')

############################################################################



############################################################################
tmp1 <- US_model %>% 
  arrange(HDI) %>% 
  mutate(RMSE = RMSE/median_RMSE, MAE = MAE/median_MAE)

tmp2 <- Global_model %>% 
  arrange(HDI) %>% 
  mutate(RMSE = RMSE/median_RMSE, MAE = MAE/median_MAE)

# 1. Quick plot of correlation between performance and GDP
ggplot(tmp1[-1,], aes(HDI, RMSE)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  stat_cor(aes(label = ..r.label..), label.x.npc = 0.85, label.y.npc = 0.95) +
  labs(x = 'HDI', y = 'RMSE/sd')

ggplot(tmp1[-1,], aes(HDI, MAE)) +
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
tmp2 <- tmp2[colSums(!is.na(tmp2)) >= 30]

correlations <- as_tibble(
  cor(tmp2[-(1:11)], tmp2$HDI, use = 'pairwise.complete.obs'),
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

stats <- as_tibble(cbind(colMeans(tmp2[-(1:10)], na.rm = T), 
                         sapply(tmp2[-(1:10)], function(x) {var(x, na.rm = T)}),
                         sapply(tmp2[-(1:10)], function(x) {max(x, na.rm = T)}),
                         sapply(tmp2[-(1:10)], function(x) {min(x, na.rm = T)})),
                   rownames = 'variable')

filtered <- left_join(filtered, stats, by = 'variable')

names(filtered) <- c('variable', 'label', 'Correlation with HDI',
                     'Global mean', 'Global variance', 'Global max', 'Global min')

write_csv(filtered, 'output/correlations_HDI.csv')

# 3. Plotting most important features
US_coef <- tmp2[which(tmp2$CNTRYID == 840), -(1:11)]
ordered_names <- names(US_coef)[order(-abs(as.matrix(US_coef)))]
tmp2 <- tmp2 %>% 
  select(CNTRYID:`(Intercept)`, ordered_names)

tmp3 <- tmp2 %>% 
  select(CNTRYID:`(Intercept)`, contains('.'))

n_tmp3 <- names(tmp3)[-(1:11)]
tmp4 <- tmp2 %>% select(-n_tmp3)

for (i in 1:15) {
  g <- tmp3 %>% 
    select(HDI, (12*i):(12*i + 11)) %>% 
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
    facet_wrap(~ variable, ncol = 3, scales = 'free') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
  ggsave(paste('categorical', i, '.pdf'), g, 'pdf', 'visualization', 
         width = 210, height = 297, units = 'mm')
}

for (i in 1:12) {
  g <- tmp4 %>% 
    select(HDI, (12*i):(12*i + 11)) %>% 
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
    facet_wrap(~ variable, ncol = 3, scales = 'free') +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 3))
  ggsave(paste('numerical', i, '.pdf'), g, 'pdf', 'visualization', 
         width = 210, height = 297, units = 'mm')
}

# 4. Plotting in the same graph
tmp5 <- left_join(tmp1, tmp2, by = 'CNTRYID') %>% 
  left_join(weights, by = 'CNTRYID')

data <- tmp5 %>% 
  select(HDI.x, weight, starts_with('RMSE')) %>% 
  gather('key', 'value', starts_with('RMSE')) 
g <- ggplot(data, aes(HDI.x, value, group = key, color = key)) +
  geom_point(aes(size = weight), alpha = 0.5, show.legend = F) +
  geom_smooth(data = subset(data, HDI.x != 0.715), method = 'lm', se = F) +
  # stat_cor(data = subset(data, HDI.x != 0.715), show.legend = F) +
  annotate('text', x = 0.77, y = 1.46, label = 'r = -0.64, p = 0.0001', color = 'grey', family = 'Helvetica', fontface = 'bold') +
  annotate('text', x = 0.77, y = 1.4, label = 'r = -0.24, p = 0.0921', color = 'darkred', family = 'Helvetica', fontface = 'bold') +
  annotate('text', x = 0.83, y = 1.34, label = 'difference between two correlation lines, p = 0.0111', family = 'Helvetica', fontface = 'bold', size = 3) +
  labs(x = 'HDI', y = '', title = 'Prediction RMSE vs RMSE of \n naive prediction (all median)') +
  scale_colour_manual(name = '',
                      breaks = c('RMSE.x', 'RMSE.y'),
                      values = c('darkgrey', 'darkred'),
                      labels = c('Trained in the U.S.,\n tested around the world',
                                 'Trained in each country,\n tested in each country')) +
  theme_bw() +
  theme(text = element_text(family = 'Helvetica', face = 'bold'),
        plot.title = element_text(hjust = 0.4),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
g

data <- tmp5 %>% 
  select(HDI.x, weight, starts_with('MAE')) %>% 
  gather('key', 'value', starts_with('MAE')) 
g <- ggplot(data, aes(HDI.x, value, group = key, color = key)) +
  geom_point(aes(size = weight), alpha = 0.5, show.legend = F) +
  geom_smooth(data = subset(data, HDI.x != 0.715), method = 'lm', se = F) +
  # stat_cor(data = subset(data, HDI.x != 0.715), show.legend = F) +
  annotate('text', x = 0.77, y = 1.59, label = 'r = -0.64, p = 0.0001', color = 'grey', family = 'Helvetica', fontface = 'bold') +
  annotate('text', x = 0.77, y = 1.51, label = 'r = -0.28, p = 0.0461', color = 'darkred', family = 'Helvetica', fontface = 'bold') +
  annotate('text', x = 0.83, y = 1.43, label = 'difference between two correlation lines, p = 0.0198', family = 'Helvetica', fontface = 'bold', size = 3) +
  labs(x = 'HDI', y = '', title = 'Prediction MAE vs MAE of \n naive prediction (all median)') +
  scale_colour_manual(name = '',
                      breaks = c('MAE.x', 'MAE.y'),
                      values = c('darkgrey', 'darkred'),
                      labels = c('Trained in the U.S.,\n tested around the world',
                                 'Trained in each country,\n tested in each country')) +
  theme_bw() +
  theme(text = element_text(family = 'Helvetica', face = 'bold'),
        plot.title = element_text(hjust = 0.4),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
g

data <- tmp5 %>% 
  select(HDI.x, weight, starts_with('corr')) %>% 
  gather('key', 'value', starts_with('corr')) 
g <- ggplot(data, aes(HDI.x, value, group = key, color = key)) +
  geom_point(aes(size = weight), alpha = 0.5, show.legend = F) +
  geom_smooth(method = 'lm', se = F) +
  # stat_cor(data = subset(data, HDI.x != 0.798), show.legend = F) +
  annotate('text', x = 0.77, y = 0.92, label = 'r = 0.54, p = 0.0001', color = 'grey', family = 'Helvetica', fontface = 'bold') +
  annotate('text', x = 0.77, y = 0.90, label = 'r = 0.24, p = 0.0837', color = 'darkred', family = 'Helvetica', fontface = 'bold') +
  annotate('text', x = 0.83, y = 0.88, label = 'difference between two correlation lines, p = 0.0719', family = 'Helvetica', fontface = 'bold', size = 3) +
  labs(x = 'HDI', y = '', title = 'Correlation between prediction and true value') +
  scale_colour_manual(name = '',
                      breaks = c('corr.x', 'corr.y'),
                      values = c('darkgrey', 'darkred'),
                      labels = c('Trained in the U.S.,\n tested around the world',
                                 'Trained in each country,\n tested in each country')) +
  coord_cartesian(ylim = c(0.55, 0.93)) +
  theme_bw() +
  theme(text = element_text(family = 'Helvetica', face = 'bold'),
        plot.title = element_text(hjust = 0.4),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
g

