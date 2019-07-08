library(tidyverse)
library(ggpubr)

Global_model <- read_csv('output/results_separate.csv')
weights <- read_csv('data/weights.csv')
prop_NA <- function(x) {mean(is.na(x))}

tmp <- Global_model %>% 
  filter(!is.na(HDI)) %>% 
  arrange(HDI) 

tmp <- tmp[sapply(tmp, function(x) {sum(!is.na(x)) >= 30})]

tmp %>% 
  select(CNTRYID, HDI, JOYSCIE) %>%   #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, JOYSCIE, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'red') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkred') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.7, y = 3.5, label = 'R = 0.39', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Enjoyment of Science') +
  theme_bw() +
  theme(text = element_text(family = 'Helvetica', face = 'bold'),
        plot.title = element_text(hjust = 0.4),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3))
