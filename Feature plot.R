library(tidyverse)
library(ggpubr)

Global_model <- read_csv('output/results_separate.csv')
weights <- read_csv('data/weights.csv')

tmp <- Global_model %>% 
  arrange(HDI) 

tmp <- tmp[sapply(tmp, function(x) {sum(!is.na(x)) >= 30})]

tmp %>% 
  select(CNTRYID, HDI, ST013Q01TA) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, ST013Q01TA, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'red') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkred') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 13, label = 'R = 0.72', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Number of books at home') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f1.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, JOYSCIE) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, JOYSCIE, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'red') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkred') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 8, label = 'R = 0.50', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Enjoyment of science') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f2.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, BSMJ) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, BSMJ, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'red') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkred') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 7, label = 'R = 0.52', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Personal aspiration') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f3.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, ST118Q01NA) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, ST118Q01NA, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkblue') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = -8, label = 'R = -0.40', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Fear test') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f4.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, OCOD1.9) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, OCOD1.9, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkblue') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = -0.5, label = 'R = -0.27', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Mom holding a lower-level\n service job or jobless') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f5.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, OCOD2.9) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, OCOD2.9, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'blue') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkblue') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 0.5, label = 'R = -0.32', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Dad holding a lower-level\n service job or jobless') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f6.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, DISCLISCI) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, DISCLISCI, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'grey60') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'grey40') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 4, label = 'R = -0.043', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Classroom discipline') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f7.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, ST078Q03NA.2) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, ST078Q03NA.2, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'grey60') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'grey40') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 2.5, label = 'R = 0.064', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Watch TV after school') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f8.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, EPIST) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, EPIST, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'grey60') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'grey40') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 11, label = 'R = 0.071', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Scientific beliefs') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f9.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, ST004D01T.2) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, ST004D01T.2, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'orange') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkorange2') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 11, label = 'R = -0.28', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Gender (boy)') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f10.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, OCOD1.4) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, OCOD1.4, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'orange') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkorange2') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = 15, label = 'R = -0.32', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Mom holding a clerical support job') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f11.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')

tmp %>% 
  select(CNTRYID, HDI, ST078Q01NA.1) %>%  #
  left_join(weights, by = 'CNTRYID') %>% 
  ggplot(aes(HDI, ST078Q01NA.1, na.rm = T)) +  #
  geom_point(aes(size = weight), alpha = 0.3, color = 'orange') +
  geom_smooth(method = 'lm', se = FALSE, linetype = 1, color = 'darkorange2') +
  # stat_cor(aes(label = ..r.label..)) +
  annotate('text', x = 0.72, y = -6, label = 'R = -0.41', family = 'Helvetica', fontface = 'bold') +
  labs(x = 'Human Development Index', y = '', title = 'Eat dinner') +  #
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
  coord_cartesian(xlim = c(0.69, 0.96)) +
  scale_x_continuous(breaks = c(0.7, 0.8, 0.9))
ggsave('f12.pdf', device = 'pdf', path = 'visualization', width = 4, height = 4, units = 'in')
