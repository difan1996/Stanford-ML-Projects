library(haven)
library(tidyverse)

Part1 <- read_sas('data/cy6_ms_cmb_stu_qqq.sas7bdat')
Part2 <- read_sas('data/cy6_ms_cmb_stu_qq2.sas7bdat')
Full_data <- left_join(Part1, Part2, by = 'CNTSTUID', suffix = c('', '.y'))
Full_data[Full_data == ''] <- NA

# 26:249, 933:941: ST questionnare responses
# 641:726: Derived variables from the ST questionnare
names <- names(Full_data)[c(26:249, 641:726, 933:941)]
list_of_id <- unique(Full_data$CNTRYID)
l <- length(list_of_id)

# matrix to store the appearance of variables
variables <- matrix(nrow = l, ncol = length(names))
for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i]) %>% 
    select(names)
  
  NA_index <- colMeans(is.na(NEW)) != 1  # index of variables with all NA's
  variables[i, ] <- NA_index  # change corresponding matrix values
}
variables <- as_tibble(variables)
names(variables) <- names

HDI <- read_csv('data/HDI.csv')

tmp <- variables %>% 
  add_column(CNTRYID = list_of_id) %>% 
  left_join(HDI, by = 'CNTRYID') 
tmp$count <- rowSums(tmp[1:319])
tmp <- tmp %>% 
  filter(!is.na(HDI)) %>% 
  arrange(-count)

v <- c()
for (i in 1:nrow(tmp)) {
  v[i] <- sum(colSums(tmp[1:i, 1:319]) == i)
}

ggplot(as_tibble(cbind(x = 1:65, v)), aes(x, v)) +
  geom_line() +
  geom_vline(xintercept = 53, linetype = 2) +
  annotate(geom = 'text', x = 58, y = 212, label = '(53, 208)') +
  labs(x = 'Number of countries', y = 'Number of common features') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

labels <- read_csv('data/labels.csv')
names_selected <- tibble(name = names[colSums(tmp[1:53, 1:319]) == 53]) %>% 
  left_join(labels, by = c('name' = 'NAME'))
write_csv(names_selected, 'names_selected.csv')

countries_selected <- tmp$CNTRYID[1:53]
names_selected <- names_selected$name[-c(164, 197:200)]  # remove inconsistent variables

# By direct inspection, decide which variables should be categorical
cat_index <- c(1:2, 4:5, 7:23, 32:34, 36, 141:162, 164:166, 179, 196:198)
(names_categorical <- names_selected[cat_index])
(names_numerical <- names_selected[-cat_index])

save(countries_selected, names_selected, names_categorical, names_numerical, file = 'names.RData')
