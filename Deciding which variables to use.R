library(haven)
library(tidyverse)

Part1 <- read_sas('data/cy6_ms_cmb_stu_qqq.sas7bdat')
Part2 <- read_sas('data/cy6_ms_cmb_stu_qq2.sas7bdat')
Full_data <- left_join(Part1, Part2, by = 'CNTSTUID', suffix = c('', '.y'))

# 26:249, 933:941: ST questionnare responses
# 641:726: Derived variables from the ST questionnare
names <- names(Full_data)[c(26:249, 641:726, 933:941)]
list_of_id <- unique(Full_data$CNTRYID)
l <- length(list_of_id)

# matrix to store the appearance of variables, initialized with 1
variables <- matrix(1, nrow = l, ncol = length(names))
for (i in 1:l) {
  NEW <- Full_data %>%
    filter(CNTRYID == list_of_id[i]) %>% 
    select(names)
  NEW[NEW == ''] <-  NA
  
  nzv <- nearZeroVar(NEW)  # index of variables with near-zero variance
  variables[i, nzv] <- 0  # change corresponding matrix values to zero
}
variables <- as_tibble(variables)
names(variables) <- names
count <- apply(variables, 2, sum)

# Visualizing the relationship between number of countries and maximum
# number of common features
tmp <- function(x) {sum(count >= x)}
x <- 1:l
ggplot(as_tibble(cbind(x, y = sapply(x, tmp))), aes(x, y)) +
  geom_line() +
  geom_vline(xintercept = 57, linetype = 2) +
  annotate(geom = 'text', x = 63, y = 240, label = '(57, 235)') +
  labs(x = 'Number of countries', y = '', 
       title = 'Maximum number of common features') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.4),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

names_selected <- names(count[count >= 57])
names_selected <- names_selected[-191]  # remove PROGN, which is not consistently coded

# By direct inspection, decide which variables should be categorical
cat_index <- c(1:3, 5:6, 8:29, 35:36, 78:87, 170:189, 226:228)
(names_categorical <- names_selected[cat_index])
(names_numerical <- names_selected[-cat_index])

save(names_selected, names_categorical, names_numerical, file = 'names.RData')
