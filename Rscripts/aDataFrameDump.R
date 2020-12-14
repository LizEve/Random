library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(tidyr)
library(dplyr)

### Make new data frame of # coms, # trees, order found, plateau len
# grab columns from data > rename all columns that start with 'x'  
plat1 <- select(data,Trees,oneNumCom,oneComOrder,oneComLen) %>% 
  rename_at(vars(contains('one')), funs(sub('one', '', .)))