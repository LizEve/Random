rm(list=ls())
library(readxl)
library(RColorBrewer)

setwd("/Users/ChatNoir/Projects/Random/BioGrads/")

xl <- read_excel("/Users/chatnoir/Projects/Random/BioGrads/The\ impact\ of\ the\ pandemic\ on\ graduate\ student\ performance(1-63).xlsx")


library(ggplot2)
library(scales)
library(tidyr)
#library(plyr)
library(dplyr)

mytitle <- "My work has suffered due to…"
b <- xl[c(1,6:13)]
mytitle <- "My performance has suffered due to..."
b <- xl[c(1,14:21)]
b <- xl[c(1,22:29)]
mytitle <- "Work conditions"
b <- xl[c(1,30:40)]
mytitle <- "My work has improved due to..."
b <- xl[c(1,41:48)]

xls <- b %>% 
  pivot_longer(cols = -c(ID),
               names_to = "Question") %>% 
  mutate(value = sub('Not Applicable', 'Not applicable', value)) 
xls$value <- factor(xls$value, levels = c("Not applicable","Strongly disagree", "Disagree",  "Neutral","Agree",  "Strongly agree"))
#xls$value <- factor(xls$value, levels = c("Strongly agree", "Agree",   "Neutral","Disagree","Strongly disagree",    "Not applicable"))

xlsum <- xls %>% 
  group_by(Question, value) %>% 
  summarise(N = n()) %>%
  mutate(Pct = (N / sum(N)*100)) 
#brewer.pal(n = 6, name = "RdYlBu")
#display.brewer.pal(n = 6, name = "RdYlBu")

a <- xlsum

mytheme <- theme_bw() + theme(panel.border = element_blank()) +
  theme_classic()

cc <- c("#737373", "#C95A26", "#FC8D59",   "#b5bdc0","#91BFDB", "#4575B4")

quartz()
g <- ggplot(a,aes(Question, Pct, fill = value)) +
  geom_bar(stat="identity", position=position_dodge(preserve = 'single')) + mytheme +
  #geom_text(data=a, aes(x = Question, y = pos, label = paste0(round(Pct,1),"%")),size=2)+  + 
  #geom_text(data=a, aes(x=Question, y=Pct, label = paste(round(Pct,0),"%",sep='')),
            #position = position_dodge(width=1),size=2) +
  coord_flip() + 
  #theme(axis.text.y=element_blank()) + 
  ggtitle(mytitle)+
  xlab("")+ ylab("Percent") +
  scale_fill_manual(values=cc)

g
