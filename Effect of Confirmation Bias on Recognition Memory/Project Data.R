library(tidyverse)
library(table1)
library(apaTables)
library(ggpubr)
library(readxl)
library(rstatix)
library(ez)
library(corrr)
library(ggplot2)
setwd("~/CBSC 250/Project Data")
surveydata <- read_xlsx("Survey Data2.xlsx") %>%
  select("Participant ID", "Gender", "Political Party", #"Ideals", 
         "Life Like Score", "Choice Like Score", 
         "% Life Correct" , "% Choice Correct") %>%
  rename(idnum = "Participant ID",
         gender = "Gender", 
         leaning = "Political Party", 
         #abortionview = "Ideals", 
         life_likescore = "Life Like Score", 
         choice_likescore = "Choice Like Score", 
         life_recall = "% Life Correct" ,
         choice_recall = "% Choice Correct")

longdata <- surveydata %>%
  pivot_longer(cols = !(c(idnum, gender, leaning)),
               names_to = 'type',
               values_to = 'score')



ggplot(surveydata, aes(x=life_likescore, 
                     y=life_recall)) +   
  geom_jitter(shape=1, width = .3, color =    
                'red') + 
  geom_smooth(method = "lm", color = 
                "red",  fill = "gold") +  
  theme_pubr() + 
  labs(x = "ProLife Likescore", y = "ProLife Recall") + 
  stat_cor(method = "pearson", label.x = 0,     
           label.y = 1)  

ggplot(surveydata, aes(x=choice_likescore, 
                       y=choice_recall)) +   
  geom_jitter(shape=1, width = .3, color =    
                'steelblue') + 
  geom_smooth(method = "lm", color = 
                "steelblue",  fill = "gold") +  
  theme_pubr() + 
  labs(x = " ProChoice Likescore", y = "ProChoice Recall") + 
  stat_cor(method = "pearson", label.x = 0,     
           label.y = 1) 





















