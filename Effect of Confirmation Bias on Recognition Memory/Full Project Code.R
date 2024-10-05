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
surveydata <- read_xlsx("Survey Data2 edited.xlsx") %>%
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

ggplot(surveydata, aes(x = life_likescore, 
                       y = life_recall)) +   
  geom_jitter(shape = 1, width = .3, color =    
                'red3') + 
  geom_smooth(method = "lm", color = 
                "red2",  fill = "gold") +  
  theme_pubr() + 
  labs(x = "Pro Life Total Likescore", y = "Pro Life Recall Percentage") + 
  stat_cor(method = "pearson", label.x = 0,     
           label.y = 1)  

ggplot(surveydata, aes(x=choice_likescore, 
                       y=choice_recall)) +   
  geom_jitter(shape=1, width = .3, color =    
                'steelblue4') + 
  geom_smooth(method = "lm", color = 
                "steelblue",  fill = "gold") +  
  theme_pubr() + 
  labs(x = " Pro Choice Total Likescore", y = "Pro Choice Recall Percentage") + 
  stat_cor(method = "pearson", label.x = 0,     
           label.y = 1) 

options(digits = 4)
sumdatalife <- surveydata %>%
  group_by(gender, leaning)%>%
  summarize(meanlifer = mean(life_recall),
            n = n(),
            SDlifer = sd(life_recall),
            SElifer = SDlifer/sqrt(n),
            CIlifer = SElifer*1.96
            )

options(digits = 4)
sumdatachoice <- surveydata %>%
  group_by(gender, leaning)%>%
  summarize(meanchoicer = mean(choice_recall),
            n = n(),
            SDchoicer = sd(life_recall),
            SEchoicer = SDchoicer/sqrt(n),
            CIchoicer = SEchoicer*1.96
            )

ggplot(data = sumdatalife, aes(gender, meanlifer, fill = leaning)) + 
  geom_bar(stat = "identity", position = position_dodge(.92), alpha = .8,
           width = .85) +
  geom_jitter(data = surveydata, aes(gender, life_recall, color = leaning),
              position = position_jitterdodge(jitter.width = .14, 
                                              dodge.width = .92),
              show.legend = FALSE, shape = 1) +
  geom_errorbar(aes(ymin = meanlifer - CIlifer, ymax = meanlifer + CIlifer),
                position = position_dodge(.92), width = 0.15) +
  labs(title = "Mean Pro Life Article Recall as a Function of 
       Gender and Political Leaning", 
       x = "Gender", y = "Mean Pro Life Article Recall Percentage") +
  theme_pubr() +
  scale_fill_manual(values = c("steelblue3", "indianred3"),
                    name = "Political Leaning", 
                    labels = c("Left", "Right")) +
  scale_color_manual(values = c("steelblue4", "indianred4")) +
  coord_cartesian(ylim = c(0, .7)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1))

ggplot(data = sumdatachoice, aes(gender, meanchoicer, fill = leaning)) + 
  geom_bar(stat = "identity", position = position_dodge(.92), alpha = .8,
           width = .85) +
  geom_jitter(data = surveydata, aes(gender, choice_recall, color = leaning),
              position = position_jitterdodge(jitter.width = .14, 
                                              dodge.width = .92),
              show.legend = FALSE, shape = 1) +
  geom_errorbar(aes(ymin = meanchoicer - CIchoicer, 
                    ymax = meanchoicer + CIchoicer),
                position = position_dodge(.92), width = 0.15) +
  labs(title = "Mean Pro Choice Article Recall as a Function of 
       Gender and Political Leaning", 
       x = "Gender", y = "Mean Pro Choice Article Recall Percentage") +
  theme_pubr() +
  scale_fill_manual(values = c("steelblue3", "indianred3"),
                    name = "Political Leaning", 
                    labels = c("Left", "Right")) +
  scale_color_manual(values = c("steelblue4", "indianred4")) +
  coord_cartesian(ylim = c(0, .7)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1))

scipen = 999
ezANOVA(data = surveydata, wid = idnum, dv = life_recall, 
        between = gender + leaning, detailed = TRUE)
#results: Effect/IV, the numerator df, the denominator df, SS, 
#F value, p value, * if significant, effect size estimate (gen. eta^2 [ges])

ezANOVA(data = surveydata, wid = idnum, dv = choice_recall, 
        between = gender + leaning, detailed = TRUE)

options(digits = 5)
t.test(data = surveydata, life_recall ~ gender, paired = FALSE, var.equal = TRUE)
cohens_d(data = surveydata, life_recall ~ gender, paired = FALSE)

t.test(data = surveydata, life_recall ~ leaning, paired = FALSE, var.equal = TRUE)
cohens_d(data = surveydata, life_recall ~ leaning, paired = FALSE)

t.test(data = surveydata, choice_recall ~ gender, paired = FALSE, var.equal = TRUE)
cohens_d(data = surveydata, choice_recall ~ gender, paired = FALSE)

t.test(data = surveydata, choice_recall ~ leaning, paired = FALSE, var.equal = TRUE)
cohens_d(data = surveydata, choice_recall ~ leaning, paired = FALSE)

















