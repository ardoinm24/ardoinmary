library(tidyverse)
library(table1)
library(ez)
library(yarrr)
library(rstatix)
library(ggpubr)
library(corrr)
library(apaTables)
library(ggplot2)
setwd("~/CBSC 250/Job Data Project ")
jobdata <- read_csv("job_data_final.csv") %>% 
  mutate(gender = ifelse(gender == 0, 'male', 'female')) %>% 
  mutate(race = ifelse(race == 0, 'white', 'black'))

table1(~ job | gender + race, data = jobdata)

correls <- correlate(jobdata)
network_plot(
  correls,
  colors = c("firebrick2", "#b2b9d2", "deepskyblue"),
  min_cor = 0
) 
apa.cor.table(jobdata, landscape = TRUE, filename = "correl table.doc")

multireg <- lm(job ~ rskill + rec + confid_post + confid_pre + extro, 
               data = jobdata)
apa.reg.table(multireg, filename = "job analysis reg table.doc")

longdata <- jobdata %>%
  select (- extro, - gpa) %>% 
  pivot_longer(cols = c(confid_pre, confid_post), names_to = 'time', 
               values_to = 'confidlevel') %>%
  arrange(id, time) 

sumdata <- jobdata %>%
  group_by(gender, race) %>%
  summarize(meanrskill = mean(rskill), 
            n=n(),
            meanrec = mean(rec), 
            meanconfidpre = mean(confid_pre), 
            meanconfidpost = mean(confid_post), 
            meanjob = mean(job), 
            SDjob = sd(job),
            SEjob = SDjob/sqrt(n),
            CIjob = SEjob*1.96) 

ggplot(data = sumdata, aes(race, meanjob, fill = gender)) +
  geom_bar(stat = "identity", position = position_dodge(.92), alpha = .5) +
  geom_jitter(data = jobdata, aes(race, job, color = gender),
              position=position_jitterdodge(jitter.width = .14, dodge.width = .92),
              show.legend = FALSE, shape = 1) +
  geom_errorbar(aes(ymin = meanjob-CIjob, ymax = meanjob+CIjob),
                position = position_dodge(.92), width = 0.1) +
                theme_pubr()


            











