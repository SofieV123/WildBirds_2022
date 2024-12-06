#Salmonella Prevalence Models
library(AICcmodavg)
library(dplyr)
library(ggeffects)
library(ggplot2)
library(glmmTMB)
library(MuMIn)
library(patchwork)
library(performance)
library(psych)


#Set Up ###################################################################
lc = read.table("./land_cover.txt", sep = "\t", header = T)

sal = read.table("./sal_live.txt", sep = "\t", header = T)

sal$Live = case_when(sal$Cow == 1 ~ 1,
                     sal$Chick == 1 ~ 1,
                     sal$Other == 1 ~ 1,
                     .default = 0)
sal = left_join(sal, lc, by = "Farm")

#Model prep ###################################################################
sal = sal[sal$Moist == 1,] #we only consider moist samples here

sal = sal %>%
  mutate(Nat.s = c(scale(Nat))) %>%
  mutate(Develop.s = c(scale(Develop))) %>%
  mutate(Ag.s = c(scale(Ag))) %>%
  mutate(Wet.s = c(scale(Wet))) %>%
  mutate(Water.s = c(scale(Water))) %>%
  mutate(Nat_ED.s = c(scale(Nat_ED))) %>%
  mutate(Nat_AREA_AM.s = c(scale(Nat_AREA_AM))) %>%
  mutate(Nat_ENN_AM.s = c(scale(Nat_ENN_AM))) %>%
  mutate(Nat_IJI.s = c(scale(Nat_IJI))) %>%
  mutate(CONTAG.s = c(scale(CONTAG))) %>%
  mutate(SHDI.s = c(scale(SHDI))) %>%
  mutate(SHEI.s = c(scale(SHEI)))

#Pearson's Correlation Between Nat Variables ##################
nat_cor_config = farms[,c(21,35:38,43, 45:46)]
nat_cor_compos = farms[c(21,44:46)]

cor_config = cor(nat_cor_config)
cor_compos = cor(nat_cor_compos)

pairs.panels(cor_config, stars = T)

#Salmonella Models ################################################
m0 = glmmTMB(Sal ~ (1|Farm/Visit),
             data = sal, family = binomial(link=logit))
m100 = glmmTMB(Sal ~ Water.s + Develop.s + Ag.s + Nat.s + Wet.s + Nat_IJI.s + 
                 Chick + Cow + Other + Live + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit),
               na.action = na.fail)

check_collinearity(m100)
#We need to evaluate land cover separately

#Model List
#Single Variables ####
ml.1 = glmmTMB(Sal ~ Water.s + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.2 = glmmTMB(Sal ~ Develop.s + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.3 = glmmTMB(Sal ~ Ag.s + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.4 = glmmTMB(Sal ~ Nat.s + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.5 = glmmTMB(Sal ~ Wet.s + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.6 = glmmTMB(Sal ~ Nat_IJI.s + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.7 = glmmTMB(Sal ~ Chick + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.8 = glmmTMB(Sal ~ Cow + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.9 = glmmTMB(Sal ~ Other + Year + (1|Farm/Visit),
               data = sal, family = binomial(link=logit))
ml.10 = glmmTMB(Sal ~ Live + Year + (1|Farm/Visit),
                data = sal, family = binomial(link=logit))

#Landscape + Chicken ####
mlch.1 = glmmTMB(Sal ~ Water.s + Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlch.2 = glmmTMB(Sal ~ Develop.s + Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlch.3 = glmmTMB(Sal ~ Ag.s + Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlch.4 = glmmTMB(Sal ~ Nat.s + Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlch.5 = glmmTMB(Sal ~ Wet.s + Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlch.6 = glmmTMB(Sal ~ Nat_IJI.s + Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))

mlch.7 = glmmTMB(Sal ~ Water.s * Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlch.8 = glmmTMB(Sal ~ Develop.s * Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlch.9 = glmmTMB(Sal ~ Ag.s * Chick + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlch.10 = glmmTMB(Sal ~ Nat.s * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlch.11 = glmmTMB(Sal ~ Wet.s * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlch.12 = glmmTMB(Sal ~ Nat_IJI.s * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

#Landscape + Cow ####
mlcw.1 = glmmTMB(Sal ~ Water.s + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcw.2 = glmmTMB(Sal ~ Develop.s + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcw.3 = glmmTMB(Sal ~ Ag.s + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcw.4 = glmmTMB(Sal ~ Nat.s + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcw.5 = glmmTMB(Sal ~ Wet.s + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcw.6 = glmmTMB(Sal ~ Nat_IJI.s + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))

mlcw.7 = glmmTMB(Sal ~ Water.s * Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcw.8 = glmmTMB(Sal ~ Develop.s * Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcw.9 = glmmTMB(Sal ~ Ag.s * Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcw.10 = glmmTMB(Sal ~ Nat.s * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcw.11 = glmmTMB(Sal ~ Wet.s * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcw.12 = glmmTMB(Sal ~ Nat_IJI.s * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

#Landscape + Other ####
mlot.1 = glmmTMB(Sal ~ Water.s + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlot.2 = glmmTMB(Sal ~ Develop.s + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlot.3 = glmmTMB(Sal ~ Ag.s + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlot.4 = glmmTMB(Sal ~ Nat.s + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlot.5 = glmmTMB(Sal ~ Wet.s + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlot.6 = glmmTMB(Sal ~ Nat_IJI.s + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))

mlot.7 = glmmTMB(Sal ~ Water.s * Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlot.8 = glmmTMB(Sal ~ Develop.s * Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlot.9 = glmmTMB(Sal ~ Ag.s * Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlot.10 = glmmTMB(Sal ~ Nat.s * Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlot.11 = glmmTMB(Sal ~ Wet.s * Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlot.12 = glmmTMB(Sal ~ Nat_IJI.s * Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

#Landscape + Live ####
mlal.1 = glmmTMB(Sal ~ Water.s + Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlal.2 = glmmTMB(Sal ~ Develop.s + Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlal.3 = glmmTMB(Sal ~ Ag.s + Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlal.4 = glmmTMB(Sal ~ Nat.s + Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlal.5 = glmmTMB(Sal ~ Wet.s + Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlal.6 = glmmTMB(Sal ~ Nat_IJI.s + Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))

mlal.7 = glmmTMB(Sal ~ Water.s * Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlal.8 = glmmTMB(Sal ~ Develop.s * Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlal.9 = glmmTMB(Sal ~ Ag.s * Live + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlal.10 = glmmTMB(Sal ~ Nat.s * Live + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlal.11 = glmmTMB(Sal ~ Wet.s * Live + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlal.12 = glmmTMB(Sal ~ Nat_IJI.s * Live + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

#Livestock ####
mliv.1 = glmmTMB(Sal ~ Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mliv.2 = glmmTMB(Sal ~ Chick * Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mliv.3 = glmmTMB(Sal ~ Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mliv.4 = glmmTMB(Sal ~ Chick * Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mliv.5 = glmmTMB(Sal ~ Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mliv.6 = glmmTMB(Sal ~ Cow * Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))

#Landscape + Chick + Cow ####
mlcc.1 = glmmTMB(Sal ~ Water.s + Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcc.2 = glmmTMB(Sal ~ Develop.s + Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcc.3 = glmmTMB(Sal ~ Ag.s + Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcc.4 = glmmTMB(Sal ~ Nat.s + Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcc.5 = glmmTMB(Sal ~ Wet.s + Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcc.6 = glmmTMB(Sal ~ Nat_IJI.s + Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))

mlcc.7 = glmmTMB(Sal ~ Water.s * Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcc.8 = glmmTMB(Sal ~ Develop.s * Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcc.9 = glmmTMB(Sal ~ Ag.s * Chick + Cow + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlcc.10 = glmmTMB(Sal ~ Nat.s * Chick + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.11 = glmmTMB(Sal ~ Wet.s * Chick + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.12 = glmmTMB(Sal ~ Nat_IJI.s * Chick + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

mlcc.13 = glmmTMB(Sal ~ Water.s * Cow + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.14 = glmmTMB(Sal ~ Develop.s * Cow + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.15 = glmmTMB(Sal ~ Ag.s* Cow + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.16 = glmmTMB(Sal ~ Nat.s * Cow + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.17 = glmmTMB(Sal ~ Wet.s * Cow + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.18 = glmmTMB(Sal ~ Nat_IJI.s * Cow + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

mlcc.19 = glmmTMB(Sal ~ Water.s + Cow * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.20 = glmmTMB(Sal ~ Develop.s + Cow * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.21 = glmmTMB(Sal ~ Ag.s + Cow * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.22 = glmmTMB(Sal ~ Nat.s + Cow * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.23 = glmmTMB(Sal ~ Wet.s + Cow * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlcc.24 = glmmTMB(Sal ~ Nat_IJI.s + Cow * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

#Landscape + Chick + Other ####
mlco.1 = glmmTMB(Sal ~ Water.s + Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlco.2 = glmmTMB(Sal ~ Develop.s + Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlco.3 = glmmTMB(Sal ~ Ag.s + Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlco.4 = glmmTMB(Sal ~ Nat.s + Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlco.5 = glmmTMB(Sal ~ Wet.s + Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlco.6 = glmmTMB(Sal ~ Nat_IJI.s + Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))

mlco.7 = glmmTMB(Sal ~ Water.s * Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlco.8 = glmmTMB(Sal ~ Develop.s * Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlco.9 = glmmTMB(Sal ~ Ag.s * Chick + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlco.10 = glmmTMB(Sal ~ Nat.s * Chick + Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.11 = glmmTMB(Sal ~ Wet.s * Chick + Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.12 = glmmTMB(Sal ~ Nat_IJI.s * Chick + Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

mlco.13 = glmmTMB(Sal ~ Water.s * Other + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.14 = glmmTMB(Sal ~ Develop.s * Other + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.15 = glmmTMB(Sal ~ Ag.s* Other + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.16 = glmmTMB(Sal ~ Nat.s * Other + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.17 = glmmTMB(Sal ~ Wet.s * Other + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.18 = glmmTMB(Sal ~ Nat_IJI.s * Other + Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

mlco.19 = glmmTMB(Sal ~ Water.s + Other * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.20 = glmmTMB(Sal ~ Develop.s + Other * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.21 = glmmTMB(Sal ~ Ag.s + Other * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.22 = glmmTMB(Sal ~ Nat.s + Other * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.23 = glmmTMB(Sal ~ Wet.s + Other * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlco.24 = glmmTMB(Sal ~ Nat_IJI.s + Other * Chick + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

#Landscape + Cow + Other ####
mlwo.1 = glmmTMB(Sal ~ Water.s + Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlwo.2 = glmmTMB(Sal ~ Develop.s + Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlwo.3 = glmmTMB(Sal ~ Ag.s + Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlwo.4 = glmmTMB(Sal ~ Nat.s + Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlwo.5 = glmmTMB(Sal ~ Wet.s + Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlwo.6 = glmmTMB(Sal ~ Nat_IJI.s + Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))

mlwo.7 = glmmTMB(Sal ~ Water.s * Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlwo.8 = glmmTMB(Sal ~ Develop.s * Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlwo.9 = glmmTMB(Sal ~ Ag.s * Cow + Other + Year + (1|Farm/Visit),
                 data = sal, family = binomial(link=logit))
mlwo.10 = glmmTMB(Sal ~ Nat.s * Cow + Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.11 = glmmTMB(Sal ~ Wet.s * Cow + Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.12 = glmmTMB(Sal ~ Nat_IJI.s * Cow + Other + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

mlwo.13 = glmmTMB(Sal ~ Water.s * Other + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.14 = glmmTMB(Sal ~ Develop.s * Other + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.15 = glmmTMB(Sal ~ Ag.s* Other + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.16 = glmmTMB(Sal ~ Nat.s * Other + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.17 = glmmTMB(Sal ~ Wet.s * Other + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.18 = glmmTMB(Sal ~ Nat_IJI.s * Other + Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

mlwo.19 = glmmTMB(Sal ~ Water.s + Other * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.20 = glmmTMB(Sal ~ Develop.s + Other * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.21 = glmmTMB(Sal ~ Ag.s + Other * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.22 = glmmTMB(Sal ~ Nat.s + Other * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.23 = glmmTMB(Sal ~ Wet.s + Other * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))
mlwo.24 = glmmTMB(Sal ~ Nat_IJI.s + Other * Cow + Year + (1|Farm/Visit),
                  data = sal, family = binomial(link=logit))

#Model Evaluation 
#Model List ####
mod_list = list(ml.1, ml.2, ml.3, ml.4, ml.5, ml.6, ml.7, ml.8, ml.9, ml.10,
                mlch.1, mlch.2, mlch.3, mlch.4, mlch.5, mlch.6,
                mlch.7, mlch.8, mlch.9, mlch.10, mlch.11, mlch.12,
                mlcw.1, mlcw.2, mlcw.3, mlcw.4, mlcw.5, mlcw.6,
                mlcw.7, mlcw.8, mlcw.9, mlcw.10, mlcw.11, mlcw.12,
                mlot.1, mlot.2, mlot.3, mlot.4, mlot.5, mlot.6,
                mlot.7, mlot.8, mlot.9, mlot.10, mlot.11, mlot.12,
                mlal.1, mlal.2, mlal.3, mlal.4, mlal.5, mlal.6,
                mlal.7, mlal.8, mlal.9, mlal.10, mlal.11, mlal.12,
                mliv.1, mliv.2, mliv.3, mliv.4, mliv.5, mliv.6,
                mlcc.1, mlcc.2, mlcc.3, mlcc.4, mlcc.5, mlcc.6, mlcc.7, mlcc.8,
                mlcc.9, mlcc.10, mlcc.11, mlcc.12, mlcc.13, mlcc.14, mlcc.15,
                mlcc.16, mlcc.17, mlcc.18, mlcc.19, mlcc.20, mlcc.21, mlcc.22,
                mlcc.23, mlcc.24, 
                mlco.1, mlco.2, mlco.3, mlco.4, mlco.5, mlco.6, mlco.7, mlco.8,
                mlco.9, mlco.10, mlco.11, mlco.12, mlco.13, mlco.14, mlco.15,
                mlco.16, mlco.17, mlco.18, mlco.19, mlco.20, mlco.21, mlco.22,
                mlco.23, mlco.24, 
                mlwo.1, mlwo.2, mlwo.3, mlwo.4, mlwo.5, mlwo.6, mlwo.7, mlwo.8,
                mlwo.9, mlwo.10, mlwo.11, mlwo.12, mlwo.13, mlwo.14, mlwo.15,
                mlwo.16, mlwo.17, mlwo.18, mlwo.19, mlwo.20, mlwo.21, mlwo.22,
                mlwo.23, mlwo.24, 
                m0, m100)
mod_nm = c("ml.1", "ml.2", "ml.3", "ml.4", "ml.5", "ml.6", "ml.7", "ml.8", "ml.9", "ml.10",
           "mlch.1", "mlch.2", "mlch.3", "mlch.4", "mlch.5", "mlch.6",
           "mlch.7", "mlch.8", "mlch.9", "mlch.10", "mlch.11", "mlch.12",
           "mlcw.1", "mlcw.2", "mlcw.3", "mlcw.4", "mlcw.5", "mlcw.6",
           "mlcw.7", "mlcw.8", "mlcw.9", "mlcw.10", "mlcw.11", "mlcw.12",
           "mlot.1", "mlot.2", "mlot.3", "mlot.4", "mlot.5", "mlot.6",
           "mlot.7", "mlot.8", "mlot.9", "mlot.10", "mlot.11", "mlot.12",
           "mlal.1", "mlal.2", "mlal.3", "mlal.4", "mlal.5", "mlal.6",
           "mlal.7", "mlal.8", "mlal.9", "mlal.10", "mlal.11", "mlal.12",
           "mliv.1", "mliv.2", "mliv.3", "mliv.4", "mliv.5", "mliv.6",
           "mlcc.1", "mlcc.2", "mlcc.3", "mlcc.4", "mlcc.5", "mlcc.6", "mlcc.7", "mlcc.8",
           "mlcc.9", "mlcc.10", "mlcc.11", "mlcc.12", "mlcc.13", "mlcc.14", "mlcc.15",
           "mlcc.16", "mlcc.17", "mlcc.18", "mlcc.19", "mlcc.20", "mlcc.21", "mlcc.22",
           "mlcc.23", "mlcc.24", 
           "mlco.1", "mlco.2", "mlco.3", "mlco.4", "mlco.5", "mlco.6", "mlco.7", "mlco.8",
           "mlco.9", "mlco.10", "mlco.11", "mlco.12", "mlco.13", "mlco.14", "mlco.15",
           "mlco.16", "mlco.17", "mlco.18", "mlco.19", "mlco.20", "mlco.21", "mlco.22",
           "mlco.23", "mlco.24", 
           "mlwo.1", "mlwo.2", "mlwo.3", "mlwo.4", "mlwo.5", "mlwo.6", "mlwo.7", "mlwo.8",
           "mlwo.9", "mlwo.10", "mlwo.11", "mlwo.12", "mlwo.13", "mlwo.14", "mlwo.15",
           "mlwo.16", "mlwo.17", "mlwo.18", "mlwo.19", "mlwo.20", "mlwo.21", "mlwo.22",
           "mlwo.23", "mlwo.24", 
           "m0", "m100")

#Evaluation ####
aic_mod = aictab(cand.set = mod_list, modnames = mod_nm, second.ord = T, sort = T)
summary(aic_mod)

mod_list = list(mlcw.11, mlcc.17, mlco.10, mlch.10, mlwo.11)
mod_avg = model.avg(mod_list)
coef_tab = coefTable(mod_avg, full = T)

pred_cw = ggpredict(mod_avg, terms = c("Wet.s[all]", "Cow"),
                    typical = "mean", type = "fixed", bias_correction = T)
pred_hn = ggpredict(mod_avg, terms = c("Nat.s[all]", "Chick"),
                    typical = "mean", type = "fixed", bias_correction = T)

#Figure 3 ###############################################################
#Format
pred_cw$Wet = (pred_cw$x * sd(sal$Wet) + mean(sal$Wet))
pred_hn$Nat = (pred_hn$x * sd(sal$Nat) + mean(sal$Nat))

#Plotting
plot_cw = ggplot() +
  geom_line(data = pred_cw, aes(x = Wet, y = predicted,
                                color = group, linetype = group),
            linewidth = 2) +
  geom_ribbon(data = pred_cw, aes (x = Wet, ymin = conf.low, ymax = conf.high,
                                   color = NULL, fill = group),
              alpha = 0.3) +
  labs(tag = "A",
       x = "Proportion Wetlands (%)", y = "Likelihood of Salmonella (%)") +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_color_manual(name = "Cows",
                     labels = c("Absent", "Present"),
                     values = c("#D55E00", "#56B4E9")) +
  scale_linetype_manual(name = "Cows",
                        labels = c("Absent", "Present"),
                        values = c("dashed", "solid")) +
  scale_fill_manual(name = "Cows",
                    labels = c("Absent", "Present"),
                    values = c("#D55E00", "#56B4E9")) +
  theme(panel.background = element_blank(), 
        legend.key = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


plot_hn = ggplot() +
  geom_line(data = pred_hn, aes(x = Nat, y = predicted,
                                color = group, linetype = group),
            linewidth = 2) +
  geom_ribbon(data = pred_hn, aes (x = Nat, ymin = conf.low, ymax = conf.high,
                                   color = NULL, fill = group),
              alpha = 0.3) +
  labs(tag = "B",
       x = "Proportion Natural (%)", y = "Likelihood of Salmonella (%)") +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "25", "50", "75", "100")) +
  scale_color_manual(name = "Chicken",
                     labels = c("Absent", "Present"),
                     values = c("#CC79A7", "#0072B2")) +
  scale_linetype_manual(name = "Chicken",
                        labels = c("Absent", "Present"),
                        values = c("dashed", "solid")) +
  scale_fill_manual(name = "Chicken",
                    labels = c("Absent", "Present"),
                    values = c("#CC79A7", "#0072B2")) +
  theme(panel.background = element_blank(), 
        legend.key = element_blank(),
        axis.line = element_line(color = "black", linewidth = 1),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

plot_cw / plot_hn





