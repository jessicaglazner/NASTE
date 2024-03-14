### PAM Linear Mixed Effects Model and 2-way ANOVA ####

# load libraries #
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sjPlot)


# load dataset

PAM_Mcap <- read.csv("C:\\Users\\jglaz\\Desktop\\PAM_Montipora.csv")


## explore dataset with boxplots ##

hist(PAM_Mcap$FvFm) #not normally distributed, left skewed


# Subset PAM df to have only 4 timepoints for boxplots #

PAM_box_a <- PAM_Mc_a[PAM_Mc_a$week %in% c("0", "3", "6", "10"),]

PAM_box_h <- PAM_Mc_h[PAM_Mc_h$week %in% c("0", "3", "6", "10"),]

# plot
ggplot(PAM_box_a, aes(x=treatment, y=FvFm, fill=treatment)) +
  geom_boxplot() +
  facet_wrap(~week)+
  stat_summary(fun = "mean", geom="point", shape=8, size=2, color="black")+
  ggtitle("MCAP Ambient")+
  ylim(0,0.6)

ggplot(PAM_box_h, aes(x=treatment, y=FvFm, fill=treatment)) +
  geom_boxplot() +
  facet_wrap(~week)+
  stat_summary(fun = "mean", geom="point", shape=8, size=2, color="black")+
  ggtitle("MCAP Heat")

# Switch boxplot facet 
PAM_box_a$week<-as.character(PAM_box_a$week) #make week a character
PAM_box_h$week<-as.character(PAM_box_h$week)

PAM_box_a_2 <- PAM_box_a
PAM_box_a_2$week <- factor(PAM_box_a_2$week, c("0","3","6","10"))

PAM_box_h_2 <- PAM_box_h
PAM_box_h_2$week <- factor(PAM_box_h_2$week, c("0","3","6","10"))

ggplot(PAM_box_a_2, aes(x=week, y=FvFm, fill=week)) +
  geom_boxplot() +
  facet_wrap(~treatment)+
  stat_summary(fun = "mean", geom="point", shape=8, size=2, color="black")+
  ggtitle("MCAP Ambient")+
  ylim(0,0.6)

ggplot(PAM_box_h_2, aes(x=week, y=FvFm, fill=week)) +
  geom_boxplot() +
  facet_wrap(~treatment)+
  stat_summary(fun = "mean", geom="point", shape=8, size=2, color="black")+
  ggtitle("MCAP Heat")

### Linear Mixed Effect Model ###

# create new dataframes for each timepoint
PAM_T0 <- PAM_Mcap[PAM_Mcap$week == "0",]
PAM_T1 <- PAM_Mcap[PAM_Mcap$week == "3",]
PAM_T2 <- PAM_Mcap[PAM_Mcap$week == "6",]
PAM_T3 <- PAM_Mcap[PAM_Mcap$week == "10",]

# run models - separate by timepoint

PAMlme <- lmer(FvFm ~ treatment + temp + (1|genotype), data=PAM_Mcap)
summary(PAMlme) #all weeks

PAMlme_T0 <- lmer(FvFm ~ treatment + temp + (1|genotype), data=PAM_T0)
summary(PAMlme_T0) #shows systematic differences, no treatment effects yet

PAMlme_T1 <- lmer(FvFm ~ treatment + temp + (1|genotype), data=PAM_T1)
summary(PAMlme_T1) #effect of nuts on PAM, indicates if there is a tank effect

PAMlme_T2 <- lmer(FvFm ~ treatment + temp + (1|genotype), data=PAM_T2)
summary(PAMlme_T2) #effect of nuts x heat on PAM

PAMlme_T3 <- lmer(FvFm ~ treatment + temp + (1|genotype), data=PAM_T3)
summary(PAMlme_T3) #effects of nutx x heat on PAM after recovery period


# plot model
plot_model(PAMlme_T0,
           show.values=TRUE, show.p=TRUE,
           title="PAM at T0")

plot_model(PAMlme_T1,
           show.values=TRUE, show.p=TRUE,
           title="PAM at T1")

plot_model(PAMlme_T2,
           show.values=TRUE, show.p=TRUE,
           title="PAM at T2")

plot_model(PAMlme_T3,
           show.values=TRUE, show.p=TRUE,
           title="PAM at T3")


# create table of model output
tab_model(PAMlme_T0,
          show.re.var = TRUE,
          dv.labels="Initial FvFm")

tab_model(PAMlme_T1,
          show.re.var = TRUE,
          dv.labels="Effects of Nuts on FvFm")


tab_model(PAMlme_T2,
          show.re.var = TRUE,
          dv.labels="Effects of Nuts and Temp on FvFm")


tab_model(PAMlme_T3,
          show.re.var = TRUE,
          dv.labels="Effects of Nuts and Temp on FvFm After Recovery")



# run 2-way ANOVA at each timepoint

anova_0 <- aov(FvFm ~treatment + temp + treatment:temp, data=PAM_T0)
summary(anova_0)

anova_1 <- aov(FvFm ~treatment + temp + treatment:temp, data=PAM_T1)
summary(anova_1)

anova_2 <- aov(FvFm ~treatment + temp + treatment:temp, data=PAM_T2)
summary(anova_2)

anova_3 <- aov(FvFm ~treatment + temp + treatment:temp, data=PAM_T3)
summary(anova_3)