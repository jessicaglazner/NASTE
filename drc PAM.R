### DOSE RESPONSE CURVE PAM ###

# load dataset
PAM_Mcap <- read.csv("C:\\Users\\jglaz\\Desktop\\PAM_Montipora.csv")


### Load libraries ###
library(tidyverse)
library(ggplot2)
library(lmerTest)
library(emmeans)
library(sjPlot)
library(drc)
library(Rmisc)
library(dplyr)
library(tidyr)

#subset dataset so only heated corals remain
PAM_Mc_h <- PAM_Mcap[PAM_Mcap$temp == "h",]

# DRC for heated corals
Mc_DRC = drm(FvFm ~ week, data = Mc_dhw, curveid = treatment,
fct = LL.3(names = c('hill', 'max', 'ed50')))
summary(Mc_DRC)
compParm(Mc_DRC, 'ed50')
compParm(Mc_DRC, 'ed50', "-")
plot(Mc_DRC)
title('Montipora')
axis(side=1, at=27:36)
points(Mc_dhw$week, Mc_dhw$FvFm)
ED(Mc_DRC, c(50))[,1]


## using ggplot ##

#all weeks
ggplot(PAM_Mc_h, aes(x=week, y=FvFm, color=treatment)) +
  geom_point() +
  geom_smooth(se=F) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))+
  ggtitle("Montipora Bleached - All Weeks")

ggplot(PAM_Mcap, aes(x=week, y=FvFm, color=treatment, linetype=temp)) +
  geom_point() +
  geom_smooth(se=F) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))+
  ggtitle("Montipora All Corals and All Weeks")

# only heated weeks
Mc_dhw <- PAM_Mc_h[PAM_Mc_h$week %in% c("3","5", "6"),]

ggplot(Mc_dhw, aes(x=week, y=FvFm, color=treatment)) +
  geom_point() +
  geom_smooth(se=F) +
  ggtitle("Montipora Bleached - Heated Weeks Only")


#only heated weeks, but with both a and h corals
Mc_dhw_all <- PAM_Mcap[PAM_Mcap$week %in% c("3","5", "6"),]

ggplot(Mc_dhw_all, aes(x=week, y=FvFm, color=treatment, linetype=temp)) +
  geom_point() +
  geom_smooth(se=F) +
  ggtitle("Montipora All Corals - Heated Weeks Only")


# load relative FvFm dataset 
relative <- read.csv("C:\\Users\\jglaz\\Desktop\\Relative_PAM_Mc.csv")


#format dataframe
relative1<- rename(relative, c("X0"="0", "X1"="1", "X2"="2", "X3"="3", "X5"="5", "X6"="6", "X7"="7", "X8"="8", "X9"="9", "X10"="10")) 

relative_FvFm <- relative1 %>%
  pivot_longer(0:10, names_to = "week", values_to = "FvFm")
print(relative_FvFm)

# relative curve for all weeks
ggplot(relative, aes(x=week, y=FvFm, color=treatment, linetype=temp)) +
  geom_point() +
  geom_smooth(se=F) +
  ggtitle("Montipora Bleached - All Weeks")


# relative curve for weeks 3-6 
Mc_dhw_rel <- relative[relative$week %in% c("3","5", "6", "7"),]

ggplot(Mc_dhw_rel, aes(x=week, y=FvFm, color=treatment, linetype=temp)) +
  geom_point() +
  geom_smooth(se=F) +
  ggtitle("Montipora All Corals - Heated Weeks Only")
