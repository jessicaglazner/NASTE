### MCAP BW ###

# load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(sjPlot)

#load dataset

BW_Mcap <- read.csv("C:\\Users\\jglaz\\Desktop\\Mcap_BW.csv")


# Create two separate dataframes by temp treatment

BW_Mc_a <- BW_Mcap[BW_Mcap$Temperature == "AMB",]

BW_Mc_h <- BW_Mcap[BW_Mcap$Temperature == "HEAT",]

# Plot values

ggplot(BW_Mc_a, aes(x=Timepoint, y=total_weight, color=Treatment))+
  geom_point() +
  geom_smooth(se=F) +
  ggtitle("MCAP Ambient")+
  ylab("weight")+
  ylim(0,20)

ggplot(BW_Mc_h, aes(x=Timepoint, y=total_weight, color=Treatment))+
  geom_point() +
  geom_smooth(se=F)+
  ggtitle("MCAP Heated")+
  ylab("weight")+
  ylim(0,20)

### Boxplots ###

#first remove t0 from delta weight df

BW_box_a <- BW_Mc_a[BW_Mc_a$Timepoint %in% c("2", "3"),]
BW_box_h <- BW_Mc_h[BW_Mc_h$Timepoint %in% c("2", "3"),]

ggplot(BW_box_a, aes(x=Treatment, y=delta_weight, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~Timepoint)+
  stat_summary(fun = "mean", geom="point", shape=8, size=2, color="black")+
  ggtitle("MCAP Ambient")+
  ylab("Change in weight")+
  ylim(-2,3)

ggplot(BW_box_h, aes(x=Treatment, y=delta_weight, fill=Treatment)) +
  geom_boxplot() +
  facet_wrap(~Timepoint)+
  stat_summary(fun = "mean", geom="point", shape=8, size=2, color="black")+
  ggtitle("MCAP Heat")+
  ylab("Change in weight")+
  ylim(-2,3)

# create df for analysis

BW_0to2 <- BW_Mcap[BW_Mcap$Timepoint=="2",]
BW_2to3 <- BW_Mcap[BW_Mcap$Timepoint=="3",]


# ANOVA to assess if change in weight was significantly different

bw_0to2 <- aov(delta_weight ~Treatment + Temperature + Treatment:Temperature, data = BW_0to2)
summary(bw_0to2)

bw_2to3 <- aov(delta_weight ~Treatment + Temperature + Treatment:Temperature, data = BW_2to3)
summary(bw_2to3)

heat_0to2 <- aov(delta_weight ~Treatment, data = BW_0to2_h)
summary(heat_0to2)

heat_2to3 <- aov(delta_weight ~Treatment, data = BW_2to3_h)
summary(heat_2to3)


### Linear Mixed Effect Models ###

BWlme_0to2 <- lmer(delta_weight ~ Treatment + Temperature + (1|Colony), data=BW_0to2)
summary(BWlme_0to2)

BWlme_2to3 <- lmer(delta_weight ~ Treatment + Temperature + (1|Colony), data=BW_2to3)
summary(BWlme_2to3)

# Visualize Model 


plot_model(BWlme_0to2,
           show.values=TRUE, show.p=TRUE,
           title="Change in weight from T0 to T2")

plot_model(BWlme_2to3,
           show.values=TRUE, show.p=TRUE,
           title="Change in weight from T2 to T3")

# Create table of model output

tab_model(BWlme_0to2,
          show.re.var = TRUE,
          dv.labels="Effects of Nuts and Heat on Change in Weight from T0-T2")

tab_model(BWlme_2to3,
          show.re.var = TRUE,
          dv.labels="Effects of Nuts and Heat on Change in Weight from T2-T3")
