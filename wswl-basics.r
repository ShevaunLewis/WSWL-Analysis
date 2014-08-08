##### Social Word Learning Analysis
##### Basic descriptive plots, etc. 
##### Created 8/7/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
#source("WSWL-Analysis/wswl-getdata.r")
load("wswl-data.Rda")

library(ggplot2)
wswl.smallplots = theme(text = element_text(family="serif", size=12),
                           axis.title = element_text(size=14),
                           plot.title=element_text(size=16),
                           legend.title = element_text(size=14, face="plain"),
                           panel.grid = element_blank())

#### Vocabulary by age ####
head(subjInfo)

ggplot(subjInfo, aes(x=ageV1Mos, shape=Group, y=WordsProduced)) +
  geom_point(stat="identity") + 
  theme_bw() + 
  labs(title="Vocabulary by age",y="Words Produced",x="Age (mos)",shape="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/VocabXAge_scatter.png", width=400, height=350)

ggplot(subjInfo, aes(x=ageGroup, y=WordsProduced)) +
  geom_boxplot() + geom_point() + theme_bw() + 
  labs(title="Vocabulary by age and group", y="Words Produced", x="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/VocabXGroup_boxplot.png", width=350, height=350)

#### Mullens by age ####
ggplot(subjInfo, aes(x=ageV1Mos, shape=Group, y=mullenRL)) +
  geom_point() + theme_bw() + 
  labs(title="Receptive language by age",y="Mullen RL raw score",x="Age (mos)",shape="Group") +
  wswl.smallplots

ggplot(subjInfo, aes(x=ageV1Mos, shape=Group, y=mullenVR)) +
  geom_point() + theme_bw() + 
  labs(title="Visual Reception by age",y="Mullen VR raw score",x="Age (mos)",shape="Group") +
  wswl.smallplots

