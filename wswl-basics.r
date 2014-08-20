##### Social Word Learning Analysis
##### Basic descriptive plots, etc. 
##### Created 8/7/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
load("wswl-data.Rda")
source("WSWL-Analysis/wswl-functions.r")

#### Subject demographics ####
wsSubjs = subjInfoBasic[subjInfoBasic$ageGroup=="WS",]
summary(wsSubjs$ageV1Mos)
td18Subjs = subjInfoBasic[subjInfoBasic$ageGroup=="18M",]
summary(td18Subjs$ageV1Mos)
td24Subjs = subjInfoBasic[subjInfoBasic$ageGroup=="24M",]
summary(td24Subjs$ageV1Mos)

#### Vocabulary by age ####
ggplot(subjInfo, aes(x=ageV1Mos, shape=Group, y=WordsProduced)) +
  geom_point(size=1) + 
  theme_bw() + 
  labs(title="Vocabulary by age",y="Words Produced",x="Age (mos)",shape="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/VocabXAge_scatter.png", width=700,height=600,res=200)

ggplot(subjInfo, aes(x=ageV1Mos, color=ageGroup, y=WordsProduced)) +
  geom_boxplot() + geom_point(size=1) + 
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Vocabulary by group and age", y="Words Produced", x="Age (mos)", color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/VocabXGroup_boxplot.png", width=700,height=600,res=200)

## median vocabulary split
xtabs(~VocabGroup, data=subjInfo)
lowGroup = subjInfo[subjInfo$VocabGroup=="low",]
summary(lowGroup)
highGroup = subjInfo[subjInfo$VocabGroup=="high",]
summary(highGroup)


#### Mullens by age ####
ggplot(subjInfo, aes(x=ageV1Mos, shape=Group, y=mullenRL)) +
  geom_point() + theme_bw() + 
  labs(title="Receptive language by age",y="Mullen RL raw score",x="Age (mos)",shape="Group") +
  wswl.smallplots

ggplot(subjInfo, aes(x=ageV1Mos, shape=Group, y=mullenVR)) +
  geom_point() + theme_bw() + 
  labs(title="Visual Reception by age",y="Mullen VR raw score",x="Age (mos)",shape="Group") +
  wswl.smallplots

