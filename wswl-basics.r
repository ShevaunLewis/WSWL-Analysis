##### Social Word Learning Analysis
##### Basic descriptive plots, etc. 
##### Created 8/7/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")

source("WSWL-Analysis/wswl-getSubjInfo.r")
source("WSWL-Analysis/wswl-functions.r")

load("ROutput/wswl-subjInfo.Rda")

#### Subject demographics ####
wswl.subjsummary(subjInfo,"ROutput/ageSummary_4-20-15.txt")

#### TD 18-24 months only ####
td = droplevels(subset(subjInfo,ageGroup %in% c("18M","24M")))

summary(lm(WordsProduced~ageV1Mos+Sex, data=td))

ggplot(td, aes(x=ageV1Mos, y=WordsProduced)) +
  geom_point(size=2) + geom_smooth(method="lm", se=F) + theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by age",y="Words Produced",x="Age (mos)")
dev.print(png,file="Plots/Vocab/TD_VocabXAge_scatter.png",width=3,height=2.75,units="in",res=300)

ggplot(td, aes(x=Sex, y=WordsProduced)) +
  geom_boxplot() + theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by sex",y="Words Produced",x="Sex")

ggplot(td, aes(x=ageV1Mos, y=WordsProduced, color = Sex)) +
  geom_point(size=1.25) + geom_smooth(method="lm") + theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by age and sex",y="Words Produced",x="Age (mos)")


#### All subjects (but not including pilots) ####
all = droplevels(subset(subjInfo, Group %in% c("TD","WS")))

## Vocabulary by age
#scatter
ggplot(all, aes(x=ageV1Mos, shape=Group, color=Group, y=WordsProduced)) +
  geom_point(size=1.25) + geom_smooth(method="lm", se=F) + theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by age",y="Vocabulary (words)",x="Age (mos)",shape="Group")
dev.print(png, file="Plots/Vocab/VocabXAge_all_scatter.png",width=800, height=600, res=200)

#boxplot (not very useful)
ggplot(all, aes(x=ageV1Mos, color=Group, y=WordsProduced)) +
  geom_boxplot(aes(outlier.colour=Group), alpha=0.4, position="identity",outlier.size=1.25) + 
  theme_bw() + wswl.posterplots + 
  labs(title="Vocabulary by group",y="Vocabulary (words)",x="Age (mos)")

#### 3 groups: 18M, 24M, and WS (no pilots, 30M) ####
subjInfo.3groups = droplevels(subset(subjInfo,ageGroup %in% c("18M","24M","WS")))
### Vocabulary by age ###
ggplot(subjInfo.3groups, aes(x=ageV1Mos, shape=Group, y=WordsProduced)) +
  geom_point(size=1.25) + theme_bw() + wswl.smallplots +
  labs(title="Vocabulary by age",y="Words Produced",x="Age (mos)",shape="Group")
dev.print(png,file="Plots/Vocab/VocabXAge_scatter.png", width=700,height=600,res=200)
dev.print(png,file="Plots/Vocab/VocabXAge_scatter_small.png", width=600,height=450,res=200)

ggplot(subjInfo.3groups, aes(x=ageV1Mos, color=ageGroup, y=WordsProduced)) +
  geom_boxplot() + geom_point(size=1) + 
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Vocabulary by group and age", y="Words Produced", x="Age (mos)", color="Group") +
  wswl.smallplots
dev.print(png,file="Plots/Vocab/VocabX3Group_boxplot.png", width=3.5,height=3,units="in",res=300)

ggplot(droplevels(subset(subjInfo,ageGroup %in% c("18M","24M","WS"))), aes(x=ageV1Mos, color=Group, y=WordsProduced)) + 
  geom_boxplot() + geom_point(size=1) + 
  theme_bw() + scale_color_manual(values = c("royalblue1","orangered")) + 
  labs(title="Vocabulary by group and age", y="Vocabulary", x="Age (mos)", color="Group") +
  wswl.smallplots
dev.print(png,file="Plots/Vocab/VocabX2Group_boxplot.png", width=3.5,height=3,units="in",res=300)

