##### Social Word Learning Analysis
##### Parent labeling combined with point following
##### Created 11/13/14

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
#source("WSWL-Analysis/wswl-getdata.r")
#source("WSWL-Analysis/wswl-functions.r")

load("wswl-data.Rda")
load("wswl-PL.Rda")
load("wswl-PF.Rda")


## What is the relationship between the proportion of immediate looks and different kinds of labels?
## Expect negative relationship with discrepant/followin, and positive relationship with sharedattn

pf.imm.bysubj = subset(pf.lookcounts.bysubj, lookType=="Imm")
colnames(pf.imm.bysubj) <- c("Subj","totalPFTrials","lookType","numImm","propImm")
pf.imm.bysubj = pf.imm.bysubj[,c(1,2,4,5)]

parentLabelTypes.immlooks = merge(droplevels(subset(parentLabelTypes,category!="unseen")),pf.imm.bysubj)

ggplot(parentLabelTypes.immlooks, aes(x=propImm, y=prop)) + 
  geom_point(aes(shape=ageGroup, color=ageGroup),size=1) + geom_smooth(method=lm,se=FALSE,color="gray") + facet_wrap(~category) + xlim(0,1) +
  theme_bw() + labs(title="Labels by proportion of Immediate looks",y="Proportion of labels",x="Proportion immediate looks") + 
  wswl.smallplots
dev.print(png, file="Results-PL/PL_LabelTypeXImmLookProp_regall.png",width=5.5, height=3, units="in",res=400)

ggplot(parentLabelTypes.immlooks, aes(x=propImm, y=prop, shape=ageGroup, color=ageGroup)) + 
  geom_point(size=1) + geom_smooth(method=lm,se=FALSE) + facet_wrap(~category) + xlim(0,1) +
  theme_bw() + labs(title="Labels by proportion of Immediate looks",y="Proportion of labels",x="Proportion immediate looks") + 
  wswl.smallplots
dev.print(png, file="Results-PL/PL_LabelTypeXImmLookProp_regByGroup.png",width=5.5, height=3, units="in", res=400)
