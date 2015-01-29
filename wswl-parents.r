##### Social Word Learning Analysis
##### Parent Labeling (first code)
##### Created 7/25/2014

#setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
setwd("~/Google Drive/WS-SocialWordLearning/Results")
source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")

load("wswl-data.Rda")

#### Reliability ####
with(parentLabelCodes,mean(as.numeric(uttType1==uttType2))) ## 92.1%
with(parentLabelCodes,mean(as.numeric(category1==category2))) ## 86.8%

#### Counts (skip to load wswl-PL.Rda if already done) ####

## parent total utterances
parentUttsTotal = ddply(parentLabels, .(Subj,VocabByGroup,ageGroup,WordsProduced), nrow)
colnames(parentUttsTotal)[5] <- "totalUtts"
parentUttsTotal = merge(parentUttsTotal, subjInfoBasic[,c("Subj","Sex","ageV1Mos","Group")], all.x=T)

## parent total labels
parentLabelsTotal = ddply(subset(parentLabels,uttType=="label"),.(Subj,VocabByGroup,ageGroup,WordsProduced),nrow)
colnames(parentLabelsTotal)[5] <- "totalLabels"
parentLabelsTotal = merge(parentLabelsTotal, subjInfoBasic[,c("Subj","Sex","ageV1Mos","Group")], all.x=T)

## Count of label types by subject
parentLabelTypeCounts = ddply(subset(parentLabels,uttType=="label"), .(Subj,category), nrow)
colnames(parentLabelTypeCounts)[3] <- "num"

# add zeros for missing values
parentLabelTypeCounts = reshape(parentLabelTypeCounts, timevar="category", idvar="Subj",direction="wide")
parentLabelTypeCounts[is.na(parentLabelTypeCounts)] <- 0

# sum labels that were not unseen
parentLabelTypeCounts$totalSeenLabels = with(parentLabelTypeCounts, num.followin+num.sharedattn+num.discrepant)
parentLabelTypeCounts = merge(parentLabelTypeCounts,parentLabelsTotal)

parentLabelTypes = reshape(parentLabelTypeCounts, idvar="Subj", 
                           varying = c("num.followin","num.sharedattn","num.discrepant","num.unseen"), direction="long")
colnames(parentLabelTypes)[10] <- "category"

parentLabelTypes = merge(parentLabelTypes,parentLabelsTotal)

parentLabelTypes$prop = parentLabelTypes$num/parentLabelTypes$totalSeenLabels

parentLabelTypes.Vocab.avg = ddply(subset(parentLabelTypes,category!="unseen"), .(ageGroup,category,VocabByGroup), function(df) {mean(df$prop)})
colnames(parentLabelTypes.Vocab.avg)[4] <- "prop"

#ddply(subset(parentLabelTypes,category!="unseen"), .(category), function(df) {mean(df$prop)})

parentLabelTypes.avg = ddply(subset(parentLabelTypes,category!="unseen"), .(ageGroup,category), function(df) {mean(df$prop)})
colnames(parentLabelTypes.avg)[3] <- "prop"

save(parentUttsTotal,parentLabelTypes,parentLabelTypeCounts,parentLabelTypes.avg,parentLabelTypes.Vocab.avg, file="wswl-PL.Rda")

######*******#######
load("wswl-PL.Rda")

### Count labels ###
xtabs(~Subj, data=parentLabels)
xtabs(~Subj + uttType, data=parentLabels)
xtabs(~Subj + category, data=parentLabels)
xtabs(~ uttType + category, data=parentLabels)

xtabs(~ageGroup + uttType, data=parentLabels)
xtabs(~ageGroup + category, data=parentLabels)
xtabs(~category + uttType + ageGroup, data=parentLabels)

#### Summary plots ####

load("wswl-PL.Rda")

## Is there a relationship between vocabulary size and the number of utterances about target objects?
ggplot(parentUttsTotal, aes(x=VocabByGroup, y=totalUtts)) +
  geom_boxplot() + geom_point(size=1) + facet_wrap(~ageGroup) +
  theme_bw() +
  labs(title="Number of utterances about objects",y="# utterances",x="Vocab Size") +
  wswl.smallplots
dev.print(png,file="Results-PL/PL_UttsXVocab.png", width=400,height=600,res=200)

ggplot(parentUttsTotal, aes(x=WordsProduced, y=totalUtts, shape=ageGroup, color=ageGroup)) +
  geom_point() + geom_smooth(method=lm, se=FALSE) + theme_bw() +
  labs(title="Parent obj utterances by vocabulary",y="# utts",x="Words Produced")+
  wswl.smallplots
dev.print(png,file="Results-PL/PL_UttsXWordsProduced_scatter.png",width=4, height=3, units="in",res=400)

## Is there a relationship between vocabulary size and the number of LABELS?
ggplot(parentLabelsTotal, aes(x=VocabByGroup, y=totalLabels)) +
  geom_boxplot() + geom_point(size=1) + facet_wrap(~ageGroup) +
  theme_bw() +
  labs(title="Number of labels for target objects",y="# labels",x="Vocab Size") +
  wswl.smallplots
dev.print(png,file="PilotResults/PL_LabelsXVocab.png", width=400,height=600,res=200)

ggplot(parentLabelsTotal, aes(x=WordsProduced, y=totalLabels, shape=ageGroup, color=ageGroup)) +
  geom_point() + geom_smooth(method=lm, se=FALSE) + theme_bw() +
  labs(title="Parent labels by vocabulary",y="# labels",x="Words Produced")+
  wswl.smallplots
dev.print(png,file="Results-PL/PL_LabelsXWordsProduced_scatter.png",width=4, height=3, units="in",res=400)

## Number of utterances/labels by age, for each group
ggplot(parentLabelsTotal, aes(x=ageV1Mos,y=totalLabels)) + 
  geom_point(aes(shape=ageGroup,color=ageGroup)) + facet_wrap(~Group,scales="free_x") +
  geom_smooth(method=lm, se=FALSE,color="gray") + theme_bw() + 
  labs(title="Parent labels by age",y="# labels",x="Age in months")+
  wswl.smallplots
dev.print(png,file="Results-PL/PL_LabelsXAge_scatter.png",width=5, height=3, units="in",res=400)

ggplot(parentUttsTotal, aes(x=ageV1Mos,y=totalUtts)) + 
  geom_point(aes(shape=ageGroup,color=ageGroup)) + facet_wrap(~Group,scales="free_x") +
  geom_smooth(method=lm, se=FALSE,color="gray") + theme_bw() + 
  labs(title="Parent object utterances by age",y="# utterances",x="Age in months")+
  wswl.smallplots
dev.print(png,file="Results-PL/PL_LabelsXAge_scatter.png",width=5, height=3, units="in",res=400)

## Proportion of each type of label for each group
ggplot(parentLabelTypes.avg, aes(x=ageGroup, fill=category, y=prop)) +
  geom_bar(stat="identity") + 
  theme_bw() + wswl.smallplots + scale_fill_grey() +
  labs(title = "Label types", y="Proportion of labels", fill="Label types", x = "Vocabulary Size")
dev.print(png, file="Results-PL/PL_LabelTypesXAgeGroup.png",width=3, height=3, units="in", res=400)

## Proportion of each type of label for each group, split by vocabulary
ggplot(parentLabelTypes.avg, aes(x=VocabByGroup, fill=category, y=V1)) +
  geom_bar(stat="identity") + facet_wrap(~ageGroup) +
  theme_bw() + wswl.smallplots + scale_fill_grey() +
  labs(title = "Label types", y="Proportion of labels", fill="Label types", x = "Vocabulary Size")
dev.print(png, file="Results-PL/PL_LabelTypesXVocabGroup.png",width=5, height=3, units="in", res=400)

## Proportion of each type of label for each group, by words produced
ggplot(parentLabelTypes, aes(x=WordsProduced, )

## Does the total number of labels affect the proportion of each type?
ggplot(droplevels(subset(parentLabelTypes, category!="unseen")), aes(x=totalLabels, y=prop, shape=category, color=category)) + 
  geom_point(stat="identity") + geom_smooth(method=lm, se=FALSE) + theme_bw() + 
  labs(title = "Label types by total # of labels", y = "Proportion of seen labels", shape="Label types",color="Label types", x = "Total # labels") + 
  wswl.smallplots
dev.print(png, file="Results-PL/PL_LabelTypesXTotalLabels.png",width=5, height=4, units="in",res=400)

parentLabelTypes.glm = glm(cbind(parentLabelTypeCounts$num.discrepant, parentLabelTypeCounts$num.sharedattn+parentLabelTypeCounts$num.followin) ~
                             VocabGroupTD, parentLabelTypeCounts, family=binomial)
summary(parentLabelTypes.glm)




head(parentUtts)

plot(Freq ~ WordsProduced, data=parentUtts)
plot(Freq ~ mullenRL, data=parentUtts)
plot(Freq ~ mullenEL, data=parentUtts)

plot(Freq ~ WordsProduced, data=subset(parentUtts,ageGroup=="18M"))
plot(Freq ~ mullenRL, data=subset(parentUtts,ageGroup=="18M"))

plot(Freq ~ WordsProduced, data=subset(parentUtts,ageGroup=="24M"))

plot(Freq ~ WordsProduced, data = subset(parentUttTypes, uttType=="label"))
plot(Freq ~ WordsProduced, data = subset(parentUttTypes, uttType=="discussion"))
plot(Freq ~ WordsProduced, data = subset(parentUttTypes, uttType=="imitation"))

plot(WordsProduced ~ mullenEL, data=subjInfo)
