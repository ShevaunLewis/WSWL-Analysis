##### Social Word Learning Analysis
##### Parent Labeling (first code)
##### Created 7/25/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")

load("wswl-data.Rda")
load("wswl-PL.Rda")

### Reliability
with(parentLabelCodes,mean(as.numeric(uttType1==uttType2))) ## 92.1%
with(parentLabelCodes,mean(as.numeric(category1==category2))) ## 86.8%

# add subject info
parentLabels = droplevels(merge(subjInfo, parentLabels, by.x = "Subj", by.y = "SubjID", all.y=T))
parentLabels$VocabGroupTD = ordered(parentLabels$VocabGroupTD, levels=c("low","high"))

##### Count labels #####
xtabs(~Subj, data=parentLabels)
xtabs(~Subj + uttType, data=parentLabels)
xtabs(~Subj + category, data=parentLabels)
xtabs(~ uttType + category, data=parentLabels)

xtabs(~ageGroup + uttType, data=parentLabels)
xtabs(~ageGroup + category, data=parentLabels)
xtabs(~category + uttType + ageGroup, data=parentLabels)

## Is there a relationship between vocabulary size and the number of utterances about target objects?
parentUttsTotal = ddply(parentLabels, .(Subj,VocabGroupTD), nrow)
colnames(parentUttsTotal)[3] <- "totalUtts"

ggplot(parentUttsTotal, aes(x=VocabGroupTD, y=totalUtts)) +
  geom_boxplot() + geom_point(size=1) + 
  theme_bw() +
  labs(title="Number of utterances about objects",y="# utterances",x="Vocab Size") +
  wswl.smallplots
dev.print(png,file="PilotResults/PL_UttsXVocab.png", width=400,height=600,res=200)

## Is there a relationship between vocabulary size and the number of LABELS?
parentLabelsTotal = ddply(subset(parentLabels,uttType=="label"),.(Subj,VocabGroupTD),nrow)
colnames(parentLabelsTotal)[3] <- "totalLabels"

ggplot(parentLabelsTotal, aes(x=VocabGroupTD, y=totalLabels)) +
  geom_boxplot() + geom_point(size=1) + 
  theme_bw() +
  labs(title="Number of labels for target objects",y="# labels",x="Vocab Size") +
  wswl.smallplots
dev.print(png,file="PilotResults/PL_LabelsXVocab.png", width=400,height=600,res=200)

## Count of label types by subject
parentLabelTypes = ddply(subset(parentLabels,uttType=="label"), .(Subj,category), nrow)
colnames(parentLabelTypes)[3] <- "num"

# add zeros for missing values
parentLabelTypeCounts = reshape(parentLabelTypes, timevar="category", idvar="Subj",direction="wide")
parentLabelTypeCounts[is.na(parentLabelTypeCounts)] <- 0

# sum labels that were not unseen
parentLabelTypeCounts$totalSeenLabels = with(parentLabelTypeCounts, num.followin+num.sharedattn+num.discrepant)
parentLabelTypeCounts = merge(parentLabelTypeCounts,parentLabelsTotal)

parentLabelTypes = reshape(parentLabelTypeCounts, idvar="Subj", 
                           varying = c("num.followin","num.sharedattn","num.discrepant","num.unseen"), direction="long")
colnames(parentLabelTypes)[5] <- "category"

parentLabelTypes = merge(parentLabelTypes,parentLabelsTotal)

parentLabelTypes$prop = parentLabelTypes$num/parentLabelTypes$totalSeenLabels

parentLabelTypes.avg = ddply(subset(parentLabelTypes,category!="unseen"), .(category,VocabGroupTD), function(df) {mean(df$prop)})
ddply(subset(parentLabelTypes,category!="unseen"), .(category), function(df) {mean(df$prop)})

ggplot(parentLabelTypes.avg, aes(x=VocabGroupTD, fill=category, y=V1)) +
  geom_bar(stat="identity") + theme_bw() + wswl.smallplots + scale_fill_grey() +
  labs(title = "Label types", y="Proportion of labels", fill="Label types", x = "Vocabulary Size")
dev.print(png, file="PilotResults/PL_LabelTypesXVocab.png",width=600, height=600, res=200)

parentLabelTypes.glm = glm(cbind(parentLabelTypeCounts$num.discrepant, parentLabelTypeCounts$num.sharedattn+parentLabelTypeCounts$num.followin) ~
                             VocabGroupTD, parentLabelTypeCounts, family=binomial)
summary(parentLabelTypes.glm)

save(parentUttsTotal,parentLabelTypes,parentLabelTypeCounts,parentLabelTypes.avg, file="wswl-PL.Rda")


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
