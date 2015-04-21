##### Social Word Learning Analysis
##### Parent Labeling (first code)
##### Created 7/25/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
#setwd("~/Google Drive/WS-SocialWordLearning/Results")
source("WSWL-Analysis/wswl-getPL.r")
source("WSWL-Analysis/wswl-functions.r")

load("ROutput/wswl-pl.Rda")
load("ROutput/wswl-subjInfo.Rda")


#### Reliability ####
with(parentLabelCodes,mean(as.numeric(uttType1==uttType2))) ## 92.7%
with(parentLabelCodes,mean(as.numeric(category1==category2))) ## 84.3%

#### overall means ####
pl.cat.plot = pl.category.bySubj
pl.cat.plot$category = ordered(pl.cat.plot$category, levels=c("followin","sharedattn","discrepant"),labels = c("follow-in","shared attn","discrepant"))

#### td 18-24m only ####
## proportion in each category
ggplot(subset(pl.cat.plot, ageGroup%in%c("18M","24M")), aes(x=category,y=(catTotal/totalSeen))) + 
  geom_boxplot() + theme_bw() +
  labs(title="Parent labels",y="Proportion of labels",x="Attention scenario")+
  wswl.posterplots
dev.print(png,file="Plots/PL/PL_TD_propCategories_boxplot.png",width=3.25, height=4, units="in",res=200)

## number in each category
ggplot(subset(pl.cat.plot, ageGroup%in%c("18M","24M")), aes(x=category,y=catTotal)) + 
  geom_boxplot() + theme_bw() +
  labs(title="Parent utterances about target objects",y="Number of utterances",x="Vocabulary") +
  wswl.posterplots

#### All TD & WS ####
## proportion in each category, by group
ggplot(pl.cat.plot, aes(x=Group,color=Group,y=(catTotal/totalSeen))) + 
  geom_boxplot() + facet_wrap(~category) + theme_bw() +
  labs(title="Parent labels",y="Proportion of labels",x="Attention scenario")+
  wswl.posterplots
dev.print(png,file="Plots/PL/PL_groups_propCategories_boxplot.png",width=5, height=3.5, units="in",res=200)

##number of utterances
summary(pl.totalUtts.bySubj$totalUtts)

ggplot(pl.totalUtts.bySubj, aes(x=Group, color=Group,y=totalUtts)) + 
  geom_boxplot() + theme_bw() + 
  labs(title="Parent utterances \nabout target objects",y="Number of utterances",x="Group") +
  wswl.posterplots
dev.print(png, file="Plots/PL/PL_groups_totalUtts_boxplot.png",width=3.5, height=3.5, units="in",res=200)

#### Stats ####
### Number of utterances: do parents of children with larger vocabularies talk more? 
## just td 18-24m:
pl.numUtts.lm = lm(totalUtts ~ ageV1Mos + WordsProduced + Sex, data=droplevels(subset(pl.bySubj, ageGroup %in% c("18M","24M"))))
anova(pl.numUtts.lm)

pl.words.lm = lm(totalWords ~ ageV1Mos + WordsProduced, data=droplevels(subset(pl.bySubj, ageGroup %in% c("18M","24M"))))
anova(pl.words.lm)

pl.mlu.lm = lm(mlu ~ ageV1Mos + WordsProduced, data=droplevels(subset(pl.bySubj, ageGroup %in% c("18M","24M"))))
anova(pl.mlu.lm)

pl.vocab.lm1 = lm(WordsProduced ~ ageV1Mos + totalUtts, data=droplevels(subset(pl.bySubj, ageGroup %in% c("18M","24M"))))
pl.vocab.lm2 = lm(WordsProduced ~ ageV1Mos + totalWords, data=droplevels(subset(pl.bySubj, ageGroup %in% c("18M","24M"))))
pl.vocab.lm3 = lm(WordsProduced ~ ageV1Mos + mlu, data=droplevels(subset(pl.bySubj, ageGroup %in% c("18M","24M"))))
anova(pl.vocab.lm3)

pl.category.bySubj.wide = droplevels(reshape(pl.category.bySubj, timevar="category", idvar=c("Subj","totalSeen","Group","ageGroup",
                                                                                             "Sex","ageV1Mos","mullenGM","mullenVR",
                                                                                             "mullenFM","mullenRL","mullenEL","WordsProduced"), 
                                             direction="wide",drop=c("VocabSplitAll","VocabSplitTD","VocabSplitWS","VocabSplitByGroup")))
pl.category.bySubj.wide$prop.followin = pl.category.bySubj.wide$catTotal.followin/pl.category.bySubj.wide$totalSeen
pl.category.bySubj.wide$prop.sharedattn = pl.category.bySubj.wide$catTotal.sharedattn/pl.category.bySubj.wide$totalSeen

pl.vocab.cat.lm1 = lm(WordsProduced ~ ageV1Mos + prop.followin + prop.sharedattn, 
                      data=droplevels(subset(pl.category.bySubj.wide, ageGroup %in% c("18M","24M"))))
anova(pl.vocab.cat.lm1)

# exclude videos where more than half of utterances were "unseen"
pl.lowseen = droplevels(subset(pl.totalUtts.bySubj, propSeen<0.5))

# Compare the frequency of different kinds of labels between different groups and different vocabulary sizes. 
# In order to do logistic regression, we need to convert the data into a format where we have a separate column for each of the
# possible outcomes. 
pl.stat = droplevels(subset(pl, !(Subj%in%pl.lowseen$Subj)))
pl.stat = merge(pl.stat, subjInfo, all.x=T)

# pl.stat$cat.fi = as.character(pl.stat$category)=="followin"
# pl.stat$cat.ja = as.character(pl.stat$category)=="sharedattn"
# pl.stat$cat.d = as.character(pl.stat$category)=="discrepant"
# pl.stat = merge(pl.stat, subjInfo)
# pl.stat$Subj = as.factor(pl.stat$Subj)
# pl.stat$numObjs = as.factor(pl.stat$numObjs)
# pl.stat$uttType = as.factor(pl.stat$uttType)
# 
# # probability of discrepant label
# library("rms")
# pl.discrepant.lrm1 = lrm(cat.d ~ ageGroup, data=pl.stat)
# pl.discrepant.lrm2 = lrm(cat.d ~ Group + ageV1Mos + WordsProduced, data=pl.stat)
# anova(pl.discrepant.lrm2)

pl.category.bySubj$category = as.factor(pl.category.bySubj$category)
summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~ category*WordsProduced, 
            data=droplevels(subset(pl.category.bySubj, !(Subj%in%pl.lowseen$Subj))),family="binomial"))

## focus just on TD group
# for all utterances
pl.stat.td = droplevels(subset(pl.stat, Group=="TD"&ageGroup!="30M"))
pl.td.discrepant.lrm = lrm(cat.d ~ ageV1Mos + WordsProduced, data=subset(pl.stat.td, category!="unseen"))
anova(pl.td.discrepant.lrm)
pl.td.sharedattn = lrm(cat.ja ~ ageV1Mos + WordsProduced, data=subset(pl.stat.td, category!="unseen"))
anova(pl.td.sharedattn)
pl.td.followin = lrm(cat.fi ~ ageV1Mos + WordsProduced, data=subset(pl.stat.td, category!="unseen"))
anova(pl.td.followin)

# limit to "label" utterances
pl.td.labels.discrepant.lrm = lrm(cat.d ~ ageV1Mos + WordsProduced, data=subset(pl.stat.td, category!="unseen"&uttType=="label"))
anova(pl.td.labels.discrepant.lrm)
pl.td.labels.sharedattn = lrm(cat.ja ~ ageV1Mos + WordsProduced, data=subset(pl.stat.td, category!="unseen"&uttType=="label"))
anova(pl.td.labels.sharedattn)
pl.td.labels.followin = lrm(cat.fi ~ ageV1Mos + WordsProduced, data=subset(pl.stat.td, category!="unseen"&uttType=="label"))
anova(pl.td.labels.followin)
xtabs(~category+uttType, data=pl.stat.td)

pl.category.stat.td = droplevels(subset(pl.category.bySubj, !(Subj%in%pl.lowseen$Subj)&ageGroup%in%tdgroup))
contrasts(pl.category.stat.td$category) = contr.helmert(3)
summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~ category+WordsProduced, 
            data=pl.category.stat.td,family="binomial"))

summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~ WordsProduced + ageV1Mos + Sex, data=subset(pl.category.stat.td, category=="followin"),family="binomial"))
anova(glm(cbind(catTotal, (totalSeen-catTotal)) ~ WordsProduced + ageV1Mos + Sex, data=subset(pl.category.stat.td, category=="followin"),family="binomial"), test="Chisq")
anova(glm(cbind(catTotal, (totalSeen-catTotal)) ~ WordsProduced + Sex + ageV1Mos, data=subset(pl.category.stat.td, category=="followin"),family="binomial"), test="Chisq")
anova(glm(cbind(catTotal, (totalSeen-catTotal)) ~ Sex + ageV1Mos + WordsProduced, data=subset(pl.category.stat.td, category=="followin"),family="binomial"), test="Chisq")
anova(glm(cbind(catTotal, (totalSeen-catTotal)) ~ ageV1Mos + Sex + WordsProduced, data=subset(pl.category.stat.td, category=="followin"),family="binomial"), test="Chisq")

summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~ WordsProduced + ageV1Mos + Sex, data=subset(pl.category.stat.td, category=="sharedattn"),family="binomial"))
summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~ WordsProduced + ageV1Mos, data=subset(pl.category.stat.td, category=="discrepant"),family="binomial"))

#### Summary plots ####
## Is there a relationship between vocabulary size and the number of utterances about target objects? - NOPE
ggplot(pl.totalUtts.bySubj, aes(x=WordsProduced, y=totalUtts, shape=ageGroup, color=ageGroup)) +
  geom_point() + geom_smooth(method=lm, se=FALSE) + theme_bw() +
  labs(title="Parent obj utterances by vocabulary",y="# utts",x="Words Produced")+
  wswl.smallplots
dev.print(png,file="Plots/PL/PL_UttsXWordsProduced_scatter.png",width=3, height=3, units="in",res=400)

ggplot(subset(pl.bySubj, ageGroup %in% c("18M","24M")), aes(x=WordsProduced, y=totalUtts)) +
  geom_point() + geom_smooth(method=lm, se=FALSE) + theme_bw() +
  labs(title="Parent obj utterances by vocabulary (TD)",y="# utts",x="Words Produced")+
  wswl.smallplots
dev.print(png, file="Plots/PL/PL_TD_UttsXWordsProduced_scatter.png", width=3, height=3, units="in", res=400)

ggplot(subset(pl.bySubj, ageGroup %in% c("18M","24M")), aes(x=Sex, y=totalUtts)) +
  geom_boxplot() + theme_bw() +
  labs(title="Parent obj utterances by child sex",y="# utts",x="Sex")+
  wswl.smallplots

## Is there a relationship between vocabulary size and the number of LABELS? - NOPE
ggplot(subset(pl.uttTypes.bySubj, uttType=="label"), aes(x=WordsProduced, y=typeTotal, shape=ageGroup, color=ageGroup)) +
  geom_point() + geom_smooth(method=lm, se=FALSE) + theme_bw() +
  labs(title="Parent labels by vocabulary",y="# labels",x="Words Produced")+
  wswl.smallplots
dev.print(png,file="Plots/PL/PL_LabelsXWordsProduced_scatter.png",width=4, height=3, units="in",res=400)

## Proportion of each category by age 
pl.category.bySubj = droplevels(subset(pl.category.bySubj, !(Subj%in%pl.lowseen$Subj)))
pl.category.bySubj$catProp = pl.category.bySubj$catTotal/pl.category.bySubj$totalSeen
pl.category.bySubj$category = ordered(pl.category.bySubj$category, levels=c("followin","sharedattn","discrepant"),
                                      labels=c("follow-in","shared attn", "discrepant"))
# TD ONLY: 
ggplot(droplevels(subset(pl.category.bySubj, ageGroup %in% c("18M","24M"))), 
       aes(x=ageV1Mos, y=catProp)) + facet_wrap(~category) +
  geom_point() + geom_smooth(method=lm, se=FALSE) +
  theme_bw() + wswl.smallplots + scale_fill_grey() +
  labs(title = "Utterance categories by age", y="Proportion of utterances", x="Age (mos)")
dev.print(png,file="Plots/PL/PL_TD_propCategoryXage_scatter.png",width=4, height=3, units="in",res=400)

## Proportion of each category by vocabulary size
# TD ONLY: 
ggplot(droplevels(subset(pl.category.bySubj, ageGroup %in% c("18M","24M"))), 
       aes(x=WordsProduced, y=catProp)) + facet_wrap(~category) +
  geom_point() + geom_smooth(method=lm, se=FALSE) +
  theme_bw() + wswl.smallplots + scale_fill_grey() +
  labs(title = "Utterance categories by vocabulary size", y="Proportion of utterances", x="Age (mos)")
dev.print(png,file="Plots/PL/PL_TD_propCategoryXvocab_scatter.png",width=4, height=3, units="in",res=400)

ggplot(droplevels(subset(pl.category.bySubj, ageGroup %in% tdgroup)), aes(x=Sex, y=(catTotal/totalSeen), color=Sex)) + 
  facet_wrap(~category) + geom_boxplot() + theme_bw() + wswl.smallplots + 
  labs(title = "Utterance categories by Sex", y="Proportion of utterances", x="Sex")


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
