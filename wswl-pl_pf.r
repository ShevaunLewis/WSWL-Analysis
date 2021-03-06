##### Social Word Learning Analysis
##### Parent labeling combined with point following
##### Created 11/13/14

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
#source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")

load("ROutput/wswl-pl.Rda")
load("ROutput/wswl-pf.Rda")
load("ROutput/wswl-subjInfo.Rda")

## What is the relationship between the proportion of immediate looks and different kinds of labels?
## Expect negative relationship with discrepant/followin, and positive relationship with sharedattn

head(pf.lookcounts.bysubj.wide)

pl.pf = droplevels(merge(droplevels(subset(pl.category.bySubj,category!="unseen")),pf.lookcounts.bysubj.wide))
pl.pf$propCat = pl.pf$catTotal/pl.pf$totalSeen
contrasts(pl.pf$Group) = contr.helmert(2)
pl.pf.td = droplevels(subset(pl.pf, ageGroup%in%tdgroup))

summary(glm(cbind(catTotal,totalSeen-catTotal) ~ num.Imm + Group, data=droplevels(subset(pl.pf, category=="followin")), family="binomial"))
summary(glm(cbind(catTotal,totalSeen-catTotal) ~ num.Imm + Group, data=droplevels(subset(pl.pf, category=="SharedAttn")), family="binomial"))
summary(glm(cbind(catTotal,totalSeen-catTotal) ~ num.Imm * Group, data=droplevels(subset(pl.pf, category=="Discrepant")), family="binomial"))

pl.pf.wide = merge(pl.category.bySubj.wide,pf.lookcounts.bysubj.wide)
pl.pf.wide = merge(pl.pf.wide, pl.wordcount.bysubj)
pl.pf.wide$propFI = pl.pf.wide$totalFollowin/pl.pf.wide$totalSeen
pl.pf.wide$propSA = pl.pf.wide$totalSharedAttn/pl.pf.wide$totalSeen
pl.pf.wide$propImm = with(pl.pf.wide,num.Imm/totalTrials)

anova(lm(WordsProduced~totalSeen,data=pl.pf.wide))
summary(lm(WordsProduced~mullenVR,data=pl.pf.wide))
summary(lm(WordsProduced~ageV1Mos+Group*num.Imm,data=pl.pf.wide))
summary(lm(WordsProduced~num.Imm,data=pl.pf.wide))
summary(lm(WordsProduced~totalSharedAttn,data=pl.pf.wide))
summary(lm(WordsProduced~ageV1Mos,data=pl.pf.wide))

pl.pf.wide.td = droplevels(subset(pl.pf.wide,Group=="TD"))
pl.pf.wide.ws = droplevels(subset(pl.pf.wide,Group=="WS"))

anova(lm(WordsProduced~mlu*ageV1Mos,data=pl.pf.wide.ws))

summary(lm(WordsProduced~mlu*ageV1Mos,data=pl.pf.wide.td))
summary(lm(WordsProduced~totalLooks*ageV1Mos,data=subset(pl.pf.wide.ws,WordsProduced<250)))

summary(lm(mlu~ageV1MosWordsProduced,data=pl.pf.wide.td))


summary(lm(WordsProduced~num.Imm*Group,data=subset(pl.pf.wide,WordsProduced<250)))

pl.pf.plot= pl.pf
pl.pf.plot$category = ordered(pl.pf.plot$category, levels=c("followin","sharedattn","discrepant"),labels=c("FollowIn","SharedAttn","Discrepant"))
ggplot(pl.pf.plot, aes(x=(num.Imm/totalTrials), y=(catTotal/totalSeen), shape=Group, color=Group)) + 
  geom_point(size=1.25) + geom_smooth(method=lm,se=FALSE) + facet_wrap(~category) + xlim(0,1) +
  theme_bw() + labs(title="Labels by proportion of Immediate looks",y="Proportion of labels",x="Proportion immediate looks") + 
  wswl.posterplots+theme(axis.text.x=element_text(size=10))
dev.print(png, file="Plots/PL/PL-PF_LabelTypeXImmLookProp.png",width=5.5, height=3, units="in",res=200)

ggplot(subset(pl.pf.plot,WordsProduced<250), aes(x=(num.Imm/totalTrials), y=(catTotal/totalSeen), shape=Group, color=Group)) + 
  geom_point(size=1.25) + geom_smooth(method=lm,se=FALSE) + facet_wrap(~category) + xlim(0,1) +
  theme_bw() + labs(title="Labels by proportion of Immediate looks",y="Proportion of labels",x="Proportion immediate looks") + 
  wswl.posterplots+theme(axis.text.x=element_text(size=10))
dev.print(png, file="Plots/PL/PL-PF_vocab<250_LabelTypeXImmLookProp.png",width=6, height=3.5, units="in",res=400)

ggplot(parentLabelTypes.immlooks, aes(x=propImm, y=prop, shape=ageGroup, color=ageGroup)) + 
  geom_point(size=1) + geom_smooth(method=lm,se=FALSE) + facet_wrap(~category) + xlim(0,1) +
  theme_bw() + labs(title="Labels by proportion of Immediate looks",y="Proportion of labels",x="Proportion immediate looks") + 
  wswl.smallplots
dev.print(png, file="Results-PL/PL_LabelTypeXImmLookProp_regByGroup.png",width=5.5, height=3, units="in", res=400)

#TD only
pl.pf.td.plot = pl.pf.td
pl.pf.td.plot$category = ordered(pl.pf.td.plot$category,levels=c("followin","sharedattn","discrepant"), labels=c("follow-in","shared attn","discrepant"))

ggplot(pl.pf.td.plot, aes(x=(num.Imm/totalTrials), y=(catTotal/totalSeen))) + 
  geom_point(size=2) + geom_smooth(method=lm,se=FALSE) + facet_wrap(~category) + xlim(0,1) +
  theme_bw() + labs(title="Label types by proportion of immediate looks",y="Proportion of labels",x="Proportion immediate looks") + 
  wswl.posterplots
dev.print(png, file="Plots/PL/PL-PF_TD_LabelTypeXImmLookProp.png",width=5.5, height=3, units="in",res=400)
       
### stats: labels vs. num immediate looks
pl.pf.imm.td$category = as.factor(pl.pf.imm.td$category)
contrasts(pl.pf.imm.td$category) = contr.helmert(3)
summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~ WordsProduced * category * propImm, data=pl.pf.imm.td, family="binomial"))

#  looks and vocabulary size are strongly correlated with each other. but immediate looks and vocab size aren't (basically because of one outlier). 
# so I guess it's ok to include both vocab and num.imm in the models. 
summary(lm(num.Imm~WordsProduced, data=subset(pf.lookcounts.bysubj.wide, ageGroup%in%tdgroup)))
summary(lm(ageV1Mos ~ WordsProduced, data=subset(pf.lookcounts.bysubj.wide, ageGroup%in%tdgroup)))
summary(lm(ageV1Mos ~ num.Imm, data=subset(pf.lookcounts.bysubj.wide, ageGroup%in%tdgroup)))

#followin only
summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~ ageV1Mos + WordsProduced + num.Imm, data=subset(pl.pf.td, category=="followin"), family="binomial"))

ggplot(subset(pl.pf.td.plot,category=="follow-in"), aes(x=(num.Imm/totalTrials), color=VocabSplitTDPL, shape=ageGroup, y=propCat)) + 
  geom_point(size=2) + facet_wrap(~category) + theme_bw()

#sharedattn only
summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~ ageV1Mos + WordsProduced + num.Imm, data=subset(pl.pf.td, category=="sharedattn"), family="binomial"))

#discrepant only
summary(glm(cbind(catTotal, (totalSeen-catTotal)) ~  ageV1Mos + WordsProduced + num.Imm, data=subset(pl.pf.td, category=="discrepant"), family="binomial"))


