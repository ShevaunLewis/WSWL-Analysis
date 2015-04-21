##### Social Word Learning Analysis
##### Word Learning (from online paper codes)
##### Created 8/7/2014
##### Updated 3/4/2015

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getWL.r")
source("WSWL-Analysis/wswl-functions.r")
load("ROutput/wswl-wl.Rda")
load("ROutput/wswl-subjInfo.Rda")

#### 
wl.byitem.td = droplevels(subset(wl.byitem,ageGroup %in% c("18M","24M") & Cond!="Practice"))
contrasts(wl.byitem.td$Cond) = contr.helmert(3)
summary(glm(cbind(item.acc,(comptrials - item.acc)) ~ WordsProduced + Cond + TrialType, data=wl.byitem.td, family="binomial"))
summary(glm(cbind(item.acc,(comptrials - item.acc)) ~ WordsProduced + Cond, data=subset(wl.byitem.td,TrialType=="novel"), family="binomial"))





#### Trials completed ####
## by group
ggplot(wl.trialsfinished, aes(x=Group,color=Group, y=totalCompTrials)) +
  geom_boxplot() + theme_bw() + 
  labs(title="WL trials completed",y="# trials completed",x="Group",color="Group") +
  wswl.posterplots
dev.print(png,file="Plots/WL/WL_trialsXGroup_boxplot.png", width=600,height=600,res=200)

## by age
ggplot(wl.trialsfinished, aes(x=ageV1Mos,color=Group, shape=Group,y=totalCompTrials)) +
  geom_point(size=1.25) + theme_bw() + 
  labs(title="WL trials completed",y="# trials completed",x="Age (mos)",color="Group") +
  wswl.posterplots
dev.print(png,file="Plots/WL/WL_trialsXGroup_scatter.png", width=600,height=600,res=200)

## by vocabulary
ggplot(wl.trialsfinished, aes(x=WordsProduced,color=Group, shape=Group,y=totalCompTrials)) +
  geom_point(size=1.25) + theme_bw() + 
  labs(title="WL trials completed",y="# trials completed",x="Vocabulary size",color="Group") +
  wswl.posterplots
dev.print(png,file="Plots/WL/WL_trialsXVocab_scatter.png", width=600,height=600,res=200)

ggplot(wl.itemsfinished, aes(x=WordsProduced,y=Freq,color=Group,shape=Group)) +
  geom_point() + geom_smooth(method=lm, se=F) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title="Word Learning trials completed",y="# trials completed",x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/WL_TrialsXVocabXGroup.png",width=3,height=3,units="in",res=300)

## TD only: 
ggplot(subset(wl.trialsfinished,ageGroup%in%c("18M","24M")), aes(x=ageV1Mos, y=Freq)) +
  geom_point(size=1) + geom_smooth(method=lm, se=FALSE) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="WL trials completed (TD)",y="# trials completed",x="Age (mos)") +
  wswl.smallplots
dev.print(png,file="Plots/WL/WL_TD_TrialsXage_scatter.png", width=3,height=3, units="in",res=400)

ggplot(subset(wl.trialsfinished,ageGroup%in%c("18M","24M")), aes(x=WordsProduced, y=Freq)) +
  geom_point(size=1) + geom_smooth(method=lm, se=FALSE) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="WL trials completed (TD)",y="# trials completed",x="Vocabulary size") +
  wswl.smallplots
dev.print(png,file="Plots/WL/WL_TD_TrialsXvocab_scatter.png", width=3,height=3, units="in",res=400)

#### Accuracy: FAMILIAR ####
## by age group
ggplot(wl.familiar.bysubj, aes(x = ageGroup, color=ageGroup, y=acc)) +
  geom_boxplot() + geom_point(size=1) + 
  scale_y_continuous(limits=c(0,1)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on familiar words",y="Proportion correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccFamiliarXGroup.png",width=700,height=600,res=200)

## by vocab
ggplot(wl.familiar.bysubj, aes(x = VocabGroup, color=VocabGroup, y=acc)) +
  geom_boxplot() +  
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on familiar words",y="Proportion correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccFamiliarXVocab.png", width=600,height=600,res=200)

## number correct (instead of proportion)
wl.familiar.sumacc.bysubj = wl.sumacc(wl.familiar.responded, "Subj")
wl.familiar.sumacc.bysubj = merge(wl.familiar.sumacc.bysubj, subjInfo)

## by age group
ggplot(wl.familiar.sumacc.bysubj, aes(x = ageGroup, color=ageGroup, y=sum_acc)) +
  geom_boxplot() + geom_point(size=1) + 
  scale_y_continuous(limits=c(0,12), breaks = c(0,2,4,6,8,10,12)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on familiar words",y="Total number correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png, file="PilotResults/WL_SumAccFamiliarXGroup.png",width=700,height=600,res=200)

#### Accuracy: NOVEL ####
### TD ###
## data frame with one row for each item, number of correct and total comprehension trials
wl.items.good.td.novel = droplevels(subset(wl.items.good, TrialType=="novel"&ageGroup %in% c("18M","24M")))
contrasts(wl.items.good.td.novel$Cond) = contr.helmert(3)
summary(glm(cbind(correct, (trialsresponded-correct)) ~ Cond * WordsProduced, data=wl.items.good.td.novel, family="binomial"))
summary(glm(cbind(correct, (trialsresponded-correct)) ~ Cond * ageV1Mos, data=wl.items.good.td.novel, family="binomial"))
summary(glm(cbind(correct, (trialsresponded-correct)) ~ Cond * ageV1Mos * WordsProduced, data=wl.items.good.td.novel, family="binomial"))
summary(glm(cbind(correct, (3-correct)) ~ Cond * WordsProduced, data=wl.items.good.td.novel, family="binomial"))
summary(glm(cbind(correct, (3-correct)) ~ Cond * ageV1Mos, data=wl.items.good.td.novel, family="binomial"))

## first comprehension trial only
wl.good.td = droplevels(subset(wl, TrialType=="novel" & ageGroup%in%c("18M","24M") & !(Subj%in%wl.badsubj$Subj)))
wl.good.td.first = subset(wl.good.td, CompTrial==1)
contrasts(wl.good.td.first$Cond) <- contr.helmert(3)
summary(glmer(acc ~ Cond* ageV1Mos + (1|Subj), data=wl.good.td.first, family=binomial))

## accuracy by subject and item:
#TD
wl.good.exp.bysubj.td = ddply(droplevels(subset(wl.items.good.td.novel, Cond!="Practice")), .(Subj,Cond), .drop=F,
                           summarize, acc.grand = (sum(correct,na.rm=T)/sum(trialsresponded,na.rm=T)), acc.strict = (sum(correct)/6),
                           acc.byitem = mean(correct/trialsresponded), acc.byitem.strict = mean(correct/3), notwrong.byitem = mean(notwrong/trialsresponded),
                           totalCorrect = sum(correct), totalNotWrong = sum(notwrong), totalResponded = sum(trialsresponded))
wl.good.exp.means.td = ddply(wl.good.exp.bysubj.td, .(Cond), 
                             summarize, acc.bysubj = mean(acc.grand), acc.byitem.bysubj = mean(acc.byitem, na.rm=T), 
                             notwrong.byitem.bysubj = mean(notwrong.byitem, na.rm=T),
                             acc.strict.bysubj = mean(acc.strict), acc.byitem.strict.bysubj = mean(acc.byitem.strict))

wl.good.exp.bysubj.td = merge(wl.good.exp.bysubj.td,subjInfo,all.x=T)
wl.good.exp.bysubj.td$VocabSplitTDWL = ifelse(wl.good.exp.bysubj.td$WordsProduced < median(wl.good.exp.bysubj.td$WordsProduced), "low","high")

wl.good.exp.means.td.vocabgroups = ddply(wl.good.exp.bysubj.td, .(Cond,VocabSplitTDWL), 
                                         summarize, acc.bysubj = mean(acc.grand, na.rm=T), acc.byitem.bysubj = mean(acc.byitem, na.rm=T), notwrong.byitem.bysubj = mean(notwrong.byitem, na.rm=T),
                                         acc.strict.bysubj = mean(acc.strict), acc.byitem.strict.bysubj = mean(acc.byitem.strict,na.rm=T))

wl.td.means.chance = ddply(wl.good.exp.bysubj.td, .(Cond, VocabSplitTDWL), summarize, correct = sum(totalCorrect), responded = sum(totalResponded), 
                           p = binom.test(correct, responded)$p.value)

save(wl.good.exp.bysubj.td, wl.good.exp.means.td, wl.good.exp.means.td.vocabgroups, file = "wl-TD-means.Rda")

#Everybody
wl.good.exp.bysubj = ddply(droplevels(subset(wl.items.good, Cond!="Practice"&TrialType=="novel")), .(Subj,Group,Cond), .drop=F,
                              summarize, acc.grand = (sum(correct,na.rm=T)/sum(trialsresponded,na.rm=T)), acc.strict = (sum(correct)/6),
                              acc.byitem = mean(correct/trialsresponded), acc.byitem.strict = mean(correct/3), 
                              totalCorrect = sum(correct), totalResponded = sum(trialsresponded))
wl.good.exp.means = ddply(wl.good.exp.bysubj, .(Group,Cond), 
                             summarize, acc.bysubj = mean(acc.grand), acc.byitem.bysubj = mean(acc.byitem), 
                             acc.strict.bysubj = mean(acc.strict), acc.byitem.strict.bysubj = mean(acc.byitem.strict))

wl.good.exp.bysubj = droplevels(merge(wl.good.exp.bysubj,subjInfo))


wl.means.chance = ddply(wl.good.exp.bysubj, .(Cond, VocabSplitTDWL), summarize, correct = sum(totalCorrect), responded = sum(totalResponded), 
                           p = binom.test(correct, responded)$p.value)

save(wl.good.exp.bysubj.td, wl.good.exp.means.td, wl.good.exp.means.td.vocabgroups, file = "wl-TD-means.Rda")



colnames(binom.test(3,5))
### all trials, by subject, overall
wl.alltrials.bysubj.byitem = wl.acc(wl.novel.responded, c("Item","Subj"))
wl.alltrials.bysubj = wl.acc(wl.alltrials.bysubj.byitem, "Subj")
wl.alltrials.bysubj = merge(wl.alltrials.bysubj, subjInfo)

## by age group
ggplot(wl.alltrials.bysubj, aes(x = ageGroup, color=ageGroup, y=acc)) +
  geom_boxplot() + geom_point(size=1) + scale_y_continuous(limits=c(0,1)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on novel words",y="Proportion correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccNovelAllXGroup.png", width=700,height=600,res=200)

## by vocab group
wl.alltrials.bysubj$VocabGroup = ordered(wl.alltrials.bysubj$VocabGroup,levels=c("low","high"))
ggplot(wl.alltrials.bysubj, aes(x = VocabGroup, y=acc)) +
  geom_boxplot() + scale_y_continuous(limits=c(0,1)) +
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on novel words",y="Proportion correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccNovelAllXVocab.png", width=600,height=600,res=200)

## number correct (instead of proportion)
wl.novel.sumacc.bysubj = wl.sumacc(wl.novel.responded, "Subj")
wl.novel.sumacc.bysubj = merge(wl.novel.sumacc.bysubj, subjInfo)

# by age group
ggplot(wl.novel.sumacc.bysubj, aes(x = ageGroup, color=ageGroup, y=sum_acc)) +
  geom_boxplot() + geom_point(size=1) + 
  scale_y_continuous(limits=c(0,18), breaks = c(0,2,4,6,8,10,12,14,16,18)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on novel words",y="Total number correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png, file="PilotResults/WL_SumAccNovelXGroup.png",width=700,height=600,res=200)

# by vocab group
wl.novel.sumacc.bysubj$VocabGroup = ordered(wl.novel.sumacc.bysubj$VocabGroup, levels=c("low","high"))
ggplot(wl.novel.sumacc.bysubj, aes(x = VocabGroup, y=sum_acc)) +
  geom_boxplot() +  scale_y_continuous(limits=c(0,18), breaks = c(0,2,4,6,8,10,12,14,16,18)) +
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on novel words",y="Total number correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_SumAccNovelAllXVocab.png", width=600,height=600,res=200)

### all trials, by subject, by condition
wl.alltrials.bycond.bysubj = wl.acc(wl.acc(wl.novel.responded, c("Item","Subj","Cond")),c("Subj","Cond"))
wl.alltrials.bycond.bysubj = merge(wl.alltrials.bycond.bysubj, subjInfo)
wl.alltrials.bycond.bysubj$Cond = ordered(wl.alltrials.bycond.bysubj$Cond, 
                                          levels=c("FollowIn","Joint","Discrepant"), labels=c("FollowIn","JointAttn","Discrepant"))
#wl.alltrials.bycond.bysubj$VocabGroup = ordered(wl.alltrials.bycond.bysubj$VocabGroup,levels=c("low","high"))

# wl.td = subset(wl.alltrials.bycond.bysubj, Group=="TD"&!(Subj %in% levels(wl.badsubj$Subj)))
# wl.td.avg = ddply(wl.td, .(Cond,VocabGroupTD), function(df){mean(df$acc)})
# wl.td.rawnums = ddply(droplevels(subset(wl.novel, Group=="TD"&Subj!="TD11"&!(Response%in%c("na","nr")))), .(Cond, VocabGroupTD), cbind(correct=function(df){sum(df$acc)}, nrow))
# wl.td.rawnums$binom = binom.test(wl.td.rawnums$V1, wl.td.rawnums$V2, p=0.5)

wl.td.plot = wl.good.exp.bysubj.td
wl.td.plot$Cond = ordered(wl.td.plot$Cond, levels=c("FollowIn","Joint","Discrepant"))
## td only, by vocab
ggplot(wl.td.plot, aes(x=WordsProduced, y=acc.byitem)) + 
  geom_point() + facet_wrap(~Cond) + geom_smooth(method=lm, se=F) + 
  theme_bw() + 
  labs(title="Accuracy on novel words",y="Proportion correct",x="Vocabulary") +
  wswl.posterplots
dev.print(png, file="Plots/WL/WL_TDgoodn10_NovelAccbyItemXVocab_scatter.png", width=4, height=3, units="in", res=400)

## td only, by age
ggplot(wl.td.plot, aes(x=ageV1Mos, y=acc.byitem)) + 
  geom_point() + facet_wrap(~Cond) + geom_smooth(method=lm, se=F) + 
  theme_bw() + 
  labs(title="Accuracy on novel words",y="Proportion correct",x="Age (mos)") +
  wswl.posterplots
dev.print(png, file="Plots/WL/WL_TDgoodn10_NovelAccbyItemXAge_scatter.png", width=4, height=3, units="in", res=400)


## age groups
dodge <-position_dodge(width=.8)
ggplot(wl.alltrials.bycond.bysubj, aes(x = Cond, color=ageGroup, y=acc)) +
  geom_boxplot()  + 
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on novel words",y="Proportion correct",x="Condition",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccNovelCondsXGroup.png", width=800,height=600,res=200)

## by vocab group
ggplot(wl.alltrials.bycond.bysubj, aes(x = Cond, color=VocabGroup, y=acc)) +
  geom_boxplot() + 
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on novel words",y="Proportion correct",x="Condition",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccNovelCondsXVocab.png", width=700,height=600,res=200)

ggplot(subset(wl.alltrials.bycond.bysubj,Group=="TD"),aes(x=WordsProduced,y=acc)) + 
  geom_point(color="royalblue1") + geom_smooth(method=lm, se=F) + facet_wrap(~Cond) + 
  theme_bw() + labs(title="Accuracy on novel words (TD only)",y="Proportion correct",x="Vocabulary") + 
  wswl.smallplots
dev.print(png,file="Results_1-29-15/WL_TD_AccNovelXVocabXCond.png",width=4,height=3,units="in",res=300)

## number correct (instead of proportion)
wl.novel.sumacc.bycond = wl.sumacc(wl.novel.responded, c("Subj","Cond"))
wl.novel.sumacc.bycond = merge(wl.novel.sumacc.bycond, subjInfo)
wl.novel.sumacc.bycond$Cond = ordered(wl.novel.sumacc.bycond$Cond, 
                                          levels=c("FollowIn","Joint","Discrepant"), labels=c("FollowIn","JointAttn","Discrepant"))
wl.novel.sumacc.bycond$VocabGroup = ordered(wl.novel.sumacc.bycond$VocabGroup,levels=c("low","high"))

ggplot(wl.novel.sumacc.bycond, aes(x = Cond, color=ageGroup, y=sum_acc)) +
  geom_boxplot() + 
  scale_y_continuous(limits=c(0,6), breaks = c(0,2,4,6)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on novel words, by condition",y="Total number correct",x="Condition",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_SumAccNovelCondsXGroup.png", width=800,height=600,res=200)

ggplot(wl.novel.sumacc.bycond, aes(x = Cond, color=VocabGroup, y=sum_acc)) +
  geom_boxplot() + 
  scale_y_continuous(limits=c(0,6), breaks = c(0,2,4,6)) +
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on novel words, by condition",y="Total number correct",x="Condition",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_SumAccNovelCondsXVocab.png", width=700,height=600,res=200)



### first trials, by subject, by cond
wl.firsttrial.bysubj = wl.acc(wl.acc(subset(wl.novel.responded, CompTrial==1), c("Item","Subj","Cond")),c("Subj","Cond"))
wl.firsttrial.bysubj = merge(wl.firsttrial.bysubj, subjInfo)

# These are all 0, .5, or 1 (of course)--need to think of how to present it. 

#### Means by age group ####
wl.alltrials.byage = wl.acc(wl.novel, c("Subj","ageGroup","Cond"))
wl.acc(wl.alltrials.byage,c("ageGroup","Cond"))

wl.firsttrial.byage = wl.acc(subset(wl.novel, CompTrial==1), c("Subj","ageGroup","Cond"))
wl.acc(wl.firsttrial.byage,c("ageGroup","Cond"))

save(wl, wl.familiar, wl.novel, wl.itemsfinished, wl.novel.sumacc.bycond, wl.alltrials.bycond.bysubj, file="wswl-WL.Rda")


#### STATS ####
wl.novel = droplevels(wl.novel)
wl.novel.nodiscrepant = droplevels(subset(wl.novel,Cond!="Discrepant"))

summary(glmer(acc~Cond*ageV1Mos+(1+Cond|Subj)+(1|Item), data=droplevels(wl.novel.nodiscrepant), family="binomial"))
