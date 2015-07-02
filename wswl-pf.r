##### Social Word Learning Analysis
##### Point following (first code)
##### Created 9/18/14
##### Updated 10/31/14

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getPF.r")
source("WSWL-Analysis/wswl-functions.r")

load("ROutput/wswl-subjInfo.Rda")
load("ROutput/wswl-pf.Rda")

## Calculate coding reliability
pf2codes = droplevels(subset(pfFull,look2!=""))
pf2codes$lookRel = as.numeric(as.character(pf2codes$look1)==as.character(pf2codes$look2))
mean(pf2codes$lookRel)
rel = with(pf2codes, aggregate(lookRel,makefactorlist(pf2codes,c("Separation","Distance")), mean))
xtabs(~lookRel+Separation+Distance,data=pf2codes)
pf2codes.exp = subset(pf2codes,Position!="center")
relexp = with(pf2codes.exp, aggregate(lookRel,makefactorlist(pf2codes.exp,c("Separation","Distance")), mean))

colnames(pf.lookcounts.bysubj)[2] <- "lookType"


##### Stats #####

### td 18-24m ###
pf.stat.td = droplevels(subset(pf.lookcounts.bysubj.wide, ageGroup %in% c("18M", "24M")))
anova(lm(totalGoodTrials ~ ageV1Mos * WordsProduced, data=pf.stat.td))
anova(lm(totalLooks ~ ageV1Mos * WordsProduced, data=pf.stat.td))
anova(lm(num.Imm ~ ageV1Mos * WordsProduced, data=pf.stat.td))
pf.td.goodtrials.lm = lm(WordsProduced ~ ageV1Mos * totalGoodTrials, data=pf.stat.td)
pf.td.totallooks.lm = lm(WordsProduced ~ ageV1Mos * totalLooks, data=pf.stat.td)
anova(pf.td.goodtrials.lm)



pf.td.looks.glm = glm(cbind(looked,notLooked) ~ WordsProduced * ageV1Mos, 
                      data=droplevels(subset(pf.conds, ageGroup %in% c("18M","24M"))), family="binomial")
pf.td.looks.glm2 = glm(cbind(looked,notLooked) ~ WordsProduced + Distance * Separation, 
                      data=droplevels(subset(pf.conds, ageGroup %in% c("18M","24M"))), family="binomial")
summary(pf.td.looks.glm2)
anova(pf.td.looks.glm, pf.td.looks.glm2)
anova(pf.td.looks.glm2, test="Chisq")
summary(pf.td.looks.glm)

#ld only
pf.td.ld.looks.glm = glm(cbind(looked,notLooked) ~ WordsProduced * ageV1Mos * Separation * PositionRel, 
                      data=droplevels(subset(pf.conds, ageGroup %in% c("18M","24M") & Distance == "far")), family="binomial")

### everybody ###
pf$Distance = as.factor(pf.full$Distance)
pf$PositionRel = as.factor(pf.full$PositionRel)
pf.stat = droplevels(subset(pf,attempt!="rep"))

###

pf.lookcounts.bysubj.wide = droplevels(pf.lookcounts.bysubj.wide)
contrasts(pf.lookcounts.bysubj.wide$Group) <- contr.helmert(2)

summary(lm(totalGoodTrials ~ Group * WordsProduced, data=pf.lookcounts.bysubj.wide))
summary(lm(totalLooks ~ Group + WordsProduced, data=pf.lookcounts.bysubj.wide))
summary(lm(num.Imm ~ Group + WordsProduced, data=pf.lookcounts.bysubj.wide))
summary(lm(num.Imm ~ Group + WordsProduced, data=subset(pf.lookcounts.bysubj.wide,WordsProduced<250)))
summary(lm(totalLooks ~ Group + WordsProduced, data=subset(pf.lookcounts.bysubj.wide,WordsProduced<250)))
summary(lm(num.Imm ~ Group * mullenVR , data=subset(pf.lookcounts.bysubj.wide,WordsProduced<250)))

summary(lm(totalGoodTrials ~ Group * ageV1Mos, data=pf.lookcounts.bysubj.wide))
summary(lm(totalLooks ~ Group * ageV1Mos, data=pf.lookcounts.bysubj.wide))
summary(lm(num.Imm ~ Group * ageV1Mos, data=pf.lookcounts.bysubj.wide))

summary(lm(mullenEL ~ num.Imm*Group, data=subset(pf.lookcounts.bysubj.wide,WordsProduced<250)))

summary(glm(cbind(totalLooks, totalTrials-totalLooks) ~ Group * ageV1Mos, data=pf.lookcounts.bysubj.wide,family="binomial"))

pf.looks.glm.far = glm(cbind(looked,notLooked) ~ Group * PositionRel * Separation, data=subset(pf.conds,Distance=="Far"), family="binomial")
summary(pf.looks.glm.far)
pf.looks.glm.near = glm(cbind(looked,notLooked) ~ Group * PositionRel, data=subset(pf.conds,Distance=="Near"), family="binomial")
summary(pf.looks.glm.near)
pf.conds.glm = glm(cbind(looked,notLooked) ~ Group * PositionRel * Distance, data=pf.conds, family="binomial")
summary(pf.conds.glm)
summary(glmer(cbind(looked,notLooked)~Group+Distance+Separation+PositionRel+(1|Subj),data=pf.conds,family="binomial"))
# 7/2/15: confirmed that glmer actually works the same with a logical output variable - don't have to use hits and misses.

library(Hmisc)

pf.glmm=glmer(I(lookF%in%c("Imm","Delay"))~PositionRel*Distance+WordsProduced+(1+PositionRel*Distance|Subj),data=pf.stat,family="binomial")
summary(pf.glmm)
somers2(binomial()$linkinv(fitted(pf.glmm)),as.numeric(pf.stat$lookF%in%c("Imm","Delay")))

pf.ld = droplevels(subset(pf.stat,Distance=="Far"))
pf.ld.glmm = glmer(I(lookF%in%c("Imm","Delay"))~PositionRel*Separation+WordsProduced+(1+PositionRel*Separation|Subj),data=pf.ld,family="binomial")
summary(pf.ld.glmm)
somers2(binomial()$linkinv(fitted(pf.ld.glmm)),as.numeric(pf.ld$lookF%in%c("Imm","Delay")))


library("languageR")
collin.fnc(pf.lookcounts.bysubj.wide,c(5,12))

### multinomial
library(mlogit)

pf.final = pf[,c(1:3,8:19,21:28)]
pf.ld = droplevels(subset(pf.final, Distance=="Far"))
pf.all.data = mlogit.data(pf.final, shape="wide", choice="lookF", id="Subj", chid.var="pointNum")
pf.ld.data = mlogit.data(pf.ld, shape="wide", choice="lookF",id="Subj",chid.var="pointNum")
pf.ld.ml = mlogit(lookF ~ 1|PositionRel + Group, pf.ld.data)


##### Summary plots #####
### td only
ggplot(pf.lookcounts.bysubj.wide, aes(x=WordsProduced,y=totalGoodTrials)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Trials with eye contact", y="Number of trials with eye contact", x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Plots/PF/PF_TD_GoodTrialsXVocab_scatter.png", width=3, height=3, units="in", res=300)

## Immediate looks (count)
ggplot(pf.stat.td, aes(x=WordsProduced, y=num.Imm)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + 
  theme_bw() + labs(title="Immediate looks to target",y="Number of immediate looks",x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Plots/PF/PF_TD_ImmLooksXVocab_scatter.png", width=3, height=3, units="in", res=300)

ggplot(pf.stat.td, aes(x=WordsProduced, y=num.Delay)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + 
  theme_bw() + labs(title="Delayed looks to target",y="Number of delayed looks",x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Plots/PF/PF_TD_DelayLooksXVocab_scatter.png", width=3, height=3, units="in", res=300)

## looks (count) by vocab size, TD only
ggplot(pf.stat.td, aes(x=WordsProduced, y=totalLooks)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + 
  theme_bw() + labs(title="Looks to target",y="Number of looks to target",x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_TD_AnyLooksXVocab_scatter.png", width=3, height=2.75, units="in", res=300)

## looks (count) by age, TD only
ggplot(pf.stat.td, aes(x=ageV1Mos, y=totalLooks)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + 
  theme_bw() + labs(title="Looks to target (imm. or delayed)",y="Number of looks to target",x="Age (mos)") + 
  wswl.smallplots
dev.print(png, file="Plots/PF/PF_TD_AnyLooksXAge_scatter.png", width=3, height=3, units="in", res=300)

### all participants
## Successful trials by age and group
ggplot(pf.lookcounts.bysubj.wide, aes(x=ageV1Mos,y=totalGoodTrials,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Trials with eye contact", y="# trials with eye contact", x="Age (mos)") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_goodtrialsXAgeXGroup.png", width=4,height=3,units="in",res=200)

## Successful trials by vocab and group
ggplot(pf.lookcounts.bysubj.wide, aes(x=WordsProduced,y=totalGoodTrials,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Trials with eye contact", y="# trials with eye contact", x="Vocabulary size") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_goodtrialsXVocabXGroup.png", width=4,height=3,units="in",res=200)

## Immediate looks by age and group
ggplot(pf.lookcounts.bysubj.wide, aes(x=ageV1Mos,y=num.Imm,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Immediate looks", y="# trials with immediate looks", x="Age (mos)") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_ImmXAgeXGroup.png", width=4,height=3,units="in",res=200)


## Immediate looks by vocab and group
ggplot(pf.lookcounts.bysubj.wide, aes(x=WordsProduced,y=num.Imm,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Immediate looks", y="# trials with immediate looks", x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_ImmXVocabXGroup.png", width=4,height=3,units="in",res=200)

ggplot(subset(pf.lookcounts.bysubj.wide,WordsProduced<250), aes(x=num.Imm,y=WordsProduced,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) +
  theme_bw() + labs(title = "Vocabulary and point following", y="Vocabulary", x = "# immediate looks") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_vocab<250_VocabXImmXGroup.png", width=4,height=3,units="in",res=200)

#Immediate looks by vocab and group, for vocab < 250
ggplot(subset(pf.lookcounts.bysubj.wide,WordsProduced<250), aes(x=WordsProduced,y=num.Imm,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Immediate looks", y="# trials with immediate looks", x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_vocab<250_ImmXVocabXGroup.png", width=4,height=3,units="in",res=200)

## Any looks by age and group
ggplot(pf.lookcounts.bysubj.wide, aes(x=ageV1Mos,y=totalLooks,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Total looks (imm. or delayed)", y="#trials with look to target", x="Age (mos)") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_TotalLooksXAgeXGroup.png", width=4,height=3,units="in",res=200)

## Any looks by vocab and group
ggplot(pf.lookcounts.bysubj.wide, aes(x=WordsProduced,y=totalLooks,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Total looks (imm. or delayed)", y="#trials with look to target", x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_TotalLooksXVocabXGroup.png", width=4,height=3,units="in",res=200)

ggplot(subset(pf.lookcounts.bysubj.wide,WordsProduced<250), aes(x=WordsProduced,y=totalLooks,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Total looks (imm. or delayed)", y="#trials with look to target", x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_vocab<250_TotalLooksXVocabXGroup.png", width=4,height=3,units="in",res=200)

##Any looks by age and group, as proportion of good trials (with eye contact)
ggplot(pf.lookcounts.bysubj.wide, aes(x=ageV1Mos,y=num.Imm/totalGoodTrials,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) +
  theme_bw() + labs(title = "Total looks (imm. or delayed)", y="Number of trials with look to target", x="Age (mos)") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_TotalLooksXAgeXGroup.png", width=3,height=3,units="in",res=200)


### FAR
pf.far.relpos.totals = ddply(droplevels(subset(pf,attempt!="rep"&Position!="center"&Distance=="Far")), .(Subj,PositionRel),nrow,.drop=F)
colnames(pf.far.relpos.totals)[3] = "trials"
pf.far.relpos.looks = ddply(subset(pf,attempt!="rep"&Position!="center"&Distance=="Far"), .(Subj,PositionRel),
                            summarise,looked=sum(looked), imm=sum(as.numeric(lookF=="Imm")), delay=sum(as.numeric(lookF=="Delay")))
pf.far.relpos = merge(pf.far.relpos.totals, pf.far.relpos.looks)
pf.far.relpos = droplevels(merge(pf.far.relpos, subjInfo))
pf.far.relpos$PositionRel = as.factor(pf.far.relpos$PositionRel)
contrasts(pf.far.relpos$Group) <- contr.helmert(2)
contrasts(pf.far.relpos$PositionRel) <- contr.helmert(2)
summary(glm(cbind(imm,(trials-imm)) ~ Group * PositionRel, data=pf.far.relpos, family="binomial"))
summary(glm(cbind(delay,(trials-delay)) ~ Group * PositionRel, data=pf.far.relpos, family="binomial"))
fsummary(glm(cbind(looked,trials-looked) ~ Group * PositionRel, data=pf.far.relpos, family="binomial"))



## by group and relative position
ggplot(pf.far.relpos, aes(x=PositionRel, color=Group, y=imm/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="Immediate looks by \ntarget position (Far)", y="Proportion immediate looks to target") +
  wswl.posterplots
dev.print(png, file="Plots/PF/PF-LD_propImmXPositionRelXGroup.png",width=3.5, height=3.5, units="in",res=300)

ggplot(pf.far.relpos, aes(x=PositionRel, color=Group, y=delay/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="Delayed looks by \ntarget position (Far)", y="Proportion delayed looks to target")+
  wswl.posterplots
dev.print(png, file="Plots/PF/PF-LD_propDelayXPositionRelXGroup.png",width=3.5, height=3.5, units="in",res=300)

ggplot(pf.far.relpos, aes(x=PositionRel, color=Group, y=looked/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="Looks (imm. or delayed) \nby target position (Far)", y="Proportion looks to target") +
  wswl.posterplots
dev.print(png, file="Plots/PF/PF-LD_propLookXPositionRelXGroup.png",width=3.5,height=3.5,units="in",res=200)

### NEAR
pf.near.relpos.totals = ddply(droplevels(subset(pf,attempt!="rep"&Distance=="Near")), .(Subj,PositionRel),nrow,.drop=F)
colnames(pf.near.relpos.totals)[3] = "trials"
pf.near.relpos.looks = ddply(subset(pf,attempt!="rep"&Distance=="Near"), .(Subj,PositionRel),
                            summarise,looked=sum(looked), imm=sum(as.numeric(lookF=="Imm")), delay=sum(as.numeric(lookF=="Delay")))
pf.near.relpos = merge(pf.near.relpos.totals, pf.near.relpos.looks)
pf.near.relpos = droplevels(merge(pf.near.relpos, subjInfo))
pf.near.relpos$PositionRel = as.factor(pf.near.relpos$PositionRel)
contrasts(pf.near.relpos$Group) <- contr.helmert(2)
contrasts(pf.near.relpos$PositionRel) <- contr.helmert(2)
summary(glm(cbind(imm,(trials-imm)) ~ Group * PositionRel, data=pf.near.relpos, family="binomial"))
summary(glm(cbind(delay,(trials-delay)) ~ Group * PositionRel, data=pf.near.relpos, family="binomial"))

## by group and relative position
ggplot(pf.near.relpos, aes(x=PositionRel, color=Group, y=imm/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="Immediate looks by\n target position (Near)", y="Proportion immediate looks to target") +
  wswl.posterplots
dev.print(png, file="Plots/PF/PF-SD_propImmXPositionRelXGroup.png",width=3.5, height=3.5, units="in",res=300)

ggplot(pf.near.relpos, aes(x=PositionRel, color=Group, y=delay/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="Delayed looks by \ntarget position (Near)", y="Proportion immediate looks to target") +
  wswl.posterplots
dev.print(png, file="Plots/PF/PF-SD_propDelayXPositionRelXGroup.png",width=3.5, height=3.5, units="in",res=300)

ggplot(pf.near.relpos, aes(x=PositionRel, color=Group, y=looked/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="Looks (imm. or delayed) \nby target position (Near)", y="Proportion looks to target") +
  wswl.posterplots
dev.print(png, file="Plots/PF/PF-SD_propLooksXPositionRelXGroup.png",width=3.5, height=3.5, units="in",res=300)

ggplot(pf.near.relpos, aes(x=PositionRel, color=Group, y=(trials-looked)/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="No look by\n target position (Near)", y="Proportion immediate looks to target") +
  wswl.posterplots


## any distance
pf.relpos.totals = ddply(droplevels(subset(pf,attempt!="rep"&Position!="center")), .(Subj,PositionRel),nrow,.drop=F)
colnames(pf.relpos.totals)[3] = "trials"
pf.relpos.looks = ddply(subset(pf,attempt!="rep"&Position!="center"), .(Subj,PositionRel),
                            summarise,looked=sum(looked), imm=sum(as.numeric(lookF=="Imm")), delay=sum(as.numeric(lookF=="Delay")))
pf.relpos = merge(pf.far.relpos.totals, pf.far.relpos.looks)
pf.relpos = droplevels(merge(pf.far.relpos, subjInfo))
pf.relpos$PositionRel = as.factor(pf.far.relpos$PositionRel)
contrasts(pf.relpos$Group) <- contr.helmert(2)
contrasts(pf.relpos$PositionRel) <- contr.helmert(2)
summary(glm(cbind(looked,(trials-looked)) ~ Group * PositionRel, data=pf.relpos, family="binomial"))
summary(glm(cbind(delay,(trials-delay)) ~ Group * PositionRel, data=pf.near.relpos, family="binomial"))

ggplot(pf.relpos, aes(x=PositionRel, color=Group, y=looked/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="Looks (imm. or delayed) \nby target position", y="Proportion looks to target") +
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_propLooksXPositionRelXGroup.png",width=3.5, height=3.5, units="in",res=300)


## Immediate looks by vocabulary size and group 
ggplot(subset(pf.lookcounts.bysubj,lookF=="Imm"), aes(x=WordsProduced, y=(num/totalTrials),color=Group, shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) +
  theme_bw() + labs(title="Proportion immediate looks", y="Proportion immediate looks", x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_propImmXVocabXGroup.png",width=3.5, height=3.5, units="in",res=300)

## Immediate looks by vocabulary size and group: good trials only
ggplot(subset(pf.lookcounts.bysubj,lookF=="Imm"), aes(x=WordsProduced, y=(num/totalGoodTrials),color=Group, shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + 
  theme_bw() + labs(title="Proportion immediate looks", y="Proportion immediate looks", x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_propImmXVocabXGroup_goodtrials.png",width=3.5, height=3, units="in",res=300)

## Immediate looks by vocabulary size and group: good subjects only (>10 trials with eye contact)
ggplot(droplevels(subset(pf.lookcounts.bysubj,lookF=="Imm"&totalGoodTrials>9)), aes(x=WordsProduced, y=(num/totalTrials),color=Group, shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title="Proportion immediate looks", y="Proportion immediate looks", x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Results_1-29-15/PF_propImmXVocab_goodsubj.png",width=3, height=3, units="in",res=300)

## looks (immediate or delayed) by vocab size and group
ggplot(subset(pf.lookcounts.bysubj,lookF=="Imm"), aes(x=WordsProduced, y=(totalLooks/totalTrials),color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + 
  theme_bw() + labs(title="Looks to target (imm. or delayed)",y="Proportion looks to target",x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_propTargetXVocabXGroup.png",width=3.5, height=3.5, units="in",res=300)

## looks (immediate or delayed) by vocab size and group (only good trials)
ggplot(subset(pf.lookcounts.bysubj,lookType=="Imm"), aes(x=WordsProduced, y=(totalLooks/totalGoodTrials),color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title="Looks to target (imm. or delayed)",y="Proportion looks to target",x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_propTargetXVocab_goodtrials.png",width=3, height=3, units="in",res=300)


### conds
# Near vs. Far, by Group
pf.distance = ddply(pf.conds, .(Subj,Distance),summarise,trials=sum(trials),looked=sum(looked),notLooked=sum(notLooked))
pf.distance = droplevels(merge(pf.distance,subjInfo))

summary(glm(cbind(looked,trials-looked)~Distance * Group, data=pf.distance,family="binomial"))

ggplot(pf.distance, aes(x=Distance, color=Group, y=looked/trials)) + 
  geom_boxplot() + theme_bw() + labs(title="Looks (imm. or delayed) \nby distance", y="Proportion looks to target") +
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_propLooksXDistanceXGroup.png",width=3.5,height=3.5,units="in",res=200)

##### Near vs. Far #####
# groups, all trials
ggplot(pf, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Group: Looks by distance",y="Proportion of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXGroupXDistance.png", width=4, height=3, units="in",res=400)

# trials with successful eye contact
ggplot(pf.good, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Group: Looks by distance (good trials only)",y="Proportion of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXGroupXDistance_good.png", width=4, height=3, units="in",res=400)

# vocab, all trials
ggplot(pf, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~VocabGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Vocab size: Looks by distance",y="Number of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXVocab-allXDistance.png", width=3.5, height=3, units="in",res=400)

# vocab, trials with successful eye contact
ggplot(pf.good, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~VocabGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Vocab size: Looks by distance (good trials only)",y="Number of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXVocab-allXDistance_good.png", width=3.5, height=3, units="in",res=400)

## compare to order of presentation
ggplot(pf, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~PF_Order) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots +
  labs(title="Looks by order and distance",y="Proportion of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXOrderXDistance.png", width=3.5, height=3, units="in",res=400)

## proportion looks of each type, averaged by subject
xtabs(~look1 + SubjID,data=pf)
pf.avg = with(pf, aggregate(look1, list("LookType"=look1,"Subj"=SubjID), length))
pf.total = with(pf, aggregate(look1, list("Subj"=SubjID), length))
colnames(pf.total)[2] <- "totalTrials"
pf.avg = merge(pf.avg, pf.total)
pf.avg$Prop = pf.avg$x/pf.avg$totalTrials
pf.avg = merge(pf.avg, subjInfoBasic)

ggplot(pf.avg, aes(x=Subj, y=Prop, fill=LookType)) + 
  facet_grid(.~ageGroup, scale="free") +
  geom_bar(stat="identity")


### plots with conds ####
# Separation X Distance, proportion (removed center targets)
ggplot(subset(pf,!(is.na(Separation))&Position!="center"), aes(x=Separation,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~Distance) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation & Distance",y="Proportion of looks",x="Separation",fill="Child's look") + 
  theme(legend.position = "none")
dev.print(png, file="Results_10-27-14/PF_looktypesXSeparationXDistance.png",width=2.6, height=3, units="in", res=300)

# Separation X Distance X Group, proportion (removed center targets)
ggplot(subset(pf,!(is.na(Separation))&Position!="center"), aes(x=Separation,fill=look)) +
  geom_bar(position="fill") + facet_grid(Distance~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation & Distance",y="Proportion of looks",x="Separation",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXSeparationXDistanceXGroup.png",width=3, height=4, units="in", res=300)

ggplot(subset(pf.good,!(is.na(Separation))&Position!="center"), aes(x=Separation,fill=look)) +
  geom_bar(position="fill") + facet_grid(Distance~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation & Distance (good trials)",y="Proportion of looks",x="Separation",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXSeparationXDistanceXGroup_good.png",width=3, height=4, units="in", res=300)



# Separation X Relative position (including center targets)
ggplot(subset(pf,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~Separation) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation & Rel. Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXSeparation.png",width=3, height=3, units="in", res=300)

# Separation X Relative position X Group (including center targets) 
ggplot(subset(pf,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(ageGroup~Separation) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation and Relative Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXSeparationXGroup.png",width=3, height=4, units="in", res=300)

ggplot(subset(pf.good,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(Separation~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation and Relative Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXSeparationXGroup_good.png",width=3, height=4, units="in", res=300)

# Distance X Relative position (including center targets)
ggplot(subset(pf,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~Distance) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Distance and Rel. Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXDistance.png",width=3, height=3, units="in", res=300)

# Distance X Relative position X Group (including center targets) 
ggplot(subset(pfLabeled,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(ageGroup~Distance) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Distance and Relative Position",y="Proportion of looks",x="Relative Position",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXDistanceXGroup.png",width=3, height=4, units="in", res=300)

ggplot(subset(pf.good,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(Distance~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Distance and Relative Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXDistanceXGroup_good.png",width=3, height=4, units="in", res=300)


#Vocab
ggplot(subset(pf.good,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~VocabGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Distance & Rel. Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXVocabGroup_good.png",width=2.6, height=3, units="in", res=300)

## animacy
ggplot(pf, aes(x=Animacy,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Group: Looks by Animacy",y="Proportion of looks",x="Distance",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXGroupXAnimacy.png", width=2.6, height=3, units="in",res=400)

ggplot(pf.good, aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(Animacy~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Group: Looks by Animacy & Rel. Pos.",y="Proportion of looks",x="Distance",fill="Child's look") 
dev.print(png, file="Results_10-27-14/PF_looktypesXGroupXAnimacy_good.png", width=4, height=4, units="in",res=400)


#### STATS ####

pf$goodtrial = as.numeric(pf$lookF!="NoEyeContact")
contrasts(pf$Group) <- contr.helmert(2)
summary(glm(goodtrial~Group*WordsProduced, data=pf, family="binomial"))

pf$Imm = as.numeric(pf$lookF=="Imm")
summary(glm(Imm~Group*WordsProduced, data=pf, family="binomial"))
summary(glm(Imm~WordsProduced, data=subset(pf, Group=="TD"), family="binomial")) #significant relationship for TD
summary(glm(Imm~WordsProduced, data=subset(pf, Group=="WS"), family="binomial")) #no relationship for WS

pf$ImmDel = as.numeric(pf$lookF %in% c("Imm","Delay"))
summary(glm(ImmDel~Group*WordsProduced, data=pf, family="binomial"))

pf.lookcounts.bysubj = droplevels(pf.lookcounts.bysubj)
contrasts(pf.lookcounts.bysubj$Group)<-contr.helmert(2)
summary(lm(WordsProduced ~ (totalGoodTrials/totalTrials)*Group,data=subset(pf.lookcounts.bysubj,lookType=="Imm")))


