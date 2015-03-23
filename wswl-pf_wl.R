##### Social Word Learning Analysis
##### Point following and Word Learning
##### Created 1/29/2015
## udpated 3/17/15


setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")

load("ROutput/wswl-pf.Rda")
load("ROutput/wswl-wl.Rda")

#source("WSWL-Analysis/wswl-wl.r")
#load("ROutput/wl-TD-means.Rda")

head(pf.lookcounts.bysubj.wide)
#head(wl.good.exp.bysubj.td)

wl.td.means.wide = reshape(wl.good.exp.bysubj.td, timevar="Cond",idvar = "Subj", drop=c(colnames(wl.good.exp.bysubj.td)[c(3,4,7:23)]), direction="wide")

wl.pf = merge(wl.items.good, pf.lookcounts.bysubj.wide)

wl.pf.td = droplevels(subset(wl.pf, ageGroup %in% c("18M","24M") & TrialType=="novel" & Cond!="Practice"))
contrasts(wl.pf.td$Cond) = contr.sum(3)

summary(glm(cbind(correct, (trialsresponded-correct)) ~ Cond * totalLooks, data=wl.pf.td, family="binomial"))
summary(glm(cbind(correct, (trialsresponded-correct)) ~ Cond * totalGoodTrials, data=wl.pf.td, family="binomial"))
summary(glm(cbind(correct, (trialsresponded-correct)) ~ Cond * WordsProduced, data=wl.pf.td, family="binomial"))



summary(glm(cbind(correct, (trialsresponded-correct)) ~ Cond + num.Imm, data=wl.pf.td, family="binomial"))

summary(glmer(cbind(correct, (3-correct)) ~ Cond * totalGoodTrials + (1|Subj), data=wl.pf.td, family="binomial"))
summary(glmer(cbind(correct, (3-correct)) ~ Cond * WordsProduced + (1|Subj), data=wl.pf.td, family="binomial"))
summary(glmer(cbind(correct, (3-correct)) ~ Cond * totalGoodTrials + WordsProduced + Cond:WordsProduced + (1|Subj), data=wl.pf.td, family="binomial"))
summary(glmer(cbind(correct, (3-correct)) ~ Cond + totalGoodTrials + WordsProduced + (1|Subj), data=wl.pf.td, family="binomial"))
anova(glm(cbind(correct, (3-correct)) ~ Cond + totalGoodTrials + WordsProduced, data=wl.pf.td, family="binomial"), test="Chisq")
anova(glm(cbind(correct, (3-correct)) ~ Cond + WordsProduced + totalGoodTrials, data=wl.pf.td, family="binomial"), test="Chisq")
anova(glm(cbind(correct, (3-correct)) ~ WordsProduced + totalGoodTrials + Cond, data=wl.pf.td, family="binomial"), test="Chisq")

anova(glm(cbind(correct, (trialsresponded-correct)) ~ Cond * totalGoodTrials  + num.Imm, data=wl.pf.td, family="binomial"), test="Chisq")


wl.pf.td.fi = subset(wl.pf.td, Cond=="FollowIn")
wl.pf.td.ja = subset(wl.pf.td, Cond=="Joint")
wl.pf.td.d = subset(wl.pf.td, Cond=="Discrepant")

summary(glm(cbind(correct, (trialsresponded-correct)) ~ ageV1Mos + WordsProduced + totalLooks, data=wl.pf.td.fi, family="binomial"))
summary(glm(cbind(correct, (trialsresponded-correct)) ~ totalGoodTrials, data=wl.pf.td.ja, family="binomial"))
summary(glm(cbind(correct, (trialsresponded-correct)) ~ totalGoodTrials, data=wl.pf.td.d, family="binomial"))

head(wl.pf.td.fi)


#### plot ####
wl.pf.td.means = merge(wl.good.exp.bysubj.td, pf.lookcounts.bysubj.wide)

ggplot(wl.pf.td.means, aes(x=(totalGoodTrials/totalTrials), y=acc.byitem.strict)) + 
  geom_point() + geom_smooth(method="lm",se=F) + facet_wrap(~Cond) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Word learning and point following",y="Average accuracy",x="# good trials") +
  wswl.posterplots

