##### Social Word Learning Analysis
##### Combining information from parent labeling and word learning task
##### Created 8/20/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
load("ROutput/wswl-pl.Rda")
load("ROutput/wswl-wl.Rda")

#need to update content of wswl-WL to include relevant summary data frames

# Make data frame that contains, for each subject: subjInfo, parent labels of each type, correct and incorrect for each WL condition
#wl.novel.bysubj.wide


wl.pl = merge(wl.novel.bysubj, pl.category.bySubj.wide)
pl.wl = merge(pl.category.bySubj, wl.novel.bysubj.wide)
wl.pl.wide = merge(wl.novel.bysubj.wide, pl.category.bySubj.wide)
wl.pl.wide$totalCorrect = with(wl.pl.wide, discrepantCorrect+followinCorrect+jointCorrect)
wl.pl.wide$totalWL = with(wl.pl.wide, discrepantCorrect+followinCorrect+jointCorrect+discrepantIncorrect+followinIncorrect+jointIncorrect)
wl.pl.wide$propCorrect = with(wl.pl.wide, totalCorrect/totalWL)
wl.pl.wide$propJoint = with(wl.pl.wide, jointCorrect/(jointCorrect+jointIncorrect))
wl.pl.wide = merge(wl.pl.wide, pl.totalUtts.bySubj, all.x=T)

wl.pl.td = droplevels(subset(wl.pl, ageGroup %in% c("18M","24M")))
pl.wl.td = droplevels(subset(pl.wl, ageGroup %in% c("18M","24M")))
wl.pl.wide.td = droplevels(subset(wl.pl.wide, ageGroup %in% c("18M","24M")))

wl.pl.vocab.lm = lm(WordsProduced ~ ageV1Mos + followinCorrect + jointCorrect + discrepantCorrect + totalSharedAttn+totalFollowin+totalDiscrepant, data=wl.pl.wide.td)
anova(wl.pl.vocab.lm)
wl.pl.vocab.lm2 = lm(WordsProduced ~ ageV1Mos + propCorrect + totalSharedAttn+totalFollowin+totalDiscrepant, data=wl.pl.wide.td)
anova(wl.pl.vocab.lm2)
wl.pl.vocab.lm3= lm(WordsProduced ~ ageV1Mos + propCorrect + totalUtts, data=wl.pl.wide.td)
anova(wl.pl.vocab.lm3)
wl.pl.vocab.lm4 = lm(WordsProduced ~ ageV1Mos + followinCorrect + jointCorrect + discrepantCorrect + totalUtts, data=wl.pl.wide.td)
anova(wl.pl.vocab.lm4)
wl.pl.vocab.lm5 = lm(WordsProduced ~ ageV1Mos + jointCorrect, data=wl.pl.wide.td)
anova(wl.pl.vocab.lm5)

wl.jointacc.pl.glm = glm(cbind(jointCorrect, jointIncorrect) ~ ageV1Mos * WordsProduced * totalSharedAttn, data=wl.pl.wide.td, family="binomial")
summary(wl.jointacc.pl.glm)
wl.jointacc.pl.glm2 = glm(cbind(jointCorrect, jointIncorrect) ~ ageV1Mos + WordsProduced + totalUtts, data=wl.pl.wide.td, family="binomial")
summary(wl.jointacc.pl.glm2)
wl.acc.pl.glm = glm(cbind(totalCorrect, (totalWL-totalCorrect)) ~ ageV1Mos * WordsProduced + totalSharedAttn, data=wl.pl.wide.td, family="binomial")
summary(wl.acc.pl.glm)

anova(wl.pl.vocab.lm, wl.pl.vocab.lm3)
## Do parents tailor their input for children's word-learning abilities?
# Is the number of shared attention labels sensitive to success on teh joint attention and discrepant conditions?
anova(lm(totalUtts ~ ageV1Mos * WordsProduced + propCorrect, wl.pl.wide.td))
anova(lm(totalSharedAttn ~ ageV1Mos * WordsProduced + jointCorrect, wl.pl.wide.td))
anova(lm(totalUtts ~ ageV1Mos + jointCorrect + discrepantCorrect + followinCorrect, wl.pl.wide.td))

anova(lm(totalSharedAttn ~ ageV1Mos + WordsProduced + totalWL, wl.pl.wide.td))

