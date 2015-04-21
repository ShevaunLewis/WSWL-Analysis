## Social Word Learning analysis
## get WL codes, combine with subjInfo

# created 4/20/2015

library(plyr)

setwd("/Volumes/landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results")

load("ROutput/wswl-subjInfo.Rda")

#### Import data ####
wordLearningFile = "DataSpreadsheets/WordLearning-paperCodes.csv"
wl = read.csv(wordLearningFile, header=T)

#### Word Learning ####
wl$Target = as.character(wl$Target)
wl$Response = as.character(wl$Response)
wl$acc = ifelse(wl$Response==wl$Target,1,0)
wl$notwrong = ifelse(wl$Response==wl$Target|wl$Response=="both",1,0)

# Comprehension trial types
comptrials = data.frame(CompTrial=c(1,2,3,4,5), TrialType=c("novel","familiar","novel","familiar","novel"))
wl = merge(wl,comptrials,all.x=T)
wl$TrialType = as.factor(ifelse(wl$Cond=="Practice","familiar",as.character(wl$TrialType)))
wl$TrialType = as.factor(wl$TrialType)

wl = merge(subjInfo,wl,all=T)
wl = droplevels(subset(wl, Group %in% c("TD","WS")))

wl.response = subset(wl,!(Response%in%c("na","nr","nc")))

#### acc by subject AND item
wl.item.responded = ddply(wl.response, .(Subj,Item,Cond,TrialType,Target), nrow)
colnames(wl.item.responded)[6] ="trialsresponded"
wl.item.acc = ddply(wl, .(Subj,Item,Cond,TrialType,Target), summarise, correct=sum(acc), notwrong=sum(notwrong))
wl.items = merge(wl.item.acc, wl.item.responded)
wl.items = merge(wl.items,subjInfo,all.x=T)

#### How many comprehension trials did each subject finish? ####
wl.trialsfinished = as.data.frame(xtabs(~Subj, data=wl.response))
colnames(wl.trialsfinished)[2] = "totalCompTrials"
wl.trialsfinished = droplevels(merge(wl.trialsfinished,subjInfo))
wl.lowtrials = subset(wl.trialsfinished, totalCompTrials<10)

wl.familiar.bysubj = ddply(droplevels(subset(wl.items, TrialType=="familiar")), .(Subj),
                           summarise, acc.grand = (sum(correct)/sum(trialsresponded)), acc.byitem = mean(correct/trialsresponded),
                           totalCorrect = sum(correct), totalResponded = sum(trialsresponded))

wl.lowacc = subset(wl.familiar.bysubj, acc.byitem<0.65)
wl.badsubj = merge(wl.lowtrials, wl.lowacc,all=T)

wl.items.good = droplevels(subset(wl.items, !(Subj %in% wl.badsubj$Subj)))

# wl.novel.bysubj = ddply(wl.novel.responded, .(Subj, Cond, acc), .drop=F, nrow)
# colnames(wl.novel.bysubj)[4] = "num"
# wl.novel.bysubj = reshape(wl.novel.bysubj, timevar="acc", idvar=c("Subj","Cond"),direction="wide")
# colnames(wl.novel.bysubj)[3:4] = c("numIncorrect","numCorrect")
# wl.novel.bysubj.wide = reshape(wl.novel.bysubj, timevar="Cond",idvar="Subj", direction="wide")
# colnames(wl.novel.bysubj.wide)[2:7] <- c("discrepantIncorrect","discrepantCorrect","followinIncorrect","followinCorrect","jointIncorrect","jointCorrect")
# wl.novel.bysubj = merge(wl.novel.bysubj, subjInfo, all.x=T)
# wl.novel.bysubj.wide = merge(wl.novel.bysubj.wide, subjInfo, all.x=T)

#### Save data, clear environment ####
save(wl, wl.badsubj, wl.familiar.bysubj,wl.item.acc, wl.item.responded, wl.items, wl.items.good, wl.response, wl.trialsfinished, file="ROutput/wswl-wl.Rda")
rm(list=ls())


