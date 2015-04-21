##### Social Word Learning Analysis
##### Created 7/24/2014

library(plyr)

#### Import data ####
# data directory and file names
setwd("/Volumes/landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/DataSpreadsheets")
subjInfoFile = "SubjInfo.csv"
parentLabelCodesFile = "ParentLabelCodes_wordcounts.csv"
mullenFile = "Mullen-raw.csv"
cdiFile = "CDI-raw.csv"
wordLearningFile = "WordLearning-paperCodes.csv"
pfFile = "PFcodes_4-20.csv"
pfTrialsFile = "PF_Design.csv"

## Import data from .csv files
subjInfo = read.csv(subjInfoFile, header=T)
parentLabelCodes = read.csv(parentLabelCodesFile, header=T, colClasses = "character")
mullen = read.csv(mullenFile,header=T)
cdi = read.csv(cdiFile, header=T)
wl = read.csv(wordLearningFile, header=T)
pf = read.csv(pfFile, header=T)
pfTrials = read.csv(pfTrialsFile, header=T)

#### Subject Info ####

# Fix dates
subjInfo$date1 = as.Date(as.character(subjInfo$date1), "%m/%d/%y")
subjInfo$date2 = as.Date(as.character(subjInfo$date2), "%m/%d/%y")
subjInfo$dob = as.Date(as.character(subjInfo$dob), "%m/%d/%y")
subjInfo$WL_date = as.Date(as.character(subjInfo$WL_date), "%m/%d/%y")
subjInfo$PF_date = as.Date(as.character(subjInfo$PF_date), "%m/%d/%y")
subjInfo$PL_date = as.Date(as.character(subjInfo$PL_date), "%m/%d/%y")
subjInfo$CDI_date = as.Date(as.character(subjInfo$CDI_date), "%m/%d/%y")
subjInfo$Mullen_date = as.Date(as.character(subjInfo$Mullen_date), "%m/%d/%y")


# Calculate age, add age groups
subjInfo$ageV1Mos = round(as.integer(subjInfo$date1 - subjInfo$dob)/365*12, digits=1)
summary(subjInfo$ageV1Mos)
subjInfo$ageGroup = as.factor(ifelse(grepl("Pilot",subjInfo$Group),as.character(subjInfo$Group),
                                     ifelse(subjInfo$Group=="TD", ifelse(subjInfo$ageV1Mos<21, "18M", 
                                                                         ifelse(subjInfo$ageV1Mos>27,"30M","24M")),"WS")))

# make data frame with just basic subject info, no dates
subjInfoBasic = subjInfo[,c("Subj","Sex","Group","ageV1Mos","ageGroup")]
subjInfoAll = subjInfo

#### CDI ####

# add WordsProduced to subject info
vocab = merge(subjInfo, cdi[,c("Subj","WordsProduced")], all.x=T)
vocab = vocab[,c("Subj","Group","ageGroup","WordsProduced")]

# median vocabulary for different subsets
vocab$VocabSplitAll = with(vocab, ifelse(WordsProduced < median(WordsProduced[!(Group%in%c("Pilot-TD","Pilot-WS"))], na.rm=T), "low","high"))
vocab$VocabSplitTD = with(vocab, ifelse(WordsProduced < median(WordsProduced[Group=="TD"], na.rm=T), "low","high"))
vocab$VocabSplitWS = with(vocab, ifelse(WordsProduced < median(WordsProduced[Group=="WS"], na.rm=T), "low", "high"))

# make column that specifies high/low vocab relative to the age group (TD18, TD24, WS) of the subject
vocab$VocabSplitByGroup = with(vocab, ifelse(ageGroup=="18M",
                                              ifelse(WordsProduced < median(WordsProduced[ageGroup=="18M"], na.rm=T), "low", "high"),
                                              ifelse(ageGroup=="24M",
                                                     ifelse(WordsProduced < median(WordsProduced[ageGroup=="24M"], na.rm=T), "low", "high"),
                                                     VocabSplitWS)))
#### Mullen ####
colnames(mullen) <- c("Subj","mullenGM","mullenVR","mullenFM","mullenRL","mullenEL")

## add CDI and Mullen scores to subject info
subjInfo = merge(subjInfoBasic, mullen, all.x=T)
subjInfo = merge(subjInfo, vocab, all.x=T)

#### Parent labels ####

# Make a new data frame to store just one set of codes per label
pl = parentLabelCodes
# Add new columns for the codes to use for analysis. Use final, coder2, or coder1 codes in 
# that order of preference.
attach(pl)
pl$transcript = ifelse(transcript2=="", transcript1, ifelse(transcriptF=="", transcript2, transcriptF))
pl$object = ifelse(object2=="", object1, ifelse(objectF=="", object2, objectF))
pl$numObjs = ifelse(numObjs2=="", numObjs1, ifelse(numObjsF=="", numObjs2, numObjsF))
pl$uttType = ifelse(uttType2=="", uttType1, ifelse(uttTypeF=="", uttType2, uttTypeF))
pl$category = ifelse(category2=="", category1, ifelse(categoryF=="", category2, categoryF))
pl$code = ifelse(transcript2=="", 1, ifelse(transcriptF=="", 2, 3))
detach(pl)

pl = pl[,c(1:6,22:28)]

## data frames for looking at utterance types, categories
pl$Subj = as.factor(pl$Subj)
pl.uttTypes.bySubj = ddply(pl, .(Subj,uttType), nrow, .drop=F)
colnames(pl.uttTypes.bySubj)[3] <- "typeTotal"
pl.totalUtts.bySubj = ddply(pl, .(Subj), nrow)
colnames(pl.totalUtts.bySubj)[2] <- "totalUtts"
pl.uttTypes.bySubj = merge(pl.uttTypes.bySubj, pl.totalUtts.bySubj)

pl.category.bySubj = ddply(droplevels(subset(pl,category!="unseen")), .(Subj,category), nrow, .drop=F)
colnames(pl.category.bySubj)[3] <- "catTotal"
pl.totalSeen.bySubj = ddply(droplevels(subset(pl,category!="unseen")), .(Subj), nrow)
colnames(pl.totalSeen.bySubj)[2] <- "totalSeen"
pl.category.bySubj = merge(pl.category.bySubj, pl.totalSeen.bySubj)
pl.category.bySubj = merge(pl.category.bySubj, subjInfo, all.x=T)

pl.category.bySubj.wide = reshape(pl.category.bySubj[,1:4], timevar="category",idvar=c("Subj","totalSeen"),direction="wide")
colnames(pl.category.bySubj.wide)[3:5] <- c("totalDiscrepant","totalFollowin","totalSharedAttn")

#add total seen utterances to pl.totalUtts.bySubj
pl.totalUtts.bySubj = merge(pl.totalUtts.bySubj, pl.totalSeen.bySubj, all.x=T)
pl.totalUtts.bySubj$propSeen = pl.totalUtts.bySubj$totalSeen/pl.totalUtts.bySubj$totalUtts
pl.totalUtts.bySubj = merge(pl.totalUtts.bySubj,subjInfo,all.x=T)

#count words in target utterances by subject
pl$wordcount_transcriptF = as.numeric(pl$wordcount_transcriptF)
pl.wordcount.bysubj = ddply(pl, .(Subj), summarise, mlu=mean(wordcount_transcriptF), totalWords=sum(wordcount_transcriptF))

pl.bySubj = ddply(pl, .(Subj,uttType,category),nrow, .drop=F)
colnames(pl.bySubj)[4] <- "num"
pl.bySubj = merge(pl.bySubj, pl.category.bySubj)
pl.bySubj = merge(pl.bySubj, pl.uttTypes.bySubj)
pl.bySubj = merge(pl.bySubj, pl.wordcount.bysubj)


#### Word Learning ####
wl$Target = as.character(wl$Target)
wl$Response = as.character(wl$Response)
wl$acc = ifelse(wl$Response==wl$Target,1,0)

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
wl.item.acc = ddply(wl, .(Subj,Item,Cond,TrialType,Target), summarise, correct=sum(acc))
wl.items = merge(wl.item.acc, wl.item.responded)
wl.items = merge(wl.items,subjInfo,all.x=T)

#### How many comprehension trials did each subject finish? ####
wl.trialsfinished = as.data.frame(xtabs(~Subj, data=wl.response))
colnames(wl.trialsfinished)[2] = "totalCompTrials"
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


#### Point Following ####
pf = merge(pf, subjInfoAll[,c("Subj","PF_date","PF_list","PF_Order")], all.x=TRUE)

## If final code is not yet available, use code2
pf$lookF = ifelse(pf$lookF=="",as.character(pf$look2),as.character(pf$lookF))
pf$directionF = ifelse(pf$directionF=="",as.character(pf$direction2),as.character(pf$directionF))
pf$distanceF = ifelse(pf$distanceF=="",as.character(pf$distance2),as.character(pf$distanceF))

## Fix column names:
colnames(pf)[19] <- "List"
colnames(pf)[17] <- "Distance"

## make single column for point direction, incorporating repeat codes (no longer necessary since directionF is available)
# pf$directionCalc = with(pf, ifelse(as.character(direction1)==as.character(direction2), as.character(direction1), 
#                                    ifelse(as.character(direction2) %in% c("LeftRep","RightRep"), as.character(direction2), 
#                                           as.character(direction1))))


## add trial num for non-repeat points
pfSetTrials = function(df) {
  trials = droplevels(subset(pfTrials, List==as.character(df$List[1])&Distance==as.character(df$Distance[1])))
  df = df[order(df$pointNum),]
  df$attempt = "only"
  trialcount = 1
  for (rownum in 1:nrow(df)) {
    dir = as.character(df[rownum,"directionF"])
    if (dir %in% c("LeftRep","RightRep")) {
      trialnum = df[rownum-1,"Trial"]
      df[rownum,"Trial"] <- trialnum
      df[rownum,"attempt"] <- "rep"
      df[df$Trial==trialnum & df$attempt=="only" & grepl(substr(dir,1,4),df$directionF), "attempt"] <- "first"
    }    
    else {
      df[rownum,"Trial"] <- trials[trialcount,"Trial"]
      trialcount = trialcount + 1
    }
  }
  return(df)
}

pfSetAllTrials = function(df) {
  df$Trial = 0
  dfout = 0
  for (subj in levels(df$Subj)) {
    near = subset(df,Subj==subj&Distance=="Near")
    far = subset(df,Subj==subj&Distance=="Far")
    near1 = pfSetTrials(near)
    far1 = pfSetTrials(far)
    if (is.data.frame(dfout)) {
      dfout = rbind(dfout, near1, far1)
    }
    else {
      dfout = rbind(near1,far1)
    }
  }
  dfout$PositionRel = ifelse(grepl("Left",dfout$directionF),"left","right")
  dfout = merge(dfout,pfTrials,all.x=TRUE)
  dfout = dfout[order(dfout$Subj),]
  return(dfout)
}

pf = pfSetAllTrials(pf)
pf$Separation = as.factor(pf$Separation)

pfFull = pf
pf = pf[,c("Subj","List","PF_Order","tagger","coder1","coder2","tagnote",
           "pointNum","attempt","Trial","Distance","PositionRel","Separation","Position","Animacy","Set",
           "directionF","lookF")]
pf = droplevels(subset(pf,lookF!="DISCUSS"))
#pf$look = ordered(pf$lookF, levels=c("Imm","Delay","NoTarget","NoEyeContact"), labels=c("Imm","Delay","NoTarget","NoEyeContact"))
pf = merge(pf, subjInfo, all.x=T)
pf.good = droplevels(subset(pf,lookF!="NoEyeContact"&attempt=="only"))

## Count of label types by subject
pf.lookcounts.bysubj = ddply(subset(pf,attempt!="rep"), .(Subj,lookF), nrow, .drop=F)
colnames(pf.lookcounts.bysubj)[3] <- "num"

pf.bysubj.totalTrials = ddply(pf.lookcounts.bysubj, .(Subj), summarise, totalTrials=sum(num))
pf.bysubj.totalLooks = ddply(subset(pf.lookcounts.bysubj, lookF %in% c("Imm","Delay")), .(Subj), summarise, totalLooks=sum(num))
pf.bysubj.totalGoodTrials = ddply(subset(pf.lookcounts.bysubj, lookF!="NoEyeContact"), .(Subj), summarise, totalGoodTrials=sum(num))

pf.lookcounts.bysubj = merge(pf.lookcounts.bysubj, pf.bysubj.totalTrials)
pf.lookcounts.bysubj = merge(pf.lookcounts.bysubj, pf.bysubj.totalLooks)
pf.lookcounts.bysubj = merge(pf.lookcounts.bysubj, pf.bysubj.totalGoodTrials)

pf.lookcounts.bysubj.wide = reshape(pf.lookcounts.bysubj, idvar=c("Subj","totalTrials","totalLooks","totalGoodTrials"),timevar="lookF", direction="wide")

pf.lookcounts.bysubj = merge(pf.lookcounts.bysubj, subjInfo, all.x=T)
pf.lookcounts.bysubj.wide = merge(pf.lookcounts.bysubj.wide, subjInfo, all.x=T)

#### Save data, clear environment ####
setwd("../ROutput")
save(subjInfo, subjInfoBasic, subjInfoAll, file="wswl-subjInfo.Rda")
save(parentLabelCodes, pl, pl.uttTypes.bySubj, pl.totalUtts.bySubj, pl.category.bySubj, pl.category.bySubj.wide, pl.bySubj, file="wswl-pl.Rda")
save(pf, pfFull, pf.good, pf.lookcounts.bysubj,pf.lookcounts.bysubj.wide, file="wswl-pf.Rda")
save(wl, wl.badsubj, wl.familiar.bysubj,wl.item.acc, wl.item.responded, wl.items, wl.items.good, wl.response, wl.trialsfinished, file="wswl-wl.Rda")
write.csv(subjInfo,file="subjDemographics.csv",row.names=FALSE)
rm(list=ls())
setwd("..")
