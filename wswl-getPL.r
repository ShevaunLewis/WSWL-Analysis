## Social Word Learning analysis
## get PL codes, combine with subjInfo

# created 4/20/2015

setwd("/Volumes/landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results")
load("ROutput/wswl-subjInfo.Rda")

#### Import data ####
parentLabelCodesFile = "DataSpreadsheets/ParentLabelCodes_wordcounts.csv"
parentLabelCodes = read.csv(parentLabelCodesFile, header=T, colClasses = "character")

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
pl.uttTypes.bySubj = droplevels(merge(pl.uttTypes.bySubj, pl.totalUtts.bySubj))

pl.category.bySubj = ddply(droplevels(subset(pl,category!="unseen")), .(Subj,category), nrow, .drop=F)
colnames(pl.category.bySubj)[3] <- "catTotal"
pl.totalSeen.bySubj = ddply(droplevels(subset(pl,category!="unseen")), .(Subj), nrow)
colnames(pl.totalSeen.bySubj)[2] <- "totalSeen"
pl.category.bySubj = merge(pl.category.bySubj, pl.totalSeen.bySubj)
pl.category.bySubj = droplevels(merge(pl.category.bySubj, subjInfo, all.x=T))

pl.category.bySubj.wide = reshape(pl.category.bySubj[,1:4], timevar="category",idvar=c("Subj","totalSeen"),direction="wide")
colnames(pl.category.bySubj.wide)[3:5] <- c("totalDiscrepant","totalFollowin","totalSharedAttn")

#add total seen utterances to pl.totalUtts.bySubj
pl.totalUtts.bySubj = merge(pl.totalUtts.bySubj, pl.totalSeen.bySubj, all.x=T)
pl.totalUtts.bySubj$propSeen = pl.totalUtts.bySubj$totalSeen/pl.totalUtts.bySubj$totalUtts
pl.totalUtts.bySubj = droplevels(merge(pl.totalUtts.bySubj,subjInfo,all.x=T))

#count words in target utterances by subject
pl$wordcount_transcriptF = as.numeric(pl$wordcount_transcriptF)
pl.wordcount.bysubj = ddply(pl, .(Subj), summarise, mlu=mean(wordcount_transcriptF), totalWords=sum(wordcount_transcriptF))

pl.bySubj = ddply(pl, .(Subj,uttType,category),nrow, .drop=F)
colnames(pl.bySubj)[4] <- "num"
pl.bySubj = merge(pl.bySubj, pl.category.bySubj)
pl.bySubj = merge(pl.bySubj, pl.uttTypes.bySubj)
pl.bySubj = merge(pl.bySubj, pl.wordcount.bysubj)

#### Save data, clear environment ####
save(parentLabelCodes, pl, pl.uttTypes.bySubj, pl.totalUtts.bySubj, pl.category.bySubj, pl.category.bySubj.wide, pl.bySubj, file="ROutput/wswl-pl.Rda")
rm(list=ls())

