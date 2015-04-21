## Social Word Learning analysis
## get subject info, mullen, cdi and combine. 

# created 4/20/2015

#### Import data ####
# data directory and file names
setwd("/Volumes/landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/DataSpreadsheets")
subjInfoFile = "SubjInfo.csv"
mullenFile = "Mullen-raw.csv"
cdiFile = "CDI-raw.csv"

## Import data from .csv files
subjInfo = read.csv(subjInfoFile, header=T)
mullen = read.csv(mullenFile,header=T)
cdi = read.csv(cdiFile, header=T)

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
mullen$mullenGM = as.numeric(as.character(mullen$mullenGM))
mullen$mullenVR = as.numeric(as.character(mullen$mullenVR))
mullen$mullenFM = as.numeric(as.character(mullen$mullenFM))
mullen$mullenRL = as.numeric(as.character(mullen$mullenRL))
mullen$mullenEL = as.numeric(as.character(mullen$mullenEL))

## add CDI and Mullen scores to subject info
subjInfo = merge(subjInfoBasic, mullen, all.x=T)
subjInfo = merge(subjInfo, vocab, all.x=T)

#### Save data, clear environment ####
setwd("../ROutput")
save(subjInfo, subjInfoBasic, subjInfoAll, file="wswl-subjInfo.Rda")
write.csv(subjInfo,file="subjDemographics.csv",row.names=FALSE)

rm(list=ls())
setwd("..")

