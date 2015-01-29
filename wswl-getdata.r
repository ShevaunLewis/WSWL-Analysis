##### Social Word Learning Analysis
##### Created 7/24/2014

#### Import data ####
# data directory and file names
setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results")
#setwd("/Users/Shevaun/Desktop/Results")
subjInfoFile = "SubjInfo.csv"
parentLabelCodesFile = "ParentLabelCodes.csv"
mullenFile = "Mullen-raw.csv"
cdiFile = "CDI-raw.csv"
wordLearningFile = "WordLearning-paperCodes.csv"
pfFile = "PFcodes.csv"
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

# Calculate age, add age groups
subjInfo$ageV1Mos = round(as.integer(subjInfo$date1 - subjInfo$dob)/365*12, digits=1)
summary(subjInfo$ageV1Mos)
subjInfo$ageGroup = as.factor(ifelse(grepl("Pilot",subjInfo$Group),as.character(subjInfo$Group),
                                     ifelse(subjInfo$Group=="TD", ifelse(subjInfo$ageV1Mos<21, "18M", 
                                                                         ifelse(subjInfo$ageV1Mos>27,"30M","24M")),"WS")))

# remove unnecessary columns
subjInfo = subjInfo[,c(1,4:12,15:16)]
subjInfoBasic = subjInfo

#### CDI ####

# Fix dates
cdi$Date = as.Date(as.character(cdi$Date), "%m/%d/%y")

# add WordsProduced to subject info
subjInfo = merge(subjInfo, cdi[,c("SubjID","WordsProduced")], all.x=T)

# median vocabulary
subjInfo$VocabGroup = ifelse(subjInfo$WordsProduced < median(subjInfo$WordsProduced[!(subjInfo$Group%in%c("Pilot-TD","Pilot-WS"))], na.rm=T), "low","high")
subjInfo$VocabGroupTD = ifelse(subjInfo$WordsProduced < median(subjInfo$WordsProduced[subjInfo$Group=="TD"], na.rm=T), 
                               "low","high")
# make column for high/low vocab for WS kids
subjInfo$VocabGroupWS = ifelse(subjInfo$WordsProduced < median(subjInfo$WordsProduced[subjInfo$Group=="WS"], na.rm=T), "low", "high")

# make column that specifies high/low vocab relative to the age group (TD18, TD24, WS) of the subject
subjInfo$VocabByGroup = with(subjInfo, ifelse(ageGroup=="18M",
                                              ifelse(WordsProduced < median(WordsProduced[ageGroup=="18M"], na.rm=T), "low", "high"),
                                              ifelse(ageGroup=="24M",
                                                     ifelse(WordsProduced <= median(WordsProduced[ageGroup=="24M"], na.rm=T), "low", "high"),
                                                     VocabGroupWS)))
#### Mullen ####

# add raw scores to subject info
mullen$Mullen_date = as.Date(as.character(mullen$Mullen_date), "%m/%d/%y")
colnames(mullen) <- c("SubjID","Date","mullenGM","mullenVR","mullenFM","mullenRL","mullenEL")
subjInfo = merge(subjInfo, mullen, all.x=T)

#### Parent labels ####

# Make a new data frame to store just one set of codes per label
parentLabels = parentLabelCodes
# Add new columns for the codes to use for analysis. Use final, coder2, or coder1 codes in 
# that order of preference.
attach(parentLabels)
parentLabels$transcript = ifelse(transcript2=="", transcript1, ifelse(transcriptF=="", transcript2, transcriptF))
parentLabels$object = ifelse(object2=="", object1, ifelse(objectF=="", object2, objectF))
parentLabels$numObjs = ifelse(numObjs2=="", numObjs1, ifelse(numObjsF=="", numObjs2, numObjsF))
parentLabels$uttType = ifelse(uttType2=="", uttType1, ifelse(uttTypeF=="", uttType2, uttTypeF))
parentLabels$category = ifelse(category2=="", category1, ifelse(categoryF=="", category2, categoryF))
parentLabels$code = ifelse(transcript2=="", 1, ifelse(transcriptF=="", 2, 3))
detach(parentLabels)

parentLabels = parentLabels[,c(1:6,22:27)]

# add subject info
parentLabels = droplevels(merge(subjInfo, parentLabels, all.y=T))
parentLabels$VocabGroupTD = ordered(parentLabels$VocabGroupTD, levels=c("low","high"))

#### Word Learning ####
wl$Target = as.character(wl$Target)
wl$Response = as.character(wl$Response)

#### Point Following ####
pf = merge(pf, subjInfo, all.x=TRUE)

## If final code is not yet available, use code2
pf$lookF = ifelse(pf$lookF=="",as.character(pf$look2),as.character(pf$lookF))
pf$directionF = ifelse(pf$directionF=="",as.character(pf$direction2),as.character(pf$directionF))
pf$distanceF = ifelse(pf$distanceF=="",as.character(pf$distance2),as.character(pf$distanceF))

## Fix column names for easier merge:
colnames(pf)[24] <- "List"
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
  for (subj in levels(df$SubjID)) {
    near = subset(df,SubjID==subj&Distance=="Near")
    far = subset(df,SubjID==subj&Distance=="Far")
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
  dfout = dfout[order(dfout$SubjID),]
  return(dfout)
}

pf = pfSetAllTrials(pf)
pf$Separation = as.factor(pf$Separation)

pf_full = pf
pf = pf[,c("SubjID","dob","Sex","Group","ageV1Mos","ageGroup","WordsProduced","VocabGroup","VocabGroupTD","VocabGroupWS","VocabByGroup",
           "PF_date","List","PF_Order","tagger","coder1","coder2",
           "tagnote","pointNum","attempt","Trial","Distance","PositionRel","Separation","Position","Animacy","Set",
           "windowOnset","windowOffset","directionF","lookF")]

pf$look = ordered(pf$lookF, levels=c("Imm","Delay","NoTarget","NoEyeContact"), labels=c("Imm","Delay","NoTarget","NoEyeContact"))
pf = droplevels(subset(pf,lookF!="DISCUSS"))
pf.good = droplevels(subset(pf,lookF!="NoEyeContact"&attempt=="only"))

#### Save data, clear environment ####
save(subjInfo, subjInfoBasic, parentLabelCodes, parentLabels, wl, pf, pfTrials, pf_full, pf.good, file="wswl-data.Rda")
write.csv(subjInfo,file="subjInfoDetails.csv",row.names=FALSE)
rm(list=ls())
