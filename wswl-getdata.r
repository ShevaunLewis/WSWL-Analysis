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

## Import data from .csv files
subjInfo = read.csv(subjInfoFile, header=T)
parentLabelCodes = read.csv(parentLabelCodesFile, header=T, colClasses = "character")
mullen = read.csv(mullenFile,header=T)
cdi = read.csv(cdiFile, header=T)
wl = read.csv(wordLearningFile, header=T)

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
subjInfo$ageGroup = as.factor(ifelse(subjInfo$Group=="TD", ifelse(subjInfo$ageV1Mos<22, "18M", "24M"),"WS"))


#### CDI ####

# Fix dates
cdi$Date = as.Date(as.character(cdi$Date), "%m/%d/%y")

# add WordsProduced to subject info
subjInfo = merge(subjInfo, cdi[,c("Subj","WordsProduced")], all.x=T)

#### Mullen ####

# add raw scores to subject info
mullen$Date = as.Date(as.character(mullen$Date), "%m/%d/%y")
colnames(mullen) <- c("Subj","Date","mullenGM","mullenVR","mullenFM","mullenRL","mullenEL")
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

#### Word Learning ####
wl$Target = as.character(wl$Target)
wl$Response = as.character(wl$Response)

#### Save data, clear environment ####
save(subjInfo, parentLabelCodes, parentLabels, wl, file="wswl-data.Rda")
rm(list=ls())
