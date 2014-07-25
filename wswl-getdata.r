##### Social Word Learning Analysis
##### Created 7/24/2014

#### Import data ####
# data directory and file names
setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results")
subjInfoFile = "SubjInfo-basic.csv"
parentLabelCodesFile = "ParentLabelCodes.csv"
mullenFile = "Mullen-raw.csv"
cdiFile = "CDI-raw.csv"

## Import data from .csv files
subjInfo = read.csv(subjInfoFile, header=T, colClasses = "character")
parentLabelCodes = read.csv(parentLabelCodesFile, header=T)
mullen = read.csv(mullenFile,header=T)
cdi = read.csv(cdiFile, header=T)

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/Analysis")

#### Subject Info ####

# Fix dates
subjInfo$date1 = as.Date(subjInfo$date1, "%m/%d/%y")
subjInfo$date2 = as.Date(subjInfo$date2, "%m/%d/%y")
subjInfo$dob = as.Date(subjInfo$dob, "%m/%d/%y")

# Calculate age, add age groups
subjInfo$ageV1Mos = round(as.integer(subjInfo$date1 - subjInfo$dob)/365*12, digits=1)
summary(subjInfo$ageV1Mos)
subjInfo$ageGroup = ifelse(subjInfo$ageV1Mos<22, "18M", "24M")

#### CDI ####

# Fix dates
cdi$Date = as.Date(as.character(cdi$Date), "%m/%d/%y")

# add WordsProduced to subject info
subjInfo = merge(subjInfo, cdi[,c("Subj","WordsProduced")], all.x=T)

#### Mullen ####

# add raw scores to subject info
colnames(mullen) <- c("Subj","mullenGM","mullenVR","mullenFM","mullenRL","mullenEL")
subjInfo = merge(subjInfo, mullen, all.x=T)

#### Save data, clear environment ####
save(subjInfo, parentLabelCodes, file="wswl-data.Rda")
rm(list=ls())
