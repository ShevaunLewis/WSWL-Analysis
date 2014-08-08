##### Social Word Learning Analysis
##### Parent Labeling (first code)
##### Created 7/25/2014

#setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
setwd("/Users/Shevaun/Desktop/Results/")
source("WSWL-Analysis/wswl-getdata.r")

load("wswl-data.Rda")

# throw out TD01 for now; not yet coded
parentLabels = droplevels(subset(parentLabels, SubjID!="TD01"))

# add subject info
parentLabels = merge(subjInfo, parentLabels, by.x = "Subj", by.y = "SubjID", all.y=T)

##### Count labels #####
xtabs(~Subj + uttType, data=parentLabels)
xtabs(~Subj + category, data=parentLabels)
xtabs(~ uttType + category, data=parentLabels)

xtabs(~ageGroup + uttType, data=parentLabels)
xtabs(~ageGroup + category, data=parentLabels)
xtabs(~category + uttType + ageGroup, data=parentLabels)

## make data frame just with the counts of different utterance types for each parent
parentUtts = xtabs(~Subj, data=parentLabels)
parentUttTypes = xtabs(~Subj + uttType, data=parentLabels)
# add subject info (including CDI and Mullen)
parentUtts = merge(parentUtts, subjInfo, all.x = T)
parentUttTypes = merge(parentUtts, subjInfo, all.x = T)

head(parentUtts)

plot(Freq ~ WordsProduced, data=parentUtts)
plot(Freq ~ mullenRL, data=parentUtts)
plot(Freq ~ mullenEL, data=parentUtts)

plot(Freq ~ WordsProduced, data=subset(parentUtts,ageGroup=="18M"))
plot(Freq ~ mullenRL, data=subset(parentUtts,ageGroup=="18M"))

plot(Freq ~ WordsProduced, data=subset(parentUtts,ageGroup=="24M"))

plot(Freq ~ WordsProduced, data = subset(parentUttTypes, uttType=="label"))
plot(Freq ~ WordsProduced, data = subset(parentUttTypes, uttType=="discussion"))
plot(Freq ~ WordsProduced, data = subset(parentUttTypes, uttType=="imitation"))

plot(WordsProduced ~ mullenEL, data=subjInfo)
