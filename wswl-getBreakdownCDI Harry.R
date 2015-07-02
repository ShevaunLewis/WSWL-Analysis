#Harry Burke
#July 3, 2015
#Loads CDI breakdown data, combine with subject info, and make plots between different variables
#Just needs csv files for data

#Table of Contents - by line number
# 5 - Libraries
# 147 - Plots
# 367 - Statistics


#### Load applicable libraries
library ("ggplot2")
library ("plyr")
library ("languageR")
library("reshape2")

#### Set working directory
setwd("/Users/harryson/Desktop/R Directory/WS study/Analysis/DataSpreadsheets/")

#### Import data ####
cdi.breakdown.sentencesFile = "CDI_CategoryBreakdown_Sentences.csv"
cdi.breakdown.gesturesFile = "CDI_CategoryBreakdown_Gestures.csv"
subjInfoFile = "SubjInfo.csv"
mullenFile = "Mullen-raw.csv"

cdi.breakdown.sentences = read.csv(cdi.breakdown.sentencesFile, header=T)
cdi.breakdown.gestures = read.csv(cdi.breakdown.gesturesFile, header=T)
subjInfo = read.csv(subjInfoFile, header=T)
mullen = read.csv(mullenFile,header=T)

# Fix dates
subjInfo$date1 = as.Date(as.character(subjInfo$date1), "%m/%d/%y")
subjInfo$date2 = as.Date(as.character(subjInfo$date2), "%m/%d/%y")
subjInfo$dob = as.Date(as.character(subjInfo$dob), "%m/%d/%y")
subjInfo$WL_date = as.Date(as.character(subjInfo$WL_date), "%m/%d/%y")
subjInfo$PF_date = as.Date(as.character(subjInfo$PF_date), "%m/%d/%y")
subjInfo$PL_date = as.Date(as.character(subjInfo$PL_date), "%m/%d/%y")
subjInfo$CDI_date = as.Date(as.character(subjInfo$CDI_date), "%m/%d/%y")
subjInfo$Mullen_date = as.Date(as.character(subjInfo$Mullen_date), "%m/%d/%y")

# Calculate age
subjInfo$ageV1Mos = round(as.integer(subjInfo$date1 - subjInfo$dob)/365*12, digits=1)

# Set age groups
subjInfo$ageGroup = as.factor(ifelse(grepl("Pilot",subjInfo$Group),as.character(subjInfo$Group),
                                     ifelse(subjInfo$Group=="TD", ifelse(subjInfo$ageV1Mos<21, "18M", 
                                                                         ifelse(subjInfo$ageV1Mos>27,"30M","24M")),"WS")))

#### subjInfo
colnames(mullen) <- c("Subj","mullenGM","mullenVR","mullenFM","mullenRL","mullenEL")

## add Mullen scores to subject info
subjInfo = merge(subjInfo, mullen, all.x=T)


#### Combine CDI breakdown data with subjInfo ####


subjInfo = rename(subjInfo, c("Subj"="Subject"))
cdi.breakdown.sentences = merge(cdi.breakdown.sentences, subjInfo)
cdi.breakdown.gestures = rename(cdi.breakdown.gestures, c("Participant"="Subject"))
cdi.breakdown.gestures = merge(cdi.breakdown.gestures, subjInfo)

# drop WS205: pilot subject #
cdi.breakdown.sentences = droplevels(subset(cdi.breakdown.sentences,Subject!="WS205"))

# Correct WordsProduced using checklist data [data for WordsProduced in subjInfo is incorrect] #
cdi.breakdown.sentences$WordsProduced <- rowSums(cdi.breakdown.sentences[,2:23])
cdi.breakdown.gestures$WordsProduced <- rowSums(cdi.breakdown.gestures[,2:20])

# median vocabulary for different subsets
cdi.breakdown.sentences$VocabSplitAll = with(cdi.breakdown.sentences, ifelse(WordsProduced < median(WordsProduced[!(Group%in%c("Pilot-TD","Pilot-WS"))], na.rm=T), "low","high"))
cdi.breakdown.sentences$VocabSplitTD = with(cdi.breakdown.sentences, ifelse(WordsProduced < median(WordsProduced[Group=="TD"], na.rm=T), "low","high"))
cdi.breakdown.sentences$VocabSplitWS = with(cdi.breakdown.sentences, ifelse(WordsProduced < median(WordsProduced[Group=="WS"], na.rm=T), "low", "high"))

cdi.breakdown.gestures$VocabSplitAll = with(cdi.breakdown.gestures, ifelse(WordsProduced < median(WordsProduced[!(Group%in%c("Pilot-TD","Pilot-WS"))], na.rm=T), "low","high"))
cdi.breakdown.gestures$VocabSplitTD = with(cdi.breakdown.gestures, ifelse(WordsProduced < median(WordsProduced[Group=="TD"], na.rm=T), "low","high"))
cdi.breakdown.gestures$VocabSplitWS = with(cdi.breakdown.gestures, ifelse(WordsProduced < median(WordsProduced[Group=="WS"], na.rm=T), "low", "high"))

# make column that specifies high/low vocab relative to the age group (TD18, TD24, WS) of the subject
cdi.breakdown.sentences$VocabSplitByGroup = with(cdi.breakdown.sentences, ifelse(ageGroup=="18M",
                                             ifelse(WordsProduced < median(WordsProduced[ageGroup=="18M"], na.rm=T), "low", "high"),
                                             ifelse(ageGroup=="24M",
                                                    ifelse(WordsProduced < median(WordsProduced[ageGroup=="24M"], na.rm=T), "low", "high"),
                                                    VocabSplitWS)))

cdi.breakdown.gestures$VocabSplitByGroup = with(cdi.breakdown.gestures, ifelse(ageGroup=="18M",
                                                                                 ifelse(WordsProduced < median(WordsProduced[ageGroup=="18M"], na.rm=T), "low", "high"),
                                                                                 ifelse(ageGroup=="24M",
                                                                                        ifelse(WordsProduced < median(WordsProduced[ageGroup=="24M"], na.rm=T), "low", "high"),
                                                                                        VocabSplitWS)))

#### Create word categories ####
# WordsProduced includes X13GamesRoutines, which isn't in any of these categories # 


### Nouns###
cdi.breakdown.sentences$Nouns <- rowSums(cdi.breakdown.sentences[,2:13])
cdi.breakdown.gestures$Nouns <- rowSums(cdi.breakdown.gestures[,2:12])

### Verbs ###
cdi.breakdown.sentences$Verbs <- cdi.breakdown.sentences[,15]
cdi.breakdown.gestures$Verbs <- cdi.breakdown.gestures[,14]

### Adjectives/Adverbs ###
cdi.breakdown.sentences$Adjectives <- rowSums(cdi.breakdown.sentences[,16:17])
cdi.breakdown.gestures$Adjectives <- rowSums(cdi.breakdown.gestures[,15:16])

### Function words ###
cdi.breakdown.sentences$Function <- rowSums(cdi.breakdown.sentences[,18:23])
cdi.breakdown.gestures$Function <- rowSums(cdi.breakdown.gestures[,17:20])

### Remove old categories ###
keep <- c("Subject","Group","ageGroup","Sex","ageV1Mos","mullenGM","mullenVR","mullenFM","mullenRL","mullenEL","WordsProduced","VocabSplitAll","VocabSplitTD","VocabSplitWS","VocabSplitByGroup","Nouns","Adjectives","Function","Verbs")
cdi.breakdown.sentences <- cdi.breakdown.sentences[keep]
cdi.breakdown.gestures <- cdi.breakdown.gestures[keep]

### Add proportion of total words for each word category ###
#Proportions don't add up to 1 because X13GamesRoutines is included in WordsProduced but not the word categories

## Nouns ##
cdi.breakdown.sentences$Proportion_Nouns <- cdi.breakdown.sentences$Nouns/cdi.breakdown.sentences$WordsProduced
cdi.breakdown.gestures$Proportion_Nouns <- cdi.breakdown.gestures$Nouns/cdi.breakdown.gestures$WordsProduced

## Verbs ##
cdi.breakdown.sentences$Proportion_Verbs <- cdi.breakdown.sentences$Verbs/cdi.breakdown.sentences$WordsProduced
cdi.breakdown.gestures$Proportion_Verbs <- cdi.breakdown.gestures$Verbs/cdi.breakdown.gestures$WordsProduced

## Adjectives ##
cdi.breakdown.sentences$Proportion_Adjectives <- cdi.breakdown.sentences$Adjectives/cdi.breakdown.sentences$WordsProduced
cdi.breakdown.gestures$Proportion_Adjectives <- cdi.breakdown.gestures$Adjectives/cdi.breakdown.gestures$WordsProduced

## Function words ##
cdi.breakdown.sentences$Proportion_Function <- cdi.breakdown.sentences$Function/cdi.breakdown.sentences$WordsProduced
cdi.breakdown.gestures$Proportion_Function <- cdi.breakdown.gestures$Function/cdi.breakdown.gestures$WordsProduced

### Define cdi.breakdown.all ###

## combine data frames ##
cdi.breakdown.all <- rbind(cdi.breakdown.sentences, cdi.breakdown.gestures)
# correct row names
rownames(cdi.breakdown.all) <- NULL
# label form type: TD, WS = cdi.breakdown.sentences, WG = cdi.breakdown.gestures
#this must be changed when data changes, consider replacing this with a function
cdi.breakdown.all$Form <- c(rep("TD", 31), rep("WS", 6), rep("WG", 3))

#### Save data ####
save(cdi.breakdown.sentences, cdi.breakdown.gestures, cdi.breakdown.all, file="wswl-cdi_breakdown.Rda")


#### Plots of Vocabulary size vs. each word category ####

### lm_eqn function to print regression equation and r-squared on plot

lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

lm_eqn_sq = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            c = format(abs(coef(m)[3]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0 & coef(m)[3] >= 0)  {
    eqn <- substitute(italic(y) == a + b %.% italic(x) + c %.% italic(x^2)*","~~italic(r)^2~"="~r2,l)
  } else {
    if (coef(m)[2] < 0 & coef(m)[3] >= 0) {
    eqn <- substitute(italic(y) == a - b %.% italic(x) + c %.% italic(x^2)*","~~italic(r)^2~"="~r2,l)    
  } else {
    if (coef(m)[2] >= 0 & coef(m)[3] < 0) {
    eqn <- substitute(italic(y) == a + b %.% italic(x) - c %.% italic(x^2)*","~~italic(r)^2~"="~r2,l) 
  } else {
    eqn <- substitute(italic(y) == a - b %.% italic(x) - c %.% italic(x^2)*","~~italic(r)^2~"="~r2,l) 

  }}}
  
  as.character(as.expression(eqn));                 
}

### cdi.breakdown.sentences TD only ###

## Nouns ##
ggplot(droplevels(subset(cdi.breakdown.sentences, Group=="TD")), aes(x=WordsProduced, y=Proportion_Nouns)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of nouns", y="Nouns as a proportion of total words produced", x="Number of total words produced")  + annotate("text", x = 200, y = 0.5, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group=="TD")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/TD_Sentences_WordsProduced_vs_Proportion_Nouns.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Verbs ##
ggplot(droplevels(subset(cdi.breakdown.sentences, Group=="TD")), aes(x=WordsProduced, y=Proportion_Verbs)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of verbs", y="Verbs as a proportion of total words produced", x="Number of total words produced")  + annotate("text", x = 485, y = 0.05, label = lm_eqn_sq(lm(Proportion_Verbs ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group=="TD")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/TD_Sentences_WordsProduced_vs_Proportion_Verbs.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height


## Adjectives ##
#removed TD06 as an outlier
ggplot(droplevels(subset(cdi.breakdown.sentences, Group=="TD" & Subject != "TD06")), aes(x=WordsProduced, y=Proportion_Adjectives)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of adjectives", y="Adjectives as a proportion of total words produced", x="Number of total words produced")  + annotate("text", x = 485, y = 0.025, label = lm_eqn_sq(lm(Proportion_Adjectives ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group=="TD")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/TD_Sentences_WordsProduced_vs_Proportion_Adjectives.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Function words ##
ggplot(droplevels(subset(cdi.breakdown.sentences, Group=="TD")), aes(x=WordsProduced, y=Proportion_Function)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of function words", y="Function words as a proportion of total words produced", x="Number of total words produced")  + annotate("text", x = 485, y = 0.01, label = lm_eqn_sq(lm(Proportion_Function ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group=="TD")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/TD_Sentences_WordsProduced_vs_Proportion_Function.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height


### cdi.breakdown.sentences WS only ###
#results are not especially accurate due to small sample size
#easy to overfit data

## Nouns ##
ggplot(droplevels(subset(cdi.breakdown.sentences, Group=="WS")), aes(x=WordsProduced, y=Proportion_Nouns)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of nouns", y="Nouns as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 235, y = 0.5, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group=="WS")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/WS_Sentences_WordsProduced_vs_Proportion_Nouns.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Verbs ##
ggplot(droplevels(subset(cdi.breakdown.sentences, Group=="WS")), aes(x=WordsProduced, y=Proportion_Verbs)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of verbs", y="Verbs as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 155, y = 0.17, label = lm_eqn_sq(lm(Proportion_Verbs ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group=="WS")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/WS_Sentences_WordsProduced_vs_Proportion_Verbs.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height


## Adjectives ##
ggplot(droplevels(subset(cdi.breakdown.sentences, Group=="WS")), aes(x=WordsProduced, y=Proportion_Adjectives)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of adjectives", y="Adjectives as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 185, y = 0.08, label = lm_eqn_sq(lm(Proportion_Adjectives ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group=="WS")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/WS_Sentences_WordsProduced_vs_Proportion_Adjectives.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Function words ##
ggplot(droplevels(subset(cdi.breakdown.sentences, Group=="WS")), aes(x=WordsProduced, y=Proportion_Function)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of function words", y="Function words as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 185, y = 0.11, label = lm_eqn_sq(lm(Proportion_Function ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group=="WS")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/WS_Sentences_WordsProduced_vs_Proportion_Function.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

### cdi.breakdown.sentences Both groups together ###
#do these graphs mean anything with the groups together?

## Nouns ##
ggplot(droplevels(subset(cdi.breakdown.sentences)), aes(x=WordsProduced, y=Proportion_Nouns)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of nouns", y="Nouns as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 460, y = 0.44, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences)))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/All_Sentences_WordsProduced_vs_Proportion_Nouns.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Verbs ##
ggplot(droplevels(subset(cdi.breakdown.sentences)), aes(x=WordsProduced, y=Proportion_Verbs)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of verbs", y="Verbs as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 470, y = 0.05, label = lm_eqn_sq(lm(Proportion_Verbs ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences)))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/All_Sentences_WordsProduced_vs_Proportion_Verbs.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height


## Adjectives ##
#removed TD06 as an outlier
ggplot(droplevels(subset(cdi.breakdown.sentences, Subject != "TD06")), aes(x=WordsProduced, y=Proportion_Adjectives)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of adjectives", y="Adjectives as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 480, y = 0.025, label = lm_eqn_sq(lm(Proportion_Adjectives ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences)))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/All_Sentences_WordsProduced_vs_Proportion_Adjectives.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Function words ##
ggplot(droplevels(subset(cdi.breakdown.sentences)), aes(x=WordsProduced, y=Proportion_Function)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of function words", y="Function words as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 480, y = 0.025, label = lm_eqn_sq(lm(Proportion_Function ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences)))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/All_Sentences_WordsProduced_vs_Proportion_Function.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

### cdi.breakdown.sentences Both groups, labeled separately ###
#largely redundant to later graph that uses with cdi.breakdown.all
#could be overfitting with WS data

## Nouns ##
ggplot(droplevels(subset(cdi.breakdown.sentences)), aes(x=WordsProduced, y=Proportion_Nouns,color=Group,shape=Group)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of nouns", y="Nouns as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 480, y = 0.8, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group == "TD")))), colour="red", size = 5, parse=TRUE) + annotate("text", x = 480, y = 0.4, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group == "WS")))), colour="turquoise3", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/Together_Sentences_WordsProduced_vs_Proportion_Nouns.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Verbs ##
ggplot(droplevels(subset(cdi.breakdown.sentences)), aes(x=WordsProduced, y=Proportion_Verbs,color=Group,shape=Group)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of verbs", y="Verbs as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 475, y = 0.01, label = lm_eqn_sq(lm(Proportion_Verbs ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group == "TD")))), colour="red", size = 5, parse=TRUE) + annotate("text", x = 475, y = 0.05, label = lm_eqn_sq(lm(Proportion_Verbs ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group == "WS")))), colour="turquoise3", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/Together_Sentences_WordsProduced_vs_Proportion_Verbs.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height


## Adjectives ##
#removed TD06 as an outlier
ggplot(droplevels(subset(cdi.breakdown.sentences, Subject != "TD06")), aes(x=WordsProduced, y=Proportion_Adjectives,color=Group,shape=Group)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of adjectives", y="Adjectives as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 450, y = 0.17, label = lm_eqn_sq(lm(Proportion_Adjectives ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group == "TD" & Subject != "TD06")))), colour="red", size = 5, parse=TRUE) + annotate("text", x = 460, y = 0.01, label = lm_eqn_sq(lm(Proportion_Adjectives ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group == "WS")))), colour="turquoise3", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/Together_Sentences_WordsProduced_vs_Proportion_Adjectives.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Function words ##
ggplot(droplevels(subset(cdi.breakdown.sentences)), aes(x=WordsProduced, y=Proportion_Function,color=Group,shape=Group)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of function words", y="Function words as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 450, y = 0.01, label = lm_eqn_sq(lm(Proportion_Function ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group == "TD")))), colour="red", size = 5, parse=TRUE) + annotate("text", x = 210, y = 0.15, label = lm_eqn_sq(lm(Proportion_Function ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.sentences, Group == "WS")))), colour="turquoise3", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/Together_Sentences_WordsProduced_vs_Proportion_Function.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

### cdi.breakdown.gestures WS only [there are no TD subjects in this condition] ###
#There are only two data points as of 6/12/15
#not a meaningful set of graphs
#missing values error is because one subject in cdi.breakdown.gestures had 0 words - has N/A values for proportion and is not used in analysis

## Nouns ##
ggplot(droplevels(subset(cdi.breakdown.gestures, Group=="WS")), aes(x=WordsProduced, y=Proportion_Nouns)) + geom_point() + geom_smooth(method=lm,se=FALSE) + labs(title = "Number of words produced vs. proportion of nouns", y="Nouns as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 500, y = 0.01, label = lm_eqn(lm(Proportion_Nouns ~ WordsProduced, droplevels(subset(cdi.breakdown.gestures, Group == "WS")))), colour="red", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/WS_Gestures_WordsProduced_vs_Proportion_Nouns.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Verbs ##
ggplot(droplevels(subset(cdi.breakdown.gestures, Group=="WS")), aes(x=WordsProduced, y=Proportion_Verbs)) + geom_point() + geom_smooth(method=lm,se=FALSE) + labs(title = "Number of words produced vs. proportion of verbs", y="Verbs as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 500, y = 0.01, label = lm_eqn(lm(Proportion_Verbs ~ WordsProduced, droplevels(subset(cdi.breakdown.gestures, Group == "WS")))), colour="red", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/WS_Gestures_WordsProduced_vs_Proportion_Verbs.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height


## Adjectives ##
ggplot(droplevels(subset(cdi.breakdown.gestures, Group=="WS")), aes(x=WordsProduced, y=Proportion_Adjectives)) + geom_point() + geom_smooth(method=lm,se=FALSE) + labs(title = "Number of words produced vs. proportion of adjectives", y="Adjectives as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 500, y = 0.01, label = lm_eqn(lm(Proportion_Adjectives ~ WordsProduced, droplevels(subset(cdi.breakdown.gestures, Group == "WS")))), colour="red", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/WS_Gestures_WordsProduced_vs_Proportion_Adjectives.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Function words ##
ggplot(droplevels(subset(cdi.breakdown.gestures, Group=="WS")), aes(x=WordsProduced, y=Proportion_Function)) + geom_point() + geom_smooth(method=lm,se=FALSE) + labs(title = "Number of words produced vs. proportion of function words", y="Function words as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 500, y = 0.01, label = lm_eqn(lm(Proportion_Function ~ WordsProduced, droplevels(subset(cdi.breakdown.gestures, Group == "WS")))), colour="red", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/WS_Gestures_WordsProduced_vs_Proportion_Function.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height


### cdi.breakdown.all Both groups, labelled separately
#missing values error is because one subject in cdi.breakdown.gestures had 0 words - has N/A values for proportion and is not used in analysis
#ws data might be overfit

## Nouns ##
ggplot(droplevels(subset(cdi.breakdown.all)), aes(x=WordsProduced, y=Proportion_Nouns,color=Group,shape=Group)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of nouns", y="Nouns as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 470, y = 0.75, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group="TD")))), colour="red", size = 5, parse=TRUE) + annotate("text", x = 470, y = 0.4, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="WS")))), colour="turquoise3", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/Together_All_WordsProduced_vs_Proportion_Nouns.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Verbs ##
ggplot(droplevels(subset(cdi.breakdown.all)), aes(x=WordsProduced, y=Proportion_Verbs,color=Group,shape=Group)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of verbs", y="Verbs as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 470, y = 0.01, label = lm_eqn_sq(lm(Proportion_Verbs ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="TD")))), colour="red", size = 5, parse=TRUE) + annotate("text", x = 470, y = 0.28, label = lm_eqn_sq(lm(Proportion_Verbs ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="WS")))), colour="turquoise3", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/Together_All_WordsProduced_vs_Proportion_Verbs.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height


## Adjectives ##
#remove TD06 as an outlier
ggplot(droplevels(subset(cdi.breakdown.all, Subject != "TD06")), aes(x=WordsProduced, y=Proportion_Adjectives,color=Group,shape=Group)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of adjectives", y="Adjectives as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 450, y = 0.005, label = lm_eqn_sq(lm(Proportion_Adjectives ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="TD")))), colour="red", size = 5, parse=TRUE) + annotate("text", x = 450, y = 0.025, label = lm_eqn_sq(lm(Proportion_Adjectives ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="WS")))), colour="turquoise3", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/Together_All_WordsProduced_vs_Proportion_Adjectives.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height

## Function words ##
ggplot(droplevels(subset(cdi.breakdown.all)), aes(x=WordsProduced, y=Proportion_Function,color=Group,shape=Group)) + geom_point() + geom_smooth(method=lm,formula=y~x+I(x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of function words", y="Function words as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 450, y = 0.01, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="TD")))), colour="red", size = 5, parse=TRUE) + annotate("text", x = 200, y = 0.15, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="WS")))), colour="turquoise3", size = 5, parse=TRUE)
dev.print(png, file="Plots/CDI Breakdown/Together_All_WordsProduced_vs_Proportion_Function.png", width=15.3, height=9, units="in", res=300)
#Plot ratio: width=1.7height



#### Create stacked bar plot of subject vs total words, with each bar grouped by word category ####

Nouns <- c(cdi.breakdown.all$Nouns)
Verbs <- c(cdi.breakdown.all$Verbs)
Adjectives <- c(cdi.breakdown.all$Adjectives)
Function <- c(cdi.breakdown.all$Function)
data1.sb <- rbind(Nouns, Verbs, Adjectives, Function)
colnames(data1.sb) <- cdi.breakdown.all$Subject
#remove TD28 (N/A data)
drops <- c("TD28")
data1.sb <- data1.sb[,!(colnames(data1.sb) %in% drops)]


### reshape data for plot 
data2.sb <- melt(data1.sb)

### Plot data ###
 
## Stacked bar chart for all groups, ordered by total number of words produced ##
ggplot(data2.sb, aes(x = reorder(Var2, value), y = value, fill = Var1)) + geom_bar(stat = "identity") + labs(title = "Subject vs. number of words produced", y="Number of words produced", x="Subject") + scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), panel.grid.major.y = element_line(colour = "black"), panel.grid.major.x = element_blank())
dev.print(png, file="Plots/CDI Breakdown/Stacked_bar_word_categories_by_subject_order_words.png", width=15.3, height=9, units="in", res=300)

## Stacked bar chart for all groups, as a proportion out of 1.00 ##
ggplot(data2.sb, aes(x = reorder(Var2, value), y = value, fill = Var1)) + geom_bar(stat = "identity", position="fill") + labs(title = "Subject vs. number of words produced as a proportion, ordered by words produced", y="Number of words produced", x="Subject") + scale_y_continuous(breaks=c(0.00,0.125,0.25,0.375, 0.5, 0.625, 0.75, 0.875, 1.00)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), panel.grid.major.y = element_line(colour = "black"))
dev.print(png, file="Plots/CDI Breakdown/Stacked_bar_word_categories_by_subject_proportion_order_words.png", width=15.3, height=9, units="in", res=300)

## Stacked bar chart for all groups, ordered by age ##
#change this when number of subjects changes
age.sb <- rep(subset(cdi.breakdown.all, Subject != "TD28")$ageV1Mos, each=4)
data2.sb.age <- cbind(data2.sb, age.sb)
ggplot(data2.sb.age, aes(x = reorder(Var2, age.sb), y = value, fill = Var1)) + geom_bar(stat = "identity") + labs(title = "Subject vs. number of words produced", y="Number of words produced", x="Subject") + scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), panel.grid.major.y = element_line(colour = "black"), panel.grid.major.x = element_blank())
dev.print(png, file="Plots/CDI Breakdown/Stacked_bar_word_categories_by_subject_order_age.png", width=15.3, height=9, units="in", res=300)

## Stacked bar chart for all groups, ordered by age, as a proportion out of 1.00 ##

age.sb <- rep(subset(cdi.breakdown.all, Subject != "TD28")$ageV1Mos, each=4)
data2.sb.age <- cbind(data2.sb, age.sb)
ggplot(data2.sb.age, aes(x = reorder(Var2, age.sb), y = value, fill = Var1)) + geom_bar(stat = "identity", position="fill") + labs(title = "Subject vs. number of words produced, ordered by age", y="Number of words produced", x="Subject") + scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), panel.grid.major.y = element_line(colour = "black"), panel.grid.major.x = element_blank())
dev.print(png, file="Plots/CDI Breakdown/Stacked_bar_word_categories_by_subject_proportion_order_age.png", width=15.3, height=9, units="in", res=300)


## two graphs, separated by group ##
#change these numbers when number of subjects changes
Groups <- c(rep("TD", 120), rep("WS", 36))
data3.sb <- cbind(data2.sb, Groups)
ggplot(data3.sb, aes(x = reorder(Var2, value), y = value, fill = Var1)) + geom_bar(stat = "identity") + facet_wrap(~Groups, scales="free_x") + labs(title = "Subject by group vs. number of words produced", y="Number of words produced", x="Subject") + scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), panel.grid.major.y = element_line(colour = "black"), panel.grid.major.x = element_blank())
dev.print(png, file="Plots/CDI Breakdown/Stacked_bar_word_categories_by_subject_by_group.png", width=15.3, height=9, units="in", res=300)

# with ugly regression lines #
ggplot(data3.sb, aes(x = reorder(Var2, value), y = value, fill = Var1)) + geom_bar(stat = "identity") + facet_wrap(~Groups, scales="free_x") + geom_smooth(aes(group=Var1, colour=Var1), method="glm", se=FALSE) + labs(title = "Subject by group vs. number of words produced", y="Number of words produced", x="Subject") + scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), panel.grid.major.y = element_line(colour = "black"), panel.grid.major.x = element_blank())
dev.print(png, file="Plots/CDI Breakdown/Stacked_bar_word_categories_by_subject_by_group_with_lines.png", width=15.3, height=9, units="in", res=300)

## Stacked bar of subjects vs words produced, but with a value on the x axis for each possible number of words produced (even when no such subject exists) ##
# doesn't work
#ggplot(data2.sb, aes(x = reorder(Var2, value), y = value, fill = Var1)) + geom_bar(stat = "identity") + labs(title = "Subject vs. number of words produced", y="Number of words produced", x="Subject") + scale_y_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600)) + theme(panel.background = element_rect(fill = 'white', colour = 'black'), panel.grid.major.y = element_line(colour = "black"), panel.grid.major.x = element_blank())
#dev.print(png, file="Plots/CDI Breakdown/Stacked_bar_word_categories_by_subject_order_words.png", width=15.3, height=9, units="in", res=300)



#### Statistics ####

### Function to return p-values for each predictor variable, as a vector ###
#need to change [x,y] values as you have more or less predictor variables. Here there are 7.

p.value.all <- function(modelobject) {
  p.value.all.result <- c(summary(modelobject)$coefficient[2,4], summary(modelobject)$coefficient[3,4], summary(modelobject)$coefficient[4,4], summary(modelobject)$coefficient[5,4], summary(modelobject)$coefficient[6,4], summary(modelobject)$coefficient[7,4], summary(modelobject)$coefficient[8,4])
  return(p.value.all.result)
}


### All subjects ###
cdi.breakdown.all <- droplevels(cdi.breakdown.all)
contrasts(cdi.breakdown.all$Group) <- contr.helmert(2)
cdi.all.glm.nouns = glm(cbind(Nouns, WordsProduced-Nouns) ~ WordsProduced * ageV1Mos * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.verbs = glm(cbind(Verbs, WordsProduced-Verbs) ~ WordsProduced * ageV1Mos * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.adjectives = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ WordsProduced * ageV1Mos * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.function = glm(cbind(Function, WordsProduced-Function) ~ WordsProduced * ageV1Mos * Group, data=cdi.breakdown.all, family="binomial")

cdi.all.glm.report.p <- rbind(p.value.all(cdi.all.glm.nouns), p.value.all(cdi.all.glm.verbs), p.value.all(cdi.all.glm.adjectives), p.value.all(cdi.all.glm.function))
cdi.all.glm.report.p <- format(cdi.all.glm.report.p, scientific=FALSE)
cdi.all.glm.report.p <- data.frame(cdi.all.glm.report.p)
rownames(cdi.all.glm.report.p) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.all.glm.report.p) <- c("WordsProduced", "ageV1Mos", "Group1", "WordsProduced:ageV1Mos", "WordsProduced:Group1", "ageV1Mos:Group1", "WordsProduced:ageV1Mos:Group1")
#contrasts(cdi.breakdown.all$Group) shows that Group1 is WS
# y-axis (rownames) are the dependent variables. x-axis (column names) are the predictor variables

## Reduced models ##

# Proportion of word category vs age and group #
# test each of these models vs one without interaction effect

cdi.all.glm.nouns.r1 = glm(cbind(Nouns, WordsProduced-Nouns) ~ ageV1Mos * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.verbs.r1 = glm(cbind(Verbs, WordsProduced-Verbs) ~ ageV1Mos * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.adjectives.r1 = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ ageV1Mos * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.function.r1 = glm(cbind(Function, WordsProduced-Function) ~ ageV1Mos * Group, data=cdi.breakdown.all, family="binomial")

cdi.all.glm.report.r1 = rbind(anova(cdi.all.glm.nouns, cdi.all.glm.nouns.r1, test="Chisq")[2,5], anova(cdi.all.glm.verbs, cdi.all.glm.verbs.r1, test="Chisq")[2,5], anova(cdi.all.glm.adjectives, cdi.all.glm.adjectives.r1, test="Chisq")[2,5], anova(cdi.all.glm.function, cdi.all.glm.function.r1, test="Chisq")[2,5])
cdi.all.glm.report.r1 <- format(cdi.all.glm.report.r1, scientific=FALSE)
cdi.all.glm.report.r1 <- data.frame(cdi.all.glm.report.r)
rownames(cdi.all.glm.report.r1) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.all.glm.report.r1) <- c("p-value")

# report p-values
p.value.all.r1 <- function(modelobject) {
  p.value.all.result.r1 <- c(summary(modelobject)$coefficient[2,4], summary(modelobject)$coefficient[3,4], summary(modelobject)$coefficient[4,4])
  return(p.value.all.result.r1)
}

cdi.all.glm.report.r1a <- rbind(p.value.all.r1(cdi.all.glm.nouns.r1), p.value.all.r1(cdi.all.glm.verbs.r1), p.value.all.r1(cdi.all.glm.adjectives.r1), p.value.all.r1(cdi.all.glm.function.r1))
cdi.all.glm.report.r1a <- format(cdi.all.glm.report.r1a, scientific=FALSE)
cdi.all.glm.report.r1a <- data.frame(cdi.all.glm.report.r1a)
rownames(cdi.all.glm.report.r1a) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.all.glm.report.r1a) <- c("ageV1Mos","Group1","ageV1Mos:Group1")



# Proportion of word category vs words produced and group #

cdi.all.glm.nouns.r2 = glm(cbind(Nouns, WordsProduced-Nouns) ~ WordsProduced * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.verbs.r2 = glm(cbind(Verbs, WordsProduced-Verbs) ~ WordsProduced * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.adjectives.r2 = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ WordsProduced * Group, data=cdi.breakdown.all, family="binomial")
cdi.all.glm.function.r2 = glm(cbind(Function, WordsProduced-Function) ~ WordsProduced * Group, data=cdi.breakdown.all, family="binomial")

cdi.all.glm.report.r2 = rbind(anova(cdi.all.glm.nouns, cdi.all.glm.nouns.r2, test="Chisq")[2,5], anova(cdi.all.glm.verbs, cdi.all.glm.verbs.r2, test="Chisq")[2,5], anova(cdi.all.glm.adjectives, cdi.all.glm.adjectives.r2, test="Chisq")[2,5], anova(cdi.all.glm.function, cdi.all.glm.function.r2, test="Chisq")[2,5])
cdi.all.glm.report.r2 <- format(cdi.all.glm.report.r2, scientific=FALSE)
cdi.all.glm.report.r2 <- data.frame(cdi.all.glm.report.r2)
rownames(cdi.all.glm.report.r2) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.all.glm.report.r2) <- c("p-value")


### TD subjects only ###

#function has 3 predictor variables
p.value.td <- function(modelobject) {
  p.value.td.result <- c(summary(modelobject)$coefficient[2,4], summary(modelobject)$coefficient[3,4], summary(modelobject)$coefficient[4,4])
  return(p.value.td.result)
}

cdi.breakdown.td <- droplevels(subset(cdi.breakdown.all, Group=="TD"))
cdi.td.glm.nouns = glm(cbind(Nouns, WordsProduced-Nouns) ~ WordsProduced * ageV1Mos, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.verbs = glm(cbind(Verbs, WordsProduced-Verbs) ~ WordsProduced * ageV1Mos, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.adjectives = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ WordsProduced * ageV1Mos, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.function = glm(cbind(Function, WordsProduced-Function) ~ WordsProduced * ageV1Mos, data=cdi.breakdown.td, family="binomial")

cdi.td.glm.report.p <- rbind(p.value.td(cdi.td.glm.nouns), p.value.td(cdi.td.glm.verbs), p.value.td(cdi.td.glm.adjectives), p.value.td(cdi.td.glm.function))
cdi.td.glm.report.p <- format(cdi.td.glm.report.p, scientific=FALSE)
cdi.td.glm.report.p <- data.frame(cdi.td.glm.report.p)
rownames(cdi.td.glm.report.p) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.td.glm.report.p) <- c("WordsProduced", "ageV1Mos", "WordsProduced:ageV1Mos")
# y-axis (rownames) are the dependent variables. x-axis (column names) are the predictor variables

## Reduced models ##

# Proportion of word categories vs. age #
cdi.breakdown.td <- droplevels(subset(cdi.breakdown.all, Group=="TD"))
cdi.td.glm.nouns.r1 = glm(cbind(Nouns, WordsProduced-Nouns) ~ ageV1Mos, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.verbs.r1 = glm(cbind(Verbs, WordsProduced-Verbs) ~ ageV1Mos, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.adjectives.r1 = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ ageV1Mos, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.function.r1 = glm(cbind(Function, WordsProduced-Function) ~ ageV1Mos, data=cdi.breakdown.td, family="binomial")

cdi.td.glm.report.r1 = rbind(anova(cdi.td.glm.nouns, cdi.td.glm.nouns.r1, test="Chisq")[2,5], anova(cdi.td.glm.verbs, cdi.td.glm.verbs.r1, test="Chisq")[2,5], anova(cdi.td.glm.adjectives, cdi.td.glm.adjectives.r1, test="Chisq")[2,5], anova(cdi.td.glm.function, cdi.td.glm.function.r1, test="Chisq")[2,5])
cdi.td.glm.report.r1 <- format(cdi.td.glm.report.r1, scientific=FALSE)
cdi.td.glm.report.r1 <- data.frame(cdi.td.glm.report.r1)
rownames(cdi.td.glm.report.r1) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.td.glm.report.r1) <- c("p-value")

# Proportion of word categories vs. words produced #
cdi.breakdown.td <- droplevels(subset(cdi.breakdown.all, Group=="TD"))
cdi.td.glm.nouns.r2 = glm(cbind(Nouns, WordsProduced-Nouns) ~ WordsProduced, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.verbs.r2 = glm(cbind(Verbs, WordsProduced-Verbs) ~ WordsProduced, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.adjectives.r2 = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ WordsProduced, data=cdi.breakdown.td, family="binomial")
cdi.td.glm.function.r2 = glm(cbind(Function, WordsProduced-Function) ~ WordsProduced, data=cdi.breakdown.td, family="binomial")

cdi.td.glm.report.r2 = rbind(anova(cdi.td.glm.nouns, cdi.td.glm.nouns.r2, test="Chisq")[2,5], anova(cdi.td.glm.verbs, cdi.td.glm.verbs.r2, test="Chisq")[2,5], anova(cdi.td.glm.adjectives, cdi.td.glm.adjectives.r2, test="Chisq")[2,5], anova(cdi.td.glm.function, cdi.td.glm.function.r2, test="Chisq")[2,5])
cdi.td.glm.report.r2 <- format(cdi.td.glm.report.r2, scientific=FALSE)
cdi.td.glm.report.r2 <- data.frame(cdi.td.glm.report.r2)
rownames(cdi.td.glm.report.r2) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.td.glm.report.r2) <- c("p-value")


### WS subjects only ###

#function has 3 predictor variables
p.value.ws <- function(modelobject) {
  p.value.ws.result <- c(summary(modelobject)$coefficient[2,4], summary(modelobject)$coefficient[3,4], summary(modelobject)$coefficient[4,4])
  return(p.value.ws.result)
}

cdi.breakdown.ws <- droplevels(subset(cdi.breakdown.all, Group=="WS"))
cdi.ws.glm.nouns = glm(cbind(Nouns, WordsProduced-Nouns) ~ WordsProduced * ageV1Mos, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.verbs = glm(cbind(Verbs, WordsProduced-Verbs) ~ WordsProduced * ageV1Mos, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.adjectives = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ WordsProduced * ageV1Mos, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.function = glm(cbind(Function, WordsProduced-Function) ~ WordsProduced * ageV1Mos, data=cdi.breakdown.ws, family="binomial")

cdi.ws.glm.report.p <- rbind(p.value.ws(cdi.ws.glm.nouns), p.value.ws(cdi.ws.glm.verbs), p.value.ws(cdi.ws.glm.adjectives), p.value.ws(cdi.ws.glm.function))
cdi.ws.glm.report.p <- format(cdi.ws.glm.report.p, scientific=FALSE)
cdi.ws.glm.report.p <- data.frame(cdi.ws.glm.report.p)
rownames(cdi.ws.glm.report.p) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.ws.glm.report.p) <- c("WordsProduced", "ageV1Mos", "WordsProduced:ageV1Mos")
# y-axis (rownames) are the dependent variables. x-axis (column names) are the predictor variables

## Reduced models ##

# Proportion of word categories vs. age #
cdi.breakdown.ws <- droplevels(subset(cdi.breakdown.all, Group=="WS"))
cdi.ws.glm.nouns.r1 = glm(cbind(Nouns, WordsProduced-Nouns) ~ ageV1Mos, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.verbs.r1 = glm(cbind(Verbs, WordsProduced-Verbs) ~ ageV1Mos, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.adjectives.r1 = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ ageV1Mos, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.function.r1 = glm(cbind(Function, WordsProduced-Function) ~ ageV1Mos, data=cdi.breakdown.ws, family="binomial")

cdi.ws.glm.report.r1 = rbind(anova(cdi.ws.glm.nouns, cdi.ws.glm.nouns.r1, test="Chisq")[2,5], anova(cdi.ws.glm.verbs, cdi.ws.glm.verbs.r1, test="Chisq")[2,5], anova(cdi.ws.glm.adjectives, cdi.ws.glm.adjectives.r1, test="Chisq")[2,5], anova(cdi.ws.glm.function, cdi.ws.glm.function.r1, test="Chisq")[2,5])
cdi.ws.glm.report.r1 <- format(cdi.ws.glm.report.r1, scientific=FALSE)
cdi.ws.glm.report.r1 <- data.frame(cdi.ws.glm.report.r1)
rownames(cdi.ws.glm.report.r1) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.ws.glm.report.r1) <- c("p-value")

# Proportion of word categories vs. words produced #
cdi.breakdown.ws <- droplevels(subset(cdi.breakdown.all, Group=="WS"))
cdi.ws.glm.nouns.r2 = glm(cbind(Nouns, WordsProduced-Nouns) ~ WordsProduced, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.verbs.r2 = glm(cbind(Verbs, WordsProduced-Verbs) ~ WordsProduced, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.adjectives.r2 = glm(cbind(Adjectives, WordsProduced-Adjectives) ~ WordsProduced, data=cdi.breakdown.ws, family="binomial")
cdi.ws.glm.function.r2 = glm(cbind(Function, WordsProduced-Function) ~ WordsProduced, data=cdi.breakdown.ws, family="binomial")

cdi.ws.glm.report.r2 = rbind(anova(cdi.ws.glm.nouns, cdi.ws.glm.nouns.r2, test="Chisq")[2,5], anova(cdi.ws.glm.verbs, cdi.ws.glm.verbs.r2, test="Chisq")[2,5], anova(cdi.ws.glm.adjectives, cdi.ws.glm.adjectives.r2, test="Chisq")[2,5], anova(cdi.ws.glm.function, cdi.ws.glm.function.r2, test="Chisq")[2,5])
cdi.ws.glm.report.r2 <- format(cdi.ws.glm.report.r2, scientific=FALSE)
cdi.ws.glm.report.r2 <- data.frame(cdi.ws.glm.report.r2)
rownames(cdi.ws.glm.report.r2) <- c("Proportion_Nouns", "Proportion_Verbs","Proportion_Adjectives","Proportion_Function")
colnames(cdi.ws.glm.report.r2) <- c("p-value")

