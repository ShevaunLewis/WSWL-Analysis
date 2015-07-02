#Harry Burke
#July 1, 2015
#just needs csv files for data


#### New CDI categories based on Bates et al. (1994) ####

#### Set working directory
setwd("/Users/harryson/Desktop/R Directory/WS/Analysis/DataSpreadsheets")

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

### Common nouns ###
# animal names, vehicles, toys, clothing, body parts, small household items, food, furniture
cdi.breakdown.sentences$Nouns <- rowSums(cdi.breakdown.sentences[,2:10])
cdi.breakdown.gestures$Nouns <- rowSums(cdi.breakdown.gestures[,2:10])

### Predicates ###
# verbs (action words) and adjectives (descriptive words)
cdi.breakdown.sentences$Predicates <- rowSums(cdi.breakdown.sentences[,15:16])
cdi.breakdown.gestures$Predicates <- rowSums(cdi.breakdown.gestures[,14:16])
  #careful, this includes Time, which shouldn't be in the count, but currently all subjects have 0 words in time

### Closed-class words ###
# pronouns, prepositions, question words, quantifiers, articles, auxiliary verbs, connectives
cdi.breakdown.sentences$Closed <- rowSums(cdi.breakdown.sentences[,18:23])
cdi.breakdown.gestures$Closed <- rowSums(cdi.breakdown.gestures[,17:20])

### Remove old categories ###
keep <- c("Subject","Group","ageGroup","Sex","ageV1Mos","mullenGM","mullenVR","mullenFM","mullenRL","mullenEL","WordsProduced","VocabSplitAll","VocabSplitTD","VocabSplitWS","VocabSplitByGroup","Nouns","Predicates","Closed")
cdi.breakdown.sentences <- cdi.breakdown.sentences[keep]
cdi.breakdown.gestures <- cdi.breakdown.gestures[keep]

### Add proportion of total words for each word category ###
#Proportions don't add up to 1 because all CDI words are included in WordsProduced but not the word categories

## Nouns ##
cdi.breakdown.sentences$Proportion_Nouns <- cdi.breakdown.sentences$Nouns/cdi.breakdown.sentences$WordsProduced
cdi.breakdown.gestures$Proportion_Nouns <- cdi.breakdown.gestures$Nouns/cdi.breakdown.gestures$WordsProduced

## Verbs ##
cdi.breakdown.sentences$Proportion_Predicates <- cdi.breakdown.sentences$Predicates/cdi.breakdown.sentences$WordsProduced
cdi.breakdown.gestures$Proportion_Predicates <- cdi.breakdown.gestures$Predicates/cdi.breakdown.gestures$WordsProduced


## Function words ##
cdi.breakdown.sentences$Proportion_Closed <- cdi.breakdown.sentences$Closed/cdi.breakdown.sentences$WordsProduced
cdi.breakdown.gestures$Proportion_Closed <- cdi.breakdown.gestures$Closed/cdi.breakdown.gestures$WordsProduced

### Define cdi.breakdown.all ###

## combine data frames ##
cdi.breakdown.all <- rbind(cdi.breakdown.sentences, cdi.breakdown.gestures)
# correct row names
rownames(cdi.breakdown.all) <- NULL
# label form type: TD, WS = cdi.breakdown.sentences, WG = cdi.breakdown.gestures
cdi.breakdown.all$Form <- c(rep("TD", 31), rep("WS", 6), rep("WG", 3))



#### Plots ####

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

## Age vs words produced ##

#age vs words produced TD subjects quadratic model (better fit than normal linear)
ggplot(droplevels(subset(cdi.breakdown.all, Group=="TD")), aes(x=ageV1Mos, y=WordsProduced)) + geom_point() + geom_smooth(method=lm,formula = y ~ x + I(x^2),se=FALSE) + labs(title = "Age vs. words produced TD", y="Total words produced", x="Age in months") + annotate("text", x = 28, y = 100, label = lm_eqn_sq(lm(WordsProduced ~ ageV1Mos + I(ageV1Mos^2), droplevels(subset(cdi.breakdown.all, Group=="TD")))), colour="red", size = 5, parse=TRUE)
dev.print(png, file="Plots/Vocabulary growth/Age vs words produced TD.png", width=15.3, height=9, units="in", res=300)

#age vs words produced TD subjects > 24mo quadratic model (better fit than normal linear)
ggplot(droplevels(subset(subset(cdi.breakdown.all, Group=="TD"), ageV1Mos>"24")), aes(x=ageV1Mos, y=WordsProduced)) + geom_point() + geom_smooth(method=lm,formula = y ~ x + I(x^2),se=FALSE) + labs(title = "Age vs. words produced TD > 24mo", y="Total words produced", x="Age in months") + annotate("text", x = 28, y = 200, label = lm_eqn_sq(lm(WordsProduced ~ ageV1Mos + I(ageV1Mos^2), droplevels(subset(subset(cdi.breakdown.all, Group=="TD"), ageV1Mos>"24")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/Vocabulary growth/Age vs words produced TD > 24mo.png", width=15.3, height=9, units="in", res=300)

#age vs words produced TD subjects < 24mo (linear model)
ggplot(droplevels(subset(subset(cdi.breakdown.all, Group=="TD"), ageV1Mos<"24")), aes(x=ageV1Mos, y=WordsProduced)) + geom_point() + geom_smooth(method=lm,formula = y ~ x,se=FALSE) + labs(title = "Age vs. words produced TD < 24mo", y="Total words produced", x="Age in months") + annotate("text", x = 20, y = 300, label = lm_eqn(lm(WordsProduced ~ ageV1Mos, droplevels(subset(subset(cdi.breakdown.all, Group=="TD"), ageV1Mos<"24")))), colour="black", size = 5, parse=TRUE)
dev.print(png, file="Plots/Vocabulary growth/Age vs words produced TD < 24mo.png", width=15.3, height=9, units="in", res=300)


## words produced vs proportion of each category ##

#words produced vs proportion of nouns TD subjects (cubic model)
#make a new lm_eqn for the cubic model, this one is only squared
ggplot(droplevels(subset(cdi.breakdown.all, Group=="TD")), aes(x=WordsProduced, y=Proportion_Nouns)) + geom_point() + geom_smooth(method=lm,formula = y ~ x + I(-x^2) + I(x^3),se=FALSE) + labs(title = "Number of words produced vs. proportion of nouns", y="Nouns as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 500, y = 0.8, label = lm_eqn_sq(lm(Proportion_Nouns ~ WordsProduced + I(-WordsProduced^2) + I(WordsProduced^3), droplevels(subset(cdi.breakdown.all, Group=="TD")))), colour="red", size = 5, parse=TRUE)
dev.print(png, file="Plots/Vocabulary growth/Words produced vs proportion of nouns TD.png", width=15.3, height=9, units="in", res=300)

#words produced vs proportion of predicates TD subjects (quadratic model)
ggplot(droplevels(subset(cdi.breakdown.all, Group=="TD")), aes(x=WordsProduced, y=Proportion_Predicates)) + geom_point() + geom_smooth(method=lm,formula = y ~ x + I(-x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of predicates", y="Predicates as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 470, y = 0.1, label = lm_eqn_sq(lm(Proportion_Predicates ~ WordsProduced + I(-WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="TD")))), colour="red", size = 5, parse=TRUE)
dev.print(png, file="Plots/Vocabulary growth/Words produced vs proportion of predicates TD.png", width=15.3, height=9, units="in", res=300)

#words produced vs proportion of closed-class words TD subjects (quadratic model)
ggplot(droplevels(subset(cdi.breakdown.all, Group=="TD")), aes(x=WordsProduced, y=Proportion_Closed)) + geom_point() + geom_smooth(method=lm,formula = y ~ x + I(-x^2),se=FALSE) + labs(title = "Number of words produced vs. proportion of closed-class words", y="Nouns as a proportion of total words produced", x="Number of total words produced") + annotate("text", x = 470, y = 0.025, label = lm_eqn_sq(lm(Proportion_Closed ~ WordsProduced + I(-WordsProduced^2), droplevels(subset(cdi.breakdown.all, Group=="TD")))), colour="red", size = 5, parse=TRUE)
dev.print(png, file="Plots/Vocabulary growth/Words produced vs proportion of closed-class TD.png", width=15.3, height=9, units="in", res=300)


remain <- c("Group", "Proportion_Nouns","Proportion_Predicates","Proportion_Closed","Subject")
cdi.breakdown.all.melt <- melt(subset(cdi.breakdown.all[remain], Group=="TD"))
cdi.breakdown.all.melt$WordsProduced <- c(rep(cdi.breakdown.all[1:31,11], times=3))


#all three on same graph
#use cubic lm_eqn, this one is squared
ggplot(cdi.breakdown.all.melt, aes(x=WordsProduced, y=value, color=variable)) + geom_smooth(method=lm,formula = y ~ x + I(-x^2) + I(x^3),se=FALSE) + labs(title = "Category shifts in early vocabulary", y="Proportion", x="Total number of total words produced") + scale_x_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600,650)) + scale_y_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6)) + scale_colour_discrete(name  ="Word category",breaks=c("Proportion_Nouns", "Proportion_Predicates","Proportion_Closed"),  labels=c("Noun", "Predicate","Closed-case"))
dev.print(png, file="Plots/Vocabulary growth/Vocabulary composition words produced vs proportion TD.png", width=15.3, height=9, units="in", res=300)

                                                                                                                                                                                                                                                                                                                                                                                                                                     
#total words produced vs number of nouns, predicates, and closed-class words
remain2 <- c("Group", "Nouns","Predicates","Closed","Subject")
cdi.breakdown.all.melt2  <- melt(subset(cdi.breakdown.all[remain2], Group=="TD"))
cdi.breakdown.all.melt2$WordsProduced <- c(rep(cdi.breakdown.all[1:31,11], times=3))

ggplot(cdi.breakdown.all.melt2, aes(x=WordsProduced, y=value, color=variable)) + geom_smooth(method=lm,formula = y ~ x + I(x^2) + I(x^3),se=FALSE) + labs(title = "Category shifts in early vocabulary", y="Number of words in each word category", x="Total number of total words produced") + scale_x_continuous(breaks=c(0,50,100,150,200,250,300,350,400,450,500,550,600,650)) + scale_y_continuous(breaks=c(0,50,100,150,200,250)) + scale_colour_discrete(name  ="Word category",breaks=c("Nouns", "Predicates","Closed"),  labels=c("Noun", "Predicate","Closed-case"))
dev.print(png, file="Plots/Vocabulary growth/Vocabulary composition words produced vs count of words TD.png", width=15.3, height=9, units="in", res=300)









#### not currently useful; testing commands ####

# put subjects into groups by words produced
cdi.breakdown.all$Production = with(cdi.breakdown.all, ifelse(WordsProduced<51, "<50",
                                                              ifelse(WordsProduced<101, "51-100",
                                                                     ifelse(WordsProduced<201, "101-200",
                                                                            ifelse(WordsProduced<301, "201-300",
                                                                                   ifelse(WordsProduced<401, "301-400",
                                                                                          ifelse(WordsProduced<501, "401-500",
                                                                                                 ifelse(WordsProduced<601, "501-600",
                                                                                                        ifelse(WordsProduced>600, ">600", "N/A")))))))))

#plot all three lines by production rather than words produced

remain <- c("Group", "Proportion_Nouns","Proportion_Predicates","Proportion_Closed","Subject")
cdi.breakdown.all.melt <- melt(subset(cdi.breakdown.all[remain], Group=="TD"))
cdi.breakdown.all.melt$Production <- c(rep(cdi.breakdown.all[1:25,23], times=3))
test.melt$Production <- as.factor(test.melt$Production)

ggplot(test.melt, aes(x=Production, y=value, color=variable, group=variable)) + geom_point() + geom_smooth(method=lm,formula = y ~ x + I(-x^2) + I(x^3),se=FALSE)


test1 <- c(with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="<50"), mean(Proportion_Nouns)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="51-100"), mean(Proportion_Nouns)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="101-200"), mean(Proportion_Nouns)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="201-300"), mean(Proportion_Nouns)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="301-400"), mean(Proportion_Nouns)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="401-500"), mean(Proportion_Nouns)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="501-600"), mean(Proportion_Nouns)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production==">600"), mean(Proportion_Nouns)))

test2 <- c(with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="<50"), mean(Proportion_Predicates)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="51-100"), mean(Proportion_Predicates)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="101-200"), mean(Proportion_Predicates)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="201-300"), mean(Proportion_Predicates)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="301-400"), mean(Proportion_Predicates)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="401-500"), mean(Proportion_Predicates)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="501-600"), mean(Proportion_Predicates)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production==">600"), mean(Proportion_Predicates)))

test3 <- c(with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="<50"), mean(Proportion_Closed)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="51-100"), mean(Proportion_Closed)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="101-200"), mean(Proportion_Closed)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="201-300"), mean(Proportion_Closed)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="301-400"), mean(Proportion_Closed)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="401-500"), mean(Proportion_Closed)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production=="501-600"), mean(Proportion_Closed)),
with(subset(subset(cdi.breakdown.all, Group=="TD"), Production==">600"), mean(Proportion_Closed)))





