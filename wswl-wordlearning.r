##### Social Word Learning Analysis
##### Word Learning (from online paper codes)
##### Created 8/7/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
load("wswl-data.Rda")


#### Accuracy ####
wl$acc = ifelse(wl$Response==wl$Target,1,0)

#### Comprehension trial types ####
comptrials = data.frame(CompTrial=c(1,2,3,4,5), TrialType=c("novel","familiar","novel","familiar","novel"))
wl = merge(wl,comptrials,all.x=T)

wl = merge(subjInfo,wl,all=T)
#### Means by subject ####

makefactorlist = function(dat, factors) {
  fact = list()
  for (f in factors) {
    fact[[f]]=dat[[f]]
  }
  return(fact)
}

wl.acc = function(dat, factors) {
  factorlist = makefactorlist(dat, factors)
  m = with(dat, aggregate(acc, factorlist, mean, na.rm=T))
  colnames(m)[length(factorlist)+1] = "acc"
  return(m)
}

### novel
wl.novel = subset(wl, Cond!="Practice"&TrialType=="novel")

# all trials, by subject
wl.alltrials.bysubj = wl.acc(wl.novel, c("Subj","Cond"))
wl.acc(wl.alltrials.bysubj,"Cond")

# first trials, by subject
wl.firsttrial.bysubj = wl.acc(subset(wl.novel, CompTrial==1), c("Subj","Cond"))
wl.acc(wl.firsttrial.bysubj,"Cond")

#### Means by age group ####
wl.alltrials.byage = wl.acc(wl.novel, c("Subj","ageGroup","Cond"))
wl.acc(wl.alltrials.byage,c("ageGroup","Cond"))

wl.firsttrial.byage = wl.acc(subset(wl.novel, CompTrial==1), c("Subj","ageGroup","Cond"))
wl.acc(wl.firsttrial.byage,c("ageGroup","Cond"))
