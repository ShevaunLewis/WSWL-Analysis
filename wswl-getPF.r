##### Social Word Learning Analysis
##### Created 7/24/2014

library(plyr)

setwd("/Volumes/landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results")

load("ROutput/wswl-subjInfo.Rda")

#### Import data ####
pfFile = "DataSpreadsheets/PFcodes_fixed.csv"
pfTrialsFile = "DataSpreadsheets/PF_Design.csv"

pf = read.csv(pfFile, header=T)
pfTrials = read.csv(pfTrialsFile, header=T)


#### Point Following ####
pf = merge(pf, subjInfoAll[,c("Subj","PF_date","PF_list","PF_Order")], all.x=TRUE)

# remove TD18 until complete (4/20/15)
pf = droplevels(subset(pf,Subj!="TD18"))

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
    if (dir=="LeftRep") { #if this is LeftRep trial, find the most recent Left 
      r = rownum  #set current row
      while (df[rownum,"Trial"]==0) {
        r = r-1   #go up one row
        if(as.character(df[r,"directionF"])=="Left") {
          df[rownum,"Trial"] = df[r,"Trial"]
          df[r,"attempt"] = "first"
          df[rownum,"attempt"] = "rep"
        }
      }
    }    
    if (dir=="RightRep") {
      r = rownum  #set current row
      while (df[rownum,"Trial"]==0) {
        r = r-1   #go up one row
        if(as.character(df[r,"directionF"])=="Right") {
          df[rownum,"Trial"] = df[r,"Trial"]
          df[r,"attempt"] = "first"
          df[rownum,"attempt"] = "rep"
        }
      }
    }   
    else { #if this isn't a repetition trial, assign the current trial number and then increment trialcount
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

pf.full = pf
pf = pf[,c("Subj","List","PF_Order","tagger","coder1","coder2","tagnote",
           "pointNum","attempt","Trial","Distance","PositionRel","Separation","Position","Animacy","Set",
           "directionF","lookF")]
pf = droplevels(subset(pf,lookF!="DISCUSS"))
pf = merge(pf, subjInfo, all.x=T)
#pf.good = droplevels(subset(pf,lookF!="NoEyeContact"&attempt=="only"))

## Count of look types by subject
## only include first/only attempts, not repetitions
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

## Count of looks in each condition, by subject

pf$looked = as.numeric(pf$lookF %in% c("Imm","Delay"))
pf.conds.totals = ddply(subset(pf,attempt!="rep"&Position!="center"), .(Subj,Distance,Separation,PositionRel),nrow,.drop=F)
colnames(pf.conds.totals)[5] = "trials"
pf.conds.looks = ddply(subset(pf,attempt!="rep"&Position!="center"), .(Subj,Distance,Separation,PositionRel),summarise,looked=sum(looked))
pf.conds = merge(pf.conds.totals, pf.conds.looks)
pf.conds$notLooked = pf.conds$trials - pf.conds$looked
pf.conds$Distance = as.factor(pf.conds$Distance)
pf.conds$PositionRel = as.factor(pf.conds$PositionRel)
pf.conds = droplevels(merge(pf.conds, subjInfo, all.x=T))
contrasts(pf.conds$Group) = contr.helmert(2)
contrasts(pf.conds$Separation) = contr.helmert(2)
contrasts(pf.conds$PositionRel) = contr.helmert(2)

#### Save data, clear environment ####
save(pf, pf.full, pf.lookcounts.bysubj, pf.lookcounts.bysubj.wide, pf.conds, file="ROutput/wswl-pf.Rda")
rm(list=ls())