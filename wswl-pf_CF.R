##### Social Word Learning Analysis
##### Point following (first code)
##### Created 9/18/14

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")

load("wswl-data.Rda")

## combine with subject info
pf = merge(pf, subjInfo, by.x = "SubjID", by.y = "Subj")
head(pf)
summary(pf)
## Get rid of any subjects with no codes
nrow(pf)
pf = subset(pf, direction1!="")

## Calculate coding reliability
pf2codes = droplevels(subset(pf,look2!=""))
pf2codes$lookRel = as.numeric(as.character(pf2codes$look1)==as.character(pf2codes$look2))
mean(pf2codes$lookRel)

##### Summary plots #####
## total looks of each type, by age group
pf$look = ordered(pf$look1, levels=c("Imm","Delay","NoTarget","NoEyeContact"), labels=c("Imm","Delay","NoTarget","NoEyeContact"))
ggplot(pf, aes(x=ageGroup,fill=look)) +
  geom_bar() + 
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) +
  labs(title="Looks by type",y="Number of looks",x="Age group",fill="Child's look")
dev.print(png, file="Results_9-25-14/PF_looktypesXGroup.png",width=500, height=600)

## total looks of each type, by vocab (TD and WS)
ggplot(subset(pf, !(is.na(VocabGroup))), aes(x=VocabGroup,fill=look)) +
  geom_bar() + 
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) +
  labs(title="Looks by type",y="Number of looks",x="Vocabulary size",fill="Child's look")
dev.print(png, file="Results_9-25-14/PF_looktypesXVocab-all.png",width=400, height=600)

## total looks of each type, by TD vocab
ggplot(subset(pf, Group=="TD"), aes(x=VocabGroupTD,fill=look)) +
  geom_bar() + 
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) +
  labs(title="Looks by type (TD only)",y="Number of looks",x="Vocabulary size",fill="Child's look")
dev.print(png, file="Results_9-25-14/PF_looktypesXVocab-TD.png",width=400, height=600)

ggplot(subset(pf, Group=="TD"), aes(x=VocabGroupTD,fill=look)) +
  geom_bar(position="fill") + 
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) +
  labs(title="Looks by type (TD only)",y="Number of looks",x="Vocabulary size",fill="Child's look")

## total looks of each type, by WS vocab
ggplot(subset(pf, Group=="WS"), aes(x=VocabGroupWS,fill=look)) +
  geom_bar() + 
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) +
  labs(title="Looks by type (TD only)",y="Number of looks",x="Vocabulary size",fill="Child's look")


## total looks of each type, by Mullen score

##### Near vs. Far #####
ggplot(pf, aes(x=distance1,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) +
  labs(title="Looks by distance",y="Number of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_9-25-14/PF_looktypesXGroupXDistance.png", width=600, height=600)

ggplot(pf, aes(x=distance1,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~VocabGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) +
  labs(title="Looks by distance",y="Number of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_9-25-14/PF_looktypesXVocab-allXDistance.png", width=600, height=600)

## just trials with successful eye contact
ggplot(subset(pf, look!="NoEyeContact"), aes(x=distance1, fill=look)) + 
  geom_bar(position="fill") + facet_wrap(~ageGroup) + theme_bw() + 
  scale_fill_manual(values=c("green4", "yellow", "red", "gray")) + 
  labs(title = "Looks by distance", y= "Number of looks", x="Distance", fill="Child's Look")

## compare to order of presentation
ggplot(pf, aes(x=distance1,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~PF_Order) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) +
  labs(title="Looks by distance",y="Number of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_9-25-14/PF_looktypesXOrderXDistance.png", width=600, height=600)

## proportion looks of each type, averaged by subject
xtabs(~look1 + SubjID,data=pf)
pf.avg = with(pf, aggregate(look1, list("LookType"=look1,"Subj"=SubjID), length))
pf.total = with(pf, aggregate(look1, list("Subj"=SubjID), length))
colnames(pf.total)[2] <- "totalTrials"
pf.avg = merge(pf.avg, pf.total)
pf.avg$Prop = pf.avg$x/pf.avg$totalTrials
pf.avg = merge(pf.avg, subjInfoBasic)

ggplot(pf.avg, aes(x=Subj, y=Prop, fill=LookType)) + 
  facet_grid(.~ageGroup, scale="free") +
  geom_bar(stat="identity")



#### ADD COND INFO ####
pf.trials.xtabs = xtabs(~SubjID + distance1, data=pf)
head(pfTrials)
subjInfoBasic
head(pf)

## make single column for point direction, incorporating repeat codes
pf$directionCalc = with(pf, ifelse(as.character(direction1)==as.character(direction2), as.character(direction1), 
                                   ifelse(as.character(direction2) %in% c("LeftRep","RightRep"), as.character(direction2), 
                                          as.character(direction1))))

## add trial num for non-repeat points
pfSetTrials = function(df) {
  trials = droplevels(subset(pfTrials, List==as.character(df$PF_list[1])&Distance==as.character(df$distance1[1])))
  trialcount = 1
  for (rownum in 1:nrow(df)) {
    if (as.character(df[rownum,"directionCalc"]) %in% c("Left","Right")) {
      df[rownum,"Trial"] <- trials[trialcount,"Trial"]
      trialcount = trialcount + 1
    }
    else if (as.character(df[rownum,"directionCalc"])=="LeftRep") {
      df[rownum,"Trial"] <- ifelse(as.character(df[rownum-1,"directionCalc"])=="Left", 
                                   df[rownum-1,"Trial"], "unknown")
    }
    else {
      df[rownum,"Trial"] <- ifelse(as.character(df[rownum-1,"directionCalc"])=="Right", 
                                   df[rownum-1,"Trial"], "unknown")
    }
  }
  return(df)
}

pf2=pf
pf2$Trial=0
tapply(pf2,list(pf2$SubjID,pf2$distance1),pfSetTrials)

pftest = subset(pf,SubjID=="TD01"&distance1=="Near")
pftest$Trial = 0
pfSetTrials(pftest)

## adjust point number to account for repeats
pf$pointNumCalc = with(pf, ifelse(as.character(directionCalc) %in% c("Right","Left"), pointNum,
                                  )

pf$Trial