##### Social Word Learning Analysis
##### Word Learning (from online paper codes)
##### Created 8/7/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")
load("wswl-data.Rda")


#### Accuracy ####
wl$acc = ifelse(wl$Response==wl$Target,1,0)

#### Comprehension trial types ####
comptrials = data.frame(CompTrial=c(1,2,3,4,5), TrialType=c("novel","familiar","novel","familiar","novel"))
wl = merge(wl,comptrials,all.x=T)
wl = merge(subjInfo,wl,all=T)

#### How many comprehension trials did each subject finish? ####
wl.response = subset(wl,!(Response%in%c("na","nr")))
wl.itemsfinished = as.data.frame(xtabs(~Subj, data=wl.response))
wl.itemsfinished = merge(wl.itemsfinished,subjInfo)

ggplot(wl.itemsfinished, aes(x=ageV1Mos,color=ageGroup, y=Freq)) +
  geom_boxplot() + geom_point(size=1) + 
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="WL trials completed",y="# trials completed",x="Age (mos)",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_TrialsXGroup.png", width=700,height=600,res=200)

wl.itemsfinished$VocabGroup = ordered(wl.itemsfinished$VocabGroup,levels=c("low","high"))
ggplot(wl.itemsfinished, aes(x=VocabGroup,y=Freq)) +
  geom_boxplot(size=.25) + 
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="WL trials completed",y="# trials completed",x="Age (mos)") +
  wswl.smallplots
dev.print(png, file="PilotResults/WL_TrialsXVocab.png",width=600,height=600,res=200)


#### Means by subject ####

### familiar
wl.familiar = subset(wl, Cond!="Practice"&TrialType=="familiar")
wl.familiar.responded = droplevels(subset(wl.familiar, !(Response%in%c("na","nr"))))
wl.familiar.bysubj.byitem = wl.acc(wl.familiar.responded, c("Item","Subj"))
wl.familiar.bysubj = wl.acc(wl.familiar.bysubj.byitem,"Subj")
wl.familiar.bysubj = merge(wl.familiar.bysubj,subjInfo)

## by age group
ggplot(wl.familiar.bysubj, aes(x = ageGroup, color=ageGroup, y=acc)) +
  geom_boxplot() + geom_point(size=1) + 
  scale_y_continuous(limits=c(0,1)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on familiar words",y="Proportion correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccFamiliarXGroup.png",width=700,height=600,res=200)

## by vocab
ggplot(wl.familiar.bysubj, aes(x = VocabGroup, color=VocabGroup, y=acc)) +
  geom_boxplot() +  
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on familiar words",y="Proportion correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccFamiliarXVocab.png", width=600,height=600,res=200)

## number correct (instead of proportion)
wl.familiar.sumacc.bysubj = wl.sumacc(wl.familiar.responded, "Subj")
wl.familiar.sumacc.bysubj = merge(wl.familiar.sumacc.bysubj, subjInfo)

## by age group
ggplot(wl.familiar.sumacc.bysubj, aes(x = ageGroup, color=ageGroup, y=sum_acc)) +
  geom_boxplot() + geom_point(size=1) + 
  scale_y_continuous(limits=c(0,12), breaks = c(0,2,4,6,8,10,12)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on familiar words",y="Total number correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png, file="PilotResults/WL_SumAccFamiliarXGroup.png",width=700,height=600,res=200)

#### novel ####
wl.novel = subset(wl, Cond!="Practice"&TrialType=="novel")
wl.novel.responded = droplevels(subset(wl.novel, !(Response%in%c("na","nr"))))

### all trials, by subject, overall
wl.alltrials.bysubj.byitem = wl.acc(wl.novel.responded, c("Item","Subj"))
wl.alltrials.bysubj = wl.acc(wl.alltrials.bysubj.byitem, "Subj")
wl.alltrials.bysubj = merge(wl.alltrials.bysubj, subjInfo)

## by age group
ggplot(wl.alltrials.bysubj, aes(x = ageGroup, color=ageGroup, y=acc)) +
  geom_boxplot() + geom_point(size=1) + scale_y_continuous(limits=c(0,1)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on novel words",y="Proportion correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccNovelAllXGroup.png", width=700,height=600,res=200)

## by vocab group
wl.alltrials.bysubj$VocabGroup = ordered(wl.alltrials.bysubj$VocabGroup,levels=c("low","high"))
ggplot(wl.alltrials.bysubj, aes(x = VocabGroup, y=acc)) +
  geom_boxplot() + scale_y_continuous(limits=c(0,1)) +
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on novel words",y="Proportion correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccNovelAllXVocab.png", width=600,height=600,res=200)

## number correct (instead of proportion)
wl.novel.sumacc.bysubj = wl.sumacc(wl.novel.responded, "Subj")
wl.novel.sumacc.bysubj = merge(wl.novel.sumacc.bysubj, subjInfo)

# by age group
ggplot(wl.novel.sumacc.bysubj, aes(x = ageGroup, color=ageGroup, y=sum_acc)) +
  geom_boxplot() + geom_point(size=1) + 
  scale_y_continuous(limits=c(0,18), breaks = c(0,2,4,6,8,10,12,14,16,18)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on novel words",y="Total number correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png, file="PilotResults/WL_SumAccNovelXGroup.png",width=700,height=600,res=200)

# by vocab group
wl.novel.sumacc.bysubj$VocabGroup = ordered(wl.novel.sumacc.bysubj$VocabGroup, levels=c("low","high"))
ggplot(wl.novel.sumacc.bysubj, aes(x = VocabGroup, y=sum_acc)) +
  geom_boxplot() +  scale_y_continuous(limits=c(0,18), breaks = c(0,2,4,6,8,10,12,14,16,18)) +
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on novel words",y="Total number correct",x="Group",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_SumAccNovelAllXVocab.png", width=600,height=600,res=200)

### all trials, by subject, by condition
wl.alltrials.bycond.bysubj = wl.acc(wl.acc(wl.novel.responded, c("Item","Subj","Cond")),c("Subj","Cond"))
wl.alltrials.bycond.bysubj = merge(wl.alltrials.bycond.bysubj, subjInfo)
wl.alltrials.bycond.bysubj$Cond = ordered(wl.alltrials.bycond.bysubj$Cond, 
                                          levels=c("FollowIn","Joint","Discrepant"), labels=c("FollowIn","JointAttn","Discrepant"))
wl.alltrials.bycond.bysubj$VocabGroup = ordered(wl.alltrials.bycond.bysubj$VocabGroup,levels=c("low","high"))

## age groups
dodge <-position_dodge(width=.8)
ggplot(wl.alltrials.bycond.bysubj, aes(x = Cond, color=ageGroup, y=acc)) +
  geom_boxplot()  + 
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on novel words",y="Proportion correct",x="Condition",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccNovelCondsXGroup.png", width=800,height=600,res=200)

## by vocab group
ggplot(wl.alltrials.bycond.bysubj, aes(x = Cond, color=VocabGroup, y=acc)) +
  geom_boxplot() + 
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on novel words",y="Proportion correct",x="Condition",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_AccNovelCondsXVocab.png", width=700,height=600,res=200)

## number correct (instead of proportion)
wl.novel.sumacc.bycond = wl.sumacc(wl.novel.responded, c("Subj","Cond"))
wl.novel.sumacc.bycond = merge(wl.novel.sumacc.bycond, subjInfo)
wl.novel.sumacc.bycond$Cond = ordered(wl.novel.sumacc.bycond$Cond, 
                                          levels=c("FollowIn","Joint","Discrepant"), labels=c("FollowIn","JointAttn","Discrepant"))
wl.novel.sumacc.bycond$VocabGroup = ordered(wl.novel.sumacc.bycond$VocabGroup,levels=c("low","high"))

ggplot(wl.novel.sumacc.bycond, aes(x = Cond, color=ageGroup, y=sum_acc)) +
  geom_boxplot() + 
  scale_y_continuous(limits=c(0,6), breaks = c(0,2,4,6)) +
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Accuracy on novel words, by condition",y="Total number correct",x="Condition",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_SumAccNovelCondsXGroup.png", width=800,height=600,res=200)

ggplot(wl.novel.sumacc.bycond, aes(x = Cond, color=VocabGroup, y=sum_acc)) +
  geom_boxplot() + 
  scale_y_continuous(limits=c(0,6), breaks = c(0,2,4,6)) +
  theme_bw() + scale_color_manual(values=c("gray72","gray33")) +
  labs(title="Accuracy on novel words, by condition",y="Total number correct",x="Condition",color="Group") +
  wswl.smallplots
dev.print(png,file="PilotResults/WL_SumAccNovelCondsXVocab.png", width=700,height=600,res=200)

### first trials, by subject, by cond
wl.firsttrial.bysubj = wl.acc(wl.acc(subset(wl.novel.responded, CompTrial==1), c("Item","Subj","Cond")),c("Subj","Cond"))
wl.firsttrial.bysubj = merge(wl.firsttrial.bysubj, subjInfo)

# These are all 0, .5, or 1 (of course)--need to think of how to present it. 

#### Means by age group ####
wl.alltrials.byage = wl.acc(wl.novel, c("Subj","ageGroup","Cond"))
wl.acc(wl.alltrials.byage,c("ageGroup","Cond"))

wl.firsttrial.byage = wl.acc(subset(wl.novel, CompTrial==1), c("Subj","ageGroup","Cond"))
wl.acc(wl.firsttrial.byage,c("ageGroup","Cond"))
