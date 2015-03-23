##### Social Word Learning Analysis
##### Point following (first code)
##### Created 9/18/14
##### Updated 10/31/14

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")

load("ROutput/wswl-subjInfo.Rda")
load("ROutput/wswl-pf.Rda")

## Calculate coding reliability
pf2codes = droplevels(subset(pf_full,look2!=""))
pf2codes$lookRel = as.numeric(as.character(pf2codes$look1)==as.character(pf2codes$look2))
mean(pf2codes$lookRel)
rel = with(pf2codes, aggregate(lookRel,makefactorlist(pf2codes,c("Separation","Distance")), mean))
xtabs(~lookRel+Separation+Distance,data=pf2codes)
pf2codes.exp = subset(pf2codes,Position!="center")
relexp = with(pf2codes.exp, aggregate(lookRel,makefactorlist(pf2codes.exp,c("Separation","Distance")), mean))

colnames(pf.lookcounts.bysubj)[2] <- "lookType"


##### Stats #####
#td only
pf.stat.td = droplevels(subset(pf.lookcounts.bysubj.wide, ageGroup %in% c("18M", "24M")))
anova(lm(totalGoodTrials ~ ageV1Mos * WordsProduced, data=pf.stat.td))
anova(lm(totalLooks ~ ageV1Mos * WordsProduced, data=pf.stat.td))
anova(lm(num.Imm ~ ageV1Mos * WordsProduced, data=pf.stat.td))
pf.td.goodtrials.lm = lm(WordsProduced ~ ageV1Mos * totalGoodTrials, data=pf.stat.td)
pf.td.totallooks.lm = lm(WordsProduced ~ ageV1Mos * totalLooks, data=pf.stat.td)
anova(pf.td.goodtrials.lm)



pf$looked = as.numeric(pf$lookF %in% c("Imm","Delay"))
pf.conds.totals = ddply(subset(pf,attempt=="only"), .(Subj,Distance,Separation,PositionRel),nrow,.drop=F)
colnames(pf.conds.totals)[5] = "trials"
pf.conds.looks = ddply(subset(pf,attempt=="only"), .(Subj,Distance,Separation,PositionRel),summarise,looked=sum(looked))
pf.conds = merge(pf.conds.totals, pf.conds.looks)
pf.conds$notLooked = pf.conds$trials - pf.conds$looked
pf.conds$Distance = as.factor(pf.conds$Distance)
pf.conds$PositionRel = as.factor(pf.conds$PositionRel)
pf.conds = merge(pf.conds, subjInfo, all.x=T)

pf.td.looks.glm = glm(cbind(looked,notLooked) ~ WordsProduced * ageV1Mos, 
                      data=droplevels(subset(pf.conds, ageGroup %in% c("18M","24M"))), family="binomial")
pf.td.looks.glm2 = glm(cbind(looked,notLooked) ~ WordsProduced + Distance * Separation, 
                      data=droplevels(subset(pf.conds, ageGroup %in% c("18M","24M"))), family="binomial")
summary(pf.td.looks.glm2)
anova(pf.td.looks.glm, pf.td.looks.glm2)
anova(pf.td.looks.glm2, test="Chisq")
summary(pf.td.looks.glm)

#ld only
pf.td.ld.looks.glm = glm(cbind(looked,notLooked) ~ WordsProduced * ageV1Mos * Separation * PositionRel, 
                      data=droplevels(subset(pf.conds, ageGroup %in% c("18M","24M") & Distance == "far")), family="binomial")

##### Summary plots #####
## total looks of each type, by age group
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

## proportion of looks by type, by age and vocab
ggplot(pf, aes(x=VocabByGroup,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) + 
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots +
  labs(title="Age and Vocab: Looks",y="Proportion of looks",x="Vocabulary size",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXVocabXAge.png",width=4, height=3, units="in",res=400)

## proportion of looks by type, by age and vocab (successful eye contact only)
ggplot(pf.good, aes(x=VocabByGroup,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) + 
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots +
  labs(title="Age and Vocab: Looks (good trials)",y="Proportion of looks",x="Vocabulary size",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXVocabXAge_good.png",width=4, height=3, units="in",res=400)

## total looks of each type, by Mullen score
pf.mullen = droplevels(merge(pf, subjInfo, by.x="SubjID", by.y="Subj", all=T))
length(levels(pf.mullen$SubjID))
ggplot(pf.mullen, aes(x=mullenVR,fill=look)) +
  geom_bar(position="fill") + 
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots +
  labs(title="Mullen VR: Looks (good trials)",y="Proportion of looks",x="Mullen VR score",fill="Child's look")

## Total immediate looks by Words Produced
ggplot(droplevels(subset(pf, look=="Imm")), aes(x=WordsProduced, color=SubjID)) + 
  geom_point(stat="bin") + facet_grid(~ageGroup) + theme_bw()

## Looks by individual vocabulary size
ggplot(pf, aes(x=WordsProduced,fill=look)) +
  geom_bar(position="fill", binwidth=2) + facet_grid(Group~.) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + xlim(0,400) +
  labs(title="Looks by type",y="Number of looks",x="Words Produced",fill="Child's look") + 
  wswl.smallplots + theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXExactVocab.png",width=7, height=3, units="in",res=300)

ggplot(pf, aes(x=WordsProduced,fill=look)) +
  geom_bar(position="fill", binwidth=25) + facet_grid(Group~.) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + xlim(0,400) +
  labs(title="Looks by type",y="Number of looks",x="Words Produced",fill="Child's look") + 
  wswl.smallplots + theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXVocab25.png",width=7, height=3, units="in",res=300)

## Successful trials by vocabulary size and group
ggplot(pf.lookcounts.bysubj.wide, aes(x=WordsProduced,y=totalGoodTrials,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title = "Trials with eye contact", y="Number of trials with eye contact", x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_goodtrialsXVocab.png", width=3.5,height=3,units="in",res=300)

ggplot(pf.lookcounts.bysubj.wide, aes(x=WordsProduced,y=(totalGoodTrials/totalTrials),color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title = "Trials with eye contact", y="Proportion of trials with eye contact", x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_goodtrialsXVocab_prop.png", width=3,height=3,units="in",res=300)

# td only
ggplot(pf.lookcounts.bysubj.wide, aes(x=WordsProduced,y=totalGoodTrials)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,20) +
  theme_bw() + labs(title = "Trials with eye contact", y="Number of trials with eye contact", x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Plots/PF/PF_TD_GoodTrialsXVocab_scatter.png", width=3, height=3, units="in", res=300)


## Successful trials by age and group
ggplot(droplevels(subset(pf.lookcounts.bysubj, lookType=="NoEyeContact"&ageGroup %in% c("18M","24M","WS"))), aes(x=ageV1Mos,y=(totalTrials-num),color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,24) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title = "Trials with eye contact", y="Number of trials with eye contact", x="Age (mos)") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_goodtrialsXVocab.png", width=3,height=3,units="in",res=300)

#reverse axes
ggplot(subset(pf.lookcounts.bysubj, lookType=="NoEyeContact"),aes(x=(totalTrials-num),y=WordsProduced,color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm, se=F)

## Immediate looks by vocabulary size and group 
ggplot(subset(pf.lookcounts.bysubj,lookType=="Imm"), aes(x=WordsProduced, y=(num/totalTrials),color=Group, shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title="Proportion immediate looks", y="Proportion immediate looks", x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_propImmXVocab.png",width=3, height=3, units="in",res=300)

## Immediate looks by vocabulary size and group: good trials only
ggplot(subset(pf.lookcounts.bysubj,lookType=="Imm"), aes(x=WordsProduced, y=(num/totalGoodTrials),color=Group, shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title="Proportion immediate looks", y="Proportion immediate looks", x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_propImmXVocab_goodtrials.png",width=3.5, height=3, units="in",res=300)

## Immediate looks by vocabulary size and group: good subjects only (>10 trials with eye contact)
ggplot(droplevels(subset(pf.lookcounts.bysubj,lookType=="Imm"&totalGoodTrials>9)), aes(x=WordsProduced, y=(num/totalTrials),color=Group, shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title="Proportion immediate looks", y="Proportion immediate looks", x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_propImmXVocab_goodsubj.png",width=3, height=3, units="in",res=300)

## Immediate looks (count), TD only
ggplot(pf.stat.td, aes(x=WordsProduced, y=num.Imm)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + 
  theme_bw() + labs(title="Immediate looks to target",y="Number of immediate looks",x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Plots/PF/PF_TD_ImmLooksXVocab_scatter.png", width=3, height=3, units="in", res=300)

ggplot(pf.stat.td, aes(x=WordsProduced, y=num.Delay)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + 
  theme_bw() + labs(title="Delayed looks to target",y="Number of delayed looks",x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Plots/PF/PF_TD_DelayLooksXVocab_scatter.png", width=3, height=3, units="in", res=300)

## looks (immediate or delayed) by vocab size and group
ggplot(subset(pf.lookcounts.bysubj,lookType=="Imm"), aes(x=WordsProduced, y=(totalLooks/totalTrials),color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title="Looks to target (imm. or delayed)",y="Proportion looks to target",x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_propTargetXVocab.png",width=3, height=3, units="in",res=300)

## looks (immediate or delayed) by vocab size and group (only good trials)
ggplot(subset(pf.lookcounts.bysubj,lookType=="Imm"), aes(x=WordsProduced, y=(totalLooks/totalGoodTrials),color=Group,shape=Group)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + ylim(0,1) + scale_color_manual(values=c("royalblue1","orangered")) +
  theme_bw() + labs(title="Looks to target (imm. or delayed)",y="Proportion looks to target",x="Vocabulary") + 
  wswl.smallplots
dev.print(png, file="Results_1-29-15/PF_propTargetXVocab_goodtrials.png",width=3, height=3, units="in",res=300)

## looks (count) by vocab size, TD only
ggplot(pf.stat.td, aes(x=WordsProduced, y=totalLooks)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + 
  theme_bw() + labs(title="Looks to target",y="Number of looks to target",x="Vocabulary") + 
  wswl.posterplots
dev.print(png, file="Plots/PF/PF_TD_AnyLooksXVocab_scatter.png", width=3, height=2.75, units="in", res=300)

## looks (count) by age, TD only
ggplot(pf.stat.td, aes(x=ageV1Mos, y=totalLooks)) + 
  geom_point() + geom_smooth(method=lm,se=FALSE) + 
  theme_bw() + labs(title="Looks to target (imm. or delayed)",y="Number of looks to target",x="Age (mos)") + 
  wswl.smallplots
dev.print(png, file="Plots/PF/PF_TD_AnyLooksXAge_scatter.png", width=3, height=3, units="in", res=300)

##### Near vs. Far #####
# groups, all trials
ggplot(pf, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Group: Looks by distance",y="Proportion of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXGroupXDistance.png", width=4, height=3, units="in",res=400)

# trials with successful eye contact
ggplot(pf.good, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Group: Looks by distance (good trials only)",y="Proportion of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXGroupXDistance_good.png", width=4, height=3, units="in",res=400)

# vocab, all trials
ggplot(pf, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~VocabGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Vocab size: Looks by distance",y="Number of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXVocab-allXDistance.png", width=3.5, height=3, units="in",res=400)

# vocab, trials with successful eye contact
ggplot(pf.good, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~VocabGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Vocab size: Looks by distance (good trials only)",y="Number of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXVocab-allXDistance_good.png", width=3.5, height=3, units="in",res=400)

## compare to order of presentation
ggplot(pf, aes(x=Distance,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~PF_Order) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots +
  labs(title="Looks by order and distance",y="Proportion of looks",x="Distance",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXOrderXDistance.png", width=3.5, height=3, units="in",res=400)

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


### plots with conds ####
# Separation X Distance, proportion (removed center targets)
ggplot(subset(pf,!(is.na(Separation))&Position!="center"), aes(x=Separation,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~Distance) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation & Distance",y="Proportion of looks",x="Separation",fill="Child's look") + 
  theme(legend.position = "none")
dev.print(png, file="Results_10-27-14/PF_looktypesXSeparationXDistance.png",width=2.6, height=3, units="in", res=300)

# Separation X Distance X Group, proportion (removed center targets)
ggplot(subset(pf,!(is.na(Separation))&Position!="center"), aes(x=Separation,fill=look)) +
  geom_bar(position="fill") + facet_grid(Distance~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation & Distance",y="Proportion of looks",x="Separation",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXSeparationXDistanceXGroup.png",width=3, height=4, units="in", res=300)

ggplot(subset(pf.good,!(is.na(Separation))&Position!="center"), aes(x=Separation,fill=look)) +
  geom_bar(position="fill") + facet_grid(Distance~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation & Distance (good trials)",y="Proportion of looks",x="Separation",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXSeparationXDistanceXGroup_good.png",width=3, height=4, units="in", res=300)



# Separation X Relative position (including center targets)
ggplot(subset(pf,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~Separation) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation & Rel. Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXSeparation.png",width=3, height=3, units="in", res=300)

# Separation X Relative position X Group (including center targets) 
ggplot(subset(pf,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(ageGroup~Separation) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation and Relative Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXSeparationXGroup.png",width=3, height=4, units="in", res=300)

ggplot(subset(pf.good,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(Separation~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Separation and Relative Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXSeparationXGroup_good.png",width=3, height=4, units="in", res=300)

# Distance X Relative position (including center targets)
ggplot(subset(pf,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~Distance) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Distance and Rel. Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXDistance.png",width=3, height=3, units="in", res=300)

# Distance X Relative position X Group (including center targets) 
ggplot(subset(pfLabeled,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(ageGroup~Distance) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Distance and Relative Position",y="Proportion of looks",x="Relative Position",fill="Child's look")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXDistanceXGroup.png",width=3, height=4, units="in", res=300)

ggplot(subset(pf.good,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(Distance~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Distance and Relative Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXDistanceXGroup_good.png",width=3, height=4, units="in", res=300)


#Vocab
ggplot(subset(pf.good,!(is.na(Separation))), aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~VocabGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Looks by Distance & Rel. Position",y="Proportion of looks",x="Relative Position",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXRelPosXVocabGroup_good.png",width=2.6, height=3, units="in", res=300)

## animacy
ggplot(pf, aes(x=Animacy,fill=look)) +
  geom_bar(position="fill") + facet_wrap(~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Group: Looks by Animacy",y="Proportion of looks",x="Distance",fill="Child's look") +
  theme(legend.position="none")
dev.print(png, file="Results_10-27-14/PF_looktypesXGroupXAnimacy.png", width=2.6, height=3, units="in",res=400)

ggplot(pf.good, aes(x=PositionRel,fill=look)) +
  geom_bar(position="fill") + facet_grid(Animacy~ageGroup) +
  theme_bw() + scale_fill_manual(values=c("green4","yellow","red","gray")) + wswl.smallplots + 
  labs(title="Group: Looks by Animacy & Rel. Pos.",y="Proportion of looks",x="Distance",fill="Child's look") 
dev.print(png, file="Results_10-27-14/PF_looktypesXGroupXAnimacy_good.png", width=4, height=4, units="in",res=400)


#### STATS ####

pf$goodtrial = as.numeric(pf$lookF!="NoEyeContact")
contrasts(pf$Group) <- contr.helmert(2)
summary(glm(goodtrial~Group*WordsProduced, data=pf, family="binomial"))

pf$Imm = as.numeric(pf$lookF=="Imm")
summary(glm(Imm~Group*WordsProduced, data=pf, family="binomial"))
summary(glm(Imm~WordsProduced, data=subset(pf, Group=="TD"), family="binomial")) #significant relationship for TD
summary(glm(Imm~WordsProduced, data=subset(pf, Group=="WS"), family="binomial")) #no relationship for WS

pf$ImmDel = as.numeric(pf$lookF %in% c("Imm","Delay"))
summary(glm(ImmDel~Group*WordsProduced, data=pf, family="binomial"))

pf.lookcounts.bysubj = droplevels(pf.lookcounts.bysubj)
contrasts(pf.lookcounts.bysubj$Group)<-contr.helmert(2)
summary(lm(WordsProduced ~ (totalGoodTrials/totalTrials)*Group,data=subset(pf.lookcounts.bysubj,lookType=="Imm")))


