##### Social Word Learning Analysis
##### Point following (first code)
##### Created 9/18/14

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")

load("wswl-data.Rda")

## Calculate coding reliability
pf2codes = droplevels(subset(pf_full,look2!=""))
pf2codes$lookRel = as.numeric(as.character(pf2codes$look1)==as.character(pf2codes$look2))
mean(pf2codes$lookRel)
rel = with(pf2codes, aggregate(lookRel,makefactorlist(pf2codes,c("Separation","Distance")), mean))
xtabs(~lookRel+Separation+Distance,data=pf2codes)
pf2codes.exp = subset(pf2codes,Position!="center")
relexp = with(pf2codes.exp, aggregate(lookRel,makefactorlist(pf2codes.exp,c("Separation","Distance")), mean))

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


