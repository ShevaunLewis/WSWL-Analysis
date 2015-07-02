##### Social Word Learning Analysis
##### Basic descriptive plots, etc. 
##### Created 8/7/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")

source("WSWL-Analysis/wswl-getSubjInfo.r")
source("WSWL-Analysis/wswl-functions.r")

load("ROutput/wswl-subjInfo.Rda")

#### Subject demographics ####
wswl.subjsummary(subjInfo,"ROutput/ageSummary_6-23-15.txt")

#### TD 18-24 months only ####
td = droplevels(subset(subjInfo,ageGroup %in% c("18M","24M")))

summary(lm(WordsProduced~ageV1Mos+Sex, data=td))

ggplot(td, aes(x=ageV1Mos, y=WordsProduced)) +
  geom_point(size=2) + geom_smooth(method="lm", se=F) + theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by age",y="Words Produced",x="Age (mos)")
dev.print(png,file="Plots/Vocab/TD_VocabXAge_scatter.png",width=3,height=2.75,units="in",res=300)

ggplot(td, aes(x=Sex, y=WordsProduced)) +
  geom_boxplot() + theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by sex",y="Words Produced",x="Sex")

ggplot(td, aes(x=ageV1Mos, y=WordsProduced, color = Sex)) +
  geom_point(size=1.25) + geom_smooth(method="lm") + theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by age and sex",y="Words Produced",x="Age (mos)")


#### All subjects (but not including pilots) ####
all = droplevels(subset(subjInfo, Group %in% c("TD","WS")))

## Vocabulary by age
#scatter
ggplot(all, aes(x=ageV1Mos, shape=Group, color=Group, y=WordsProduced)) +
  geom_point(size=1.25) + geom_smooth(method="lm", se=F) + theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by age",y="Vocabulary (words)",x="Age (mos)",shape="Group")
dev.print(png, file="Plots/Vocab/VocabXAge_all_scatter.png",width=800, height=600, res=200)

ggplot(all, aes(x=ageV1Mos, shape=Group, color=Group, y=WordsProduced)) +
  geom_point(size=1.25) + geom_smooth(method="lm", se=F) + theme_bw() + wswl.posterplots +
  scale_color_manual(values=c("black","gray")) +
  labs(title="Vocabulary by age",y="Vocabulary (words)",x="Age (mos)",shape="Group")
dev.print(png, file="Plots/Vocab/VocabXAge_all_scatter-bw.png",width=800, height=600, res=200)

contrasts(all$Group)= contr.helmert(2)
vocab.lm1 = lm(WordsProduced~Group+ageV1Mos, data=all)
vocab.lm2 = lm(WordsProduced~Group*ageV1Mos, data=all)
vocab.lm3 = lm(WordsProduced~Group*ageV1Mos+Sex, data=all)
vocab.lm4 = lm(WordsProduced~Group*ageV1Mos*Sex, data=all)
anova(vocab.lm1, vocab.lm2, vocab.lm3, vocab.lm4)
summary(vocab.lm4)

#boxplot
ggplot(all, aes(x=ageV1Mos, color=Group, y=WordsProduced)) +
  geom_boxplot(aes(outlier.colour=Group), alpha=0.4, position="identity",outlier.size=1.25) + 
  theme_bw() + wswl.posterplots + 
  labs(title="Vocabulary by group",y="Vocabulary (words)",x="Age (mos)")
dev.print(png, file="Plots/Vocab/VocabXAge_all_boxplot.png",width=3.5, height=3, units="in",res=200)

# overlay norms from CDI
norms = read.csv("DataSpreadsheets/CDI_norms.csv",header=T)

ggplot(norms, aes(x=Age)) + geom_line(aes(y=X50th),linetype="dotted", color="gray") + 
  geom_ribbon(aes(ymin=X5th,ymax=X95th),alpha=0.05) +
  geom_point(data=all, aes(x=ageV1Mos, shape=Group, color=Group, y=WordsProduced),size=1.25) + 
  geom_smooth(data=all, aes(x=ageV1Mos,y=WordsProduced,color=Group),method="lm",se=F) +
  theme_bw() + wswl.posterplots +
  labs(title="Vocabulary by age (compared to CDI norms)",y="Vocabulary (words)",x="Age (mos)",shape="Group") 
dev.print(png,file="Plots/Vocab/VocabXAge_all_scatter_norms.png",width=6,height=4,units="in",res=200)


#### 3 groups: 18M, 24M, and WS (no pilots, 30M) ####
subjInfo.3groups = droplevels(subset(subjInfo,ageGroup %in% c("18M","24M","WS")))
### Vocabulary by age ###
ggplot(subjInfo.3groups, aes(x=ageV1Mos, shape=Group, y=WordsProduced)) +
  geom_point(size=1.25) + theme_bw() + wswl.smallplots +
  labs(title="Vocabulary by age",y="Words Produced",x="Age (mos)",shape="Group")
dev.print(png,file="Plots/Vocab/VocabXAge_scatter.png", width=700,height=600,res=200)
dev.print(png,file="Plots/Vocab/VocabXAge_scatter_small.png", width=600,height=450,res=200)

ggplot(subjInfo.3groups, aes(x=ageV1Mos, color=ageGroup, y=WordsProduced)) +
  geom_boxplot() + geom_point(size=1) + 
  theme_bw() + scale_color_manual(values=c("royalblue1","royalblue4","orangered")) +
  labs(title="Vocabulary by group and age", y="Words Produced", x="Age (mos)", color="Group") +
  wswl.smallplots
dev.print(png,file="Plots/Vocab/VocabX3Group_boxplot.png", width=3.5,height=3,units="in",res=300)

ggplot(droplevels(subset(subjInfo,ageGroup %in% c("18M","24M","WS"))), aes(x=ageV1Mos, color=Group, y=WordsProduced)) + 
  geom_boxplot() + geom_point(size=1) + 
  theme_bw() + scale_color_manual(values = c("royalblue1","orangered")) + 
  labs(title="Vocabulary by group and age", y="Vocabulary", x="Age (mos)", color="Group") +
  wswl.smallplots
dev.print(png,file="Plots/Vocab/VocabX2Group_boxplot.png", width=3.5,height=3,units="in",res=300)

### Mullens for each group
mullens = reshape(droplevels(subset(subjInfo, Group%in%c("TD","WS"))), 
                  varying=c("mullenGM","mullenVR","mullenFM","mullenRL","mullenEL"),
                  times=c("mullenGM","mullenVR","mullenFM","mullenRL","mullenEL"),
                  idvar="Subj",direction="long",v.names="score", new.row.names=NULL)
colnames(mullens)[11]="mullen"
mullens$mullen = ordered(mullens$mullen, levels=c("mullenGM","mullenFM","mullenVR","mullenRL","mullenEL"),
                         labels=c("Gross Motor","Fine Motor","Visual Reception","Receptive Lang.","Expressive Lang."))
mullens = droplevels(subset(mullens, mullen!="Gross Motor"))

ggplot(mullens, aes(x=Group, y=score, color=Group)) + 
  geom_boxplot() +facet_grid(.~mullen) + 
  theme_bw() + labs(title="Mullen scores", y="Raw score") +
  wswl.posterplots + theme(strip.text = element_text(size=10))
dev.print(png, file="Plots/Vocab/MullensXGroup_boxplot.png",width=6.5, height=4, units="in",res=200)

ggplot(mullens, aes(x=ageV1Mos, y=score, color=Group)) + 
  geom_point() + geom_smooth(method="lm",se=F,color="gray") + facet_grid(.~mullen) + 
  theme_bw() + labs(title="Mullen scores", y="Raw score", x="Age (mos)") +
  wswl.posterplots + theme(strip.text = element_text(size=10))
dev.print(png, file="Plots/Vocab/MullensXAgeXGroup_scatter.png",width=6.5, height=4, units="in",res=200)

ggplot(all, aes(x=mullenVR,y=WordsProduced,color=Group)) + 
  geom_point() +geom_smooth(method="lm",se=F)

ggplot(subset(all,WordsProduced<250), aes(x=mullenVR,y=WordsProduced,color=Group)) + 
  geom_point() +geom_smooth(method="lm",se=F)


contrasts(all$Group) = contr.sum(2)

summary(lm(mullenFM~Group*ageV1Mos, data=all,contrasts=list(Group="contr.helmert")))
summary(lm(mullenVR~ageV1Mos+Group, data=all,contrasts=list(Group="contr.helmert")))
rl.lm1 = lm(mullenRL~ageV1Mos+Group, data=all,contrasts=list(Group="contr.helmert"))
rl.lm2 = lm(mullenRL~ageV1Mos*Group, data=all,contrasts=list(Group="contr.helmert"))
summary(lm(mullenEL~ageV1Mos+Group, data=all,contrasts=list(Group="contr.helmert")))

summary(lm(mullenFM~Group, data=all))
summary(lm(mullenVR~Group, data=all))
summary(lm(mullenRL~Group, data=all))
summary(lm(mullenEL~Group, data=all))

summary(mullens)

