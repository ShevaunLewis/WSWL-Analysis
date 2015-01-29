##### Social Word Learning Analysis
##### Point following and Word Learning
##### Created 1/29/2015


setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
source("WSWL-Analysis/wswl-getdata.r")
source("WSWL-Analysis/wswl-functions.r")

load("wswl-data.Rda")
load("wswl-pf.Rda")
load("wswl-WL.Rda")

wl.simple = wl.alltrials.bycond.bysubj[,c("Subj",)]
pf.wl = merge(wl.alltrials.bycond.bysubj, pf.lookcounts.bysubj, all=T)
ggplot(subset(pf.wl,Group=="TD"&lookType=="Imm"), aes(x=prop,y=acc)) + 
  geom_point() + facet_wrap(~Cond)