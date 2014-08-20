##### Social Word Learning Analysis
##### Combining information from parent labeling and word learning task
##### Created 8/20/2014

setwd("/Volumes/Landau/PROJECTS/WS-SocialWordLearning_Shevaun/Results/")
load("wswl-data.Rda")
load("wswl-PL.Rda")
load("wswl-WL.Rda")

wl.bysubj.wide = reshape(wl.alltrials.bycond.bysubj[wl.alltrials.bycond.bysubj$Group=="TD",c(1:3)],
                         idvar="Subj", timevar="Cond", direction="wide")

pl.bysubj.wide = reshape(parentLabelTypes[,c(1,5,6,7)], idvar="Subj", timevar="category", direction="wide")

wl.pl = merge(wl.bysubj.wide, pl.bysubj.wide)
