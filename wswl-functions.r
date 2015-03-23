##### Social Word Learning Analysis
##### Functions for plotting, aggregation, etc. 
##### Created 8/11/2014

library(plyr)
library(lme4)
library(stats)

tdgroup = c("18M","24M")

#### PLOTTING ####
library(ggplot2)
wswl.smallplots = theme(text = element_text(family="serif", size=10),
                        axis.title = element_text(size=10),
                        plot.title=element_text(size=12),
                        legend.title = element_text(size=10, face="plain"),
                        panel.grid = element_blank())
wswl.posterplots = theme(text = element_text(family="serif", size=14),
                         plot.title=element_text(size=16),
                         legend.title = element_text(face="plain"),
                         panel.grid = element_blank())


#### MEANS ####
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

### total accurate
wl.sumacc = function(dat, factors) {
  factorlist = makefactorlist(dat, factors)
  m = with(dat, aggregate(acc, factorlist, sum))
  colnames(m)[length(factorlist)+1] = "sum_acc"
  return(m)
}

#### Subject demographics ####
wswl.getAgeStats = function(subjGroup,name) {
  c(paste(name," n =",nrow(subjGroup)),
    paste("min: ",min(subjGroup$ageV1Mos)),
    paste("max: ",max(subjGroup$ageV1Mos)),
    paste("mean: ",round(mean(subjGroup$ageV1Mos),digits=1)),
    paste("median: ",median(subjGroup$ageV1Mos)))
}

wswl.subjsummary = function(subjInfoDf,filename) {
  stats = c(wswl.getAgeStats(subset(subjInfoDf, ageGroup=="WS"),"WS"),"\n",
            wswl.getAgeStats(subset(subjInfoDf, Group=="TD"),"TD"),"\n",
            wswl.getAgeStats(subset(subjInfoDf, ageGroup=="18M"),"TD: 18mos"),"\n",
            wswl.getAgeStats(subset(subjInfoDf, ageGroup=="24M"),"TD: 24mos"))
  writeLines(stats)
  writeLines(stats,filename)
}