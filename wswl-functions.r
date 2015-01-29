##### Social Word Learning Analysis
##### Functions for plotting, aggregation, etc. 
##### Created 8/11/2014

library(plyr)
library(lme4)
library(stats)


#### PLOTTING ####
library(ggplot2)
wswl.smallplots = theme(text = element_text(family="serif", size=10),
                        axis.title = element_text(size=10),
                        plot.title=element_text(size=12),
                        legend.title = element_text(size=10, face="plain"),
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