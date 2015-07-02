#Harry Burke
#July 1, 2015
#combines CDI and point following and does plots/statistics
# run getBreakdownCDI and getPF to get the rda files


#### Load applicable libraries
library ("ggplot2")
library ("plyr")
library ("languageR")
library("reshape2")

### Set working directory
setwd("/Users/harryson/Desktop/R Directory/WS study/Analysis/")

#### Load point following data ####
# Data is correct as of 6/18/2015
load("wswl-pf.Rda")
load("wswl-cdi_breakdown.Rda")

#### merge cdibreakdown and point following data ####
keep <- c("Subj", "totalTrials", "totalLooks", "totalGoodTrials", "num.Delay", "num.Imm", "num.NoEyeContact", "num.NoTarget")
pf.lookcounts.bysubj.wide.subset <- pf.lookcounts.bysubj.wide[keep]
dflist <- list(cdi.breakdown.all, pf.lookcounts.bysubj.wide.subset)
idx <- Reduce(intersect, lapply(dflist, rownames))
cdi.pf <- cbind(cdi.breakdown.all[idx,], pf.lookcounts.bysubj.wide.subset[idx,])


##### Analysis ####

#### Number of immediate looks vs each word category ####

### TD ###

cdi.pf.td <- droplevels(subset(cdi.pf, Group=="TD"))
Nouns.td <- cdi.pf.td$Proportion_Nouns
Verbs.td <- cdi.pf.td$Proportion_Verbs
Adjectives.td <- cdi.pf.td$Proportion_Adjectives
Function.td <- cdi.pf.td$Proportion_Function
df.immlooks.td <- data.frame(Nouns.td, Verbs.td, Adjectives.td, Function.td, cdi.pf.td$Subject)
df.immlooks1.td <- melt(df.immlooks.td)
immlooks.td <- cdi.pf.td$num.Imm
immlooks1.td <- rep(immlooks.td, 4)
df.immlooks2.td <- cbind(df.immlooks1.td, immlooks1.td)
ggplot(df.immlooks2.td, aes(immlooks1.td, value, colour = variable)) + geom_point() + geom_smooth(method=lm,se=FALSE)
dev.print(png, file="Plots/CDI-pf/Number of immediate looks vs word categories_TD.png", width=15.3, height=9, units="in", res=300)

### WS ###
#only has two subjects, lines don't mean much

cdi.pf.ws <- droplevels(subset(cdi.pf, Group=="WS"))
Nouns.ws <- cdi.pf.ws$Proportion_Nouns
Verbs.ws <- cdi.pf.ws$Proportion_Verbs
Adjectives.ws <- cdi.pf.ws$Proportion_Adjectives
Function.ws <- cdi.pf.ws$Proportion_Function
df.immlooks.ws <- data.frame(Nouns.ws, Verbs.ws, Adjectives.ws, Function.ws, cdi.pf.ws$Subject)
df.immlooks1.ws <- melt(df.immlooks.ws)
immlooks.ws <- cdi.pf.ws$num.Imm
immlooks1.ws <- rep(immlooks.ws, 4)
df.immlooks2.ws <- cbind(df.immlooks1.ws, immlooks1.ws)
ggplot(df.immlooks2.ws, aes(immlooks1.ws, value, colour = variable)) + geom_point() + geom_smooth(method=lm,se=FALSE)
dev.print(png, file="Plots/CDI-pf/Number of immediate looks vs word categories_WS.png", width=15.3, height=9, units="in", res=300)






