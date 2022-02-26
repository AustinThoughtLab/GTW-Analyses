#Tyler and Katharine's awesome analysis file!

#### Install Libraries ####
#source('~/Documents/Search_for_the_Truth/Research/*Methods/Stats/R/KSmith_Rfunctions.R')
source('~/Dropbox/Projects/69 TempGestDev/Rfunctions.R')
library(gmodels)
library(gplots)
library(plyr)
library(ordinal)
library(lme4)
library(ggplot2)

library(gridExtra)
library(MPDiR)
library(dplyr)
library(tidyr)
library(quickpsy)

source('~/Documents/Search_for_the_Truth/Research/*Methods/Stats/ggplot_theme.R')

#### Get Data ####
#Set working directory
#setwd("~/Documents/Search_for_the_Truth/Research/Time/TempGestDev/Results/Analysis/Timeline")
setwd("~/Dropbox/Projects/69 TempGestDev")

#Data
Temp <- read.csv('TempGestDataKids082616withtestsite.csv', header=T)
Temp <- subset(Temp, SubjID!="") #remove blank lines

#Remove excluded kids
Temp <- subset(Temp, Excluded != "X")

#Remove wrong ages
Temp$Age <- as.numeric(as.character(Temp$Age))
Temp$AgeYears <- factor(floor(Temp$Age))
Temp <- subset(Temp, AgeYears %in% c('3','4','5','6','7','8')) #restrict to target age range
Temp$AgeYears <- factor(Temp$AgeYears)

#get and combine adult data
adult.data <- read.csv('tempgestdata_adults031816.csv')

adult.data <- subset(adult.data, SubjID!="") #remove blank lines
adult.data$AgeYears <- "adult"

Temp <- rbind.fill(Temp[1:32], adult.data)
Temp$Age <- as.numeric(as.character(Temp$Age))
Temp$AgeYears <- factor(Temp$AgeYears)

#### Add some extra variables ####
#is it forced choice?

Temp$FC <- Temp$ItemType %in% c("TermCompFC1","TermCompFC2") 
#For forced-choice items, set "CorrectBN" to past (i.e. 1) for items/events in the past; keep the same value for timeline items
Temp$CorrectBN <- with(Temp, ifelse(ItemType=="TermCompFC1", ifelse(Item %in% c("BreakfastToday","LastAge"),1,0), CorrectBN)) 

#Calculate RankDif, the difference between rank on timeline and correct rank
Temp$Rank <- as.numeric(as.character(Temp$Rank))
Temp$RankDif <- abs(Temp$CorrectRank - Temp$Rank)


#### Timeline Task ####
#Subset data
Temp.TL <- subset(Temp, FC==F) 
# 052014-SL is entered twice, and the numbers differ. Removed for now as a clerical error. 
Temp.TL <- subset(Temp.TL, SubjID != "052014-SL") 

Temp.TL$Past <- factor(Temp.TL$CorrectBN)
Temp.TL$Past <- factor(with(Temp.TL, ifelse(Item=="LastWeek", 1, as.character(Past))))
Temp.TL$AgeYears <- ordered(Temp.TL$AgeYears)
Temp.TL$Item <- factor(Temp.TL$Item)
#TempDistance indicates how far events should be from NOW -- 1 for more proximal events/times, 2 for more distal events/times
Temp.TL$TempDistance <- factor(with(Temp.TL, ifelse(Item %in% c("BreakfastToday","DinnerToday","ThisMorning","Tonight","Yesterday","Tomorrow"), 1, 
                                                    ifelse(Item %in% c("LastWeek","NextWeek","LastAge","NextAge","LastYear","NextYear"), 2,
                                                           "other"))))

#### Trial Number ####
#Now figure out the trialnumber and make it numeric
Temp.TL$TrialNum <- with(Temp.TL,ifelse(Order==1,
                                         match(Item,c("BreakfastToday","NextAge","DinnerToday","LastAge","LastWeek","Tomorrow",
                                                      "Tonight","ThisMorning","NextWeek","NextYear","Yesterday","LastYear")
                                         ),
                                         match(Item,c("LastAge","DinnerToday","NextAge","BreakfastToday","ThisMorning","Tonight",
                                                      "Tomorrow","LastWeek","LastYear","Yesterday","NextYear","NextWeek")
                                         )
                                  ))
Temp.TL$TrialNum <- as.numeric(Temp.TL$TrialNum)

Temp.TL$BNRight <- with(Temp.TL, ifelse(Item != "ThisMorning" & DistFromMid=="0", 0, BNRight))
Temp.TL$BNRight <- with(Temp.TL, ifelse(Item=="ThisMorning" & DistFromMid=="0", 1, BNRight))

#### Line Number ####
#Some ppts have incorrect line numbers, so just do it manually
# xtabs(~Item, subset(Temp.TL, LineNum==1))
# xtabs(~Item, subset(Temp.TL, LineNum==2))
# xtabs(~Item, subset(Temp.TL, LineNum==3))
Temp.TL$LineNum <- with(Temp.TL, ifelse(Item %in% c("BreakfastToday","DinnerToday","LastAge","NextAge"), 1, 
                                         ifelse(Item %in% c("LastWeek","ThisMorning","Tomorrow","Tonight"), 2,
                                                ifelse(Item %in% c("NextWeek","NextYear","Yesterday","LastYear"),3,4))))


Temp.TL$DistFromMid <- as.numeric(as.character(Temp.TL$DistFromMid))

#### Plotting median positions along timeline ####
Temp.TL$DistFromMidNum <- as.numeric(Temp.TL$DistFromMid)
Temp.TL$DistFromMidNum <- ifelse(Temp.TL$DistFromMidNum == 0, .01, Temp.TL$DistFromMidNum)

Temp.TL.max <- aggregate(abs(DistFromMidNum) ~ SubjID + LineNum, Temp.TL, max)
colnames(Temp.TL.max) <- c("SubjID","LineNum","maxLineDist")

Temp.TL$SubjID <- factor(Temp.TL$SubjID)

Temp.TL <- merge(Temp.TL, Temp.TL.max)

Temp.TL$signedScaledDist <- Temp.TL$DistFromMidNum/Temp.TL$maxLineDist
Temp.TL$relOrd <- with(Temp.TL, ifelse(Past == "1", abs(CorrectRank-3), CorrectRank-2))

#### Plot all median timelines ####
median.lines <- aggregate(signedScaledDist ~ relOrd + Past + AgeYears + Item + LineNum, subset(Temp.TL), median)
median.lines$se <- aggregate(signedScaledDist ~ relOrd + Past + AgeYears + Item + LineNum, subset(Temp.TL), se)$signedScaledDist
median.lines$overallDeicticOrder <- as.ordered(as.character(mapvalues(median.lines$Item, c("LastYear","LastWeek","Yesterday","ThisMorning","Tonight","Tomorrow","NextWeek","NextYear"), c(1,2,3,4,5,6,7,8))))
median.lines$deicticSize <- 12 + as.numeric(median.lines$overallDeicticOrder)
median.lines$NewItems <- with(median.lines, ifelse(Item == "DinnerToday","Dinner", ifelse(Item=="BreakfastToday","Breakfast",as.character(Item))))
# median.lines$NewItemsNum <- with(median.lines, match(NewItems, c("LastWeek","ThisMorning","Tonight","Tomorrow","LastYear","Yesterday","NextWeek","NextYear","LastAge","Breakfast","Dinner","NextAge")))
median.lines$ItemName <- factor(with(median.lines, ifelse(AgeYears =="adult", as.character(NewItems), "")))

median.lines$lineNames <- with(median.lines, ifelse(LineNum==1, "Events", ifelse(LineNum==2, "Deictic Terms 1","Deictic Terms 2")))
# median.lines$AgeYears = with(median.lines, factor(AgeYears, levels = rev(levels(AgeYears))))

timeline.endpoints2 <- ggplot(subset(median.lines, AgeYears %in% c(3,4,5,6,7,8,"adult")), 
                              aes(x=AgeYears, y=signedScaledDist, group=Item, color=Past, fill=Past)) + 
  geom_ribbon(aes(ymin=signedScaledDist-se, ymax=signedScaledDist+se), alpha=.3) +
  geom_path(size=2) + 
  geom_point(aes(size=relOrd*3),size=8) +
  geom_segment(aes(y=0, yend=0, x=.9, xend=1.1), linetype="solid", size=1, color="black") + 
  geom_segment(aes(y=0, yend=0, x=1.9, xend=2.1), linetype="solid", size=1, color="black") + 
  geom_segment(aes(y=0, yend=0, x=2.9, xend=3.1), linetype="solid", size=1, color="black") + 
  geom_segment(aes(y=0, yend=0, x=3.9, xend=4.1), linetype="solid", size=1, color="black") + 
  geom_segment(aes(y=0, yend=0, x=4.9, xend=5.1), linetype="solid", size=1, color="black") + 
  geom_segment(aes(y=0, yend=0, x=5.9, xend=6.1), linetype="solid", size=1, color="black") + 
  geom_segment(aes(y=0, yend=0, x=6.9, xend=7.1), linetype="solid", size=1, color="black") + 
  geom_text(aes(label=ItemName), color="black", size=6, angle=25, nudge_x = .35) + 
  facet_wrap(~lineNames) +
  scale_color_discrete(name='', labels=c("future","past")) + 
  scale_fill_discrete(name='', labels=c("future","past")) + 
  scale_y_continuous(name="median location", limits=c(-1.24,1.24), breaks=c(0), labels=c("")) + 
  scale_x_discrete(name="age") + 
  theme_bw() + 
  theme(legend.position = "right",
        panel.grid.major.y = element_line(size=2, color="darkgrey"),
        panel.grid.major.x = element_line(size=0),
        panel.grid.minor.x = element_line(size=0),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(2)),
        legend.text = element_text(size = rel(1.5)),
        strip.text = element_text(size = rel(1.5), face="bold"),
        strip.background = element_rect(fill = 'white', color="white"),
        panel.background = element_rect(color="white"),
        panel.border = element_blank(),
        # panel.margin = unit(8, "lines"),
        axis.line = element_line(size = 3, colour = "blue"),
        axis.ticks = element_blank()) +
  coord_flip()
timeline.endpoints2

#### Analyses of ordinal rank ####
#Sum for the entire line, to get an aggregate score of how much people diverged
# from the correct ordering. (i.e. error score for each line)
d.TL4 <- aggregate(RankDif ~ linenum + SubjID + agegroup + itemtype, d.TL3, sum)

#Now average that error score across all the participant' timelines 
Temp.TL4 <- aggregate(RankDif ~ ItemType +  AgeYears + SubjID, Temp.TL3, mean)
Temp.TL4$ItemType <- factor(Temp.TL4$ItemType)


# Anova of aggregate Rank error ####
rank.agg <- subset(Temp.TL4)
dim(rank.agg)
Temp.TL.aov <- aov(RankDif ~ AgeYears * ItemType + Error(SubjID/(ItemType)),rank.agg)
summary(Temp.TL.aov) #highly sig effect of age

aggregate(RankDif~ItemType, subset(Temp.TL4), mean)
aggregate(RankDif~AgeYears, subset(Temp.TL4), mean)

#linear regression of order error onto age (in years)
Temp.TL5 <- aggregate(RankDif ~ AgeYears + SubjID, subset(Temp.TL4, AgeYears != "adult"), mean)
Temp.TL5$AgeYears <- as.numeric(as.character(Temp.TL5$AgeYears))
rank.agg.lm <- lm(RankDif ~ AgeYears, subset(Temp.TL5))
summary(rank.agg.lm)

rank.agg2 <- subset(rank.agg, AgeYears != "adult")
rank.agg2$AgeYears <- as.numeric(as.character(rank.agg2$AgeYears))
rank.agg.lm.d <- lm(RankDif ~ AgeYears, subset(rank.agg2, ItemType == "DeicticLine"))
summary(rank.agg.lm.d)
rank.agg.lm.e <- lm(RankDif ~ AgeYears, subset(rank.agg2, ItemType == "EventLine"))
summary(rank.agg.lm.e)

# t-tests by age of aggregate Rank error ####
# t-tests at each age
Temp.TL5 <- aggregate(RankDif ~ AgeYears + SubjID, subset(Temp.TL4), mean)

with(subset(Temp.TL5, AgeYears %in% c("3")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, AgeYears %in% c("4")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, AgeYears %in% c("5")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, AgeYears %in% c("6")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, AgeYears %in% c("7")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, AgeYears %in% c("8")), t.test(RankDif, mu=5))

with(subset(Temp.TL5, AgeYears %in% c("3","4")), t.test(RankDif~AgeYears, var.equal=T))
with(subset(Temp.TL5, AgeYears %in% c("4","5")), t.test(RankDif~AgeYears, var.equal=T))
with(subset(Temp.TL5, AgeYears %in% c("5","6")), t.test(RankDif~AgeYears, var.equal=T))
with(subset(Temp.TL5, AgeYears %in% c("6","7")), t.test(RankDif~AgeYears, var.equal=T))
with(subset(Temp.TL5, AgeYears %in% c("7","8")), t.test(RankDif~AgeYears, var.equal=T))

with(subset(Temp.TL5, AgeYears %in% c("7","adult")), t.test(RankDif~AgeYears, var.equal=T))
with(subset(Temp.TL5, AgeYears %in% c("8","adult")), t.test(RankDif~AgeYears, var.equal=T))

# plot aggregate rank over time - bar graph ####
plot.data <- subset(Temp.TL4)
plot.data.agg <- aggregate(RankDif ~ AgeYears, plot.data, mean)
plot.data.agg$varRank <- aggregate(RankDif ~ AgeYears , plot.data, se)$RankDif
# plot.data.agg$AgeYears <- as.numeric(plot.data.agg$AgeYears)
plot.rank1 <- ggplot(data=plot.data.agg, aes(x=AgeYears, y=RankDif)) + 
  geom_hline(yintercept=5, linetype="solid",color="black",size=1.3) +
  geom_pointrange(aes(ymax=RankDif + varRank, ymin=RankDif - varRank), size=1.5, position=position_dodge(width=0.3)) +
  # geom_smooth(method="lm", size=2, se = F, color = "black") +
  scale_x_discrete(name = "age (years)") +
  scale_y_continuous(name = "Order errors") +
  science.theme +
  theme(legend.position = c(1,.7))
plot.rank1
  

# Cumulative link mixed model on kids, with RankDif as an ordered DV ####
Temp.TL$AgeYearsNum <- as.numeric(Temp.TL$AgeYears)
Temp.TL$cAgeYearsNum <- Temp.TL$AgeYearsNum - 3
Temp.TL$oRankDif <- as.ordered(Temp.TL$RankDif)
Temp.TL$ItemType <- factor(Temp.TL$ItemType)
Temp.TL$cItemType <- sapply(Temp.TL$ItemType,function(i) contr.sum(2)[i,])

temp.TL.m1 <- clmm(oRankDif ~ 1 + cItemType * cAgeYearsNum + (1 + cItemType|SubjID) + (1 + cAgeYearsNum|Item), subset(Temp.TL, AgeYears!="adult"), link = "logit") 
# temp.TL.m1.adults <- clmm(oRankDif ~ ItemType * AgeYears + (ItemType|SubjID) + (AgeYearsNum|Item), subset(Temp.TL), link = "logit") 
summary(temp.TL.m1)
# summary(temp.TL.m1.adults)
newdata <- subset(Temp.TL, AgeYearsNum==3 & ItemType == "DeicticLine")
newCI <- ci(predict(temp.TL.m1, newdata=newdata, type="response"))
fitted.values(temp.TL.m1)

temp.TL.m2 <- clmm(oRankDif ~ cItemType + cAgeYearsNum:cItemType + (cItemType|SubjID) + (cAgeYearsNum|LineNum), subset(Temp.TL, AgeYears!="adult"), link = "logit") 
summary(temp.TL.m2)
anova(temp.TL.m1, temp.TL.m2)

# for each subject, compare each trial to the previous trial #############
Temp.TL$ComparedToPrevious <- ""
RelRankData <- subset(Temp.TL, SubjID=="nothing")
for(sub_i in levels(Temp.TL$SubjID)){
  for(trial_i in c(2,3,4,6,7,8,10,11,12)){
    foo <- subset(Temp.TL, SubjID==sub_i & TrialNum==trial_i)
    fooPrev <- subset(Temp.TL, SubjID==sub_i & TrialNum==trial_i-1)
    foo$ComparedToPrevious <- ifelse(foo$Rank > fooPrev$Rank, "R","L")
    foo$CorrComparedToPrevious <- ifelse(foo$CorrectRank > fooPrev$CorrectRank, "R","L")
    RelRankData <- rbind(RelRankData, foo)
  }
  #        for(trial_i in c(1,5,9)){
  #          foo <- subset(data, SubjID==sub_i & TrialNum==trial_i)
  #          newdata <- rbind(newdata, foo)
  #        }
}

RelRankData$AccRelRank <- with(RelRankData, ifelse(ComparedToPrevious == CorrComparedToPrevious, 1, 0))
RelRankData <- subset(RelRankData, AgeYears %in% c("3","4","5","6","7","8","adult"))
aggregate(AccRelRank~ItemType + AgeYears, RelRankData, mean, na.rm=T)
RelRankData.agg <- aggregate(AccRelRank~AgeYears + ItemType  + SubjID, RelRankData, mean, na.rm=T)
RelRankData.agg$ItemType <- factor(RelRankData.agg$ItemType)
RelRankData.agg$SubjID <- factor(RelRankData.agg$SubjID)
xtabs(~CorrectBN + ItemType, RelRankData.agg)
subset(as.data.frame(xtabs(~SubjID, RelRankData.agg)), Freq<2)
RelRankData.agg$tAccRelRank <- asin(sqrt(RelRankData.agg$AccRelRank))

RelRankData.aov <- aov(AccRelRank~AgeYears * ItemType + Error(SubjID/(ItemType)), RelRankData.agg)
summary(RelRankData.aov) #main effect of age, marginal interaction
aggregate(AccRelRank ~ ItemType, RelRankData.agg, mean)
aggregate(AccRelRank ~ AgeYears, RelRankData.agg, mean)

RelRankData.agg2 <- aggregate(AccRelRank ~ AgeYears + SubjID, RelRankData.agg, mean)
aggregate(AccRelRank ~ AgeYears, RelRankData.agg, mean)
RelRankData.agg2$tAccRelRank <- asin(sqrt(RelRankData.agg2$AccRelRank))

with(subset(RelRankData.agg2, AgeYears=="3"), t.test(x=AccRelRank, mu=.5))
with(subset(RelRankData.agg2, AgeYears=="4"), t.test(x=AccRelRank, mu=.5))

with(subset(RelRankData.agg2, AgeYears %in% c("7", "adult")), t.test(AccRelRank ~ AgeYears, var.equal=T))
with(subset(RelRankData.agg2, AgeYears %in% c("8", "adult")), t.test(AccRelRank ~ AgeYears, var.equal=T))

# check for site effects
RelRank.site.lm <- lmer(AccRelRank ~ AgeYears*TestSite + (1|SubjID), subset(RelRankData, AgeYears %in% c('6','7','8') & TestSite %in% c('UCSD','Cal')))
Anova(RelRank.site.lm)


# Plot 1-back order accuracy ####
plot.data.agg <- aggregate(AccRelRank ~ AgeYears, RelRankData.agg, mean)
plot.data.agg$varRank <- aggregate(AccRelRank ~ AgeYears, RelRankData.agg, se)$AccRelRank

plot.1back.order <- ggplot(data=plot.data.agg, aes(x=AgeYears, y=AccRelRank)) + 
  geom_hline(yintercept=.5, linetype="dashed", color="red", size=1.3) +
  # geom_hline(yintercept=1, linetype="solid",color="black",size=1.3) +
  geom_pointrange(size=1.5, position=position_dodge(width=0.3), aes(ymax=AccRelRank + varRank, ymin=AccRelRank - varRank)) +
  scale_x_discrete(name = "Age (years)") +
  scale_y_continuous(name = "Order", breaks= c(.5,.6,.7,.8,.9,1), limits = c(.45, 1)) +
  science.theme +
  theme(axis.title = element_text(size=20))
plot.1back.order

#### Analysis of past/future assignment ####

# Past/Future -- do subjects attribute P items to left of now and F items to right of now?
Temp.PF2 <- aggregate(BNRight ~ Age + SubjID + AgeYears + ItemType, Temp.TL, mean)
Temp.PF2$ItemType <- factor(Temp.PF2$ItemType)
Temp.PF2$tBNRight <- asin(sqrt(Temp.PF2$BNRight))

Temp.PF.aov <- aov(BNRight ~ AgeYears * ItemType + Error(SubjID/ItemType),Temp.PF2)
summary(Temp.PF.aov) #Effects of Age

aggregate(BNRight ~ AgeYears, Temp.PF2, mean)

Temp.PF3 <- aggregate(BNRight ~ Age + AgeYears + SubjID, subset(Temp.PF2), mean)
with(subset(Temp.PF3, AgeYears=="3"), t.test(x=BNRight, mu=.5))
with(subset(Temp.PF3, AgeYears=="4"), t.test(x=BNRight, mu=.5))
with(subset(Temp.PF3, AgeYears %in% c("6", "7")), t.test(BNRight ~ AgeYears, var.equal=T))
with(subset(Temp.PF3, AgeYears %in% c("6", "adult")), t.test(BNRight ~ AgeYears, var.equal=T))
with(subset(Temp.PF3, AgeYears %in% c("7", "adult")), t.test(BNRight ~ AgeYears, var.equal=T))
with(subset(Temp.PF3, AgeYears %in% c("7", "8")), t.test(BNRight ~ AgeYears, var.equal=T))
with(subset(Temp.PF3, AgeYears %in% c("8", "adult")), t.test(BNRight ~ AgeYears, var.equal=T))

Temp.PF4 <- subset(Temp.PF3, AgeYears != "adult")
Temp.PF4$AgeYears <- as.numeric(as.character(Temp.PF4$AgeYears))
Temp.PF4.m1 <- lm(BNRight ~ AgeYears, Temp.PF4)
summary(Temp.PF4.m1)

# check for test-site effects
PF.site <- lmer(BNRight ~ AgeYears * TestSite + (1|SubjID), subset(Temp.TL, AgeYears %in% c('6','7','8') & TestSite %in% c('UCSD','Cal')))
Anova(PF.site)

# item order effects

Temp.PF5 <- aggregate(BNRight ~ AgeYears + SubjID + Order + ItemType, Temp.TL, mean)
Temp.PF6 <- aggregate(BNRight ~ AgeYears + SubjID + Order + Item, Temp.TL, mean)

Temp.PF.ord.aov <- aov(BNRight ~ AgeYears * ItemType * Order + Error(SubjID/ItemType),Temp.TL)
summary(Temp.PF.ord.aov)  

Temp.PF.ord.d <- aov(BNRight ~ AgeYears *  Order + Error(SubjID), subset(Temp.TL, ItemType=="DeicticLine"))
summary(Temp.PF.ord.d)   -- main effects, no interaction

Temp.PF.ord.e <- aov(BNRight ~ AgeYears *  Order + Error(SubjID), subset(Temp.TL, ItemType=="EventLine"))
summary(Temp.PF.ord.e) -- ageyears and interaction

with(subset(Temp.PF6, Item=='BreakfastToday' & AgeYears !='adult'), t.test(BNRight ~ Order, var.equal=T))  # first in order 1
with(subset(Temp.PF6, Item=='LastAge' & AgeYears !='adult'), t.test(BNRight ~ Order, var.equal=T))  # first in order 2
with(subset(Temp.PF6, Item=='LastWeek' & AgeYears !='adult'), t.test(BNRight ~ Order, var.equal=T))  # first in order 1
with(subset(Temp.PF6, Item=='ThisMorning' & AgeYears !='adult'), t.test(BNRight ~ Order, var.equal=T)) # first in order 2
with(subset(Temp.PF6, Item=='NextWeek' & AgeYears !='adult'), t.test(BNRight ~ Order, var.equal=T)) # first in order 1 -- better when tested last vs first
with(subset(Temp.PF6, Item=='LastYear' & AgeYears !='adult'), t.test(BNRight ~ Order, var.equal=T)) # first in order 2

with(subset(Temp.TL, Item=='BreakfastToday' & AgeYears !='adult'), t.test(RankRight ~ Order, var.equal=T))  # first in order 1
with(subset(Temp.TL, Item=='LastAge' & AgeYears !='adult'), t.test(RankRight ~ Order, var.equal=T))  # first in order 2; marginally better in order 2
with(subset(Temp.TL, Item=='LastWeek' & AgeYears !='adult'), t.test(RankRight ~ Order, var.equal=T))  # first in order 1
with(subset(Temp.TL, Item=='ThisMorning' & AgeYears !='adult'), t.test(RankRight ~ Order, var.equal=T)) # first in order 2
with(subset(Temp.TL, Item=='NextWeek' & AgeYears !='adult'), t.test(RankRight ~ Order, var.equal=T)) # first in order 1 -- better when tested last vs first
with(subset(Temp.TL, Item=='LastYear' & AgeYears !='adult'), t.test(RankRight ~ Order, var.equal=T)) # first in order 2




# plot PastFuture accuracy over time  ####
plot.data <- subset(Temp.PF2)
plot.data.agg <- aggregate(BNRight ~ AgeYears, plot.data, mean)
plot.data.agg$varRank <- aggregate(BNRight ~ AgeYears, plot.data, se)$BNRight

plot.pastfuture2 <- ggplot(data=plot.data.agg, aes(x=AgeYears, y=BNRight)) + 
  # geom_hline(yintercept=1, linetype="solid",color="black",size=1.3) +
  geom_hline(yintercept=.5, linetype="dashed",color="red",size=1.3) +
  geom_pointrange(size=1.5, position=position_dodge(width=0.3), aes(ymax=BNRight + varRank, ymin=BNRight - varRank)) +
  scale_x_discrete(name = "Age (years)") +
  scale_y_continuous(name = "Deictic Status", breaks= c(.5,.6,.7,.8,.9,1), limits = c(.45, 1)) +
  science.theme +
  theme(axis.title = element_text(size=20))
plot.pastfuture2

#### Analysis of relative location (metric structure) ####
# For each child and each line, calculate ratio between locations for both past and both future items
Temp.TL$DistFromMidNum <- as.numeric(Temp.TL$DistFromMid)
Temp.TL$DistFromMidNum <- ifelse(Temp.TL$DistFromMidNum == 0, .01, Temp.TL$DistFromMidNum)
Temp.TL.RelLocation <- subset(Temp.TL, SubjID=="")

Temp.TL.RelLocation$distCat <- ifelse(Temp.TL.RelLocation$Item %in% c("BreakfastToday","DinnerToday", "Tonight","ThisMorning"),1,
                                      ifelse(Temp.TL.RelLocation$Item %in% c("Yesterday","Tomorrow"),2,
                                             ifelse(Temp.TL.RelLocation$Item %in% c("LastWeek","NextWeek"),4,
                                                    ifelse(Temp.TL.RelLocation$Item %in% c("LastYear","NextYear","LastAge","NextAge"),7,NA
                                                    ))))



for (SubVar in levels(Temp.TL$SubjID)){
  foo.temp <- subset(Temp.TL, SubjID == SubVar)
  foo.temp$RelLocPast2 <- abs(foo.temp$DistFromMidNum[foo.temp$Item == "LastWeek"] / foo.temp$DistFromMidNum[foo.temp$Item == "ThisMorning"])
  foo.temp$RelLocPast3 <- abs(foo.temp$DistFromMidNum[foo.temp$Item == "LastYear"] / foo.temp$DistFromMidNum[foo.temp$Item == "Yesterday"])
  foo.temp$RelLocFuture2 <- abs(foo.temp$DistFromMidNum[foo.temp$Item == "Tomorrow"] / foo.temp$DistFromMidNum[foo.temp$Item == "Tonight"])
  foo.temp$RelLocFuture3 <- abs(foo.temp$DistFromMidNum[foo.temp$Item == "NextYear"] / foo.temp$DistFromMidNum[foo.temp$Item == "NextWeek"])
  Temp.TL.RelLocation <- rbind(Temp.TL.RelLocation, foo.temp)
}

Temp.TL.RelLocation$RelLocPastD <- (Temp.TL.RelLocation$RelLocPast2 + Temp.TL.RelLocation$RelLocPast3)/2
Temp.TL.RelLocation$RelLocFutureD <- (Temp.TL.RelLocation$RelLocFuture2 + Temp.TL.RelLocation$RelLocFuture3)/2


Temp.TL.RelLocation$RelLoc <- with(Temp.TL.RelLocation, ifelse(CorrectBN==1, RelLocPastD, RelLocFutureD))
Temp.TL.RelLocation.agg <- aggregate(RelLoc ~ AgeYears + ItemType + CorrectBN + SubjID, Temp.TL.RelLocation, mean)
Temp.TL.RelLocation.agg$RelLocPastD <- aggregate(RelLocPastD ~ AgeYears + ItemType + CorrectBN + SubjID, Temp.TL.RelLocation, mean)$RelLocPastD
Temp.TL.RelLocation.agg$RelLocFutureD <- aggregate(RelLocFutureD ~ AgeYears + ItemType + CorrectBN + SubjID, Temp.TL.RelLocation, mean)$RelLocFutureD
Temp.TL.RelLocation.aov <- aov(RelLoc ~ AgeYears * ItemType * CorrectBN + Error(SubjID/(ItemType * CorrectBN)), subset(Temp.TL.RelLocation.agg, RelLocFutureD < 30 & RelLocPastD < 30))
summary(Temp.TL.RelLocation.aov)
aggregate(RelLoc ~ ItemType + AgeYears, Temp.TL.RelLocation.agg, mean)
aggregate(RelLoc ~ CorrectBN + AgeYears, subset(Temp.TL.RelLocation.agg, RelLocFutureD < 30 & RelLocPastD < 30), mean)



# For each child and each line, find the ratio between this measure and the mean adult ratio
RelLoc <- subset(Temp.TL.RelLocation, Item=="BreakfastToday")
# RelLoc.aov <- aov(RelLocPastD ~ AgeYears, RelLoc)
# summary(RelLoc.aov)
RelLoc.adult <- subset(RelLoc, AgeYears == "adult", select=c(SubjID, RelLocPast2, RelLocPast3, RelLocFuture2, RelLocFuture3, RelLocFutureD, RelLocPastD, AgeYears, Age))
RelLoc.adult$SubjID <- factor(RelLoc.adult$SubjID)
RelLoc.adult.agg <- aggregate(RelLocPast2 ~ AgeYears, RelLoc.adult, mean)
RelLoc.adult.agg$RelLocPast3 <- aggregate(RelLocPast3 ~ AgeYears, RelLoc.adult, mean)$RelLocPast3
RelLoc.adult.agg$RelLocFuture2 <- aggregate(RelLocFuture2 ~ AgeYears, RelLoc.adult, mean)$RelLocFuture2
RelLoc.adult.agg$RelLocFuture3 <- aggregate(RelLocFuture3 ~ AgeYears, RelLoc.adult, mean)$RelLocFuture3
RelLoc.adult.agg$RelLocPastD <- aggregate(RelLocPastD ~ AgeYears, RelLoc.adult, mean)$RelLocPastD
RelLoc.adult.agg$RelLocFutureD <- aggregate(RelLocFutureD ~ AgeYears, RelLoc.adult, mean)$RelLocFutureD

RelLoc.kids <- subset(RelLoc, AgeYears != "adult", select=c(SubjID, RelLocPast2, RelLocPast3, RelLocFuture2, RelLocFuture3, RelLocFutureD, RelLocPastD, AgeYears, Age))
RelLoc.kids$RelLocPast2A <- RelLoc.adult.agg$RelLocPast2
RelLoc.kids$RelLocPast3A <- RelLoc.adult.agg$RelLocPast3
RelLoc.kids$RelLocFuture2A <- RelLoc.adult.agg$RelLocFuture2
RelLoc.kids$RelLocFuture3A <- RelLoc.adult.agg$RelLocFuture3
RelLoc.kids$RelLocPastDA <- RelLoc.adult.agg$RelLocPastD
RelLoc.kids$RelLocFutureDA <- RelLoc.adult.agg$RelLocFutureD

RelLoc.kids$RelLocPast2R <- RelLoc.kids$RelLocPast2 / RelLoc.kids$RelLocPast2A 
RelLoc.kids$RelLocPast3R <- RelLoc.kids$RelLocPast3 / RelLoc.kids$RelLocPast3A 
RelLoc.kids$RelLocFuture2R <- RelLoc.kids$RelLocFuture2 / RelLoc.kids$RelLocFuture2A 
RelLoc.kids$RelLocFuture3R <- RelLoc.kids$RelLocFuture3 / RelLoc.kids$RelLocFuture3A 
RelLoc.kids$RelLocPastDR <- RelLoc.kids$RelLocPastD / RelLoc.kids$RelLocPastDA 
RelLoc.kids$RelLocFutureDR <- RelLoc.kids$RelLocFutureD / RelLoc.kids$RelLocFutureDA 

RelLoc.kids$RelLocE <- (RelLoc.kids$RelLocPast2R + RelLoc.kids$RelLocPast3R + RelLoc.kids$RelLocFuture2R + RelLoc.kids$RelLocFuture3R)/4
RelLoc.kids$RelLocD <- (RelLoc.kids$RelLocPastD + RelLoc.kids$RelLocFutureD)/2

aggregate(RelLocE ~ AgeYears, RelLoc.kids, mean)
aggregate(RelLocD ~ AgeYears, RelLoc.kids, mean)
with(subset(RelLoc.kids, AgeYears %in% c("7","8")), t.test(RelLocD ~ AgeYears))
with(subset(RelLoc.kids, AgeYears %in% c("3","8")), t.test(RelLocD ~ AgeYears))

library(reshape2)
RelLoc.kids.long <- melt(RelLoc.kids, measure.vars=c("RelLocD", "RelLocE"), variable.name="ItemType", value.name="relLoc")

foom <- aggregate(relLoc ~ ItemType, RelLoc.kids.long, mean)
foosd <- aggregate(relLoc ~ ItemType, RelLoc.kids.long, sd)
RelLoc.kids.long$foom <- ifelse(RelLoc.kids.long$ItemType == "RelLocD", subset(foom, ItemType=="RelLocD")$relLoc, subset(foom, ItemType=="RelLocE")$relLoc)
RelLoc.kids.long$foosd <- ifelse(RelLoc.kids.long$ItemType == "RelLocD", subset(foosd, ItemType=="RelLocD")$relLoc, subset(foosd, ItemType=="RelLocE")$relLoc)
xtabs(~(relLoc<foom + 2*foosd) + ItemType, RelLoc.kids.long)
RelLoc.kids.long.sub <- subset(RelLoc.kids.long, relLoc<foom + 2*foosd)

aggregate(relLoc ~ AgeYears + ItemType, RelLoc.kids.long.sub, mean)
with(subset(RelLoc.kids.long.sub, AgeYears=="8"), t.test(relLoc, mu=1))

# separating by line
RelLoc.kids.lines <- melt(RelLoc.kids, measure.vars=c("RelLocPast2R", "RelLocPast3R", "RelLocFuture2R","RelLocFuture3R","RelLocFutureDR","RelLocPastDR"), variable.name="ItemType", value.name="relLoc")
RelLoc.kids.lines$ItemType <- ifelse(RelLoc.kids.lines$ItemType %in% c("RelLocFutureDR","RelLocPastDR"), "deictic","event")
RelLoc.kids.lines$cItemType <- ifelse(RelLoc.kids.lines$ItemType == "deictic", -1, 1)
RelLoc.kids.lines$cAgeYearsNum <- scale(RelLoc.kids.lines$AgeYearsNum)
RelLoc.kids.lines$PF <- ifelse(RelLoc.kids.lines$ItemType %in% c("RelLocFuture2R","RelLocFuture3R","RelLocFutureDR"), "future","past")
RelLoc.kids.lines$cPF <- ifelse(RelLoc.kids.lines$PF == "past", -1, 1)

foom <- aggregate(relLoc ~ ItemType, RelLoc.kids.lines, mean)
colnames(foom) <- c("ItemType","foom")
foosd <- aggregate(relLoc ~ ItemType, RelLoc.kids.lines, sd)
colnames(foosd) <- c("ItemType","foosd")
RelLoc.kids.lines <- merge(RelLoc.kids.lines, foom, by="ItemType")
RelLoc.kids.lines <- merge(RelLoc.kids.lines, foosd, by="ItemType")
head(RelLoc.kids.lines)
xtabs(~(value<foom + 2*foosd) + variable, RelLoc.kids.lines)
RelLoc.kids.lines.sub <- subset(RelLoc.kids.lines, relLoc<foom + 2*foosd)
dim(RelLoc.kids.lines.sub)
dim(RelLoc.kids.lines)

# Analyze this by child, past/future, and maybe include line as random effect
library(lmerTest)
RelLoc.kids.lines$cItemType <- scale(as.numeric(as.factor(RelLoc.kids.lines$ItemType)))

RelLoc.kids.lines.m1 <- lmer(value ~ cAgeYearsNum * cItemType + (cAgeYearsNum * cItemType|SubjID) + (cAgeYearsNum * cItemType|variable), RelLoc.kids.lines)
RelLoc.kids.lines.m1 <- lmer(value ~ cAgeYearsNum * cItemType + (1 + cItemType|SubjID) + (1 + cAgeYearsNum|variable), RelLoc.kids.lines)
summary(RelLoc.kids.lines.m1)

RelLoc.kids.lines.m1 <- lmer(value ~ cAgeYearsNum * cItemType + (cAgeYearsNum * cItemType|SubjID) + (cAgeYearsNum * cItemType|variable), RelLoc.kids.lines.sub)
RelLoc.kids.lines.m1 <- lmer(value ~ cAgeYearsNum * cItemType + (cItemType + cAgeYearsNum:cItemType|SubjID) + (cAgeYearsNum + cAgeYearsNum:cItemType|variable), RelLoc.kids.lines.sub)
RelLoc.kids.lines.m1 <- lmer(value ~ cAgeYearsNum * cItemType + (cItemType|SubjID) + (cAgeYearsNum|variable), RelLoc.kids.lines.sub)



RelLoc.kids.lines.m2 <- lmer(value ~ cAgeYearsNum * cItemType - cAgeYearsNum + (cAgeYearsNum * cItemType|SubjID) + (cAgeYearsNum * cItemType|variable), RelLoc.kids.lines)
RelLoc.kids.lines.m2 <- lmer(value ~ cAgeYearsNum * cItemType - cAgeYearsNum + (1 + cItemType|SubjID) + (1 + cAgeYearsNum|variable), RelLoc.kids.lines)
anova(RelLoc.kids.lines.m1, RelLoc.kids.lines.m2)

#### New remoteness measure ####
# For each line and kid, regress the abs position relative to midpoint of each item onto mean adult placement
# Then take R^2 as a scale-invariant error measure, for each kid and each line
# See if R^2 decreases over time. 
# Do the same for adults. Slope should be one but we have a measure of mean variability.

# Mahesh: for each kid, divide all distances by max distance used. And THEN do the regression onto adults. 
# Use cook's distance as a measure of how far each item is from the adult-like distance?

#Possible 1-back measure: First scale, then take 1-back difference -- and compare to mean adult 1-back difference 
Temp.TL$SubjID <- factor(Temp.TL$SubjID)
Temp.TL$ItemType <- factor(Temp.TL$ItemType)
Temp.TL$aDist <- abs(Temp.TL$DistFromMidNum)
Temp.TL$oItem <- ordered(Temp.TL$Item, levels = c("BreakfastToday","DinnerToday","ThisMorning","Tonight","Yesterday","Tomorrow","LastWeek","NextWeek","LastAge","NextAge","LastYear","NextYear"))

Temp.TL.max <- aggregate(aDist ~ SubjID, Temp.TL, max)
colnames(Temp.TL.max) <- c("SubjID","maxDist")
Temp.TL <- merge(Temp.TL, Temp.TL.max)
Temp.TL$sDist <- Temp.TL$aDist/Temp.TL$maxDist
Temp.TL$signedScaledDist <- Temp.TL$DistFromMidNum/Temp.TL$maxDist


Temp.TL.adults <- subset(Temp.TL, AgeYears == "adult")
Temp.TL.adults.agg <- aggregate(sDist ~ oItem, Temp.TL.adults, mean)
colnames(Temp.TL.adults.agg) <- c("oItem","meanAdultDist")
Temp.TL <- merge(Temp.TL, Temp.TL.adults.agg)
Temp.TL$sCorrectRank <- (Temp.TL$CorrectRank - 2) /2 #center on zero and rescale

ggplot(Temp.TL, aes(x = meanAdultDist, y = sDist, group = AgeYears, color = AgeYears, fill = AgeYears)) +
  geom_point() + 
  geom_smooth(method="lm") +
  science.theme

Temp.TL.m1 <- lm(sDist ~ CorrectRank + meanAdultDist, subset(Temp.TL, AgeYears == "5"))
Temp.TL.m1 <- lm(sDist ~ CorrectRank + meanAdultDist, subset(Temp.TL, SubjID == "012214-NF"))

library(relaimpo)  
Temp.TL$SubjID <- factor(Temp.TL$SubjID)
relLocCoef <- data.frame(SubjID = levels(Temp.TL$SubjID), InterceptCoef = NA, CorrectRankCoef = NA, RankP = NA, meanAdultDistCoef = NA, meanAdultDistP = NA, R2 = NA, RelP = NA, ItemType = NA)

# relLocCoef2 <- relLocCoef
# relLocCoef$ItemType <- "DeicticLine"
# relLocCoef2$ItemType <- "EventLine"
# relLocCoef2 <- rbind(relLocCoef, relLocCoef2)

for (subnum in levels(Temp.TL$SubjID)){
  newdata <- subset(Temp.TL, SubjID == subnum)
  newdata.m1 <- lm(sDist ~ sCorrectRank + meanAdultDist, newdata)
  newdata.m1.s <- summary(newdata.m1)
  
  relLocCoef[relLocCoef$SubjID == subnum,]$InterceptCoef <- coef(newdata.m1.s)["(Intercept)",1]
  relLocCoef[relLocCoef$SubjID == subnum,]$CorrectRankCoef <- coef(newdata.m1.s)["sCorrectRank",1]
  relLocCoef[relLocCoef$SubjID == subnum,]$RankP <- coef(newdata.m1.s)["sCorrectRank",4] < .05
  relLocCoef[relLocCoef$SubjID == subnum,]$meanAdultDistCoef <- coef(newdata.m1.s)["meanAdultDist",1]
  relLocCoef[relLocCoef$SubjID == subnum,]$meanAdultDistP <- coef(newdata.m1.s)["meanAdultDist",4] < .05
  relLocCoef[relLocCoef$SubjID == subnum,]$R2 <- newdata.m1.s$r.squared
  relLocCoef[relLocCoef$SubjID == subnum,]$RelP <- calc.relimp( lm(sDist ~ sCorrectRank + meanAdultDist, newdata), type = c("last") )$last["meanAdultDist"] 
}

inflm.SR <- influence.measures(newdata.m1)
which(apply(inflm.SR$is.inf, 1, any))
plot(rstudent(newdata.m1) ~ hatvalues(newdata.m1))
library(car)
influencePlot(newdata.m1)
avPlots(newdata.m1)

Temp.TL.relloc <- merge(Temp.TL, relLocCoef, by = "SubjID")
Temp.TL.relloc <- subset(Temp.TL.relloc, TrialNum == 1)

xtabs(~meanAdultDistP + AgeYears, Temp.TL.relloc)

aggregate(R2 ~ AgeYears, Temp.TL.relloc, mean)
aggregate(meanAdultDistCoef ~ AgeYears, subset(Temp.TL.relloc), mean)
aggregate(CorrectRankCoef ~ AgeYears, subset(Temp.TL.relloc), mean)
xtabs(~RankP + meanAdultDistP + AgeYears, Temp.TL.relloc)

t.test(RelP ~ AgeYears, subset(Temp.TL.relloc, AgeYears %in% c("3","4")), var.equal=T)
t.test(RelP ~ AgeYears, subset(Temp.TL.relloc, AgeYears %in% c("4","5")), var.equal=T)
t.test(RelP ~ AgeYears, subset(Temp.TL.relloc, AgeYears %in% c("7","adult")), var.equal=T)
t.test(RelP ~ AgeYears, subset(Temp.TL.relloc, AgeYears %in% c("8","adult")), var.equal=T)

#### test for site effects

relloc.site <- lmer(RelP ~ AgeYears*TestSite + (1|SubjID) , subset(Temp.TL.relloc, AgeYears %in% c('6','7','8') & TestSite %in% c('Cal','UCSD')))


###




allmeasures <- merge(Temp.PF3, relLocCoef)
allmeasures <- merge(allmeasures, aggregate(RankDif ~ AgeYears + SubjID, Temp.TL4, mean))
allmeasures <- merge(allmeasures, RelRankData.agg2)

summary(lm(RelP ~ Age, subset(allmeasures, AgeYears != "adult")))
summary(lm(RelP ~ Age, subset(allmeasures)))
Temp.TL.relloc.agg <- aggregate(RelP ~ AgeYears, allmeasures, mean)
colnames(Temp.TL.relloc.agg) <- c("AgeYears","distCoef")
Temp.TL.relloc.agg$distSd <- aggregate(RelP ~ AgeYears, allmeasures, se)[,2]

reldist1 <- ggplot(data=Temp.TL.relloc.agg, aes(x=AgeYears, y=distCoef)) + 
  geom_pointrange(size=1.5, position=position_dodge(width=0.3), aes(ymax=distCoef + distSd, ymin=distCoef - distSd)) +
  scale_x_discrete(name = "Age (years)") +
  scale_y_continuous(name = "Temporal Remoteness", breaks=c(.1,.2,.3,.4,.5,.6,.7)) +
  science.theme +
  theme(axis.title = element_text(size=20))
  # annotate("text",x = 1, y=1.2, label="(C)")
reldist1
grid.arrange(plot.pastfuture2, plot.1back.order, reldist1, nrow=1)



  

##### Cluster temp knowledge measures ####

foo <- kmeans(allmeasures$BNRight, centers=2)
if(foo$centers[1] > foo$centers[2]){
  allmeasures$deicticCluster <- abs(foo$cluster - 2)
} else {
  allmeasures$deicticCluster <- foo$cluster -1
}

foo <- kmeans(allmeasures$AccRelRank, centers=2)
if(foo$centers[1] > foo$centers[2]){
  allmeasures$orderCluster <- abs(foo$cluster - 2)
} else {
  allmeasures$orderCluster <- foo$cluster -1
}

foo <- kmeans(allmeasures$RelP, centers=2)
foo$centers
if(foo$centers[1] > foo$centers[2]){
  allmeasures$remoteCluster <- abs(foo$cluster - 2)
} else {
  allmeasures$remoteCluster <- foo$cluster -1
}

allmeasures.kids <- subset(allmeasures, AgeYears != "adult")
OgD <- xtabs(~orderCluster, subset(allmeasures.kids, deicticCluster == 1))
RgD <- xtabs(~remoteCluster, subset(allmeasures.kids, deicticCluster == 1))

DgO <- xtabs(~deicticCluster, subset(allmeasures.kids, orderCluster == 1))
RgO <- xtabs(~remoteCluster, subset(allmeasures.kids, orderCluster == 1))

DgR <- xtabs(~deicticCluster, subset(allmeasures.kids, remoteCluster == 1))
OgR <- xtabs(~orderCluster, subset(allmeasures.kids, remoteCluster == 1))

(OgDp <- OgD[2]/(OgD[1] + OgD[2]))
(DgOp <- DgO[2]/(DgO[1] + DgO[2]))
RgDp <- RgD[2]/(RgD[1] + RgD[2])
RgOp <- RgO[2]/(RgO[1] + RgO[2])
DgRp <- DgR[2]/(DgR[1] + DgR[2])
OgRp <- OgR[2]/(OgR[1] + OgR[2])

OgDp - 1.96 *(sqrt(OgDp*(1-OgDp)/sum(OgD)))
OgDp + 1.96 *(sqrt(OgDp*(1-OgDp)/sum(OgD)))

DgOp - 1.96 *(sqrt(DgOp*(1-DgOp)/sum(DgO)))
DgOp + 1.96 *(sqrt(DgOp*(1-DgOp)/sum(DgO)))

DgRp - 1.96 *(sqrt(DgRp*(1-DgRp)/sum(DgR)))
DgRp + 1.96 *(sqrt(DgRp*(1-DgRp)/sum(DgR)))

OgRp - 1.96 *(sqrt(OgRp*(1-OgRp)/sum(OgR)))
OgRp + 1.96 *(sqrt(OgRp*(1-OgRp)/sum(OgR)))

RgDp - 1.96 *(sqrt(RgDp*(1-RgDp)/sum(RgD)))
RgDp + 1.96 *(sqrt(RgDp*(1-RgDp)/sum(RgD)))

RgOp - 1.96 *(sqrt(RgOp*(1-RgOp)/sum(RgO)))
RgOp + 1.96 *(sqrt(RgOp*(1-RgOp)/sum(RgO)))


#### Plot relations between temp knowledge measure ####
transitionPs <- matrix(nrow=3, ncol=3, dimnames=list(c("deictic","order","remote"),c("deictic","order","remote")))
transitionPs[1,1] <- 0
transitionPs[2,2] <- 0
transitionPs[3,3] <- 0
transitionPs[1,2] <- OgDp
transitionPs[1,3] <- RgDp
transitionPs[2,1] <- DgOp
transitionPs[2,3] <- RgOp
transitionPs[3,1] <- DgRp
transitionPs[3,2] <- OgRp
(transitionPs <- round(transitionPs, 2))

# plot the transition matrix
library(diagram)

pdf("final-plots/Fig4-transitionProbs.pdf", width=8, height=8)
plotmat(t(transitionPs), 
        box.col="grey90", box.size=.15, cex=2, my=-.1, box.cex=1.9,
        # box.size=0, 
        box.type = "circle", 
        shadow.size=0, arr.lwd=(t(transitionPs)-.4)*25, 
        arr.lcol="black", arr.col = "black", 
        # arr.pos=.65,
        # endhead=T,
        arr.width=.4, #arr.type="simple",
        curve=.07,
        segment.from=.2,
        segment.to=.8,
        dtext = .2,
        name= c("Deictic Status\nKnower","Order\nKnower","Remoteness\nKnower"))
dev.off()
#### psychometric-style curves of developmental trajetory ####

plot.data1 <- aggregate(deicticCluster ~ AgeYears, allmeasures.kids, mean)
colnames(plot.data1) <- c("AgeYears","cluster")

plot.data2 <- aggregate(orderCluster ~ AgeYears, allmeasures.kids, mean)
colnames(plot.data2) <- c("AgeYears","cluster")

plot.data3 <- aggregate(remoteCluster ~ AgeYears, allmeasures.kids, mean)
colnames(plot.data3) <- c("AgeYears","cluster")

allmeasures.kids$n <- 1
foo1 <- quickpsy(allmeasures.kids, Age, deicticCluster, n, guess= 0, fun = weibull_fun, B=100)
foo2 <- quickpsy(allmeasures.kids, Age, orderCluster, n, guess= 0, fun = weibull_fun, B=100)
foo3 <- quickpsy(allmeasures.kids, Age, remoteCluster, n, guess= 0, fun = weibull_fun, B=100)
foo1$thresholds
(foo1$thresholds[1] - floor(foo1$thresholds[1])) *12
foo1$thresholdsci
(foo1$thresholdsci - floor(foo1$thresholdsci)) *12

foo2$thresholds
(foo2$thresholds[1] - floor(foo2$thresholds[1])) *12
foo2$thresholdsci
(foo2$thresholdsci - floor(foo2$thresholdsci)) *12

foo3$thresholds
(foo3$thresholds[1] - floor(foo3$thresholds[1])) *12
foo3$thresholdsci
(foo3$thresholdsci - floor(foo3$thresholdsci)) *12
# plot(foo3$curves)

ebarHeight <- .1
avgPointSize <- 4
 
psy.ds <- plotcurves(foo1, thresholds=F, ci = F) + science.theme + 
  geom_point(data=plot.data1, 
             aes(x = as.numeric(as.character(AgeYears)) + .5, y=cluster), 
             size=avgPointSize, color = "blue") + 
  scale_color_discrete(guide=F) + #ggtitle("Deictic Status") + 
  coord_cartesian(xlim=c(3,9)) +
#   geom_segment(aes(y=.5, yend=.5, 
#                    x=3, xend=as.numeric(foo1$thresholds[1])), 
#                color="red", linetype="dashed", size=1) +
  geom_segment(aes(y=0, yend=.5, 
                   x=as.numeric(foo1$thresholds[1]), xend=as.numeric(foo1$thresholds[1])), 
               color="red", linetype="dashed", size=1) +
  geom_point(data=foo1$thresholds, aes(x=thre, y = .5), size=4) + 
  geom_errorbarh(data=foo1$thresholdsci, aes(x = as.numeric(foo1$thresholds[1]), xmin=threinf, xmax=thresup, y = .5), size=2, height=ebarHeight) +
  theme(axis.title = element_text(size = rel(2))) +
  scale_y_continuous(name="P(Deictic)", breaks=c(0,.5,1)) + 
  scale_x_continuous("Age (years)", breaks=c(3,4,5,6,7,8,9)) 

# psy.ds

psy.order <- plotcurves(foo2, thresholds=F, ci = F) + science.theme + 
  geom_point(data=plot.data2, 
             aes(x = as.numeric(as.character(AgeYears)) + .5, y=cluster), 
             size=avgPointSize, color = "blue") + 
  scale_color_discrete(guide=F) + #ggtitle("Order") + 
  coord_cartesian(xlim=c(3,9)) +
#   geom_segment(aes(y=.5, yend=.5, 
#                    x=3, xend=as.numeric(foo2$thresholds[1])), 
#                color= "red", linetype="dashed", size=1) +
  geom_segment(aes(y=0, yend=.5, 
                   x=as.numeric(foo2$thresholds[1]), xend=as.numeric(foo2$thresholds[1])), 
               color="red", linetype="dashed", size=1) + 
  geom_point(data=foo2$thresholds, aes(x=thre, y = .5), size=4) + 
  geom_errorbarh(data=foo2$thresholdsci, aes(x = as.numeric(foo2$thresholds[1]), xmin=threinf, xmax=thresup, y = .5), size=2, height=ebarHeight) +
  theme(axis.title = element_text(size = rel(2))) +
  scale_y_continuous(name="P(Order)", breaks=c(0,.5,1)) +
  scale_x_continuous("Age (years)", breaks=c(3,4,5,6,7,8,9)) 

# psy.order

psy.remote <- plotcurves(foo3, thresholds=F, ci = F) + science.theme + 
  geom_point(data=plot.data3, 
             aes(x = as.numeric(as.character(AgeYears)) + .5, y=cluster), 
             size=avgPointSize, color = "blue") + 
  scale_color_discrete(guide=F) + #ggtitle("Remoteness") +
  coord_cartesian(xlim=c(3,9)) +
#   geom_segment(aes(y=.5, yend=.5, 
#                    x=3, xend=as.numeric(foo3$thresholds[1])), 
#                color="red", linetype="dashed", size=1) +
  geom_segment(aes(y=0, yend=.5, 
                   x=as.numeric(foo3$thresholds[1]), xend=as.numeric(foo3$thresholds[1])), 
               color="red", linetype="dashed", size=1) +
  geom_errorbarh(data=foo3$thresholdsci, aes(x = as.numeric(foo3$thresholds[1]), xmin=threinf, xmax=thresup, y = .5), size=2, height=ebarHeight) +
  geom_point(data=foo3$thresholds, aes(x=thre, y = .5), size=4) + 
  theme(axis.title = element_text(size = rel(2))) +
  scale_y_continuous(name="P(Remoteness)", breaks=c(0,.5,1)) +
  scale_x_continuous("Age (years)", breaks=c(3,4,5,6,7,8, 9)) 

pdf("final-plots/Fig4-knowerOnset.pdf",width = 6, height=9.5)
grid.arrange(psy.ds, psy.order, psy.remote)
dev.off()
#

# Plot relative location - bar graph ####
plot.data <- subset(RelLoc.kids.lines)
head(RelLoc.kids.lines)
plot.data.agg <- aggregate(value ~ AgeYears + ItemType, plot.data, mean)
plot.data.agg$varLoc <- aggregate(value ~ AgeYears + ItemType, plot.data, se)$value
plot.relloc <- ggplot(data=plot.data.agg, aes(x=AgeYears, y=value, group=ItemType, color=ItemType, shape=ItemType, fill=ItemType)) + 
  geom_pointrange(size=1.5, position=position_dodge(width=0.3), aes(ymax=value + varLoc, ymin=value - varLoc)) +
  scale_x_discrete(name = "age (years)") +
  scale_y_continuous(name = "Relative Location, compared to adults") +
  scale_fill_discrete(name="Time Condition",
                      labels=c("deictic terms", "life events")) + 
  scale_shape(name="Time Condition",
              labels=c("deictic terms", "life events")) +  
  scale_color_discrete(name="Time Condition",
                       labels=c("deictic terms", "life events")) + 
  science.theme +
  theme(legend.position = c(1,.7)) + 
  geom_hline(y=1, linetype="dashed",color="red",size=1.3)
plot.relloc


Temp.TL.RelLocation$distCat <- ifelse(Temp.TL.RelLocation$Item %in% c("BreakfastToday","DinnerToday", "Tonight","ThisMorning"),1,
                                      ifelse(Temp.TL.RelLocation$Item %in% c("Yesterday","Tomorrow"),2,
                                             ifelse(Temp.TL.RelLocation$Item %in% c("LastWeek","NextWeek"),4,
                                                    ifelse(Temp.TL.RelLocation$Item %in% c("LastYear","NextYear","LastAge","NextAge"),7,NA
                                                           ))))

hist(Temp.TL.RelLocation$DistFromMidNum)
summary(lm(abs(DistFromMidNum) ~ distCat, subset(Temp.TL.RelLocation, AgeYears=="3")))
summary(lm(abs(DistFromMidNum) ~ distCat, subset(Temp.TL.RelLocation, AgeYears=="4")))
summary(lm(abs(DistFromMidNum) ~ distCat, subset(Temp.TL.RelLocation, AgeYears=="5")))
summary(lm(abs(DistFromMidNum) ~ distCat, subset(Temp.TL.RelLocation, AgeYears=="8")))

match("BreakfastToday",levels(Temp.TL.RelLocation$Item))
str(Temp.TL.RelLocation)

##### Forced Choice #############################################################################

Temp.FC <- subset(Temp, FC==T)
head(Temp.FC, 10)
Temp.FC$FCRightN <- as.numeric(as.character(Temp.FC$FCRight))
subset(Temp.FC, FCRight!=1 & FCRight!=0) #some subjects have missing accuracy data
Temp.FC <- subset(Temp.FC, !(SubjID %in% c("061913-S1","CSC013014-OT","DM020814-MM","DM030814-KJ","DM041614-JP"))) #remove those kids for now
Temp.FC$Past <- Temp.FC$Item %in% c("YestVsLW","LWVsLY")

# Look at acuracy by age and FC task
Temp.FC.agg <- aggregate(FCRightN ~ SubjID + AgeYears + ItemType, Temp.FC, mean)
Temp.FC.aov <- aov(FCRightN ~ AgeYears * ItemType + Error(SubjID/ItemType), Temp.FC.agg)
summary(Temp.FC.aov) #main effects of age and task, no interaction
Temp.FC.agg2 <- aggregate(FCRightN ~ ItemType + AgeYears, Temp.FC, mean)
Temp.FC.agg2

# temporal order FC -- do they get both past items wrong?
Temp.FC2 <- subset(Temp.FC, ItemType=="TermCompFC2")
Temp.FC2$Past <- Temp.FC2$Item %in% c("YestVsLW","LWVsLY")
Temp.FC2.agg <- aggregate(FCRightN ~ AgeYears + Past + SubjID, Temp.FC2, mean)
Temp.FC2.aov <- aov(FCRightN ~ AgeYears*Past + Error(SubjID/Past), Temp.FC2.agg)
summary(Temp.FC2.aov) #main effects of age and past/future, and 2-way interaction

Temp.FC2.agg2 <- aggregate(FCRightN ~ Past + AgeYears, Temp.FC2, mean)
Temp.FC2.agg2 # basically at chance for Past items

#When kids are wrong, are they responding at random or they systematically incorrect?
xtabs(~FCRightN + AgeYears + Past, Temp.FC2.agg) 
xtabs(~FCRightN + AgeYears, subset(Temp.FC2.agg, Past==F)) 
fisher.test(xtabs(~FCRightN + Past, Temp.FC2.agg)) #Super significant! :)
#Past: Maybe they start off pretty random at 5 and gradually become more bimodal, either fully correct or fully incorrect?
#Future: mostly correct across the board. 

#### Correlate FC and Timeline ############################################################################
Temp.FC.bysub <- aggregate(FCRightN ~ SubjID + AgeYears, subset(Temp.FC, Past==F), mean)

Temp.merged.cor <- merge(allmeasures.kids, Temp.FC.bysub)

summary(lm(FCRightN ~ Age, Temp.merged.cor))
aggregate(FCRightN ~ AgeYears, Temp.FC.bysub, mean)
with(subset(Temp.FC.bysub, AgeYears==3), t.test(FCRightN, mu=.5))
with(subset(Temp.FC.bysub, AgeYears==4), t.test(FCRightN, mu=.5))
with(subset(Temp.FC.bysub, AgeYears %in% c(6,"adult")), t.test(FCRightN ~ AgeYears, var.equal=T))
with(subset(Temp.FC.bysub, AgeYears %in% c(7,"adult")), t.test(FCRightN ~ AgeYears, var.equal=T))



Temp.merged.cor$cRelP <- scale(Temp.merged.cor$RelP)
Temp.merged.cor$cBNRight <- scale(Temp.merged.cor$BNRight)
Temp.merged.cor$cAccRelRank <- scale(Temp.merged.cor$AccRelRank)

summary(lm(FCRightN ~ cBNRight, Temp.merged.cor)) #But on its own, it predicts FC accuracy
summary(lm(FCRightN ~ cAccRelRank, Temp.merged.cor)) #But on its own, it predicts FC accuracy
summary(lm(FCRightN ~ cRelP, Temp.merged.cor)) #But on its own, it predicts FC accuracy

summary(lm(FCRightN ~ Age * cBNRight, Temp.merged.cor)) #But on its own, it predicts FC accuracy
summary(lm(FCRightN ~ Age * cAccRelRank, Temp.merged.cor)) #But on its own, it predicts FC accuracy
summary(lm(FCRightN ~ Age * cRelP, Temp.merged.cor)) #

summary(lm(FCRightN ~ cBNRight + cAccRelRank + cRelP, Temp.merged.cor)) #

FC.plot.DS <- ggplot(Temp.merged.cor, aes(x=BNRight, y=FCRightN)) + 
  geom_smooth(method="lm", color="black") +
  geom_point(aes(color=AgeYears),size=2, position = position_jitter(height = .05)) +
  theme_bw()

FC.plot.order <- ggplot(Temp.merged.cor, aes(x=AccRelRank, y=FCRightN)) + 
  geom_smooth(method="lm", color="black") +
  geom_point(aes(color=AgeYears),size=2, position = position_jitter(height = .05)) +
  theme_bw()

FC.plot.remote <- ggplot(Temp.merged.cor, aes(x=RelP, y=FCRightN)) + 
  geom_smooth(method="lm", color="black") +
  geom_point(aes(color=AgeYears),size=2, position = position_jitter(height = .05)) +
  theme_bw()
grid.arrange(FC.plot.DS, FC.plot.order, FC.plot.remote)
