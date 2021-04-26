
# load packages
library(tidyverse) # for reshaping data
library(nloptr) # for calling lme4
library(lme4)
library(ggplot2) # plotting data
library(reshape2)
library(ggpubr)
library(dplyr) # for data manipulation
library(rstatix)
library(car)
library(coin)
library(scales)  # for scale_y_continuous(label = percent)
library(psych) # for calculating ICC reliability check on timeline task
library(ggthemes) # for scale_fill_few('medium')
library(broom) # tukeyHSD posthoc test

# read in the data
d.e <- read.csv('data_austin.csv')
d.e <- d.e %>%
  select(-ID,-dob, -dot, -agedays, -monolingual, -site, -covered, -comments, -experimenter) %>% # remove unnecessary columns
  mutate(language = 'english')
d.e[d.e == "999"] <- NA # Note that 999 indicates missing data
d.e[d.e == ""] <- NA # replace missing data and empty cells with NA
d.e <- mutate(d.e, response = ifelse(itemtype == 'deictic' & is.na(response2) == T, response1,
                                     ifelse(itemtype == 'verbal', response, response2))) # aggregate response1 and response2 columns for deictic items only

# do some more data management/wrangling
d.e <- d.e %>%
  mutate(item = as.factor(item),
       item = recode_factor(item, 'morning ' = 'thismorning',
                            'twodaysago' = 'beforeyesterday', 
                            'dayokweek' = 'daysofweek',
                            'last year' = 'lastyear',
                            'twofromnow' = 'aftertomorrow',
                            'twoago' = 'beforeyesterday',
                            'inaday' = 'tomorrow',
                            'dayago' = 'yesterday',
                            'onedayfromnow' = 'tomorrow',
                            'thisevening' = 'tonight'),
       itemnum = as.factor(itemnum),
       weekday = as.factor(weekday),
       weekday = recode_factor(weekday, '1' = '7',
                               '2' = '1',
                               '3' = '2',
                               '4' = '3',
                               '5' = '4',
                               '6' = '5',
                               '7' = '6'),
       # weekday function in excel codes Sun-Sat as 1-7 so here I am re-coding so the variable levels match our calendar task (e.g., 1-7 from Mon-Sun)
       exclude = replace_na(exclude, 0),
       distfrommid = distfrommid_c1)

future.words = c('aftertoday','aftertomorrow','dinner','nextbday','nextweek','nextyear','tomorrow','tonight') # Code all future time words

# Code correct responses (to compare to 'correctr' variable)
d.e <- d.e %>%
  mutate(correctR = case_when(task == "calendar" & itemtype == "deictic" & item == "yesterday" ~ '3',
                              task == "calendar" & itemtype == "deictic" & item == "beforeyesterday" ~ '2',
                              task == "calendar" & itemtype == "deictic" & item == "tomorrow" ~ '5',
                              task == "calendar" & itemtype == "deictic" & item == "aftertomorrow" ~ '6',
                              task == "calendar" & itemtype == "verbal" & item == "aftertoday" ~ '1',
                              task == "calendar" & itemtype == "verbal" & item == "beforetoday" ~ '2',
                              task == "calendar" & itemtype == "verbal" & item == "daysofweek" ~ '7',
                              # assuming a 1 indicates that the child correctly identified all 7 days of the week
                              task == "calendar" & itemtype == "verbal" & item == "today" & weekday == "1" ~ "1",
                              task == "calendar" & itemtype == "verbal" & item == "today" & weekday == "2" ~ "2",
                              task == "calendar" & itemtype == "verbal" & item == "today" & weekday == "3" ~ "3",
                              task == "calendar" & itemtype == "verbal" & item == "today" & weekday == "4" ~ "4",
                              task == "calendar" & itemtype == "verbal" & item == "today" & weekday == "5" ~ "5",
                              task == "calendar" & itemtype == "verbal" & item == "today" & weekday == "6" ~ "6",
                              task == "calendar" & itemtype == "verbal" & item == "today" & weekday == "7" ~ "7",
                              # note that weekday values range from 1-7  (e.g.,Sunday = 1, Monday = 2, etc.)
                              task == "calendar" & itemtype =="verbal" & item =="yesterday" & weekday == "1" ~ '7',
                              task == "calendar" & itemtype =="verbal" & item =="yesterday" & weekday == "2" ~ '1',
                              task == "calendar" & itemtype =="verbal" & item =="yesterday" & weekday == "3" ~ '2',
                              task == "calendar" & itemtype =="verbal" & item =="yesterday" & weekday == "4" ~ '3',
                              task == "calendar" & itemtype =="verbal" & item =="yesterday" & weekday == "5" ~ '4',
                              task == "calendar" & itemtype =="verbal" & item =="yesterday" & weekday == "6" ~ '5',
                              task == "calendar" & itemtype =="verbal" & item =="yesterday" & weekday == "7" ~ '6',
                              task == "calendar" & itemtype =="verbal" & item =="tomorrow" & weekday == "1" ~ '2',
                              task == "calendar" & itemtype =="verbal" & item =="tomorrow" & weekday == "2" ~ '3',
                              task == "calendar" & itemtype =="verbal" & item =="tomorrow" & weekday == "3" ~ '4',
                              task == "calendar" & itemtype =="verbal" & item =="tomorrow" & weekday == "4" ~ '5',
                              task == "calendar" & itemtype =="verbal" & item =="tomorrow" & weekday == "5" ~ '6',
                              task == "calendar" & itemtype =="verbal" & item =="tomorrow" & weekday == "6" ~ '7',
                              task == "calendar" & itemtype =="verbal" & item =="tomorrow" & weekday == "7" ~ '1',
                              task == "timeline" & linenum == "1" & item == "lastbday" ~ '1',
                              task == "timeline" & linenum == "1" & item == "breakfast" ~ '2',
                              task == "timeline" & linenum == "1" & item == "dinner" ~ '3', 
                              task == "timeline" & linenum == "1" & item == "nextbday" ~ '4',
                              task == "timeline" & linenum == "2" & item == "lastweek" ~ '1',
                              task == "timeline" & linenum == "2" & item == "thismorning" ~ '2',
                              task == "timeline" & linenum == "2" & item == "tonight" ~ '3',
                              task == "timeline" & linenum == "2" & item == "tomorrow" ~ '4',
                              task == "timeline" & linenum == "3" & item == "lastyear" ~ '1',
                              task == "timeline" & linenum == "3" & item == "yesterday" ~ '2',
                              task == "timeline" & linenum == "3" & item == "nextweek" ~ '3',
                              task == "timeline" & linenum == "3" & item == "nextyear" ~ '4',
                              task == "timeline" & linenum == "4" & item == "beforeyesterday" ~ '1',
                              task == "timeline" & linenum == "4" & item == "yesterday" ~ '2',
                              task == "timeline" & linenum == "4" & item == "tomorrow" ~ '3',
                              task == "timeline" & linenum == "4" & item == "aftertomorrow" ~ '4'))

d.e <- d.e %>%
  filter(exclude==0) %>%
  mutate(response = as.numeric(response),
         correctR = as.numeric(correctR),
         order = as.factor(order),
         agegroup = as.factor(agegroup),
         linenum = as.factor(linenum),
         # create variable and code whether the item was placed in the correct rank (timeline task) or box (calendar task)
         correct = ifelse(response == correctR, 1, 0),
         # create variable to quantify how far the rank/box placement was from the correct rank/box
         dist.error = correctR - response,
         # create variable ignoring the direction of the error
         dist.error.a = abs(dist.error),
         # create variable and code whether the item's final placement was in the future (deictic status of response)
         resp.stat = case_when(task=='timeline' & distfrommid > 0 ~ 1,
                               task=='calendar' & response > 4 ~ 1,
                               task=='verbal' & item == 'today' & response > correctR ~ 1, 
                               TRUE ~ 0), # "what day will it be yesterday/today/tomorrow etc. e.g., Wednesday would be coded as 3"
         # create variable and code deictic status of the *first* sticker placement and drawn location on the lineline
         resp.stat.1 = case_when(task=='timeline' & distfrommid > 0 ~ 1,
                                 task=='calendar' & response1 > 4 ~ 1,
                                 task=='verbal' & item == 'today' & response1 > correctR ~ 1, 
                                 TRUE ~ 0),
         # create variable and code whether the item's correct placement is in the future
         item.stat = ifelse(item %in% future.words, 1, 0),
         # create variable and code whether the participant correctly placed the item in the past vs. future
         stat.correct = ifelse(item.stat == resp.stat, 1, 0),
         # create variable and code whether the deictic status of the first sticker placement was correct
         stat.correct.1 = ifelse(item.stat == resp.stat.1, 1, 0))


# caculate correct remoteness
d.e <- d.e %>%
  # create a variable to quantify how far the rank/box placement was from 'today'
  mutate(resp.remoteness = case_when(task == 'calendar' & itemtype == 'deictic' ~ abs(response - 4))) %>% 
  # create new variable to code if response remoteness was correct for all items on the calendar and timeline tasks
  mutate(cor.remote = case_when(task == "calendar" & itemtype == "deictic" & resp.remoteness == abs(correctR - 4) ~ '1',
                                task == "timeline" & linenum == "1" & item == "lastbday" & between(distfrommid, -7, -3.5) ~ '1',
                                task == "timeline" & linenum == "1" & item == "breakfast" & between(distfrommid, -3.5, 0) ~ '1',
                                task == "timeline" & linenum == "1" & item == "dinner" & between(distfrommid, 0, 3.5) ~ '1', 
                                task == "timeline" & linenum == "1" & item == "nextbday" & between(distfrommid, 3.5, 7) ~ '1',
                                task == "timeline" & linenum == "2" & item == "lastweek" & between(distfrommid, -7, -3.5) ~ '1',
                                task == "timeline" & linenum == "2" & item == "thismorning" & between(distfrommid, -3.5, 0) ~ '1',
                                task == "timeline" & linenum == "2" & item == "tonight" & between(distfrommid, 0, 3.5) ~ '1',
                                task == "timeline" & linenum == "2" & item == "tomorrow" & between(distfrommid, 3.5, 7) ~ '1',
                                task == "timeline" & linenum == "3" & item == "lastyear" & between(distfrommid, -7, -3.5) ~ '1',
                                task == "timeline" & linenum == "3" & item == "yesterday" & between(distfrommid, -3.5, 0) ~ '1',
                                task == "timeline" & linenum == "3" & item == "nextweek" & between(distfrommid, 0, 3.5) ~ '1',
                                task == "timeline" & linenum == "3" & item == "nextyear" & between(distfrommid, 3.5, 7) ~ '1',
                                task == "timeline" & linenum == "4" & item == "beforeyesterday" & between(distfrommid, -7, -3.5) ~ '1',
                                task == "timeline" & linenum == "4" & item == "yesterday" & between(distfrommid, -3.5, 0) ~ '1',
                                task == "timeline" & linenum == "4" & item == "tomorrow" & between(distfrommid, 0, 3.5) ~ '1',
                                task == "timeline" & linenum == "4" & item == "aftertomorrow" & between(distfrommid, 3.5, 7) ~ '1',
                                TRUE ~ '0'),
         cor.remote = as.numeric(cor.remote))

# participant counts
subs <- d.e %>%
  select(subjid, agegroup) %>%
  group_by(agegroup) %>%
  distinct
subs_counts <- subs %>%
  group_by(agegroup) %>%
  summarize(n())

#### Analysis of past/future assignment on the calendar task ####

# create data frame with only deictic calendar items
cal.d <- d.e %>% 
  filter(task=="calendar" & itemtype=="deictic")
cal.d2 <- cal.d %>%
  select(-linelength,-distfrommid) %>%
  mutate(cor.first = ifelse(response1==correctR, 1, 0), # did they get it right on the first trial
         prox = ifelse(item %in% c('yesterday','tomorrow'),1,0), # yesterday and tomorrow coded as proximal terms
         prox = as.factor(prox))

# summarize data including 3- and 7-year-olds
cal.sum <- cal.d2 %>% 
  mutate(item = factor(item, levels = c("beforeyesterday", "aftertomorrow","yesterday","tomorrow")),
         agegroup = recode_factor(agegroup, '3' = 'Age 3',
                                '4' = 'Age 4',
                                '5' = 'Age 5',
                                '6' = 'Age 6',
                                '7' = 'Age 7')) %>%
  group_by(agegroup, item) %>%
  summarize(first.m = mean(cor.first, na.rm=T), # % who put item in right box on first try
            sd.correct = sd(cor.first, na.rm=T),
            n = n(),
            se.correct = sd.correct/sqrt(n),
            deictic1.m = mean(stat.correct.1), # who got correct status (first answer)
            deictic.m = mean(stat.correct), # % who got correct status (final answer)
            deictic1.sd = (sd(stat.correct.1)), # standard deviation
            deictic1.se = deictic1.sd/sqrt(n), # calculate standard error
            deictic1.lower = deictic1.m - deictic1.se, # calculate lower 95% CI
            deictic1.upper = deictic1.m + deictic1.se, # calculate upper 95% CI
            rank.m = mean(correct), # right box (final answer)
            countCor.first = sum(cor.first))  # sum proportion correct for making bar graph instead of histogram

#Does age or item (prox vs. distal status) predict successful placement of time words in the past vs. the future? [interaction term included]
d.lm.ec1 <-  glmer(stat.correct.1 ~ prox*scale(ageyears) + (1|subjid), family='binomial', data=cal.d2) 
summary(d.lm.ec1)
Anova(d.lm.ec1, Type=3) 

#now check for main effects of age and item [no interaction term included in the model]
d.lm.ec2 <-  glmer(stat.correct.1 ~ prox+scale(ageyears) + (1|subjid), family='binomial', data=cal.d2) 
summary(d.lm.ec2)
Anova(d.lm.ec2, Type=3) #main effect of age

Temp.PF2 <- aggregate(stat.correct.1 ~ agegroup + ageyears + subjid, subset(cal.d2), mean)
with(subset(Temp.PF2, agegroup=="3"), t.test(x=stat.correct.1, mu=.5))
with(subset(Temp.PF2, agegroup=="4"), t.test(x=stat.correct.1, mu=.5))
with(subset(Temp.PF2, agegroup=="5"), t.test(x=stat.correct.1, mu=.5))
with(subset(Temp.PF2, agegroup=="6"), t.test(x=stat.correct.1, mu=.5))
with(subset(Temp.PF2, agegroup=="7"), t.test(x=stat.correct.1, mu=.5))
with(subset(Temp.PF2, agegroup %in% c("4", "5")), t.test(stat.correct.1 ~ agegroup, var.equal=T))

# Plot correct deictic status on the calendar task
correctStatus_cal <- ggplot(data=cal.sum, 
                        aes(x=agegroup, y=deictic1.m, group = item, color=item)) +
  #geom_line(aes(linetype=item)) +
  geom_line(aes(color=item))+
  #scale_linetype_manual(values=c("twodash", "dotted", "solid", "longdash"))+
  geom_point() +
  ylab('Proportion Correct') +
  xlab('') +
  ylim(0,1) +
  theme_minimal(base_size = 14) +
  theme(plot.title=element_text(family = "Times", color = "black", size = 14, hjust = 0.5), 
        axis.title.y = element_text(family = "Times", color = "black", size = 14),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(family = "Times", color = "black", size = 14), 
        axis.title.x = element_text(family = "Times", color = "black", size = 14),
        axis.text.x = element_text(family = "Times", color = "black", size = 14)) +
  theme(legend.position = c(0.7, 0.2)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
correctStatus_cal

#### Analysis of past/future assignment on the timeline task ####
d.e.T <- d.e %>%
  subset(task == "timeline" & itemtype == "deictic" & linenum == "4") # restricting analyses to timeline 4 so items are matched across the timeline and calendar task

# Past/Future -- do subjects attribute P items to left of now and F items to right of now?
d.e.T2 <- aggregate(stat.correct ~ agegroup + subjid + ageyears + item, d.e.T, mean)
d.e.T2$item <- factor(d.e.T2$item) # filtered out event words so adding item to the model rather than itemtype
d.e.T2$tBNRight <- asin(sqrt(d.e.T2$stat.correct))

#Does age or item predict successful placement of time words in the past vs. the future? [interaction term included]
d.lm.eT <-  glmer(stat.correct ~ item*scale(ageyears) + (1|subjid), family='binomial', data=d.e.T2) 
summary(d.lm.eT)
Anova(d.lm.eT, Type=3) 

#now check for main effects of age and item [no interaction term included in the model]
d.lm.eT <-  glmer(stat.correct ~ item + scale(ageyears) + (1|subjid), family='binomial', data=d.e.T2) 
summary(d.lm.eT)
Anova(d.lm.eT, Type=3) 

# analysis of variance model
stat.correct.aov <- aov(stat.correct ~ ageyears * item + Error(subjid/item),d.e.T2)
summary(stat.correct.aov) #Effects of Age
# pairwise tests to check item level effects
pairwise.t.test(d.e.T2$stat.correct, d.e.T2$item, p.adj = "bonf")


Temp.PF3 <- aggregate(stat.correct ~ agegroup + ageyears + subjid, subset(d.e.T2), mean)
with(subset(Temp.PF3, agegroup=="3"), t.test(x=stat.correct, mu=.5))
with(subset(Temp.PF3, agegroup=="4"), t.test(x=stat.correct, mu=.5))
with(subset(Temp.PF3, agegroup=="5"), t.test(x=stat.correct, mu=.5))
with(subset(Temp.PF3, agegroup=="6"), t.test(x=stat.correct, mu=.5))
with(subset(Temp.PF3, agegroup=="7"), t.test(x=stat.correct, mu=.5))
with(subset(Temp.PF3, agegroup %in% c("4", "5")), t.test(stat.correct ~ agegroup, var.equal=T))


# summarize timeline deictic status knowledge
timeline.sum <- d.e.T %>% 
  mutate(item = factor(item, levels = c("beforeyesterday", "aftertomorrow","yesterday","tomorrow")),
         agegroup = recode_factor(agegroup, '3' = 'Age 3',
                                  '4' = 'Age 4',
                                  '5' = 'Age 5',
                                  '6' = 'Age 6',
                                  '7' = 'Age 7')) %>%
  group_by(agegroup, item) %>%
  summarize(n = n(),
            deictic1.m = mean(stat.correct.1), # who got correct status (first answer)
            deictic.m = mean(stat.correct), # % who got correct status (final answer)
            deictic1.sd = (sd(stat.correct.1)), # standard deviation
            deictic1.se = deictic1.sd/sqrt(n), # calculate standard error
            deictic1.lower = deictic1.m - deictic1.se, # calculate lower 95% CI
            deictic1.upper = deictic1.m + deictic1.se) # calculate upper 95% CI

# Plot correct deictic status on the timeline
correctStatus_time <- ggplot(data=timeline.sum, 
                        aes(x=agegroup, y=deictic1.m, group = item, color=item)) +
  #geom_line(aes(linetype=item)) +
  geom_line(aes(color=item))+
  #scale_linetype_manual(values=c("twodash", "dotted", "solid", "longdash"))+
  geom_point() +
  ylab('Proportion Correct') +
  xlab('') +
  ylim(0,1) +
  theme_minimal(base_size = 14) +
  theme(plot.title=element_text(family = "Times", color = "black", size = 14, hjust = 0.5), 
        axis.title.y = element_text(family = "Times", color = "black", size = 14),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(family = "Times", color = "black", size = 14), 
        axis.title.x = element_text(family = "Times", color = "black", size = 14),
        axis.text.x = element_text(family = "Times", color = "black", size = 14)) +
  theme(legend.position = c(0.7, 0.2)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
correctStatus_time

# combine timeline data and calendar data plots into one figure
CorrectStatusFigure <- ggarrange(correctStatus_cal, correctStatus_time,
                                 ncol = 2, nrow = 1, common.legend = TRUE, legend = "top")
# save figure as a .jpeg file
ggsave(CorrectStatusFigure, file="correctStatusFigure.jpeg", width = 10, height = 5, dpi = 300)


# compare past/future placement on timeline vs calendar task
d.e.Tc <- rbind(cal.d, d.e.T)

TC_sum <- d.e.Tc %>%
  group_by(agegroup, task) %>%
  summarize(n = n(),
            deictic1.m = mean(stat.correct.1), # who got correct status (first answer)
            deictic.m = mean(stat.correct), # % who got correct status (final answer)
            deictic1.sd = (sd(stat.correct.1)), # standard deviation
            deictic1.se = deictic1.sd/sqrt(n), # calculate standard error
            deictic1.lower = deictic1.m - deictic1.se, # calculate lower 95% CI
            deictic1.upper = deictic1.m + deictic1.se) # calculate upper 95% CI

ggplot(TC_sum, aes(task, deictic1.m)) +
  geom_boxplot()
t.test(stat.correct.1 ~ task, data = d.e.Tc)



# calculate percentage of trials in which kids of each age group responded with correct remoteness on calendar questions
cal.sum2 <- cal.d %>%
  group_by(agegroup, item) %>%
  summarize(n = n(),
            remoteness.m = (mean(cor.remote))*100, # who got correct status (first answer)
            remoteness.sd = (sd(cor.remote))*100, # standard deviation
            remoteness.se = remoteness.sd/sqrt(n), # calculate standard error
            remoteness.lower = remoteness.m - remoteness.se, # calculate lower 95% CI
            remoteness.upper = remoteness.m + remoteness.se) # calculate upper 95% CI

#Does age or item predict placement of time words the exact distance from the present? [interaction term included]
d.lm.ec3 <-  glmer(cor.remote ~ prox*scale(ageyears) + (1|subjid), family='binomial', data=cal.d2) 
summary(d.lm.ec3)
Anova(d.lm.ec3, Type=3) 

#now check for main effects of age and item [no interaction term included in the model]
d.lm.ec4 <-  glmer(cor.remote ~ item + scale(ageyears) + (1|subjid), family='binomial', data=cal.d2) 
summary(d.lm.ec4)
Anova(d.lm.ec4, Type=3) 

Temp.R3 <- aggregate(cor.remote ~ agegroup + ageyears + subjid, subset(cal.d2), mean)
with(subset(Temp.R3, agegroup=="3"), t.test(x=cor.remote, mu=.5))
with(subset(Temp.R3, agegroup=="4"), t.test(x=cor.remote, mu=.5))
with(subset(Temp.R3, agegroup=="5"), t.test(x=cor.remote, mu=.5))
with(subset(Temp.R3, agegroup=="6"), t.test(x=cor.remote, mu=.5))
with(subset(Temp.R3, agegroup=="7"), t.test(x=cor.remote, mu=.5))
with(subset(Temp.R3, agegroup %in% c("4", "5")), t.test(cor.remote ~ agegroup, var.equal=T))

ggplot(cal.sum2, aes(item, remoteness.m)) +
  geom_boxplot()


# Analyses of ordinal rank
# create new variable to code rank
d.e.T <- d.e.T %>%
mutate(cor.rank = case_when(item == "beforeyesterday" ~ '1',
                            item == "yesterday"  ~ '2',
                            item == "tomorrow"  ~ '3',
                            item == "aftertomorrow" ~ '4',
                            TRUE ~ '0'),
       cor.rank = as.numeric(cor.rank),
       rankCor = ifelse(cor.rank == rank, 1, 0))

# summarize timeline sequential order knowledge
timeline.orderSum <- d.e.T %>%
group_by(agegroup, item) %>%
  summarize(n = n(),
            order.m = mean(rankCor, na.rm = TRUE), # who got correct status (first answer)
            order.sd = (sd(rankCor, na.rm = TRUE)), # standard deviation
            order.se = order.sd/sqrt(n), # calculate standard error
            order.lower = order.m - order.se, # calculate lower 95% CI
            order.upper = order.m + order.se) # calculate upper 95% CI

#Calculate RankDif, the difference between rank on timeline and correct rank
d.e.T$RankDif <- abs(d.e.T$cor.rank - d.e.T$rank)

# Anova of Rank error ####
Temp.TL.aov <- aov(RankDif ~ ageyears * item + Error(subjid/(item)), d.e.T)
summary(Temp.TL.aov) #highly sig effect of age

# I think data is unbalanced so I need to use lm
rank.lm <- lm(RankDif ~ ageyears, rank)
summary(rank.lm)

# t-tests by age of aggregate Rank error ####
# t-tests at each age

Temp.TL5 <- aggregate(RankDif ~ agegroup + subjid, subset(d.e.T), mean)
with(subset(Temp.TL5, agegroup %in% c("3")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, agegroup %in% c("4")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, agegroup %in% c("5")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, agegroup %in% c("6")), t.test(RankDif, mu=5))
with(subset(Temp.TL5, agegroup %in% c("7")), t.test(RankDif, mu=5))

with(subset(Temp.TL5, agegroup %in% c("3","4")), t.test(RankDif~agegroup, var.equal=T))


###left off here
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
