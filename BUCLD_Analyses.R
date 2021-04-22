
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
                                task == "timeline" & linenum == "4" & item == "aftertomorrow" & between(distfrommid, 3.5, 7) ~ '1'),
         cor.remote = case_when(cor.remote == NA ~ '0'),
         cor.remote = as.numeric(cor.remote))

# participant counts
subs <- d.e %>%
  select(subjid, agegroup) %>%
  group_by(agegroup) %>%
  distinct
subs_counts <- subs %>%
  group_by(agegroup) %>%
  summarize(n())

#Does age or item predict successful placement of time words in the past vs. the future within each language group separately? [no interaction term]
d.lm.2e <-  glmer(stat.correct.1 ~ prox + scale(ageyears) + (1|subjid), family='binomial', data=cal.e) 
summary(d.lm.2e)
Anova(d.lm.2e, Type=3) #main effect of age
