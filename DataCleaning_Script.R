
# ipak function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

lib = c("tidyverse", "nloptr", "lme4", "ggplot2", "reshape2", "ggpubr", "dplyr", "rstatix", "car", "stats", "coin", "scales", "psych", "ggthemes", "brms")
ipak(lib)

# Read in the  Data
d.g <- read.csv('german_data.csv')
d.g <- d.g %>%      
  select(-dob, -dot, -agedays, -agemonths, -monolingual, -rank, -experimenter, -comments) %>% # remove unnecessary columns
  mutate(language = 'german',
        linenum = ifelse(item=="lastyear", 3, linenum)) # there appears to be an error where this item was incorrectly attributed to line 4

d.g[d.g == "999"] <- NA # Note that 999 indicates missing data
d.g[d.g == ""] <- NA # replace missing data and empty cells with NA

d.e <- read.csv('english_data.csv')
d.e <- d.e %>%
  select(-ID,-dob, -dot, -agedays, -monolingual, -rank, -comments, -experimenter) %>% # remove unnecessary columns
  mutate(language = 'english')
#d.a <- read.csv('TimelineData_All.csv', header=T) # read in adult timeline data
#d.a <- d_a

d.e[d.e == "999"] <- NA # Note that 999 indicates missing data
d.e[d.e == ""] <- NA # replace missing data and empty cells with NA


d.e <- mutate(d.e, response = ifelse(itemtype == 'deictic' & is.na(response2) == T, response1,
                            ifelse(itemtype == 'verbal', response, response2))) # aggregate response1 and response2 columns for deictic items only

d.g <- mutate(d.g, response = ifelse(itemtype %in% c('deictic', 'event') & is.na(response2) == T, response1, 
                              ifelse(itemtype == 'verbal', response1, response2)))

# fill in non-overlapping columns with NAs
#d.a[setdiff(names(d.e), names(d.a))] <- NA
#d.e[setdiff(names(d.a), names(d.e))] <- NA
d.g[setdiff(names(d.e), names(d.g))] <- NA

# Some Data Management and wrangling
# Combine German and English data and fix typos in data entry and re-code weekday (1-7 corresponds to Sun-Sat)#
d.all <- rbind(d.g, d.e) %>% 
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
         weekday = as.factor(weekday),
         weekday = recode_factor(weekday, '1' = '7',
                                 '2' = '1',
                                 '3' = '2',
                                 '4' = '3',
                                 '5' = '4',
                                 '6' = '5',
                                 '7' = '6'), # recode default weekday function in excel to match calendar task (e.g., 1-7)
        distfrommid = distfrommid_c1, # _c1/_c2 for ICC but for main analyses use distfrommid to avoid confusion
        language = as.factor(language),
        itemnum = as.factor(itemnum),
        exclude = replace_na(exclude, 0))

future.words = c('aftertoday','aftertomorrow','dinner','nextbday','nextweek','nextyear','tomorrow','tonight') # Code all future time words

# Code correct responses (to compare to 'correctr' variable)
d.all <- d.all %>%
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
## seems to match correctr variable that already exists in the data file with two exceptions: verbal 'beforetoday' and 'aftertoday' are coded as indicated on the paper coding sheet e.g., 1 or 0.
check <- d.all %>%
  mutate(matchR = ifelse(correctr == correctR, 'YES', 'NO')) %>%
  filter(matchR == 'NO')   ## 74 mismatches, mostly on verbal Q's, in other cases correctR seems to be correct
# Exclusions, change format of variables from factors to numeric and vice versa, create new variables to code correct responses and deictic status of responses on both the calendar and timeline task
d.all <- d.all %>%
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

#' Create new variables to code correct remoteness status of responses on the calendar and timeline tasks:
#' 1. calculate response remoteness on calendar task: abs(response placement - location of 'today'))
#' 2. code correct remoteness on calendar task: 1 = yesterday/tomorrow; 2 = beforeyesterday/aftertomorrow
#' 3. calculate response remoteness on teimeline task: Rank 1 = -7 < x < -3.5; Rank 2 = -3.5 < x < 0; Rank 3 = 0 < x < 3.5; Rank 4 = 3.5 < x < 7
#' 4. calculate correct remoteness on timeline task: if response remoteness falls within appropriate numerical range------------------------------------------------------------------------------------
d.all <- d.all %>%
  # create a variable to quantify how far the rank/box placement was from 'today'
  mutate(resp.remoteness = case_when(task == 'calendar' & itemtype == 'deictic' ~ abs(response - 4))) %>%
  # create new variable to code if response remoteness was correct for all items on the calendar and timeline tasks
  mutate(cor.remote = case_when(task == "calendar" & itemtype == "deictic" & resp.remoteness == abs(correctR - 4) ~ '1',
                                
                                task == "calendar" & itemtype == "deictic" & resp.remoteness > abs(correctR - 4) ~ '0',
                                task == "calendar" & itemtype == "deictic" & resp.remoteness < abs(correctR - 4) ~ '0',
                                task == "calendar" & itemtype == "deictic" & resp.remoteness == NA ~ 'NA'))
                                #task == "timeline" & linenum == "1" & item == "lastbday" & between(distfrommid, -7, -3.5) ~ '1',
                                #task == "timeline" & linenum == "1" & item == "breakfast" & between(distfrommid, -3.5, 0) ~ '1',
                                #task == "timeline" & linenum == "1" & item == "dinner" & between(distfrommid, 0, 3.5) ~ '1', 
                                #task == "timeline" & linenum == "1" & item == "nextbday" & between(distfrommid, 3.5, 7) ~ '1',
                                #task == "timeline" & linenum == "2" & item == "lastweek" & between(distfrommid, -7, -3.5) ~ '1',
                                #task == "timeline" & linenum == "2" & item == "thismorning" & between(distfrommid, -3.5, 0) ~ '1',
                                #task == "timeline" & linenum == "2" & item == "tonight" & between(distfrommid, 0, 3.5) ~ '1',
                                #task == "timeline" & linenum == "2" & item == "tomorrow" & between(distfrommid, 3.5, 7) ~ '1',
                                #task == "timeline" & linenum == "3" & item == "lastyear" & between(distfrommid, -7, -3.5) ~ '1',
                                #task == "timeline" & linenum == "3" & item == "yesterday" & between(distfrommid, -3.5, 0) ~ '1',
                                #task == "timeline" & linenum == "3" & item == "nextweek" & between(distfrommid, 0, 3.5) ~ '1',
                                #task == "timeline" & linenum == "3" & item == "nextyear" & between(distfrommid, 3.5, 7) ~ '1',
                                #task == "timeline" & linenum == "4" & item == "beforeyesterday" & between(distfrommid, -7, -3.5) ~ '1',
                                #task == "timeline" & linenum == "4" & item == "yesterday" & between(distfrommid, -3.5, 0) ~ '1',
                                #task == "timeline" & linenum == "4" & item == "tomorrow" & between(distfrommid, 0, 3.5) ~ '1',
                                #task == "timeline" & linenum == "4" & item == "aftertomorrow" & between(distfrommid, 3.5, 7) ~ '1')
cal <- d.all %>%
  filter(task == "calendar" & itemtype == "deictic")

write.csv(d.all,"combinedData_cleaned.csv")

# d.all$cor.remote[is.na(d.all$cor.remote) == T] <- '0' # replace missing data with 0
# d.all$cor.remote <- as.numeric(d.all$cor.remote) # convert from character length to numeric variable