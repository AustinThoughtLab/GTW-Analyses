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
d.all <- read.csv('Child Data and Analyses/combinedData_cleaned.csv')

# Participant counts
subs <- d.all %>%
  select(subjid, agegroup, language) %>%
  group_by(agegroup, language) %>%
  distinct
subs_counts <- subs %>%
  group_by(agegroup, language) %>%
  summarize(n())


# Calendar Task Analyses Begin Here
calv.d <- d.all %>% 
  filter(task == "calendar" & itemtype %in% c('deictic', 'verbal')) %>% #include all calendar items (verbal Qs too)
  select(-linelength,-distfrommid) %>%
  mutate(cor.first = ifelse(response1==correctR, 1, 0), # did they get it right on the first trial
         prox = ifelse(item %in% c('yesterday','tomorrow'),1,0), # yesterday and tomorrow coded as proximal terms
         prox = as.factor(prox))

cal.d <- d.all %>% #create data frame that includes 3- to 7-year-olds
  filter(task=="calendar" & itemtype=="deictic") %>% # only select deictic calendar items (verbal items not involved in the calendar task)
  # filter(agegroup %in% c('4','5','6') & ageyears<7.0) %>% 
  select(-linelength,-distfrommid) %>%
  mutate(cor.first = ifelse(response1==correctR, 1, 0), # did they get it right on the first trial
         prox = ifelse(item %in% c('yesterday','tomorrow'),1,0), # yesterday and tomorrow coded as proximal terms
         prox = as.factor(prox))

cal.sum2 <- cal.d %>% # summarize data including 3- and 7-year-olds
  mutate(item = factor(item, levels = c("beforeyesterday", "aftertomorrow","yesterday","tomorrow"))) %>% # for each word, how many kids got them right on the first try
  group_by(language, agegroup, item) %>%
  summarize(first.m = mean(cor.first, na.rm=T), # % who put item in right box on first try
            sd.correct = sd(cor.first, na.rm=T),
            n = n(),
            se.correct = sd.correct/sqrt(n),
            deictic1.m = mean(stat.correct.1), # who got correct status (first answer)
            deictic.m = mean(stat.correct), # % who got correct status (final answer)
            rank.m = mean(correct), # right box (final answer)
            countCor.first = sum(cor.first))  # sum proportion correct for making bar graph instead of histogram

cal.d2 <- d.all %>% # create new data frame and remove 3- and 7-year-olds
  filter(task=="calendar" & itemtype=="deictic" & agegroup %in% c('4','5','6') & ageyears<7.0) %>% # only select deictic calendar items
  select(-linelength,-distfrommid) %>%
  mutate(cor.first = ifelse(response1==correctR, 1, 0), # did they get it right on the first trial
         prox = ifelse(item %in% c('yesterday','tomorrow'),1,0), # yesterday and tomorrow coded as proximal terms
         prox = as.factor(prox))

cal.sum <- cal.d2 %>% # summary of data frame excluding 3- and 7-year-olds
  mutate(item = factor(item, levels = c("beforeyesterday", "aftertomorrow","yesterday","tomorrow")),
         prox = case_when(item == 'beforeyesterday' ~ 'Distal Terms',
                          item == 'aftertomorrow' ~ 'Distal Terms',
                          item == 'yesterday' ~ 'Proximal Terms',
                          item == 'tomorrow' ~ 'Proximal Terms')) %>%
  group_by(language, agegroup, item, prox) %>%
  summarize(first.m = mean(cor.first, na.rm=T), # % who put item in right box on first try
            sd.correct = sd(cor.first, na.rm=T),
            n = n(),
            se.correct = sd.correct/sqrt(n),
            deictic1.m = mean(stat.correct.1), # % who got correct status (first answer)
            deictic.m = mean(stat.correct), # % who got correct status (final answer)
            remoteness.m = mean(cor.remote), # % who got correct remoteness
            rank.m = mean(correct), # % right box (final answer)
            countCor.first = sum(cor.first))  # sum proportion correct for making bar graph instead of histogram

# calculate percentage of trials kids of each age group responded with correct deictic status on calendar questions
cal.sum3 <- cal.d %>%
  group_by(agegroup) %>%
  summarize(n = n(),
            deictic1.m = (mean(stat.correct.1))*100, # who got correct status (first answer)
            deictic1.sd = (sd(stat.correct.1))*100, # standard deviation
            deictic1.se = deictic1.sd/sqrt(n), # calculate standard error
            deictic1.lower = deictic1.m - deictic1.se, # calculate lower 95% CI
            deictic1.upper = deictic1.m + deictic1.se) # calculate upper 95% CI

# calculate percentage of trials in which kids of each age group responded with correct remoteness on calendar questions
cal.sum5 <- cal.d %>%
  group_by(agegroup) %>%
  summarize(n = n(),
            remoteness.m = (mean(cor.remote))*100, # who got correct status (first answer)
            remoteness.sd = (sd(cor.remote))*100, # standard deviation
            remoteness.se = remoteness.sd/sqrt(n), # calculate standard error
            remoteness.lower = remoteness.m - remoteness.se, # calculate lower 95% CI
            remoteness.upper = remoteness.m + remoteness.se) # calculate upper 95% CI

cal.e <- cal.d %>% # filter english speakers only and create new data frame
  filter(language == "english") %>%
  mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

cal.g <- cal.d %>%
  filter(language == "german") %>% # filter german speakers only and create new data frame
  mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

cal.d %>%
  mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

# Calendar task only: Frequency distribution of box placement for each time word
calhist.e <- cal.d %>% #filter english speakers and create new data frame
  filter(language == "english" & agegroup %in% c('4','5','6') & ageyears<7.0) %>%
  mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

calhist.g <- cal.d %>% #filter german speakers and create new data frame
  filter(language == "german" & agegroup %in% c('4','5','6') & ageyears<7.0) %>%
  mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

all <- ggplot(cal.d, aes(x = response1)) +
  geom_histogram(position = "identity", colour = "grey40", alpha = 0.2, binwidth = 1) +
  xlab("box placement") +
  ggtitle("English Speakers") +
  ylim(0, 30) +
  facet_grid(language ~ item ~ agegroup)
all

calhist.e$agebin[calhist.e$agegroup == '4'] <- "Age 4"
calhist.e$agebin[calhist.e$agegroup == '5'] <- "Age 5"
calhist.e$agebin[calhist.e$agegroup == '6'] <- "Age 6"
calhist.g$agebin[calhist.g$agegroup == '4'] <- "Age 4"
calhist.g$agebin[calhist.g$agegroup == '5'] <- "Age 5"
calhist.g$agebin[calhist.g$agegroup == '6'] <- "Age 6"

# Frequency distributions of first response only
par(mfrow=c(1,1))
E <- ggplot(calhist.e, aes(x=response1)) +
  geom_histogram(position = "identity", colour = "grey40", alpha = 0.2, binwidth = 1) +
  xlab("square placement") +
  ggtitle("English Speakers") +
  ylim(0, 40) +
  theme_classic() +
  facet_grid(item ~ agebin)

G <- ggplot(calhist.g, aes(x=response1)) +
  geom_histogram(fill="white", position ="identity", colour = "grey40", alpha = 0.2, binwidth = 1) +
  xlab("square placement") +
  ggtitle("German Speakers") +
  ylim(0, 40) +
  theme_classic() +
  facet_grid(item ~ agebin)

histograms <- ggarrange(E, G, 
                        ncol = 2, nrow = 1)
ggsave(histograms, file= "BoxPlacementByItem.jpeg", width = 10, height = 5, dpi = 300)


calhist.g <- calhist.g %>%
  mutate(ColorBar = case_when(item == "tomorrow" & response1 == 5 ~ "0",
                              item == "yesterday" & response1 ==  3 ~ "0",
                              item == "aftertomorrow" & response1 ==  6 ~ "0",
                              item == "beforeyesterday" & response1 ==  2 ~ "0")) # create new variable for filling correct placement

calhist.g$ColorBar[is.na(calhist.g$ColorBar) == T] <- '1' # replace missing data with 0

calhist.e <- calhist.e %>%
  mutate(ColorBar = case_when(item == "tomorrow" & response1 == 5 ~ "0",
                              item == "yesterday" & response1 ==  3 ~ "0",
                              item == "aftertomorrow" & response1 ==  6 ~ "0",
                              item == "beforeyesterday" & response1 ==  2 ~ "0"))
calhist.e$ColorBar[is.na(calhist.e$ColorBar) == T] <- '1' # replace missing data with 0

# recode Items for Facet Labels
calhist.g$Term[calhist.g$item == 'tomorrow'] <- "tomorrow"
calhist.g$Term[calhist.g$item == 'yesterday'] <- "yesterday"
calhist.g$Term[calhist.g$item == 'beforeyesterday'] <- "before-yesterday"
calhist.g$Term[calhist.g$item == 'aftertomorrow'] <- "after-tomorrow"
calhist.e$Term[calhist.e$item == 'tomorrow'] <- "tomorrow"
calhist.e$Term[calhist.e$item == 'yesterday'] <- "yesterday"
calhist.e$Term[calhist.e$item == 'beforeyesterday'] <- "before-yesterday"
calhist.e$Term[calhist.e$item == 'aftertomorrow'] <- "after-tomorrow"

cols <- c("0" = "red", "1" = "grey50")

# Graph Correct First Placement (but show proportion of kids who placed sticker in each box)
#exclude 3- and 7-year-olds from cal.sum4
cal.sum4 <- cal.d2 %>%
  mutate(item = factor(item, levels = c("beforeyesterday", "aftertomorrow","yesterday","tomorrow"))) %>%
  group_by(language, agegroup, item, response1) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n))

# CogSci reviewer asked about our fixed effect coding (which is nonexistent up to this point). "Some kinds of effect coding shouldn’t be used when interactions are present (such as the treatment coding that is default in lmer() and glmer())" 
# R default is to "dummy" code: Reference level coded as zero; Others as 1
# Sum or effect coding is more appropriate for Type III ANOVA: Levels coded so they sum to zero. If 2 levels, coded as -1 and 1 (BUT, R defaults to Type II so dummy coding should be fine here)

# recode language (german = 1, english = -1) and prox (proximal = 1, distal = -1)
# the relevel command can also be used to change this
cal.d <- cal.d %>%
  mutate(language.ec = case_when(language == 'german' ~ '1',
                                 language == 'english' ~ '-1'),
         prox.ec = case_when(prox == '1' ~'1',
                             prox == '0' ~ '-1'),
         language.d = case_when(language == 'german' ~ '1',
                                language == 'english' ~ '0'),
         item.level = case_when(item == 'beforeyesterday' ~ '1',
                                item == 'yesterday' ~ '2',
                                item == 'tomorrow' ~ '3',
                                item == 'aftertomorrow' ~ '4'))
cal.d$language.ec <- as.numeric(cal.d$language.ec)
cal.d$prox.ec <- as.numeric(cal.d$prox.ec)
cal.d$language.d <- as.numeric(cal.d$language.d)

# dummy code item
cal.d$item_yesterday <- ifelse(cal.d$item.level=="2",1,0)
cal.d$item_tomorrow <- ifelse(cal.d$item.level=="3",1,0)
cal.d$item_after <- ifelse(cal.d$item.level=="4",1,0)

# Calendar Task: does language spoken, item, or age (in years) predict successful placement of time words in the past or the future? [interaction term included in the model]
d.lm1 <- glmer(stat.correct.1 ~ language*item*scale(ageyears) + (1|subjid), family = 'binomial', data=cal.d) #FTC
summary(d.lm1) 
Anova(d.lm1, Type=3) #suggests there is a main effect of age, but no main effects of language, item or interactions

#run it again with new effect coded variables
d.lm1.0 <- glmer(stat.correct.1 ~ language.ec*item_yesterday*item_tomorrow*item_after*scale(ageyears) + (1|subjid), family = 'binomial', data=cal.d) 
summary(d.lm1.0) 
Anova(d.lm1.0, Type=3) 

# it seems like (from Roman's slides) since we manipulate prox within-subjects, we should include this as a random slope e.g., prox|subjid
d.lm2 <- glmer(stat.correct.1 ~ prox*language.d*scale(ageyears) + (1+prox|subjid), family = 'binomial', data=cal.d) #FTC
summary(d.lm2) 
Anova(d.lm2, Type=3) #suggests there is a main effect of age, but no main effects of language, item or interactions

# suggestion from cogsci reviewer to rerun model without the random intercept for subjects (but keep random slope because glmer requires random effect term to be specified)

# when we also use effect coded variables, the model appears to converge (I say appears bc boundary(singluar) fit = TRUE)

#d.lm3 <- glmer(cal.d.stat.correct.1 ~ cal.d.item_yesterday*cal.d.item_tomorrow*cal.d.item_after*cal.d.language.ec*scale(cal.d.ageyears) + (0 + cal.d.item.level +1|cal.d.subjid), family = 'binomial', data=regdata)

#summary(d.lm3) 

#Anova(d.lm3, Type=3) #suggests there is a main effect of age, an interaction of age and item, but no main effects of language, item (yes prox/no prox)

#isSingular(d.lm3, tol=1e-04)
#lme4 failed to estimate the by-participant varying
#slopes
# True which means that some "dimensions" of the variance-covariance matrix have been estimated as exactly zero. [the random effects covariance matrices of the fitted model are singular which could mean that this is an overfitted model with poor power]
#coef(d.lm3)

# do it the bayes way - like Mike Frank :-) 
#d.lm3.0 <- brm(stat.correct.1 ~ prox.ec*language.ec*scale(ageyears) + (prox.ec + 1|subjid), data=cal.d)
#summary(d.lm3.0) 
#save(d.lm3.0, file = "mod_brm")

#summary(d.lm3.0, waic = TRUE)
#posterior_summary(d.lm3.0, pars = c("^b_", "^sd_", "sigma"), probs = c(0.025, 0.975))
#estimates above are the log-odds scale. To interpret them more easily, we need to apply the inverse link function (i.e., the logit-inverse) 
#a <- fixef(d.lm3.0)[1]
#now transform it back to the probability scale (equivalent to plogis(a))
#exp(a) / (1 + exp(a))

#' #d.lm3.0.type3 <- Anova(d.lm3.0, type='III') 
#' 
#' # specify contr.sum in R to use type III
#' # cal.d$language.ec = relevel(cal.d$language, ref="german")
#' # cal.d$prox.ec = relevel(cal.d$prox, ref = "1")
#' # contrasts(cal.d$prox.ec)= contr.sum
#' # contrasts(cal.d$language.ec) = contr.sum
#' # d.lm3.0.type3
#' 
#' 
#' Does age or item predict successful placement of time words in the past vs. the future within each language group separately? [no interaction term]

cal.e <- cal.d %>% # filter english speakers only and create new data frame
  filter(language == "english") %>%
  mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

cal.g <- cal.d %>%
  filter(language == "german") %>% # filter german speakers only and create new data frame
  mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

d.lm.6e <-  glmer(stat.correct.1 ~ prox.ec * scale(ageyears) + (0 + prox.ec|subjid), family='binomial', data=cal.e) 
summary(d.lm.6e)
Anova(d.lm.6e, Type=3) #main effect of age, sig interaction of prox + age

#Wilcox_test for item level comparisons
English.testP <- cal.e %>%
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = stat.correct.1 ~ prox.ec, p.adjust.method = "BH") %>%
  add_significance()
English.testP

d.lm.7g <-  glmer(stat.correct.1 ~ prox.ec * scale(ageyears) + (0+ prox.ec|subjid), family='binomial', data=cal.g) 
summary(d.lm.7g)
Anova(d.lm.7g, Type=3) #main effect of age, no interaction

German.testP <- cal.g %>%
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = stat.correct.1 ~ prox.ec, p.adjust.method = "BH") %>%
  add_significance()
German.testP

#' Knowledge of precise meanings on calendar Task: does language spoken, item,  or age (in years) predict successful placement of time words in the correct squares on the first try? [interaction term included in the model]
## -------------------------------------------------------------------------------------------------
cal.lm1 <- glmer(cor.first ~ language.ec*prox.ec*scale(ageyears) + (0 + prox.ec|subjid), family = 'binomial', data=cal.d) 
# boundary (singluar) fit: see ?isSingular
summary(cal.lm1) 
Anova(cal.lm1, Type=3) # suggests there are effects of language, item, and ageyears with no interactions
isSingular(cal.lm1, tol = 1e-04)


# regular regression w/fixed effects only
cal.lm1.0 <- glm(cor.first ~ prox.ec*language.ec*scale(ageyears), family = 'binomial', data=cal.d)
summary(cal.lm1.0)

cal.lm1.0.type3 <- Anova(cal.lm1.0, type='III') 

# specify contr.sum in R to use type III
#cal.d$language.ec = relevel(cal.d$language, ref="german")
#cal.d$prox.ec = relevel(cal.d$prox, ref = "1")
#contrasts(cal.d$prox.ec)= contr.sum
#contrasts(cal.d$language.ec) = contr.sum
#cal.lm1.0.type3


#' Knowledge of precise meanings on calendar Task: does language spoken, deictic status (past vs future),  or age (in years) predict successful placement of time words in the correct squares on the first try? [interaction term included in the model]
## -------------------------------------------------------------------------------------------------
# item.stat: 1 - future, 0 - past
cal.d <- cal.d %>%
  mutate(item.stat.ec = case_when(item.stat == '1' ~'1',
                                  item.stat == '0' ~ '-1'))
cal.d$item.stat.ec <- as.numeric(cal.d$item.stat.ec)

cal.lm5 <- glmer(cor.first ~ language.ec*item.stat.ec*scale(ageyears) + (1|subjid), family = 'binomial', data=cal.d) 
summary(cal.lm5) 
Anova(cal.lm5, Type=3) 

# Does age or item predict successful placement of time words in the correct square on the first try within each language group separately? [no interaction term]
cal.lm.2e <- glmer(cor.first ~ item + scale(ageyears) + (1|subjid), family = 'binomial', data=cal.e) 
summary(cal.lm.2e)
Anova(cal.lm.2e, Type=3)

cal.lm.2g <- glmer(cor.first ~ item + scale(ageyears) + (1|subjid), family = 'binomial', data=cal.g) 
summary(cal.lm.2g)
Anova(cal.lm.2g, Type=3)

# Wilcox_test for item level comparisons (cor.first) within each language group separately
#filter by language & remove 3 and 7-year-olds
cal.e2 <- cal.e %>%
  filter(agegroup %in% c('4','5','6')) %>%
  droplevels() %>%
  select(agegroup, item, cor.first)
#mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

cal.g2 <- cal.g %>%
  filter(agegroup %in% c('4','5','6')) %>%
  droplevels() %>%
  select(agegroup, item, cor.first)
#mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

#Wilcox_test for item level comparisons
English.test <- cal.e2 %>%
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = cor.first ~ item, p.adjust.method = "BH") %>%
  add_significance()
English.test

German.test <- cal.g2 %>%
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = cor.first ~ item, p.adjust.method = "BH") %>%
  add_significance()
German.test

#filter by language & remove 3 and 7-year-olds
cal.d3 <- cal.d %>%
  filter(agegroup %in% c('4','5','6')) %>%
  droplevels() %>%
  select(agegroup, item, cor.first, language, prox)
#mutate(item = factor(item, levels = c("beforeyesterday", "yesterday", "tomorrow", "aftertomorrow")))

language.test <- cal.d3 %>%
  group_by(agegroup, item) %>%
  pairwise_wilcox_test(formula = cor.first ~ language, p.adjust.method = "BH") %>%
  add_significance()
language.test

language.test2 <- cal.d3 %>%
  group_by(item) %>%
  pairwise_wilcox_test(formula = cor.first ~ language, p.adjust.method = "BH") %>%
  add_significance()
language.test2

language.proxTest <- cal.d3 %>%
  group_by(agegroup, prox) %>%
  pairwise_wilcox_test(formula = cor.first ~ language, p.adjust.method = "BH") %>%
  add_significance()
language.proxTest
# 0 = distal, 1 = proximal

# don't split it by age group
language.proxTest2 <- cal.d3 %>%
  group_by(prox) %>%
  pairwise_wilcox_test(formula = cor.first ~ language, p.adjust.method = "BH") %>%
  add_significance()
language.proxTest2

# Does language group, age, and temporal location (proximal vs. distal) predict correct placement? [interaction term included in the model]
cal.lm3 <-  glmer(cor.first ~ language*prox*scale(ageyears) + (1|subjid), family='binomial', data=cal.d) #FTC
summary(cal.lm3)
Anova(cal.lm3, Type=3) # suggests there are no main effects of language, proximity, or age and no interactions

# Does language group, age, and temporal location (proximal vs. distal) predict correct placement? [interaction term removed from the model]
cal.lm4 <-  glmer(cor.first ~ language + prox + scale(ageyears) + (1|subjid), family='binomial', data=cal.d) 
summary(cal.lm4)
Anova(cal.lm4, Type=3) #main effect of age

#-language:prox:ageyears 
verbal <- d.all %>%
  filter(itemtype=="verbal" & agegroup %in% c('4','5') & item=="daysofweek") %>%
  group_by(language, item) %>%
  summarize(correct.m <- mean(correct))

#recode agegroup
cal.sum$agebin[cal.sum$agegroup == '4'] <- "Age 4"
cal.sum$agebin[cal.sum$agegroup == '5'] <- "Age 5"
cal.sum$agebin[cal.sum$agegroup == '6'] <- "Age 6"

#recode language spoken
cal.sum$languagegroup[cal.sum$language == 'english'] <- "English Speakers"
cal.sum$languagegroup[cal.sum$language == 'german'] <- "German Speakers"

#recode Item for Legend
cal.sum$Term[cal.sum$item == 'tomorrow'] <- "tomorrow"
cal.sum$Term[cal.sum$item == 'yesterday'] <- "yesterday"
cal.sum$Term[cal.sum$item == 'beforeyesterday'] <- "before-yesterday"
cal.sum$Term[cal.sum$item == 'aftertomorrow'] <- "after-tomorrow"


# create separate dataframes for each language group (for graphing) 
Englishcal_sum <- cal.sum %>%
  filter(language == "english")
Germancal_sum <- cal.sum %>%
  filter(language == "german")

#exclude 3- and 7-year-olds from cal.sum (and collapse across language)
cal.sum3 <- cal.d2 %>%
  mutate(item = factor(item, levels = c("beforeyesterday", "aftertomorrow","yesterday","tomorrow"))) %>% #for each word, how many kids got them right on the first try
  group_by(agegroup, item) %>%
  summarize(first.m = mean(cor.first, na.rm=T),     # % who put item in right box on first try
            sd.correct = sd(cor.first, na.rm=T),
            n = n(),
            se.correct = sd.correct/sqrt(n),
            deictic1.m = mean(stat.correct.1), # who got correct status (first answer)
            deictic.m = mean(stat.correct), # % who got correct status (final answer)
            rank.m = mean(correct), # right box (final answer)
            countCor.first = sum(cor.first))  # sum proportion correct for making bar graph instead of histogram

## check on bizarre results: English speaking older kids are doing worse with yesterday and tomorrow than the younger ones (not a problem anymore since we corrected the data files, yay!)
Englishcal_sum%>%
  group_by(item, agegroup) %>%
  summarize(mean(deictic.m))
cal.e %>%
  group_by(item, agegroup) %>%
  summarize(mean(stat.correct))

# check percentage of participants (in each language group separately) that were able to correctly recite the days of the week

daysofweek <- d.all %>%
  select(-weekday, -ageyears, -order, -itemnum, -timelinefirst, -task, -linenum, -itemtype, -linelength, -distfrommid, -response2, -prompts, -item.stat, -stat.correct) %>%
  filter(item == "daysofweek")

German_daysofweek <- d.all %>%
  filter(language == "german", item == "daysofweek", exclude == "0") %>%
  select(subjid, agegroup, correct) %>%
  group_by(correct, agegroup) %>%
  summarise(n())

English_daysofweek <- d.all %>%
  filter(language == "english", item == "daysofweek", exclude == "0") %>%
  select(subjid, agegroup, correct) %>%
  group_by(correct, agegroup) %>%
  summarise(n())

##########################COGSCI PAPER ANALYSES END HERE##########################

# does age or item predict children's performance on verbal questions?
Cal.v <- d.all %>%
  filter(task=="calendar" & itemtype == 'verbal') %>% # filter only verbal calendar items
  select(-order, -linenum, -linelength, -distfrommid, -correctr, -timelinefirst, -itemnum) %>%
  mutate(subjid = as.factor(subjid),
         cor.first = ifelse(response1==correctR, 1, 0), # did they get it right on the first trial
         prox = ifelse(item %in% c('yesterday','tomorrow'),1,0), # yesterday and tomorrow coded as proximal terms
         prox = as.factor(prox),
         agebin = case_when(agegroup == '4' ~ 'Age 4',
                            agegroup == '5' ~ 'Age 5',
                            agegroup == '6' ~ 'Age 6'))

#run model with age, item, and language group
age.verbal4 <- glmer(correct ~ language*item*scale(ageyears) + (1|subjid), family = binomial(link='logit'), data=Cal.v)
summary(age.verbal4)
Anova(age.verbal4, Type =3) # failed to converge

age.verbal5 <- glmer(correct ~ language + item + scale(ageyears) + (1|subjid), family = binomial(link='logit'), data=Cal.v)
summary(age.verbal5)
Anova(age.verbal5, Type =3) # failed to converge

age.verbal6 <- glm(correct ~ language + item + scale(ageyears), family = binomial(link='logit'), data=Cal.v)
summary(age.verbal6)
Anova(age.verbal6, Type =3)

# look at bargraph of proportion correct for each item to see what's going on (for each P how many trials did they get correct)

#calculate percentage of trials in which kids of each age group got verbal questions correct
verbal.sum <- Cal.v %>%
  filter(agegroup %in% c('4', '5', '6')) %>%
  group_by(language, agegroup, item, agebin) %>%
  summarize(correct.m.v = mean(correct, na.rm=T), # % who answered correctly
            sd.correct.v = sd(correct, na.rm=T), #calculate standard deviation
            n = n(),
            se.correct.v = sd.correct.v/sqrt(n)) #calculate standard error

ggplot(data = verbal.sum, aes (x = agegroup, y = correct.m.v, fill = language)) +
  geom_bar(stat = 'identity', position = 'dodge', alpha = 2/3) + # or use position = 'dodge' for grouped bars
  labs(x = "Age", title = ('Proportions Correct by Age Group')) +
  facet_wrap(~item)

# make stand alone graph for days of the week
verbal.sum2 <- verbal.sum %>%
  filter(item == "daysofweek")

ggplot(data = verbal.sum2, aes(x=agebin, y = correct.m.v, fill = language)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x = "", y = "Proportion Correct") +
  theme_minimal(base_size = 14) +
  theme(plot.title=element_text(family = "Times", color = "black", size = 14, hjust = 0.5), 
        axis.title.y = element_text(family = "Times", color = "black", size = 14),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(family = "Times", color = "black", size = 14), 
        axis.title.x = element_text(family = "Times", color = "black", size = 14),
        axis.text.x = element_text(family = "Times", color = "black", size = 14)) +
  theme(legend.position = "right", legend.title = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))

##########################BUCLD Abstract ANALYSES BEGIN HERE##########################

# order measure from timeline task

# compare deictic status performance on timeline and calendar task (omit verbal Qs)
# is prop correct similar on the two tasks?

# calendar task gives you remoteness 


#' `````` ## go back and fix all the analyses below this line ------
#' #Cal.v$correct <- factor(Cal.v$correct) #tell r to use correct as a factor variable
#' 
#' #age.verbal <- glm(correct ~ scale(ageyears), family = binomial(link='logit'), data=Cal.v)
#' #summary(age.verbal)
#' #Anova(age.verbal, Type =3)
#' 
#' age.verbal2 <- glm(correct ~ item*scale(ageyears), family = binomial(link='logit'), data=Cal.v) #subject not included as a random factor
#' summary(age.verbal2)
#' Anova(age.verbal2, Type =3)
#' 
#' #### run above model again without the days of the week question
#' target_item <- c("aftertoday", "beforetoday", "today", "yesterday", "tomorrow")
#' Cal.v2 <- d.all %>%
#'   filter(task=="calendar" & itemtype == 'verbal' & item == target_item) %>% # filter verbal Qs but omit days of the week Q %>%
#'   select(-order, -linenum, -linelength, -distfrommid, -correctr, -timelinefirst, -itemnum) %>% # make sure correctR is # accurately coded
#'   mutate(cor.first = ifelse(response1==correctR, 1, 0), # did they get it right on the first trial
#'          prox = ifelse(item %in% c('yesterday','tomorrow'),1,0), # yesterday and tomorrow coded as proximal terms
#'          prox = as.factor(prox))
#' 
#' age.verbal3 <- glm(correct ~ item*scale(ageyears), family = binomial(link='logit'), data=Cal.v2)
#' summary(age.verbal3)
#' Anova(age.verbal3, Type =3)
#' 
#' ##add language to the model
#' age.verbal4 <- glm(correct ~ language*item*scale(ageyears), family = binomial(link='logit'), data=Cal.v2)
#' summary(age.verbal4)
#' Anova(age.verbal4, Type =3)
#' ```
#' 
#' Wilcox_test for item level comparisons (correct) within each language group separately
## -------------------------------------------------------------------------------------------------
#filter by language 
Cal.v2e <- Cal.v2 %>%
  filter(language == 'english') %>%
  filter(agegroup %in% c('4','5','6')) %>%
  droplevels() %>%
  select(agegroup, item, correct)

Cal.v2g <- Cal.v2 %>%
  filter(language == 'german') %>%
  filter(agegroup %in% c('4','5','6')) %>%
  droplevels() %>%
  select(agegroup, item, correct)

#Wilcox_test for item level comparisons
English.testv <- Cal.v2e %>%
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = correct ~ item, p.adjust.method = "BH") %>%
  add_significance()
English.testv

# maybe not enough data to run this? many more NAs in German data compared to English data
German.testv <- Cal.v2g %>%
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = correct ~ item, p.adjust.method = "BH") %>%
  add_significance()
German.testv

#' Calculate % of children who correctly answered verbal questions
## -------------------------------------------------------------------------------------------------
verbal_items <- c("aftertoday", "beforetoday", "yesterday", "tomorrow", "today")

#calculate percentage of trials in which kids of each age group got verbal questions correct
verbal.sum <- d.all %>%
  filter(itemtype == "verbal" & agegroup %in% c('4', '5', '6', '7') & item == verbal_items) %>% #filter only 5 verbal questions
  group_by(language, agegroup, item) %>%
  summarize(correct.m.v = mean(correct, na.rm=T), # % who answered correctly
            sd.correct.v = sd(correct, na.rm=T), #calculate standard deviation
            n = n(),
            se.correct.v = sd.correct.v/sqrt(n), #calculate standard error
            correctv.lower = correct.m.v - se.correct.v, # calculate lower 95% CI
            correctv.upper = correct.m.v + se.correct.v, # calculate upper 95% CI
            countCorrect.v = sum(correct, na.rm=T))  # sum proportion correct for making bar graph instead of histogram

verbal.sum2 <- d.all %>%
  filter(itemtype == "verbal" & agegroup %in% c('4', '5', '6', '7') & item == verbal_items) %>% #filter only 5 verbal questions
  group_by(language, agegroup) %>%
  summarize(correct.m.v = mean(correct, na.rm=T), # % who answered correctly
            sd.correct.v = sd(correct, na.rm=T), #calculate standard deviation
            n = n(),
            se.correct.v = sd.correct.v/sqrt(n), #calculate standard error
            correctv.lower = correct.m.v - se.correct.v, # calculate lower 95% CI
            correctv.upper = correct.m.v + se.correct.v, # calculate upper 95% CI
            countCorrect.v = sum(correct, na.rm=T))  # sum proportion correct for making bar graph instead of histogram

verbal.sum3 <- d.all %>%
  filter(itemtype == "verbal" & item == verbal_items) %>% #filter only 5 verbal questions
  group_by(agegroup) %>%
  summarize(correct.m.v = mean(correct, na.rm=T), # % who answered correctly
            sd.correct.v = sd(correct, na.rm=T), #calculate standard deviation
            n = n(),
            se.correct.v = sd.correct.v/sqrt(n), #calculate standard error
            correctv.lower = correct.m.v - se.correct.v, # calculate lower 95% CI
            correctv.upper = correct.m.v + se.correct.v, # calculate upper 95% CI
            countCorrect.v = sum(correct, na.rm=T))  # sum proportion correct for making bar graph instead of histogram

deictic.sum <- calv.d %>%
  filter(itemtype == "deictic" & agegroup %in% c('4', '5', '6', '7')) %>%
  group_by(language, agegroup, item) %>%
  summarize(correct.m.d = mean(cor.first, na.rm=T), # % who answered correctly
            sd.correct.d = sd(cor.first, na.rm=T),
            n = n(),
            se.correct.d = sd.correct.d/sqrt(n),
            deicticv.lower = correct.m.d - se.correct.d, # calculate lower 95% CI
            deicticv.upper = correct.m.d + se.correct.d, # calculate upper 95% CI
            countCorrect.d = sum(cor.first))  # sum proportion correct for making bar graph instead of histogram

sum.all <- rbind(verbal.sum, deictic.sum) # combine data into one data frame
sum.all.g <- sum.all %>%
  filter(language == "german")
sum.all.e <- sum.all %>%
  filter(language == "english")

#' 
#' ##Did kids who answered verbal Qs correctly also answer Deictic Qs correctly?
#' ##use verbal Q's to predict deictic Q's performance
## -------------------------------------------------------------------------------------------------
# create new data frame
target <- c("verbal", "deictic")
Cal.v3 <- d.all %>%
  filter(task=="calendar" & itemtype == target) %>% # filter only calendar %>%
  select(-order, -linenum, -linelength, -distfrommid, -correctr, -timelinefirst, -itemnum) %>% # make sure correctR is # accurately coded
  mutate(cor.first = ifelse(response1==correctR, 1, 0), # did they get it right on the first trial
         prox = ifelse(item %in% c('yesterday','tomorrow'),1,0), # yesterday and tomorrow coded as proximal terms
         prox = as.factor(prox))

verbal.deictic <- glm(correct ~ itemtype, family = binomial(link='logit'), data=Cal.v3) #run logistic regression
summary(verbal.deictic)
Anova(verbal.deictic)

# add language to the model
verbalL.deictic2 <- glm(correct ~ itemtype + language, family = binomial(link='logit'), data=Cal.v3) #run logistic regression
summary(verbalL.deictic2)
Anova(verbalL.deictic2)

Calv.dg <- Cal.v3 %>% # filter out German participants only
  filter(language == "german")
German_VerbalQuestions <- ggplot(Calv.dg, aes(x = factor(correct))) +
  geom_bar(color = "black") +
  theme_minimal(base_size = 10) +
  facet_grid(itemtype~agegroup)
German_VerbalQuestions

German.test2 <- Calv.dg %>% # conduct Wilcox_test on German sample only
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = correct ~ itemtype, p.adjust.method = "BH") %>%
  add_significance()
German.test2

Calv.de <- Cal.v3 %>% # filter out english participants only
  filter(language == "english")
English_VerbalQuestions <- ggplot(Calv.de, aes(x = factor(correct))) +
  geom_bar(color = "black") +
  theme_minimal(base_size = 10) +
  facet_grid(itemtype~agegroup)
English_VerbalQuestions

English.test2 <- Calv.de %>% # conduct Wilcox_test on english sample only
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = correct ~ itemtype, p.adjust.method = "BH") %>%
  add_significance()
English.test2


#' 
#' ##use verbal Q's to predict deictic Q's performance [split kids by if they recited days of week or not]
## -------------------------------------------------------------------------------------------------
Calv.d2 <- calv.d %>%
  filter(itemtype %in% c('verbal', 'deictic')) %>%
  select(subjid, agegroup, language, itemtype, item, correct)
Calv.d2T  <- reshape(Calv.d2, idvar = c("subjid", "agegroup"), timevar = "itemtype", direction = "wide")
# create new variable to code whether child correctly recited the days of the week
Calv.d2T <- Calv.d2T %>%
  mutate(daysofweek_cor = case_when(item.verbal == "daysofweek" & correct.verbal == '1' ~ '1',
                                    item.verbal == "daysofweek" & correct.verbal == '0' ~ '0'))

Calv.d2T1 <- Calv.d2T %>%
  filter(daysofweek_cor == '1') %>% #select only kids who correctly recited the days of the week
  select(-language.deictic)

Calv.d3 <- reshape(Calv.d2T1, #transpose data back to long format
                   direction = "long",
                   varying = c("correct.verbal", "correct.deictic"), 
                   v.names = c('correct'),
                   timevar = "itemtype", #1 = verbal, 2 = deictic
                   idvar = c("subjid", "agegroup"))

Calv.d3 <- Calv.d3 %>%
  mutate(item = case_when(itemtype == '1' ~ item.verbal, # recode new item variable
                          itemtype == '2' ~ item.deictic)) %>%
  mutate(itemtype = case_when(itemtype == '1' ~ 'verbal', # recode itemtype
                              itemtype == '2' ~ 'deictic')) %>%
  mutate(language = language.verbal) %>% # recode language
  select(-language.verbal, -item.verbal, -item.deictic) # remove duplicate columns

## For children who recited the days of the week, does performance on verbal Q's predict performance on deictic Q's?
verbal.deictic2 <- glm(correct ~ itemtype, family = binomial(link='logit'), data=Calv.d3) #run logistic regression
summary(verbal.deictic2)

Calv.d2T0 <- Calv.d2T %>%
  filter(daysofweek_cor == '0') %>% #select kids who did not correctly recite the days of the week
  select(-language.deictic)

Calv.d4 <- reshape(Calv.d2T0, #transpose data back to long format
                   direction = "long",
                   varying = c("correct.verbal", "correct.deictic"), 
                   v.names = c('correct'),
                   timevar = "itemtype", #1 = verbal, 2 = deictic
                   idvar = c("subjid", "agegroup"))

Calv.d4 <- Calv.d4 %>%
  mutate(item = case_when(itemtype == '1' ~ item.verbal, # recode new item variable
                          itemtype == '2' ~ item.deictic)) %>%
  mutate(itemtype = case_when(itemtype == '1' ~ 'verbal', # recode itemtype
                              itemtype == '2' ~ 'deictic')) %>%
  mutate(language = language.verbal) %>% # recode language
  select(-language.verbal, -item.verbal, -item.deictic) # remove duplicate columns

## For children who did not recite the days of the week correctly, does performance on verbal Q's predict performance on deictic Q's?
verbal.deictic3 <- glm(correct ~ itemtype, family = binomial(link='logit'), data=Calv.d4) # run logistic regression
summary(verbal.deictic3)

## run model on all kids (regardless of whether they recited the days of the week correctly or not)
# I think this model is redundant 
#Calv.d5 <- rbind(Calv.d3, Calv.d4)
#verbal.deictic4 <- glm(correct ~ itemtype, family = binomial(link='logit'), data=Calv.d5) #run logistic regression
#summary(verbal.deictic4)
#Anova(verbal.deictic4)

#' 
## -------------------------------------------------------------------------------------------------
remote1 <- glm(cor.remote ~ language + item + scale(ageyears), family = binomial(link='logit'), data=cal.d)
summary(remote1)
Anova(remote1)


English.remote <- Calv.de %>% # conduct Wilcox_test on English sample only
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = correct ~ itemtype, p.adjust.method = "BH") %>%
  add_significance()
English.remote

German.remote <- Calv.dg %>% # conduct Wilcox_test on German sample only
  group_by(agegroup) %>%
  pairwise_wilcox_test(formula = correct ~ itemtype, p.adjust.method = "BH") %>%
  add_significance()
German.remote

#' 
#' # Plot Proportion Correct
## -------------------------------------------------------------------------------------------------
# first recode correct variable
cal.e$correct_cat[cal.e$correct == 0] <- 'Incorrect'
cal.e$correct_cat[cal.e$correct == 1] <- 'Correct'

# calculate proportions (English Data)
cal.eprop <- cal.e %>%
  count(agegroup, correct_cat) %>% # group_by() & summarize(n=n()) are implicit
  mutate(prop = prop.table(n)) # prop = n/sum(n) works too
as.data.frame(cal.eprop) # strip tbl_df() properties to print

# create ggplot
prop_e <- ggplot(data = cal.eprop, 
                 aes (x = agegroup, y = prop, fill = correct_cat)) +
  geom_bar(stat = 'identity', position = 'stack', alpha = 2/3) + # or use position = 'dodge' for grouped bars
  scale_y_continuous(labels = percent) + # or =waiver() or NULL for the default labels computed by the transformation object
  scale_fill_few('Medium', drop = FALSE) + # keeps levels if data is filtered
  labs(x = "Age", y = NULL, fill = 'Correct Placement',
       title = 'Proportions Correct by Age Group (English Data)')
prop_e

# just another way to calculate proportions
# cal_de_prop <- round(100*prop.table(table(cal.e$correct)), digits=0)
# cal_de_prop <- as.data.frame(cal_de_prop)

# first recode correct variable
cal.g$correct_cat[cal.g$correct == 0] <- 'Incorrect'
cal.g$correct_cat[cal.g$correct == 1] <- 'Correct'

# calculate proportions (German Data)
cal.gprop <- cal.g %>%
  count(agegroup, correct_cat) %>% # group_by() & summarize(n=n()) are implicit
  mutate(prop = prop.table(n)) # prop = n/sum(n) works too
cal.gprop <- as.data.frame(cal.gprop) # strip tbl_df() properties to print

# create ggplot
prop_g <- ggplot(data = cal.gprop, 
                 aes (x = agegroup, y = prop, fill = correct_cat)) +
  geom_bar(stat = 'identity', position = 'stack', alpha = 2/3) + # or use position = 'dodge' for grouped bars
  scale_y_continuous(labels = percent) +
  #scale_fill_few('medium', drops = FALSE) + # keeps levels if data is filtered
  labs(x = "Age", y = NULL, fill = 'Correct Placement',
       title = 'Proportions Correct by Age Group (German Data)')
prop_g

#' # graph prop correct for each verbal question separately
## -------------------------------------------------------------------------------------------------
# calculate proportions (English Data)
cal.eprop <- cal.e %>%
  count(agegroup, item, correct_cat) %>% # group_by() & summarize(n=n()) are implicit
  mutate(prop = prop.table(n)) # prop = n/sum(n) works too
as.data.frame(cal.eprop) # strip tbl_df() properties to print

# create ggplot
prop_e <- ggplot(data = cal.eprop, 
                 aes (x = agegroup, y = prop, fill = correct_cat)) +
  geom_bar(stat = 'identity', position = 'stack', alpha = 2/3) + # or use position = 'dodge' for grouped bars
  scale_y_continuous(labels = percent) + # or =waiver() or NULL for the default labels computed by the transformation object
  scale_fill_few('Medium', drop = FALSE) + # keeps levels if data is filtered
  labs(x = "Age", y = NULL, fill = 'Correct Placement',
       title = 'Proportions Correct by Age Group (English Data)') +
  facet_wrap(~item)
prop_e

# calculate proportions (German Data)
cal.gprop <- cal.g %>%
  count(agegroup, item, correct_cat) %>% # group_by() & summarize(n=n()) are implicit
  mutate(prop = prop.table(n)) # prop = n/sum(n) works too
cal.gprop <- as.data.frame(cal.gprop) # strip tbl_df() properties to print

# create ggplot
prop_g <- ggplot(data = cal.gprop, 
                 aes (x = agegroup, y = prop, fill = correct_cat)) +
  geom_bar(stat = 'identity', position = 'stack', alpha = 2/3) + # or use position = 'dodge' for grouped bars
  scale_y_continuous(labels = percent) +
  #scale_fill_few('medium', drops = FALSE) + # keeps levels if data is filtered
  labs(x = "Age", y = NULL, fill = 'Correct Placement',
       title = 'Proportions Correct by Age Group (German Data)') +
  facet_wrap(~item)
prop_g

# timeline analyses begin here 
# Timelines ask reliability check: one-way mixed consistency, single measure ICC
# English sample only

TimeLine_Reliability <- d.e %>%
  select(distfrommid_c1, distfrommid_c2)
TimeLine_Reliability$distfrommid_c2<- as.numeric(TimeLine_Reliability$distfrommid_c2)
ICC(TimeLine_Reliability)

# subset timeline data
d.all.timeline <- d.all %>%
  subset(task == "timeline")

# calculate maximum distance of each item placed on the timeline
d.all.timeline %>%
  group_by(itemtype, item) %>%
  summarize(average = mean(distfrommid_c1, na.rm=T), minimum = min(distfrommid_c1, na.rm = T), maximum = max(distfrommid_c1, na.rm=T)) # oops found a few TL coding errors that need to be checked/re-coded

# calculate maximum placement distance for each timeline
d.all.timeline %>%
  group_by(linenum) %>%
  summarize(average = mean(distfrommid_c1, na.rm=T), minimum = min(distfrommid_c1, na.rm = T), maximum = max(distfrommid_c1, na.rm=T)) # oops found a few TL coding errors that need to be checked/re-coded

# add maximum placements to dataframe
d.all <- d.all %>%
  mutate(dist_maxF = case_when(task == "timeline" & linenum == "1" ~ '7.4',
                               task == "timeline" & linenum == "2" ~ '7.3',
                               task == "timeline" & linenum == "3" ~ '7.5',
                               task == "timeline" & linenum == "4" ~ '7.2'),
         dist_maxP = case_when(task == "timeline" & linenum == "1" ~ '8.4', 
                               task == "timeline" & linenum == "2" ~ '7.2',
                               task == "timeline" & linenum == "3" ~ '7.3',
                               task == "timeline" & linenum == "4" ~ '7.7'))

# translate dist_max into numeric variable
d.all$dist_maxF <-as.numeric(d.all$dist_maxF)
d.all$dist_maxP <-as.numeric(d.all$dist_maxP)

# standardize distfrommid_c1 [raw distance/max distance on the given timeline] so that they range from -1 to 1
# check with Katharine that this is the correct way to standardize
d.all <- d.all %>%
  group_by(linenum) %>%
  mutate(distfrommid_std = case_when(task == "timeline" & distfrommid_c1 < 0 ~ (distfrommid_c1/dist_maxP),
                                     task == "timeline" & distfrommid_c1 > 0 ~ (distfrommid_c1/dist_maxF)))

# check and make sure scores were standardized correctly (this might correct itself once the coding has been fixed)
d.all %>%
  group_by(linenum) %>%
  summarize(minimum = min(distfrommid_std, na.rm = T), maximum = max(distfrommid_std, na.rm=T))

# assess comprehension of the decitic status of time words on the timeline task only

# calculate percentage of trials kids of each age group responded with correct deictic status for each timeline
tl.sum <- d.all.timeline %>%
  group_by(agegroup, linenum, language) %>%
  summarize(n = n(),
            deictic.m = (mean(stat.correct)), # who got correct status
            deictic.sd = (sd(stat.correct)), # standard deviation
            deictic.se = deictic.sd/sqrt(n), # calculate standard error
            deictic.lower = deictic.m - deictic.se, # calculate lower 95% CI
            deictic.upper = deictic.m + deictic.se) # calculate upper 95% CI

English.tl.sum <- d.all.timeline %>%
  subset(language == "english") %>%
  group_by(agegroup, linenum) %>%
  summarize(n = n(),
            deictic.m = (mean(stat.correct)), # who got correct status
            deictic.sd = (sd(stat.correct)), # standard deviation
            deictic.se = deictic.sd/sqrt(n), # calculate standard error
            deictic.lower = deictic.m - deictic.se, # calculate lower 95% CI
            deictic.upper = deictic.m + deictic.se) # calculate upper 95% CI

German.tl.sum <- d.all.timeline %>%
  subset(language == "german") %>%
  group_by(agegroup, linenum) %>%
  summarize(n = n(),
            deictic.m = (mean(stat.correct)), # who got correct status
            deictic.sd = (sd(stat.correct)), # standard deviation
            deictic.se = deictic.sd/sqrt(n), # calculate standard error
            deictic.lower = deictic.m - deictic.se, # calculate lower 95% CI
            deictic.upper = deictic.m + deictic.se) # calculate upper 95% CI

# calculate average accuracy for all items' placement relative to "now" for each timeline and subject

deictic_linenum <- d.all.timeline %>%
  group_by(subjid, linenum) %>%
  mutate(deictic.m = (mean(stat.correct))) # average correct status

# Plot correct status by timeline (for each language separately)

EnglishStatus.tl <- ggplot(data=English.tl.sum, aes(x=agegroup, y=deictic.m, group=linenum, color=linenum)) +
  geom_line(aes(color=linenum))+
  geom_point() +
  ylab('Proportion Correct') +
  xlab('English Speakers') +
  ylim(0,1) +
  theme_minimal(base_size = 14) +
  theme(plot.title=element_text(family = "Times", color = "black", size = 14, hjust = 0.5), 
        axis.title.y = element_text(family = "Times", color = "black", size = 14),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(family = "Times", color = "black", size = 14), 
        axis.title.x = element_text(family = "Times", color = "black", size = 14),
        axis.text.x = element_text(family = "Times", color = "black", size = 14)) +
  theme(legend.position = "top") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
EnglishStatus.tl

GermanStatus.tl <-ggplot(data=German.tl.sum, aes(x=agegroup, y=deictic.m, group=linenum, color=linenum)) +
  geom_line(aes(color=linenum))+
  geom_point() +
  ylab('Proportion Correct') +
  xlab ('German Speakers') +
  ylim(0,1) +
  theme_minimal(base_size = 14) +
  theme(plot.title=element_text(family = "Times", color = "black", size = 14, hjust = 0.5), 
        axis.title.y = element_text(family = "Times", color = "black", size = 14),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(family = "Times", color = "black", size = 14), 
        axis.title.x = element_text(family = "Times", color = "black", size = 14),
        axis.text.x = element_text(family = "Times", color = "black", size = 14)) +
  theme(legend.position = "top") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
GermanStatus.tl

CorrectStatus_timeline <- ggarrange(EnglishStatus.tl, GermanStatus.tl,
                                    ncol = 2, nrow = 1, common.legend = TRUE, legend = "top")
CorrectStatus_timeline

# ggsave(CorrectStatus_timeline, file="correctStatus_timeline.jpeg", width = 10, height = 5, dpi = 300)

# calculate percentage of trials kids of each age group responded with correct deictic status for each timeline type (deictic vs. event)

tl.sum2 <- d.all.timeline %>%
  group_by(agegroup, itemtype, language) %>%
  summarize(n = n(),
            deictic.m = (mean(stat.correct)), # who got correct status
            deictic.sd = (sd(stat.correct)), # standard deviation
            deictic.se = deictic.sd/sqrt(n), # calculate standard error
            deictic.lower = deictic.m - deictic.se, # calculate lower 95% CI
            deictic.upper = deictic.m + deictic.se) # calculate upper 95% CI

English.tl.sum2 <- d.all.timeline %>%
  subset(language == "english") %>%
  group_by(agegroup, itemtype) %>%
  summarize(n = n(),
            deictic.m = (mean(stat.correct)), # who got correct status
            deictic.sd = (sd(stat.correct)), # standard deviation
            deictic.se = deictic.sd/sqrt(n), # calculate standard error
            deictic.lower = deictic.m - deictic.se, # calculate lower 95% CI
            deictic.upper = deictic.m + deictic.se) # calculate upper 95% CI

German.tl.sum2 <- d.all.timeline %>%
  subset(language == "german") %>%
  group_by(agegroup, itemtype) %>%
  summarize(n = n(),
            deictic.m = (mean(stat.correct)), # who got correct status
            deictic.sd = (sd(stat.correct)), # standard deviation
            deictic.se = deictic.sd/sqrt(n), # calculate standard error
            deictic.lower = deictic.m - deictic.se, # calculate lower 95% CI
            deictic.upper = deictic.m + deictic.se) # calculate upper 95% CI

# Calculate mean deictic status accuracy for each subject and each type of timeline (Deictic vs. Event). 

deictic_itemtype <- d.all.timeline %>%
  group_by(subjid, itemtype) %>%
  mutate(deictic.m = (mean(stat.correct))) # average correct status

# Plot correct status by item type (for each language separately)
EnglishStatus.tl2 <- ggplot(data=English.tl.sum2, aes(x=agegroup, y=deictic.m, group=itemtype, color=itemtype)) +
  geom_line(aes(color=itemtype))+
  geom_point() +
  ylab('Proportion Correct') +
  xlab('English Speakers') +
  ylim(0,1) +
  theme_minimal(base_size = 14) +
  theme(plot.title=element_text(family = "Times", color = "black", size = 14, hjust = 0.5), 
        axis.title.y = element_text(family = "Times", color = "black", size = 14),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(family = "Times", color = "black", size = 14), 
        axis.title.x = element_text(family = "Times", color = "black", size = 14),
        axis.text.x = element_text(family = "Times", color = "black", size = 14)) +
  theme(legend.position = "top") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
EnglishStatus.tl2

GermanStatus.tl2 <-ggplot(data=German.tl.sum2, aes(x=agegroup, y=deictic.m, group=itemtype, color=itemtype)) +
  geom_line(aes(color=itemtype))+
  geom_point() +
  ylab('Proportion Correct') +
  xlab ('German Speakers') +
  ylim(0,1) +
  theme_minimal(base_size = 14) +
  theme(plot.title=element_text(family = "Times", color = "black", size = 14, hjust = 0.5), 
        axis.title.y = element_text(family = "Times", color = "black", size = 14),
        axis.line.y = element_line(color = "black"),
        axis.text.y = element_text(family = "Times", color = "black", size = 14), 
        axis.title.x = element_text(family = "Times", color = "black", size = 14),
        axis.text.x = element_text(family = "Times", color = "black", size = 14)) +
  theme(legend.position = "top") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"))
GermanStatus.tl2

CorrectStatus_timeline2 <- ggarrange(EnglishStatus.tl2, GermanStatus.tl2,
                                     ncol = 2, nrow = 1, common.legend = TRUE, legend = "top")
CorrectStatus_timeline2

# ggsave(CorrectStatus_timeline, file="correctStatus_timeline.jpeg", width = 10, height = 5, dpi = 300)

# Analyze deictic status accuracy with a three-way mixed ANOVA, with item Type (Deictic vs. Event) as a within-subjects factor and Age (3 through 7 years) and language  (German vs. English) as between-subject factors. 

deictic_itemtype %>%
  group_by(itemtype, agegroup, language) %>%
  get_summary_stats(deictic.m, type="mean_sd")

# visualize the data
deictic_bxp <- ggboxplot(
  deictic_itemtype, x = "agegroup", y = "deictic.m",
  color = "itemtype", palette = "jco",
  facet.by = "language"
)
deictic_bxp

# check for outliers
deictic_itemtype %>%
  group_by(agegroup, itemtype, language) %>%
  identify_outliers(deictic.m) # looks like there are some extreme outliers - check with Katharine about this

# check normality assumption (ask Katharine if we need to worry about this)

deictic_itemtype %>%
  group_by(agegroup, itemtype, language) %>%
  shapiro_test(deictic.m)

# re-number subjid variable
#deictic_itemtype <- deictic_itemtype %>%
#mutate(num=group_indices_(deictic_itemtype, .dots =c('subjid')))

# convert itemtype to factor length variable
deictic_itemtype <- deictic_itemtype %>%
  convert_as_factor(itemtype)

#recode itemtype
deictic_itemtype$itemtype2[deictic_itemtype$itemtype == 'event'] <- 1
deictic_itemtype$itemtype2[deictic_itemtype$itemtype == 'deictic'] <- 2

# convert itemtype2 to factor length variable
deictic_itemtype <- deictic_itemtype %>%
  convert_as_factor(itemtype2)

deictic.aov <- anova_test(
  data = deictic_itemtype, dv = deictic.m, wid = subjid, 
  within = itemtype2, between = c(agegroup, language)
)
get_anova_table(deictic.aov)

# need to wait on adult comparison data to test knowledge of remoteness
# calculate percentage of trials in which kids of each age group responded with correct remoteness on calendar questions
# tl.sum1 <- d.all.timeline %>%
# group_by(agegroup, linenum, language) %>%
# summarize(n = n(),
# remoteness.m = (mean(cor.remote))*100, # who got correct status (first answer)
# remoteness.sd = (sd(cor.remote))*100, # standard deviation
# remoteness.se = remoteness.sd/sqrt(n), # calculate standard error
# remoteness.lower = remoteness.m - remoteness.se, # calculate lower 95% CI
# remoteness.upper = remoteness.m + remoteness.se) # calculate upper 95% CI

#' #################################### completed analyses end here 
#' 
#' #What are errors based on? distance from today?
#' #first.m = mean(cor.first, na.rm=T),     # average # of participants who put item in right box on first try
#'             #sd.correct = sd(cor.first, na.rm=T),
#'             #se.correct = sd.correct/sqrt(n),
#'             #deictic.m = mean(stat.correct), # average #participants who got correct status
#'             
#' ###Did children place paired distal, proximal, past, and future items in the correct location? (e.g., correct placement for both tomorrow/after tomorrow)
## -------------------------------------------------------------------------------------------------
#create new data frame
cal.allT <- cal.d %>%
  select (subjid, agegroup, language, item, correct)
#transpose item (i.e., transform from long to wide format)
cal.allT <- reshape(cal.allT, idvar = c("subjid", "agegroup", "language"), timevar = "item", direction = "wide")

#Code for tomorrow & yesterday both correct (proxCorr = 1)
cal.allT <- cal.allT %>%
  mutate(proxCorr = case_when(correct.tomorrow == "1" & correct.yesterday == "1" ~ 1, TRUE ~ 0)) %>%
  #Code for after tomorrow & before yesterday both correct (distCorr = 1)
  mutate(distCorr = case_when(correct.aftertomorrow == "1" & correct.beforeyesterday == "1" ~ 1, TRUE ~ 0)) %>%
  #Code for tomorrow & after tomorrow correct (tomorrow2Corr = 1)
  mutate(tomorrow2Corr = case_when(correct.tomorrow == "1" & correct.aftertomorrow == "1" ~ 1, TRUE ~ 0)) %>%
  #Code for yesterday & before yesterday correct (yesterday2Corr = 1)
  mutate(yesterday2Corr = case_when(correct.yesterday == "1" & correct.beforeyesterday == "1" ~ 1, TRUE ~ 0))

#create new data frame (again) and transpose from wide format back to long format
cal.allTrecoded <- cal.allT %>%
  select(subjid, agegroup, language, proxCorr, distCorr, tomorrow2Corr, yesterday2Corr)
cal.allTrecoded <- reshape(cal.allTrecoded,
                           idvar = "subjid", 
                           varying = list(c(4, 5, 6, 7)), 
                           direction = "long", 
                           v.names = c('BothCorr'), 
                           timevar = "Pair")
#rename values in 'Pair' variable column
cal.allTrecoded <- cal.allTrecoded %>%
  mutate(Pair = as.factor(Pair), 
         item = recode_factor(Pair, '1' = 'proxCorr', 
                              '2' = 'distCorr', 
                              '3' = 'tomorrow2Corr', 
                              '4' = 'yesterday2Corr'))