# GTW

12/14/2020 Katie created a repository for GTW and uploaded R Markdown file with in progress analyses. File does not currrently include main analyses
but does include histogram plots with age group faceting for each diectic time word in the calendar task. 

Weird percentages in the summary data.frame have been corrected (i.e., 5YO give placement for 'tomorrow' consistent with its correct location 70% of the time 
and give response consistent with the diectic status of tomorrow 85% of the time).

Changes made:
1. updated the variable 'response' to aggregate response1 and response2 for both language groups (previously the code was aggregating response1 and response2 
for the English speaking sample but not the German speaking sample).
2. updated the 'correctr' variable (correct box and/or order placement). I'm not sure if this was initially done by hand or not, but it looked like there may 
have been a few errors. I created a new variable 'corrrectR' but I'd like to go over this together to make sure I coded it correctly since i'm less familiar 
with the tasks. Once we're confident in the new 'correctR' variable, I'll remove 'correctr' to avoid confusion later.

Notes:
*as of Dec 2020 the first day of the week named when asked to recite (e.g., Sunday or Monday) is not included in the data entry sheet [this may be useful down the road e.g., if we want to split kids by those who started counting days of the week with Monday vs. Sunday]
*as of Feb 2022 the All_analyses_Script is outdated. Individual Calendar and Timeline analyses files are the most up-to-date
