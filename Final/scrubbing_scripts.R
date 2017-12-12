
#### load libraries #######################################################

library(dplyr)
library(tidyr)
library(gtrendsR)
library(lubridate)
library(ggplot2)
library(rvest)

#### CDC data by state 1984-2014 ####################################################

# raw data originally retrieved from https://wonder.cdc.gov/std-sex.html
# used page's dropdowns, checkboxes, etc to manually retrieve: 
# disease, state, year, and gender
std <- read.csv("https://raw.githubusercontent.com/spitakiss/Data608_Work/master/Week12/std.csv",
                header=T, stringsAsFactors = F)  

# clean CDC raw data: 
#   exclude unknown gender, 
#   include only Chlamydia, Gonorrhea, Chancroid, and Total Syphillis disease
#   change "Total Syphillis" name to "Syphillis"  
std_redux <- std %>% 
  filter(Gender != "Unknown", Disease.Code %in% c(273,274,280,320)) %>%
  mutate(
    Rate = as.numeric(Rate), # convert to numeric, was chr
    Population = as.numeric(Population), # convert to numeric, was chr
    Disease = ifelse(Disease.Code == 320, "Syphilis", Disease) 
  ) %>%
  select(Year, State, Disease, Gender, Count, Population, Rate)

write.csv(std_redux, "std_redux.csv", row.names=FALSE)

# total state population per year
pop_data <- std_redux %>%
  select(Year, State, Population, Gender) %>%
  mutate(Population = as.numeric(Population)) %>%
  distinct() %>%
  group_by(Year, State) %>%
  summarise(tot_pop=sum(Population))

# calculate rate of std change from 2011 to 2014, by state
std_chg <- std_redux %>%
  filter(Year %in% c(2011,2014)) %>%
  group_by(Year,State) %>% 
  summarise(Count = sum(Count)) %>%
  inner_join(pop_data, by = c('Year','State')) %>%
  mutate(Rate = round(Count/tot_pop*100000,1)) %>% 
  select(State,Year, Rate) %>%
  spread(Year, Rate) 
  
names(std_chg) <- c("state","Yr2011","Yr2014")
std_chg <- std_chg %>% mutate(pct_chg = round(Yr2014/Yr2011-1,3))

  
##### Nationwide CDC Data - US Totals through 2016 #################################

# scrape 2016 data from cdc page 
url <- 'https://www.cdc.gov/std/stats16/tables/1.htm#modalIdString_CDCTable_0'  

std_us <- url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table(fill=TRUE) %>%
  data.frame()

# clean up columns, filter for 1984 and later, calculate total rate
std_us <- std_us[3:nrow(std_us),c(1,3,13,15,17)]
names(std_us) <- c("Year","Syphilis","Chlamydia","Gonorrhea","Chancroid")
std_us <- data.frame(apply(std_us,2,as.numeric)) %>%
  filter(Year >= 1984) %>%
  mutate("(All)" = Syphilis + Chlamydia + Gonorrhea + Chancroid) %>%
  gather("Disease", "Rate", 2:6)
  
write.csv(std_us,"std_natl.csv", row.names=FALSE)


#### Pull Tinder Data ###############################################################

# data pull: keyword = 'tinder', US only data, web searches, all available dates 
tinder_all <- gtrends("tinder", geo="US", gprop = "web", time = "all")

# save tinder US data as csv
# write.csv(tinder_all$interest_over_time,"tinder_all_us.csv")

# tinder monthly detail
tinder_monthly <- tinder_all$interest_over_time %>%
  select(date, hits) %>%
  mutate(date = decimal_date(as.Date(date))) %>%
  filter(date >= '1984-01-01' & date <= '2016-12-31')

# save tinder monthly detail to csv
write.csv(tinder_monthly, "tinder_monthly.csv", row.names = FALSE)


### data pull: 9/12/2012 (app live date) through 2014, 
tinder_live <- gtrends("tinder", geo="US", gprop = "web", time = "2012-09-12 2014-12-31")

# save interest by region to csv: only use if gtrends doesn't work
#write.csv(tinder_live$interest_by_region, "tinder_by_state.csv")

 
###### prep data for comparison of tinder popularity and std rate of change#######################3
tinder_live <- tinder_live$interest_by_region
tinder_live <- rename(tinder_live, state=location, popularity=hits)

std_chg <- std_chg %>%
  inner_join(tinder_live, by='state') %>%
  select(state, popularity, pct_chg)


write.csv(std_chg, "std_pct_chg.csv", row.names=FALSE)


