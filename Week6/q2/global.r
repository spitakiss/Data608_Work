################ QUESTION 2 ########################################################
# Often you are asked whether particular States are improving their mortality rates
# (per cause) faster than, or slower than, the national average. Create a visualization
# that lets your clients see this for themselves for one cause of death at the time. 
# Keep in mind that the national average should be weighted by the national population.
######################################################################################

## libraries ###
library(shiny)
library(dplyr)
library(googleVis)


### data ###

# github url to csv file
url <- "https://raw.githubusercontent.com/spitakiss/Data608_Work/master/Week6/cleaned-cdc-mortality-1999-2010-2.csv"  

# read in data to data frame, convert state abbreviations to State Name
df <- read.csv(url, stringsAsFactors = FALSE) %>%
  mutate(StateName = setNames(state.name, state.abb)[State])
df$StateName <- with(df,ifelse(is.na(StateName),'Distric of Columbia',StateName))  

# calculate national average by condition and Year
dfNatl <- df %>%
  group_by(Year, ICD.Chapter) %>%
  summarise(Deaths = sum(Deaths), NatlPopulation = sum(Population)) %>%
  mutate(NatlCrude.Rate = round(Deaths / NatlPopulation * 10^5,2)) %>%
  select(Year, ICD.Chapter, NatlCrude.Rate)



