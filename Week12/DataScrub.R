library(dplyr)
library(tidyr)


# raw data
my_data <- read.csv("https://raw.githubusercontent.com/spitakiss/Data608_Work/master/Week12/std.csv", header=T, stringsAsFactors = F)

# total population per year
pop_data <- my_data %>%
  select(Year, State, Population, Gender) %>%
  filter(Population != 'Not Applicable') %>%
  mutate(Population = as.numeric(Population)) %>%
  distinct() %>%
  group_by(Year) %>%
  summarise(tot_pop=sum(Population))


# total count of new cases per yer
count_data <- my_data %>%
  group_by(Year) %>%
  summarise(Count=sum(Count))

# calculate aggregated incidence rates
rate_data <- count_data %>%
  inner_join(pop_data, by="Year") %>%
  mutate(Rate = round(Count/tot_pop * 100000,1))

# write csv file with aggregate std rates
write.csv(rate_data,"agg_std.csv", row.names = F)  

# remove unknown gender.  Pull in only relevant fields 
std_mod <- my_data %>% 
  filter(Gender != "Unknown") %>%
  mutate(Rate = as.numeric(Rate)) %>%
  select(Disease, State, Year, Gender, Rate)

# export filtered data  
write.csv(std_mod,"std_mod.csv", row.names = F)  

