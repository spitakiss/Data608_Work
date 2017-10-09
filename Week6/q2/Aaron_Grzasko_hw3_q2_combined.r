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


 
ui <- fluidPage(
  headerPanel('Ratio of State Mortality Rate to U.S. Average, by Year'),
  selectInput(inputId = 'icd', label = 'Select Condition',
              choices = unique(df$ICD.Chapter),
              selected = 'Certain infectious and parasitic diseases'
  ),
  sliderInput(inputId = 'year', label = 'Select Year',
              value = 1999, min = min(df$Year), max = max(df$Year),
              step = 1, sep = "", 
              animate = animationOptions(interval = 1500, loop = TRUE)),
  
  htmlOutput('map')
  
)

server <- function(input, output){
  
  # filter data for selected icd condition
  dfMod <- reactive({
    df %>%
      filter(ICD.Chapter == input$icd, Year == input$year)
  })
  
  output$map <- renderGvis({
    
    # ratio of state mortality rate to national avg, by year
    dfRatio <- dfMod() %>%
      inner_join(dfNatl, by = c("Year"="Year","ICD.Chapter"="ICD.Chapter")) %>%
      mutate(StToNatl = round(Crude.Rate / NatlCrude.Rate, 2)) %>%
      select(StateName,StToNatl)
      
    
    #gvisMotionChart(data = dfRatio, idvar="StateName",timevar = "Year")
    gvisGeoChart(dfRatio, "StateName", "StToNatl",
                 options = list(region = "US", displayMode = "regions",
                                resolution = "provinces",
                                colorAxis = "{values:[0,1,2],colors:['blue','white','red']}",
                                width = 650, height = 400)
                                
    )
    
  })
  
}

shinyApp(ui = ui, server = server)


 



