###### QUESTION 1 ####################################################

# As a researcher, you frequently compare mortality rates from
# particular causes across different States. You need a visualization
# that will let you see (for 2010 only) the crude mortality rate,
# across all States, from one cause (for example, Neoplasms, which
# are effectively cancers). Create a visualization that allows you to
# rank States by crude mortality for each cause of death.

######################################################################

### libraries ###
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



### Shiny Scripts ###
ui <- fluidPage(
  headerPanel('2010 U.S. Mortality Rates by State'),
  selectInput(inputId = 'icd', label = 'Select Condition',
              choices = unique(df$ICD.Chapter),
              selected = 'Certain infectious and parasitic diseases'
              ),
  htmlOutput('map')
  
)



server <- function(input, output){
  
  # filter data for Year 2010 only, selct st
  dfMod <- reactive({  
    df %>%
      filter(Year == 2010, ICD.Chapter == input$icd ) %>%
      select(StateName, Crude.Rate)
  })  
  
  output$map <- renderGvis({
  
  gvisGeoChart(dfMod(), "StateName", "Crude.Rate",
               options = list(region = "US", displayMode = "regions",
                              resolution = "provinces",
                              colorAxis = "{colors:['red']}",
                              width = 650, height = 400,
                              gvis.editor = 'Edit Chart Type',
                              title = '2010 US Mortality Rates by State')
               )
    
  })
  
}

shinyApp(ui = ui, server = server)
