fluidPage(
  headerPanel('Ratio of State Mortality Rate to U.S. Average, by Year'),
  selectInput(inputId = 'icd', label = 'Select Condition',
                choices = unique(df$ICD.Chapter),
                selected = 'Certain infectious and parasitic diseases'
    ),
    sliderInput(inputId = 'year', label = 'Select Year',
                value = 1999, min = min(df$Year), max = max(df$Year),
                step = 1, sep = "", 
                animate = animationOptions(interval = 1500, loop = TRUE)
    ),
    
  htmlOutput('map')
)
  



