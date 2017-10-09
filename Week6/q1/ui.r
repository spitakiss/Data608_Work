fluidPage(
  headerPanel('2010 U.S. Mortality Rates by State'),
  selectInput(inputId = 'icd', label = 'Select Condition',
              choices = unique(df$ICD.Chapter),
              selected = 'Certain infectious and parasitic diseases'
  ),
  htmlOutput('map')
  
)