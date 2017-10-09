function(input, output, session){
  
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