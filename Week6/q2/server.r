### server function ###  

function(input, output , session){
  
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
    
    
    # plot
    gvisGeoChart(dfRatio, "StateName", "StToNatl",
                 options = list(region = "US", displayMode = "regions",
                                resolution = "provinces",
                                colorAxis = "{values:[0,1,2],colors:['blue','white','red']}",
                                width = 650, height = 400)
    )
    
  })
  
}


