library(shiny)
library(dplyr)
library(tidyr)
library(shinythemes)
library(googleVis)
library(ggplot2)
library(grid)
library(lubridate)
library(ggthemes)

# read in data: manual method, assuming in working directory
#std_redux <- read.csv("std_redux.csv", stringsAsFactors = FALSE)
#tinder_monthly <- read.csv('tinder_monthly.csv', stringsAsFactors = FALSE)
#std_natl <- read.csv('std_natl.csv', stringsAsFactors = FALSE)
#pct_chg_df <- read.csv('std_pct_chg.csv', stringsAsFactors = FALSE)

# read in data: auto method from github
std_redux <- read.csv("https://raw.githubusercontent.com/spitakiss/Data608_Work/master/Final/std_redux.csv",
                      stringsAsFactors = FALSE)
tinder_monthly <- read.csv("https://raw.githubusercontent.com/spitakiss/Data608_Work/master/Final/tinder_monthly.csv",
                           stringsAsFactors = FALSE)
std_natl <- read.csv("https://raw.githubusercontent.com/spitakiss/Data608_Work/master/Final/std_natl.csv",
                     stringsAsFactors = FALSE)
pct_chg_df <- read.csv("https://raw.githubusercontent.com/spitakiss/Data608_Work/master/Final/std_pct_chg.csv",
                       stringsAsFactors = FALSE)


# disease values
diseases <- c('(All)',unique(std_redux$Disease)) 

# unique years
years <- unique(sort(std_redux$Year))

# unique gender
genders <- unique(std_redux$Gender)

# tinder start_date
tinder_start <- decimal_date(as.Date('2012-09-12'))

# outlier observations
outlier_states <- c("District of Columbia", "New York","Massachusetts","Utah",
                    "North Dakota","Vermont","Oklahoma")
outlier_abb <- c('DC',"MA","NY","ND","OK","UT","VT")

outliers_df <- pct_chg_df %>%
  filter(state %in% outlier_states) %>%
  transform(stat_abb = outlier_abb)

# set up linear model
mylm <- lm(pct_chg ~ popularity, data=pct_chg_df)
r_sq <- summary(mylm)$r.squared
alpha <- mylm$coefficients[1]
beta <- mylm$coefficients[2]
lm_eqn <- paste0("y = ",round(beta,4),"x + ",round(alpha,4))
r2_val <- paste0("R^2 == ", round(r_sq,4))


# Define UI ----
ui <- navbarPage("STDs", theme = shinytheme('darkly'),
  #theme = shinytheme("darkly"),
  #shinythemes::themeSelector(), 
  tabPanel("Overview",
    mainPanel(
      h1("Overview"),  
      HTML('<p>According to the <a href = "https://www.cdc.gov/media/releases/2017/p0926-std-prevention.html" target="_blank" >Centers for Disease Control and Prevention (CDC)</a>
            ,&nbsp; incidence rates for multiple sexually
            transmitted diseases (STDs) in the U.S. have increased dramatically in the last few years.</p>'),  
      p('Recent news reports have also confirmed this alarming phenomenon:'),  
      HTML('<ul>
              <li><a href="https://www.theatlantic.com/health/archive/2016/06/how-syphilis-came-roaring-back/488375/"
                   target = "_blank">How Syphilils Came Roaring Back</a>
              </li>
              <li><a href="https://www.huffingtonpost.com/derrick-y-mcdaniel/sex-and-seniors-stds-a-ne_b_9619778.html"
                   target = "_blank">Sex and Seniors - STDs A New Reality For The Elderly</a>
              </li>
              <li><a href="https://www.thesun.co.uk/news/2077213/tinder-could-be-behind-plague-of-stds-engulfing-millennials/"
                  target = "_blank">Tinder could be behind plague of STDs engulfing millennials</a>
              </li>
            </ul>'),
      p("In this app, we let you explore some of the geographic and demographic patterns associated with the recent
        rise in STD incidence rates."),
      p("We also ask that you dig deeper into suggestions by the media that online dating apps such as Tinder are 
        responsible for the rising rates. While this app will not allow you to establish a cause-and-effect relationship,
        you can still use the inluded plots to subjectively determine if a relationship exists."),
      p("Please click on the tabs above to explore."),
      p("Have fun!")
      
    )
    
   ),
  tabPanel("State STD Trends",
    titlePanel('STD Incidence Rates in the U.S.'),
    sidebarLayout(
    
      sidebarPanel(
      
        checkboxGroupInput("sel_gen","Gender",
                          choices = genders,
                          selected = genders, inline=TRUE),
        selectInput("sel_dis", "Disease", 
                    choices = diseases,
                    selected = "(All)"),
        sliderInput(inputId = 'sel_year', label = 'Year',
                    value = 2014, min = min(years), max = max(years),
                    step = 1, sep = "", 
                    animate = animationOptions(interval = 1500, loop = TRUE))
      ),
      
      mainPanel(
        htmlOutput('map'),
        br(),
        HTML("<p><i>Source: CDC</i></p>"),
        p("Colors are based on percentiles calculated over
          all years and sexes, but are specific to the specified disease:"),
        HTML("<ul>
             <li><em>Green</em> represents the 25th percentile.</li>
             <li><em>Yellow</em> represents the 50th percentile.</li>
             <li><em>Red</em> represents the 75th percentile.</li>
             </ul>"),
        p("Incidence rates are per 100,000 individuals and represent new reported cases per time period.")
      )
      ,position = 'right')
    
  ),
  tabPanel('National STD Trends + Tinder',
      titlePanel('U.S. STD Incidence Rates and Tinder Popularity'),
      sidebarLayout(
             
          sidebarPanel(
               
              selectInput("sel_dis_natl", "Disease",
                          choices = diseases,
                          selected = "(All)"),
              sliderInput(inputId = 'sel_year_natl', label = 'Year Range',
                          value = c(2011,2016), min = 1984, max = 2016,
                          step = 1, sep = "")
             ),
             
             mainPanel(
               
               plotOutput('tinder'),
               br(),
               HTML("<p><i>Source: &nbsp; CDC and Google Trends</i></p>"),
               HTML("<p><em>Popularity</em> refers to web search interest relative to peak 
                    interest over the specified time interval.<br> A value of 50 should be interpreted as
                    half as popular as a value of 100.</p>"),
               p("Incidence rates are per 100,000 individuals and represent new reported cases per time period.")
               
             )
             ,position='right'
          )
  ),
  tabPanel("A Regression with Tinder",
    titlePanel('Test Linear Relationship: Tinder and STD'),
      sidebarLayout(
             
        sidebarPanel(
               
          checkboxGroupInput("lm_check", "Chart Options",
                             choices = list("Show Regression Line" = 1, "Label Outliers" = 2))
               
        ),
             
        mainPanel(
          plotOutput('regress'),
          br(),
          HTML("<p><i>Source: &nbsp; CDC and Google Trends</i></p>"),
          HTML('<p><em>Popularity</em> values in this chart are calculated per state.<br>
                A value of 100 refers to the location with the highest percentage of total "tinder"
               searches.<br>A value of 50 is half as popular as the most popular location.<br>
               Popularity was defined over the period from 9/12/12 (Tinder "go-live") through 12/31/14.'), 
          HTML("<p><em>Percent Change</em> refers to the relative change in  each state's total STD incidence rate from
             2011 (pre-Tinder) to 2014 (the most recent state-level data available).<p>")


          
          
        ),
        position='right'
      )         
           
  )
 )


# Define server logic ----
server <- function(input, output) {
  df <- reactive({
    std_redux %>%
      filter(Gender %in% input$sel_gen,
             Year == input$sel_year,
             input$sel_dis == "(All)" | Disease == input$sel_dis
             ) %>%
      group_by(State) %>%
      summarise(Rate = round(sum(Count) / sum(unique(Population)) * 100000,2))
  })
  
  # df without year, Gender filter
  df_mod <- reactive({
    std_redux %>%
      filter(input$sel_dis == "(All)" | Disease == input$sel_dis
      ) %>%
      group_by(State) %>%
      summarise(Rate = round(sum(Count) / sum(unique(Population)) * 100000,2))
  })  
  
  # set percentiles for filter plot
  myvals <- reactive(quantile(df_mod()$Rate, probs = c(0.25,0.5,0.75)))
  output$map <- renderGvis({
    
    gvisGeoChart(df(), "State", "Rate",
                 options = list(region = "US", displayMode = "regions",
                                resolution = "provinces",
                                colorAxis = paste0("{values:[",myvals()[1],",",myvals()[2],",",myvals()[3],
                                                   "],colors:['green','yellow','red']}"),
                                width = 650, height = 400,
                                gvis.editor = 'Edit Chart Type',
                                title = 'US STD Incidence Rates by State')
    )
  })
  
  std_natl_mod <- reactive({
    std_natl %>%
      filter(Year >= input$sel_year_natl[1],
             Year <= input$sel_year_natl[2],
             Disease == input$sel_dis_natl)
  })
  
  # date beg parameter 
  dt_beg_react <- reactive({input$sel_year_natl[1]})
  
  
  output$tinder <- renderPlot({
    p1 <- ggplot(std_natl_mod(), aes(Year,Rate)) + geom_line(color="blue") + theme_bw() +
      geom_vline(data = data.frame(dt = tinder_start), aes(xintercept = dt),
                 linetype=2) + scale_y_continuous(limits = c(0, NA)) + 
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(), 
            axis.ticks.x=element_blank()) + 
      labs(y= "Incidence Rate", title = 'STD Rates and Tinder Popularity vs. Time' )
    
    p2 <- ggplot(tinder_monthly,aes(date,hits)) + geom_line(color="blue") + theme_bw() + 
      coord_cartesian(xlim=c(input$sel_year_natl[1],input$sel_year_natl[2])) + 
      geom_vline(data = data.frame(dt = tinder_start), aes(xintercept = dt),
                 linetype=2) + annotate("text", x= tinder_start, y=80, hjust=1.2, label = 'Tinder "go-live"') + 
      labs(x="year", y="popularity")
    
    grid.newpage()
    grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
  })
  
  output$regress <- renderPlot({
    r1 <- ggplot(pct_chg_df, aes(popularity,pct_chg)) + geom_point() + theme_bw() + 
      labs(y = "% change", title = "Percent Change STD Incidence Rate vs. Tinder Popularity",
           subtitle= "Comparing 2014 vs 2011 at the State Level")
    
    if (1 %in% input$lm_check) {
      r1 <- r1 + geom_smooth(method="lm",se=FALSE) + 
        annotate("text",x=48, y= 0.375,label=lm_eqn ) +
        annotate("text", x=48, y = 0.32, label=r2_val, parse=TRUE)
    }
    
    if(2 %in% input$lm_check) {
      r1 <- r1 + geom_point(data=outliers_df, aes(popularity,pct_chg), color='red') + 
        geom_text(data = outliers_df, aes(popularity, pct_chg,label=stat_abb),vjust=-0.4, size=5, color='red')
    }
    
    r1
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
