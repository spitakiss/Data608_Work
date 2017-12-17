library(shiny)
library(dplyr)
library(tidyr)
library(shinythemes)
library(googleVis)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(plotly)


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

# clean up percentage formatting
pct_chg_df$pct_chg <- pct_chg_df$pct_chg * 100

# rename tinder_monthly df colnames
tinder_monthly <- tinder_monthly %>%
  rename(year = date, popularity = hits)

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


# create dataframe of outlier states, i.e. those with significant error relative to OLS line
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
r2_val <- paste0("R<sup>2</sup> = ", round(r_sq,4))

# natl rates of change in std incidence rates
pct_chg_natl_df <- std_natl %>% 
  mutate(post_tinder = ifelse(Year < 2013, 'n','y'),
         pct_chg = ifelse(lag(Disease) == Disease, round((Rate / lag(Rate) -1)*100,1), NA))

# Define UI ----
ui <- navbarPage("STDs", theme = shinytheme('darkly'),
  
  # Introduction tab 
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
        you can use the included plots to subjectively determine if a relationship exists."),
      p("Please click on the tabs above to explore."),
      p("Have fun!")
      
    )
    
   ),
  # Map of US tab
  tabPanel("State STD View",
    titlePanel('STD Incidence Rates in the U.S. (1984-2014)'),
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
        HTML('<p>Aggregate incidence rates are increasing over time; however, the trends are more nuanced when</br>
              viewed at the gender, disease, and state levels. Go see for yourself!</p>'),
        htmlOutput('map'),
        br(),
        HTML("<p><i>Source: CDC</i></p>"),
        p("Colors are based on percentiles calculated over
          all years and sexes, but are specific to the indicated disease:"),
        HTML("<ul>
             <li><em>Green</em> represents the 25th percentile.</li>
             <li><em>Yellow</em> represents the 50th percentile.</li>
             <li><em>Red</em> represents the 75th percentile.</li>
             </ul>"),
        p("Incidence rates are per 100,000 individuals and represent newly reported cases in the specified time period.")
      )
      ,position = 'right')
    
  ),
  # Regression tab
  tabPanel("State-Level Regression",
           titlePanel("State-Level Tinder Popularity and STDs"),
           sidebarLayout(
             
             sidebarPanel(
               
               checkboxGroupInput("lm_check", "Chart Options",
                                  choices = list("Show Regression Line" = 1, "Label Outliers" = 2))
               
             ),
             
             mainPanel(
               HTML('<h4>Question:</h4><h5>Can we accurately explain changes in STD rates by state using
                    Tinder data and simple linear regression?</h5>'),
               plotlyOutput('regress', width = "75%"),
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
           
  ),
  # National visualization tab
  tabPanel('National STD + Tinder View',
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
               HTML('<p>Using the provided controls, experiment with different time ranges to get a feel for both short and long term trends.</p>'),
               plotlyOutput('tinder', width = '75%'),
               br(),
               HTML("<p><i>Source: &nbsp; CDC and Google Trends</i></p>"),
               HTML("<p><em>Popularity</em> refers to web search interest relative to peak 
                    interest over the specified time interval.<br> A value of 50 should be interpreted as
                    half as popular as a value of 100.</p>"),
               p("Incidence rates are per 100,000 individuals and represent newly reported cases in the specified time period.")
               
             )
             ,position='right'
          )
  ),
  # Boxplot tab, national comparison
  tabPanel("National Before and After",
           titlePanel("Annual STD Rates of Change, Pre and Post Tinder"),
           sidebarLayout(
             
             sidebarPanel( 
               selectInput("disease_roc", "Disease",
                           choices = diseases[c(1,3:5)],
                           selected = "(All)"),
               radioButtons("radio_time", "Select pre-Tinder Time Period:",
                            choices = list("Four Years Before Tinder Launch"=2009, "All pre-Tinder Years (1984-2012)"=1984),
                            selected = 1984)
               
               
               
               
             ),
             
             mainPanel(
              HTML('<h4>Question:</h4><h5>Do the percentage increases in STD rates appear to be different
                   in the Post-Tinder era?</h5>'),
              HTML("<p>Experiment with different pre-Tinder time periods.</p>"),
              
               plotlyOutput('natl_roc', width = '70%'),
               br(),
               HTML("<p><i>Source: &nbsp; CDC and Google Trends</i></p>"),
               HTML("<p><i>Note:</i>&nbsp; Chancroid is not included in the drop-down box
                    choices because incidence rates rounded to zero</br> for many years in the
                    data set, resulting in divide-by-zero errors when calculating rates of change.</p>"),
               HTML("<p><i>Pre Tinder</i> refers to the time period 1984
                    through the end of 2012.</p>"),
              HTML("<p><i>Post Tinder</i> refers to four years of complete data (2013-2016) following Tinder's
                  introduction.</p>")
               
               
               
               
               
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
  # reactive national data df,changes according to year and disease inputs
  std_natl_mod <- reactive({
    std_natl %>%
      filter(Year >= input$sel_year_natl[1],
             Year <= input$sel_year_natl[2],
             Disease == input$sel_dis_natl)
  })
  
  # date beg parameter 
  dt_beg_react <- reactive({input$sel_year_natl[1]})
  
  # plot national std incidence rates and tinder popularity
  output$tinder <- renderPlotly({
    p1 <- ggplot(std_natl_mod(), aes(Year,Rate)) + geom_line(color="blue") + theme_bw() +
      geom_vline(data = data.frame(dt = tinder_start), aes(xintercept = dt),
                 linetype=2) + scale_y_continuous(limits = c(0, NA)) + 
      theme(axis.title.x=element_blank(),axis.text.x=element_blank(), 
            axis.ticks.x=element_blank()) + 
      labs(x = "Year",y= "Incidence Rate", title = 'STD Rates and Tinder Popularity vs. Time' )
    
    
    p2 <- ggplot(tinder_monthly,aes(year,popularity)) + 
      geom_line(color="blue") +
      theme_bw() + coord_cartesian(xlim=c(input$sel_year_natl[1],input$sel_year_natl[2])) + 
      geom_vline(data = data.frame(dt = tinder_start), aes(xintercept = dt),
                                            linetype=2) +
      annotate("text", x= tinder_start - 0.19 * (tinder_start - min(std_natl_mod()$Year)),
                                        y=80, size=3, label = 'Tinder live') + 
      labs(x="Year", y="Popularity")
    
    
    subplot(p1,p2, nrows = 2, titleY = TRUE, titleX = TRUE) %>%
      layout(margin = list(l = 50, t = 50))
    
  })
  
  # regression plot
  output$regress <- renderPlotly({
    
    if(is.null(input$lm_check)){
    
      plot_ly() %>%
        add_trace(data = pct_chg_df, x = ~popularity, y = ~pct_chg, hoverinfo = 'text',
                  text = ~paste("state:",state,"</br></br> popularity:",popularity,"</br> percent change:",pct_chg),
                  type = 'scatter',marker = list(size = 10, color = 'blue',
                                                 line = list(color = 'rgba(105, 105, 105, .8)',
                                                            width = 1)), name='state data') %>%
        layout(title = 'Percentage Change in STD Rates (2014 vs. 2011) </br></br> Against Tinder Popularity',
               yaxis = list(zeroline = FALSE, title = "Percent Change"),
               xaxis = list(zeroline = FALSE, title = "Popularity"),
               showlegend = FALSE,
               margin = list(t = 50))
      
    }
    
    else if (input$lm_check == "1" & length(input$lm_check) == 1) {
      plot_ly() %>%
        add_trace(data = pct_chg_df, x = ~popularity, y = ~pct_chg, hoverinfo = 'text',
                  text = ~paste("state:",state,"</br></br> popularity:",popularity,"</br> percent change:",pct_chg),
                  type = 'scatter',marker = list(size = 10, color = 'blue',
                                                 line = list(color = 'rgba(105, 105, 105, .8)',
                                                             width = 1)), name='state data') %>%
        add_trace(data = pct_chg_df, x = ~popularity, y = fitted(mylm), mode = "lines", name = "OLS Line") %>%
        layout(title = 'Percentage Change in STD Rates (2014 vs. 2011) </br></br> Against Tinder Popularity',
               yaxis = list(zeroline = FALSE, title = "Percent Change"),
               xaxis = list(zeroline = FALSE, title = "Popularity"),
               showlegend = FALSE,
               annotations = list(
                 x = 50,
                 y = 40,
                 text = paste0(lm_eqn,"\n",r2_val),
                 showarrow=FALSE),
                 margin = list(t = 50)
               )
      
    } 
    
    else if (input$lm_check == c("1","2")) {
      plot_ly() %>%
        add_trace(data = pct_chg_df, x = ~popularity, y = ~pct_chg, hoverinfo = 'text',
                  text = ~paste("state:",state,"</br></br> popularity:",popularity,"</br> percent change:",pct_chg),
                  type = 'scatter',marker = list(size = 10, color = 'blue',
                                                 line = list(color = 'rgba(105, 105, 105, .8)',
                                                             width = 1)), name='state data') %>%
        add_trace(data = pct_chg_df, x = ~popularity, y = fitted(mylm), mode = "lines", name = "OLS Line") %>%
        add_trace(data = outliers_df, x = ~popularity, y = ~pct_chg,text = ~stat_abb, mode = "text", 
                 textposition = "top right",  textfont = list(color = 'red', size = 14), name="outlier") %>%
        add_trace(data = outliers_df, x = ~popularity, y = ~pct_chg, hoverinfo = 'text',
                  text = ~paste("state:",state,"</br></br> popularity:",popularity,"</br> percent change:",pct_chg),
                  marker = list(size = 10, color = 'red',
                                line = list(color = 'rgba(105, 105, 105, .8)',
                                            width = 1), mode = "markers"), name="state abb") %>%
        layout(title = 'Percentage Change in STD Rates (2014 vs. 2011) </br></br> Against Tinder Popularity',
               yaxis = list(zeroline = FALSE, title = "Percent Change"),
               xaxis = list(zeroline = FALSE, title = "Popularity"),
               showlegend = FALSE,
               annotations = list(
                 x = 50,
                 y = 40,
                 text = paste0(lm_eqn,"\n",r2_val),
                 showarrow=FALSE),
                 margin = list(t = 50)
               )
    }
    
    else if (input$lm_check == "2") {
      plot_ly() %>%
        add_trace(data = pct_chg_df, x = ~popularity, y = ~pct_chg, hoverinfo = 'text',
                  text = ~paste("state:",state,"</br></br> popularity:",popularity,"</br> percent change:",pct_chg),
                  type = 'scatter',marker = list(size = 10, color = 'blue',
                                                 line = list(color = 'rgba(105, 105, 105, .8)',
                                                             width = 1)), name='state data') %>%
        add_trace(data = outliers_df, x = ~popularity, y = ~pct_chg,text = ~stat_abb, mode = "text", 
                  textposition = "top right",  textfont = list(color = 'red', size = 14), name="outlier") %>%
        add_trace(data = outliers_df, x = ~popularity, y = ~pct_chg, hoverinfo = 'text',
                  text = ~paste("state:",state,"</br></br> popularity:",popularity,"</br> percent change:",pct_chg),
                  marker = list(size = 10, color = 'red',
                                line = list(color = 'rgba(105, 105, 105, .8)',
                                            width = 1), mode = "markers"), name="state abb") %>%
        layout(title = 'Percentage Change in STD Rates (2014 vs. 2011) </br></br> Against Tinder Popularity',
               yaxis = list(zeroline = FALSE, title = "Percent Change"),
               xaxis = list(zeroline = FALSE, title = "Popularity"),
               showlegend = FALSE,
               margin = list(t = 50))
        
        
        
        
    }
    
  
      
    
  })
  
  # reactive annual % chg in std rates, reponsds to disease and year inputs
  pct_chg_natl_roc <- reactive ({
    pct_chg_natl_df %>%
      filter(Disease == input$disease_roc,
             Year >= input$radio_time)
  })
  
  
 
  # make boxplots
  output$natl_roc <- renderPlotly({
    plot_ly(type = 'box') %>% 
      add_boxplot(y = pct_chg_natl_roc()[pct_chg_natl_roc()$post_tinder == 'n',]$pct_chg, jitter = 0.3, boxpoints = 'all',
                  marker = list(color = 'rgb(7,40,89)'),
                  line = list(color = 'rgb(7,40,89)'),
                  name = "pre Tinder") %>% 
      add_boxplot(y = pct_chg_natl_roc()[pct_chg_natl_roc()$post_tinder == 'y',]$pct_chg, jitter = 0.3, boxpoints = 'all',
                  marker = list(color = 'red'),
                  line = list(color = 'red'),
                  name = "post Tinder") %>% 
      layout(showlegend = FALSE,
             title="Annual Percentage Change in STD Incidence Rates",
             yaxis = list(title = "Percent Change"),
             margin = list(t = 50)
             )
    
  
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
