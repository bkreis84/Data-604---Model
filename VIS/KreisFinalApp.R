library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(stringr)


state_tot <- read.csv("https://raw.githubusercontent.com/bkreis84/Data-604---Model/master/VIS/state_tot.csv", stringsAsFactors = FALSE)
final <- read.csv("https://raw.githubusercontent.com/bkreis84/Data-604---Model/master/VIS/final.csv", stringsAsFactors = FALSE)
summSt <- data.frame(read.csv("https://raw.githubusercontent.com/bkreis84/Data-604---Model/master/VIS/summSt.csv", stringsAsFactors = FALSE))

final <- final %>%
  arrange(STATE)

summSt$STATE <- str_to_title(summSt$STATE)

ui <- dashboardPage(
  dashboardHeader(
    title = "IRS Tax Data"
  ),
  
  dashboardSidebar(
    sidebarMenu(id="sbmenu",
                menuItem("Project Info", tabName = "projectinfo", icon = icon("th")),
                menuItem("AGI Comparison", tabName = "agi", icon = icon("th")),
                menuItem("State Rankings", tabName = "rank", icon = icon("th")),
                menuItem("Map", tabName = "map", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "agi",
                      fluidRow(
                        box(
                          title = "Controls",
                          selectInput("st", "State", unique(final$STATE), selected="new york"),
                          selectInput("var2", "Variable of interest", choices = c('Unemployment $ per Return' = 'UNEMP_COMP_PR', 
                                                                                  '% of Returns with Business Income' = 'PERC_BUSINESS_RETURN', 
                                                                                  '% with Real Estate Deduction' = 'PERC_RE', 
                                                                                  'AGI Per Return' = 'AGI_PR'),
                                      selected = '% with Business Income'))
                      ),
                      box(
                        plotlyOutput('plot1')
                        
                      )
                      
              
              
              ),
      tabItem("projectinfo",
              fluidRow(
                box(
                h2("Final Project: Fall 2017"),
                h3("Brian Kreis - Data 608"),
                p("The IRS publishes data annually with numerous variables broken out by AGI level and zip code. This information is especially relevant now as large scale changes to the tax code appear to be on the horizon. This data set could be explored even further to determine the potential impact as the bill is finalized. Here we examine just 
                   a small portion of the interesting variables that are available from the years 2010 to 2015. These years were selected because the variables of interest were not available in prior years."),
                p("Visualizations allow you to compare states over time in four variables:"),
                tags$div(
                  tags$ul(
                    tags$li("Percentage of returns claiming business income"),
                    tags$li("Percentage of returns deducting real estate taxes"),
                    tags$li("Dollar amount per return of unemployment benefits"),
                    tags$li("AGI per return")
                    
                  )
                ),
                h4("AGI Comparison"),
                p("On the state comparison tab, you can see the differences within each state by AGI level, as it pertains to the above variables. This visualization uses plotly."),
                h4("State Ranking"),
                p("You can see select a metric and year of interest and see how each state ranks. Tooltips appear as you hover over each state. "),
                h4("Map"),
                p("Here you can access a map view of these metrics at the state level. You can adjust the slide bar to change the year and select the variable you wish to look at in the drop down. This visualization uses ggplot to construct and plot the visualization using latitude and longitude. In the future, I would likely prefer to use leaflet or plotly to add interactivity and tooltips."),
                h4("Interesting things to note"),
                tags$div(
                  tags$ul(
                    tags$li(" You can see the level of unemployment benefits per return decreasing substantially on the map view over the five year period. "),
                    tags$li("Real estate ownership in the Dakotas appears low as you may expect with the number of individuals who moved to the area for natural gas jobs. "),
                    tags$li("The highest percentage of returns with business income in New York were found in people making over 200k and those making under 25k.")
                  )),
                h4("Related to Project"),
                wellPanel(
                  helpText(a("Data Cleaning", href="http://rpubs.com/bkreis84/342356")),
                  helpText(a("Shiny Code", href="https://github.com/bkreis84/Data-604---Model/blob/master/VIS/KreisFinalApp.R"))
                ),
                h4("Sources"),
                wellPanel(
                  helpText(a("Link to IRS Data on Kaggle", href="https://www.kaggle.com/irs/individual-income-tax-statistics/data")),
                  helpText(a("State Conversion", href="https://favorableoutcomes.wordpress.com/2012/10/19/create-an-r-function-to-convert-state-codes-to-full-state-name/")),            
                  helpText(a("Mapping in ggplot", href="http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html")),
                  helpText(a("More Mapping in ggplot", href="https://shiny.rstudio.com/articles/google-analytics.html"))              
                  ),
                width = 8)
                )
              
              ),
        
      
      tabItem(tabName = "rank",
                fluidRow(
                  box(
                    title = "Controls",
                    selectInput("var3", "Variable of interest", choices = c('Unemployment $ per Return' = 'UNEMP_COMP_PR', 
                                                                            '% of Returns with Business Income' = 'PERC_BUSINESS_RETURN', 
                                                                            '% with Real Estate Deduction' = 'PERC_RE', 
                                                                            'AGI Per Return' = 'AGI_PR'),
                                selected = '% with Business Income'),

                    selectInput("yr2",
                                "Select Year:", choices = c(2010:2015),
                                selected = 2015)),
                
                box(
                  plotlyOutput('plot2')
                  
                )
        )
                
                
        ),
      tabItem(tabName = "map",
              fluidRow(
                box(
                  title = "Controls",
                  selectInput("var", 
                              label = "Select Variable:",
                              choices = c('Unemployment $ per Return' = 'UNEMP_COMP_PR', 
                                          '% of Returns with Business Income' = 'PERC_BUSINESS_RETURN', 
                                          '% with Real Estate Deduction' = 'PERC_RE', 
                                          'AGI Per Return' = 'AGI_PR'),
                              selected = '% with Business Income'),
                  
                  sliderInput("yr",
                              "Select Year:",
                              min = 2010,
                              max = 2015,
                              value = 2015)),
                
                box(
                  plotOutput("map")
                  
                )
              )
              
              
      )
    )
  )
)


server <- function(input, output) {
  
  select <- reactive({
    year_sel <- input$yr
    
  })  
  
  df <- reactive({
    state_tot %>%
      filter(YEAR == select())
  })
  
  high <- reactive({ 
    switch(input$var,
           "PERC_BUSINESS_RETURN" = "green",
           "AGI_PR" = "green",
           "PERC_RE" = "green",
           "UNEMP_COMP_PR" = "red")
  })
  
  low <- reactive({ 
    switch(input$var,
           "PERC_BUSINESS_RETURN" = "red",
           "AGI_PR" = "red",
           "PERC_RE" = "red",
           "UNEMP_COMP_PR" = "green")
  })
  
  
  
  
  df_rank <- reactive({
    new <- summSt %>%
      filter(YEAR == input$yr2) %>%
      arrange(STATE)
    # This next portion was required to try to sort by value.
    # https://stackoverflow.com/questions/40224892/r-plotly-barplot-sort-by-value
    new$STATE <- factor(new$STATE, levels = unique(new$STATE)[order(new[[input$var3]],
                                                                    decreasing = FALSE)])
    new
  })
  
  
  output$map <- renderPlot({
    fillvar <- df()[[input$var]]
    
    print(ggplot(df(), aes(long, lat)) +
            geom_polygon(aes(fill = fillvar,
                             group = group, height = 500), color = "white") +
            scale_fill_continuous(name = input$var, limits = c(min(state_tot[[input$var]]), max(state_tot[[input$var]])), 
                                  low = low(), high = high(), 
                                  guide = guide_colorbar(nbin=100, draw.ulim = FALSE, draw.llim = FALSE)) +
            
            
            theme_classic() + 
            theme (axis.line.x = element_blank(), 
                   axis.line.y = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text.x = element_blank(), 
                   axis.text.y = element_blank(),
                   legend.position="bottom",
                   legend.direction="horizontal",
                   legend.key.height = unit(0.3, "cm"),
                   legend.key.width  = unit(4.0, "cm"),
                   legend.title = element_blank()) +
            ylab("") + xlab("")) 
  })
  
  selectedData <- reactive({
    
    
    dfSlice <- final %>%
      filter(STATE == input$st) %>%
      arrange(desc(YEAR))
    
  })
  
  output$plot1 <- renderPlotly({
    
    #create the plot
    m <- list(l=75, r=75, b=40, t=60, pad=6)
    plot_ly(selectedData(), x = ~YEAR, y = selectedData()[[input$var2]], height = 500, 
            color = ~INCOME, type = 'scatter', mode ='lines+markers') %>%
      layout(title = paste(input$st, "Comparison by Income"), margin = m,
             xaxis = list(showgrid = F),
             yaxis = list(showgrid = F))
    
  })
  
  output$plot2 <- renderPlotly({
    m <- list(l=250, r=50, b=50, t=60, pad=4)
    plot_ly(df_rank(), x = df_rank()[[input$var3]], y = ~STATE, color = ~STATE, 
            type = 'bar', orientation = 'h', height = 900, width = 0.5) %>%
      layout(title = "State Rank", showlegend = FALSE, margin = m,
             yaxis = list(title = FALSE))
    
  })
  
}



shinyApp(ui, server)

