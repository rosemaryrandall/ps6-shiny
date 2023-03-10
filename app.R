library(shiny)
library(tidyverse)

bikes <- read_delim("SeoulBikeData3.csv", locale=locale(encoding="latin1"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Analysing Seoul Bike Rental Data"),
  p("For INFO201's PS6 Assignment, by Rosemary Randall"),
  tabsetPanel(type = "tabs",
              tabPanel("Home",
                       h2("About the Data"),
                       p("I accessed this data from the University of California at Irvine's online Machine
                       Learning Repository, which serves to provide datasets for use by data analysis and machine learning.
                         It concerns the number of bikes rented from January 1st of 2017 through November 30 of 2018, 
                         documenting the weather conditions, seasonal days of interest such as holidays, and date information.
                         As such, for this project I am examining the impact of precitation on bike rental rates, as seen by both graphical and 
                         table presentations. In the", strong("Plot"), "you'll find a graph showing the trend between precipitation in the form 
                         of rainfall (in millimeters) and number of bikes rented. In the", strong("Table"), "tab, you'll find a sample of the data 
                         from which you can choose the season of the sample, presenting a more specific view of the data.")),
              tabPanel("Plot", 
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("precipSlider", label = h3("Select Precipitation (mm) Range"), min = 0, 
                                       max = 35, value = c(0, 15)),
                           selectInput("selectColor", label = h3("Select Point Color"), 
                                       choices = list("Black" = "black", "Blue" = "blue", "Green" = "green", "Red" = "red"), 
                                       selected = "Black"),
                         ),   
                         mainPanel(plotOutput("precip_plot"),
                                   p("This graphical display shows us that there is a distinct negative correlation between bike rental
                                     rates and precipitation. This makes sense, as those who choose to bike not out of necessity
                                     would likely opt for other modes of transportation if it were raining, which is significantly less
                                     pleasant in the rain. Of course this isn't universally true, as some people likely rely on 
                                     bikes to get to places around the city like their place of work. The average number of bikes rented
                                     for this range of precipitation is:"),
                                   strong(textOutput("avPrecip"))),
                       )),
              tabPanel("Table",
                       sidebarLayout(
                         sidebarPanel(
                           checkboxGroupInput("checkSeason", label = h3("Select Season(s) of Sample"),
                                              choices = list("Spring" = "Spring", "Summer" = "Summer", "Autumn" = "Autumn", "Winter" = "Winter"), 
                                        selected = "Spring"),
                         ),
                         mainPanel(tableOutput("sampleTable"),
                                   p("This table shows a random sample of 10 days between January 1st of 2017 and November 30th of 2018 - 
                                     the entire dataset. This is to serve the purpose of orienting the user with the data they're seeing
                                     in the", strong("Plot"), "tab, showing the user what kind of other information about
                                     environmental conditions and bike rentals are being collected within Seoul during the surveyed time period.
                                     The average number of bikes rented in this random sample is:"),
                                   strong(textOutput("meanBikes"))),
                       ))
  )
)


# Define server logic 
server <- function(input, output) {
  
  #define as reactive for plot
  bikesInput <- reactive({
    bikes %>% 
      filter(rainfall %in% input$precipSlider)
  })
  
  #make reactive text for plot
  output$avPrecip <- renderText({
    print(mean(bikesInput()$num_bikes_rented))
  })
  
  #define as reactive for table
  bikesTInput <- reactive({
    bikes %>% 
      filter(seasons %in% input$checkSeason)
  })
  
  #make reactive text for table
  output$meanBikes <- renderText({
    print(mean(bikesTInput()$num_bikes_rented))
  })
  
  #access range from slider on plot page
  output$range <- renderPrint({ input$precipSlider })
  
  #access dot color changer for plot
  output$value <- renderPrint({ input$selectColor })
  
  #make plot
  output$precip_plot <- renderPlot({
    bikes %>% 
      select(rainfall,num_bikes_rented) %>% 
      filter(rainfall>=input$precipSlider[1], rainfall<=input$precipSlider[2]) %>% 
      group_by(rainfall) %>% 
      summarize(avg_bikes=mean(num_bikes_rented)) %>% 
      ggplot(aes(rainfall, avg_bikes))+
      geom_point(col = input$selectColor)+
      geom_smooth()+
      labs(x = "Precipitation (mm) per day", y = "Number of Bikes Rented per day",
           title = "Analyzing Impact of Precipitation on Bike Rental Rates in Seoul")
  })
  
  #access value from radio buttons on table page
  output$value <- renderPrint({ input$checkSeason })
  
  #make sample table
  output$sampleTable <- renderTable({
    bikes %>% 
      filter(seasons %in% input$checkSeason) %>% 
      sample_n(10)

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

