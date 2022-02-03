#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(lubridate)
#process data first
covid = read.csv("illinois_shield_covid_data.csv", header = TRUE)
begin = ymd(20200817)
days = c(begin)
for (i in 1:457) {
  days[i+1] = begin +i
}
covid$days = days

choose = names(covid)[3:(length(names(covid)) - 1)]

#ui page
ui = navbarPage(
  title = "Covid19",
  tabPanel(title = "App",
           titlePanel("Covid Data"),
           
           # Sidebar with a slider input for number of bins
           sidebarLayout(
             sidebarPanel(
               selectInput("Color",
                           "Color for data",
                           choices = c("red","blue","black")),
          
               selectInput("CaseType",
                           "Case or test type",
                           choices =  choose),
               dateInput(inputId = "start",
                         label = "start day (start by 2020-08-17)",
                         value = "2020-08-17"),
               
               dateInput(inputId = "end",
                         label = "end day (end by 2021-11-17)",
                         value = "2021-11-17"),
               selectInput("Break",
                           "break for date",
                           choices = c("by month" = "1 month",
                                       "by week" = "1 week",
                                       "by bay" = "1 day"))
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("case_plot"),
               
             )
           )),
  tabPanel(title = "Table", dataTableOutput("table")),
  tabPanel(title = "About", includeMarkdown("about.Rmd")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    covid2 = reactive({
      
      covid %>%
            filter(days >= input$start) %>%
            filter(days <= input$end)
        
    })
    
    output$table = renderDataTable(covid)
    
    output$case_plot <- renderPlot({
        ggplot(data = covid2(), mapping = aes(x = days, y = !!as.symbol(input$CaseType))) + 
            geom_point(color = input$Color) +
            labs(title = "Date vs Number of case or test",y = "Case or Test", x = "date") +
            geom_smooth() +
            scale_x_date(breaks = input$Break)
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

