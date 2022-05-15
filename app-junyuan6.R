#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(lubridate)
library(readr)
library(jsonlite)


nba <- fromJSON("https://uofi.box.com/shared/static/grgt4wkiumfjgpug3fsx2vfkipv5rxz1.json")

nba = nba %>% 
  mutate(`3 point rate` = round(`3P` / `3PA`, 2),
         `2 point rate` = round(`2P` / `2PA`, 2),
         `free throw rate` = round(FT / FTA, 2),
         `Field goal rate` = round(FG / FGA, 2),
         `Points per game` = round(PTS / 82, 2)) %>% 
  drop_na()
ret = write.csv(nba, file = "nba")
  

ui = navbarPage(
    title = "NBA data",
    tabPanel(title = "App",
             titlePanel("NBA Data"),
             
             sidebarLayout(
                 sidebarPanel(
                     selectInput("Team",
                                 "Team to choose",
                                 choices = unique(nba$Tm)),
                     selectInput("Score_type",
                                 "Score type:",
                                 choices = c("Field goal rate","3 point rate",
                                             "2 point rate", "free throw rate",
                                             "Points per game"),
                                 multiple = F),
                     selectInput("Position",
                                 "Position: ",
                                 choices = unique(nba$Pos)
                     )
                 ),
                 
                 mainPanel(
                     plotOutput("plot"),
                     tableOutput("summary_table"),
                     tableOutput("Best")
                     
                 )
             )),
    tabPanel(title = "Table", dataTableOutput("table"))
)



server <- function(input, output) {
    nba_react = reactive( {
        nba %>%
            filter(Tm == input$Team) %>%
            filter(Pos == input$Position)
    })
    
    nba_react2 = reactive( {
      nba %>%
        filter(Tm == input$Team)
    })
    
    output$table = renderDataTable(nba)
    
    output$plot <- renderPlot({
        
        
        ggplot(data = nba_react(),
               aes(x = PlayerName, y = !!as.symbol(input$Score_type), fill = PlayerName)) +
          geom_bar(stat='identity') +
        theme(text = element_text(size = 20)) +
        geom_text( aes(label = !!as.symbol(input$Score_type), y = !!as.symbol(input$Score_type)),size = 12, position = position_stack(vjust = 0.5))
        
    })
    
    output$summary_table = renderTable(
      data_frame("Average Field goal" = mean(nba_react2()$`Points per game`),
                 "Average 2 point rate" = mean(nba_react2()$`2 point rate`),
                 "Average 3 point rate" = mean(nba_react2()$`3 point rate`),
                 "Average field goal rate" = mean(nba_react2()$`Field goal rate`),
                 "Average free throw rate" = mean(nba_react2()$`free throw rate`)
                 )
    )
    
    output$Best = renderTable(
      nba_react2()[which.max(nba_react2()$`Points per game`), ] %>% 
        select(c("PlayerName","Points per game", "Field goal rate","3 point rate", "2 point rate", "free throw rate")) %>% 
        rename(`Best Scorer among team` = PlayerName)
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

 