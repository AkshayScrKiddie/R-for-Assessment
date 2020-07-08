library(shinydashboard)
  library(shiny)
require(ggplot2)
library(plotly)
require(dplyr)



shinyUI(
  
  dashboardPage( title = "Result Analysis", skin = "green",
    
    dashboardHeader(title = "Stream Prediction",
        
            dropdownMenu(type = "message",
                         messageItem(from = "University",message = "Deadlines are near"),
                         messageItem(from="Jobs",message = "You have 0 invitations",icon=icon("bar-chart"),time="22:00"),
                         messageItem(from="Projects",message = "No Projects yet!!",icon = icon("handshake-o"))
                        ),
            dropdownMenu(type="notifications",
                         notificationItem(
                           text="2 new tabs added to the dashboard",
                           icon = icon("dashboard"),
                           status="success"
                         ),
                         notificationItem(text="server is currently running at 95% load",
                                          icon = icon("warning"),
                                          status = "warning"
                                          )  
                         ),
            
            dropdownMenu(type="tasks",
                         taskItem(
                         value=80,
                         color = "red",
                         "Dashboard Tasks "
                         ),
                         
                         taskItem(
                           value = 60,
                           color = "blue",
                           "Health Status"
                         )
                         
                         )
            
                      ),

    dashboardSidebar(
      br(),
      sidebarMenu(
      sidebarSearchForm("Search text","buttonSearch","Search"),
         # add tabname of respective tab
        
      menuItem("Please Upload Dataset",badgeLabel = "new",badgeColor = "yellow",icon=icon("file-word"),tabName = "pracs"),
              menuSubItem("Result based on Previous Data",tabName = "test")
      
    )),
    dashboardBody(
      # tabitems is for making a page for each menuitems
      tabItems(
    
        tabItem( 
        tabName = "pracs",h1("Upload "),
        fileInput(inputId = "file",
                  label = "Choose a file",
                  accept = c(".csv")
        ),
        verbatimTextOutput("dat")
        
        #TukeyHSD("result of anova")
        
      ),
      
      tabItem(
        tabName = "test",h2("Result"),
        
        verbatimTextOutput("PredictionOutput")
        
        
      )
 )
      
      
    )
  )
  
  
)


server <- function(input, output, session) {
  output$Data <- renderPrint({
    if(is.null(input$file)){
      print("Import CSV data file")
    } else {
      inFile <- input$file
      df <- read.csv(inFile$datapath)
      print(df)
      str(df)
      programming <- df %>% filter(Subjects=="IP" | Subjects=="ESFP-I"|Subjects=="OOP" | Subjects=="ESFP-II"| Subjects=="FP"| Subjects=="BOSS" | Subjects=="DBMS" | Subjects=="DS" | Subjects=="WT" | Subjects=="OS" | Subjects=="TC" )
      programming_avg_marks <- mean(programming$Marks)
      print("Average of programming subject is ")
      print(programming_avg_marks )
      
      
      gate <-  df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM" |Subjects=="ASB" |Subjects=="AEM" |Subjects=="PNS" |Subjects=="OS")
      gate_avg_marks <- mean(gate$Marks)
      print("Average of Gate subject is ")
      print(gate_avg_marks )
      
      
      networking <- df %>% filter(Subjects=="DE"|Subjects=="BE" |Subjects=="CO" |Subjects=="BCS" |Subjects=="MNM"  )
      net_avg_marks <- mean(networking$Marks)
      print("Average of networking subject is ")
      print(net_avg_marks )
      
      
      if(programming_avg_marks > gate_avg_marks && programming_avg_marks> net_avg_marks)
      {
        print("Your programming subjects are good.So,you Should opt for programming language")
      } 
      else if(gate_avg_marks > programming_avg_marks && gate_avg_marks > net_avg_marks)
      {
        print("Your Gate subjects are good. So,you should opt for Gate")
      } 
      else 
      {
        print("Your networking subjects are good.So, you should opt for network related area")
      }
      
      
    }
  })
  
}
shinyApp(ui = ui, server = server)





