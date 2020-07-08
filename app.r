library(shinydashboard)
library(shiny)
require(ggplot2)
library(plotly)



shinyUI(
    
    ui <-  dashboardPage( title = "Result Analysis", skin = "green",
                          
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
                                  menuSubItem("Result based on Previous Data",tabName = "test"),
                                  menuSubItem("Graphs",tabName="graph",icon = icon("bar-chart-o"))
                                  
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
                                      
                                     
                                      infoBoxOutput("net"),
                                      infoBoxOutput("prog"),
                                      infoBoxOutput("gate"),
                                      valueBoxOutput("vbox")
                                      
                                      
                                  ),
                                  
                                  tabItem(
                                      tabName = "graph",h3("Graphical"),
                                     plotOutput("graph"),
                                     plotOutput("graph2")
                                  )
                              )
                              
                              
                          )
    )
    
    
)



server <- function(input, output, session) {
    
    output$dat <- renderPrint({
        if(is.null(input$file)){
            print("Import CSV data file")
        } else {
            inFile <- input$file
            df <- read.csv(inFile$datapath)
           
            print(df)
            str(df)
            }
    })
    
    
    output$net <-  renderInfoBox({
        inFile <- input$file
        df <- read.csv(inFile$datapath)
        
        programming <- df %>% filter(Subjects=="IP" | Subjects=="ESFP-I"|Subjects=="OOP" | Subjects=="ESFP-II"| Subjects=="FP"| Subjects=="BOSS" | Subjects=="DBMS" | Subjects=="DS" | Subjects=="WT" | Subjects=="OS" | Subjects=="TC" )
        programming_avg_marks <- mean(programming$Marks)
        
        
        gate <-  df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM" |Subjects=="ASB" |Subjects=="AEM" |Subjects=="PNS" |Subjects=="OS")
        gate_avg_marks <- mean(gate$Marks)
        
        
        networking <- df %>% filter(Subjects=="DE"|Subjects=="BE" |Subjects=="CO" |Subjects=="BCS" |Subjects=="MNM"  )
        net_avg_marks <- mean(networking$Marks)
        
        infoBox("Networking",net_avg_marks,icon=icon("bar-chart"),color = "blue")
    })
    
    output$gate <-  renderInfoBox({
        inFile <- input$file
        df <- read.csv(inFile$datapath)
        
        programming <- df %>% filter(Subjects=="IP" | Subjects=="ESFP-I"|Subjects=="OOP" | Subjects=="ESFP-II"| Subjects=="FP"| Subjects=="BOSS" | Subjects=="DBMS" | Subjects=="DS" | Subjects=="WT" | Subjects=="OS" | Subjects=="TC" )
        programming_avg_marks <- mean(programming$Marks)
        
        
        gate <-  df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM" |Subjects=="ASB" |Subjects=="AEM" |Subjects=="PNS" |Subjects=="OS")
        gate_avg_marks <- mean(gate$Marks)
        
        
        networking <- df %>% filter(Subjects=="DE"|Subjects=="BE" |Subjects=="CO" |Subjects=="BCS" |Subjects=="MNM"  )
        net_avg_marks <- mean(networking$Marks)
        
        infoBox("Gate",gate_avg_marks,icon=icon("bar-chart"),color = "purple")
        
    })
    
    output$prog <-  renderInfoBox({
        inFile <- input$file
        df <- read.csv(inFile$datapath)
        
        programming <- df %>% filter(Subjects=="IP" | Subjects=="ESFP-I"|Subjects=="OOP" | Subjects=="ESFP-II"| Subjects=="FP"| Subjects=="BOSS" | Subjects=="DBMS" | Subjects=="DS" | Subjects=="WT" | Subjects=="OS" | Subjects=="TC" )
        programming_avg_marks <- round(mean(programming$Marks),4)
        
        
        gate <-  df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM" |Subjects=="ASB" |Subjects=="AEM" |Subjects=="PNS" |Subjects=="OS")
        gate_avg_marks <- mean(gate$Marks)
        
        
        networking <- df %>% filter(Subjects=="DE"|Subjects=="BE" |Subjects=="CO" |Subjects=="BCS" |Subjects=="MNM"  )
        net_avg_marks <- mean(networking$Marks)
        
        
        infoBox("Programming",programming_avg_marks,icon=icon("bar-chart"),color = "yellow")
     
    })
    
    output$vbox <- renderValueBox({
        inFile <- input$file
        df <- read.csv(inFile$datapath)
        
        programming <- df %>% filter(Subjects=="IP" | Subjects=="ESFP-I"|Subjects=="OOP" | Subjects=="ESFP-II"| Subjects=="FP"| Subjects=="BOSS" | Subjects=="DBMS" | Subjects=="DS" | Subjects=="WT" | Subjects=="OS" | Subjects=="TC" )
        programming_avg_marks <- round(mean(programming$Marks),4)
        
        gate <-  df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM" |Subjects=="ASB" |Subjects=="AEM" |Subjects=="PNS" |Subjects=="OS")
        gate_avg_marks <- mean(gate$Marks)
        
        
        networking <- df %>% filter(Subjects=="DE"|Subjects=="BE" |Subjects=="CO" |Subjects=="BCS" |Subjects=="MNM"  )
        net_avg_marks <- mean(networking$Marks)
        
        
        if(programming_avg_marks > gate_avg_marks && programming_avg_marks> net_avg_marks)
        {
            valueBox(programming_avg_marks,"Go For Programming",icon=icon("thumbs-up"),color = "green")
        } 
        else if(gate_avg_marks > programming_avg_marks && gate_avg_marks > net_avg_marks)
        {
            valueBox(gate_avg_marks,"Go For Gate",icon=icon("thumbs-up"),color = "green")
        } 
        else 
        {
            valueBox(programming_avg_marks,"Go For Networking",icon=icon("thumbs-up"),color = "green")
        }
        
    })
    
    output$graph <- renderPlot({
        inFile <- input$file
        df <- read.csv(inFile$datapath)
        
        programming <- df %>% filter(Subjects=="IP" | Subjects=="ESFP-I"|Subjects=="OOP" | Subjects=="ESFP-II"| Subjects=="FP"| Subjects=="BOSS" | Subjects=="DBMS" | Subjects=="DS" | Subjects=="WT" | Subjects=="OS" | Subjects=="TC" )
        programming_avg_marks <- round(mean(programming$Marks),4)
        
        gate <-  df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM" |Subjects=="ASB" |Subjects=="AEM" |Subjects=="PNS" |Subjects=="OS")
        gate_avg_marks <- mean(gate$Marks)
        
        
        networking <- df %>% filter(Subjects=="DE"|Subjects=="BE" |Subjects=="CO" |Subjects=="BCS" |Subjects=="MNM"  )
        net_avg_marks <- mean(networking$Marks)
        
        x <- data.frame(value <- c(programming_avg_marks,gate_avg_marks, net_avg_marks),labels <- c("Programming", "Gate", "Networking"))
        library(ggplot2)
        # Barplot
        bp<- ggplot(x, aes(x="", y=value, fill=labels))+
            geom_bar(width = 1, stat = "identity")
        bp
        
        pie <- bp + coord_polar("y", start=0) + ggtitle("Pie Chart of Analysis")
        pie
        pie + scale_fill_brewer(palette="Dark2")
        
        
    })
    
    output$graph2 <- renderPlot({
        inFile <- input$file
        df <- read.csv(inFile$datapath)
        
        programming <- df %>% filter(Subjects=="IP" | Subjects=="ESFP-I"|Subjects=="OOP" | Subjects=="ESFP-II"| Subjects=="FP"| Subjects=="BOSS" | Subjects=="DBMS" | Subjects=="DS" | Subjects=="WT" | Subjects=="OS" | Subjects=="TC" )
        programming_avg_marks <- round(mean(programming$Marks),4)
        
        gate <-  df %>% filter(Subjects=="Calculus" |Subjects=="DE"|Subjects=="BE" |Subjects=="MNM" |Subjects=="ASB" |Subjects=="AEM" |Subjects=="PNS" |Subjects=="OS")
        gate_avg_marks <- mean(gate$Marks)
        
        
        networking <- df %>% filter(Subjects=="DE"|Subjects=="BE" |Subjects=="CO" |Subjects=="BCS" |Subjects=="MNM"  )
        net_avg_marks <- mean(networking$Marks)
        
        x <- data.frame(value <- c(programming_avg_marks,gate_avg_marks, net_avg_marks),labels <- c("Programming", "Gate", "Networking"))
        library(ggplot2)
        ggplot(x,aes(y=labels,x=value)) + geom_boxplot() + ggtitle("Box Plot of Analysis")
        
    })
    
    
}
shinyApp(ui = ui, server = server)