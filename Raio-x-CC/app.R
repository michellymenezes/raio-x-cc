library(shinydashboard)
library(shinyjs)
source("processa_dados.R")

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Raio-x CC"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(id = "menu",
                menuItem("Tab1", tabName = "tab1", icon = icon("bookmark")),
                menuItem("Tab2", tabName = "tab2", icon = icon("bookmark")),
                sliderInput("bins",
                            "Number of bins:",
                            min = 1980,
                            max = 2017,
                            value = c(10,30),
                            step = 0.5)
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "tab1",
              fluidRow(
                plotOutput("distPlot")
              )
      ),
      
      tabItem(tabName = "tab2",
              fluidRow(
                
              )
      
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)