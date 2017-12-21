library(shinydashboard)
library(shinyjs)
source("processa_dados.R")
library(readr)
library(plotly)


matriculas = get_matriculas()
diciplinas = get_disciplinas()
matriculas_nome = matriculas %>% 
  left_join(disciplinas %>% select(DIS_DISCIPLINA, DIS_DESCRICAO), by = c("MAT_TUR_DIS_DISCIPLINA" = "DIS_DISCIPLINA"))
values = list(PERIODO_MAT = (matriculas %>% select(PERIODO_MAT) %>% unique() %>% arrange(PERIODO_MAT))$PERIODO_MAT)

ui <- dashboardPage(
  dashboardHeader(title = "Raio-x CC"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(id = "menu",
                menuItem("Tab1", tabName = "tab1", icon = icon("bookmark")),
                menuItem("Tab2", tabName = "tab2", icon = icon("bookmark"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "tab1",
              fluidRow(
                column(width = 4,
                       box(width = NULL, selectInput('tab1_selectDisciplina', 'Estados', 
                                                     choices = matriculas_nome %>%
                                                       select(DIS_DESCRICAO) %>% distinct() %>% na.omit() %>% arrange(DIS_DESCRICAO), 
                                                     multiple=F, selectize=TRUE),
                           radioButtons(inputId = "tab1_tipoTurma", label = "Modo de exibição:",
                                        choices = c("Por turma", "Agrupada"), selected = "Por turma")
  #                         verbatimTextOutput('summary')
                           
                       )),
                column(width = 8,
                       box(width = NULL, plotlyOutput("n_matriculas_periodo")),
                       box(width = NULL,uiOutput('selectUI'),
                           sliderInput(inputId = "target", label = "Períodos:",
                                       min = 0, max = length(values$PERIODO_MAT) - 1,
                                       step = 1, sep = "",
                                       value = c(0,length(values$PERIODO_MAT) - 1)))
                )
              )
              
      ),
      
      
      tabItem(tabName = "tab2",
              fluidRow(
                
              )
              
      )
    )
  )
)


server <- function(input, output, session) {
  
  # output$summary <- renderPrint({
  #   print((input$target))
  #   print(input$target + 1)
  #   print((values[["PERIODO_MAT"]][input$target + 1])[1])
  #   print((values[["PERIODO_MAT"]][input$target + 1])[2])
  #   
  # })
  
  output$selectUI <- renderUI({
    
    sel_values <- paste(paste0('"', values[["PERIODO_MAT"]], '"'), collapse = ',')
    print(sel_values)
    list(
      (HTML(
        sprintf('
                        <script type="text/javascript">
                        $(document).ready(function() {
                        var vals = [%s];
                        $(\'#target\').data(\'ionRangeSlider\').update(
                        {values:vals,
                        min: 0,
                        max: %s,
                        from:%s})
                        })
                        </script>
                        ', sel_values, 
                length(values[["PERIODO_MAT"]]) - 1,
                length(values[["PERIODO_MAT"]]) - 1)))
    )
  })
  
  output$n_matriculas_periodo <- renderPlotly({
    
    n_matriculas = matriculas_nome %>%
      filter(DIS_DESCRICAO == input$tab1_selectDisciplina &
               PERIODO_MAT >= ((values[["PERIODO_MAT"]][input$target + 1])[1]) &
               PERIODO_MAT <= ((values[["PERIODO_MAT"]][input$target + 1])[2]))
    
    if(n_matriculas %>% nrow() > 0){
    
      if(input$tab1_tipoTurma == "Agrupada"){
        n_matriculas = n_matriculas %>%
          select(PERIODO_MAT, DIS_DESCRICAO) %>%
          group_by(PERIODO_MAT, DIS_DESCRICAO) %>%
          summarise(n = n())
        
       ggplotly(ggplot(n_matriculas %>% na.omit(), aes(as.character(PERIODO_MAT), n, group = DIS_DESCRICAO)) + geom_point() + geom_line())
      }else{
        n_matriculas = n_matriculas %>%
          select(PERIODO_MAT, DIS_DESCRICAO, MAT_TUR_TURMA) %>%
          group_by(PERIODO_MAT, DIS_DESCRICAO, MAT_TUR_TURMA) %>%
          summarise(n = n())
        
        ggplotly(ggplot(n_matriculas %>% na.omit(), aes(x = as.character(PERIODO_MAT), y = n, group = MAT_TUR_TURMA, color = MAT_TUR_TURMA)) + 
                   geom_point() + geom_line())
      }
    }else{
      return(NULL)
    }
    
  })
}

shinyApp(ui = ui, server = server)