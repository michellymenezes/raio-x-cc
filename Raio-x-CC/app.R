library(shinydashboard)
library(shinyjs)
source("processa_dados.R")
library(readr)
library(ggplot2)
library(highcharter)
source("pergunta02.R")
library(plotly)

matriculas = get_matriculas()
diciplinas = get_disciplinas()
disciplinas_qnt_alunos_aptos = get_disciplinas_qnt_alunos_aptos()
matriculas_nome = matriculas %>% 
  left_join(disciplinas %>% select(DIS_DISCIPLINA, DIS_DESCRICAO), by = c("MAT_TUR_DIS_DISCIPLINA" = "DIS_DISCIPLINA"))
values = list(PERIODO_MAT = (matriculas %>% select(PERIODO_MAT) %>% unique() %>% arrange(PERIODO_MAT))$PERIODO_MAT %>% na.omit())

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
                column(width = 4,
                       box(width = NULL, 
                           selectInput("tab2_selectDisciplina", label = h3("Disciplinas"),
                                       choices = unique(disciplinas_qnt_alunos_aptos$NOME_DISCIPLINA), multiple=T
                           )
                       )
                ),
                column(width = 8,
                       box(width = NULL, highchartOutput("n_alunos_aptos"))
                )
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
                        from:[0,%s]})
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
          mutate(PERIODO_MAT = as.character(PERIODO_MAT)) %>%
          group_by(PERIODO_MAT, DIS_DESCRICAO) %>%
          summarise(n = n())
        names(n_matriculas)[1:2] = c("Período", "Disciplina")
        
       g = ggplot(n_matriculas %>% na.omit(), aes(Período, n, group = Disciplina)) +
         geom_point() +
         geom_line() +
         ggtitle("Número de matrículas realizadas por período") + 
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
       
       ggplotly(g, tooltip=c("x", "y"))
       
      }else{
        n_matriculas = n_matriculas %>%
          select(PERIODO_MAT, DIS_DESCRICAO, MAT_TUR_TURMA) %>%
          mutate(PERIODO_MAT = as.character(PERIODO_MAT)) %>%
          group_by(PERIODO_MAT, DIS_DESCRICAO, MAT_TUR_TURMA) %>%
          summarise(n = n())
        names(n_matriculas)[1:3] = c("Período", "Disciplina", "Turma")
        
        g = ggplot(n_matriculas %>% na.omit(), aes(x = Período, y = n, group = Turma, color = Turma)) +
          geom_point() +
          geom_line() + 
          ggtitle("Número de matrículas realizadas por período") + 
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

        ggplotly(g, tooltip=c("x", "y", "group"))
      }
    }else{
      return(NULL)
    }
    
  })

  
  output$n_alunos_aptos <- renderHighchart({
    
    
    disciplinas_qnt_alunos_aptos = disciplinas_qnt_alunos_aptos %>%
      filter(NOME_DISCIPLINA %in% input$tab2_selectDisciplina) %>%
      reorder_by_qnt()
    
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Alunos aptos a pagar disciplinas") %>% 
      hc_subtitle(text = "(não cursou a disciplina, mas já cumpriu seus pré-requisitos") %>%
      hc_xAxis(categories = disciplinas_qnt_alunos_aptos$NOME_DISCIPLINA) %>% 
      hc_add_series(data = (disciplinas_qnt_alunos_aptos$QNT_ALUNOS_APTOS), 
                    name = "Quantidade de alunos aptos a pagar",  color = "#B71C1C")
  })
  
}

shinyApp(ui = ui, server = server)