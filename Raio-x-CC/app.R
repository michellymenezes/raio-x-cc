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
                menuItem("Matrículas em disciplinas", tabName = "tab1", icon = icon("bookmark")),
                menuItem("Alunos aptos", tabName = "tab2", icon = icon("bookmark")),
                menuItem("Matrículas totais", tabName = "tab3", icon = icon("bookmark"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "tab1",
              h3("Quantas pessoas se matricularam na disciplina X no período letivo Y?", align = "center"),
              br(),
              fluidRow(
                column(width = 3,
                       box(width = NULL, selectInput('tab1_selectDisciplina', 'Estados', 
                                                     choices = matriculas_nome %>%
                                                       select(DIS_DESCRICAO) %>% distinct() %>% na.omit() %>% arrange(DIS_DESCRICAO), 
                                                     multiple=F, selectize=TRUE),
                           radioButtons(inputId = "tab1_tipoTurma", label = "Modo de exibição:",
                                        choices = c("Por turma", "Agrupada"), selected = "Por turma")
                           #verbatimTextOutput('summary')
                           
                       )),
                column(width = 7,
                       box(width = NULL, plotlyOutput("n_matriculas_periodo")),
                       box(width = NULL,uiOutput('selectUI'),
                           sliderInput(inputId = "target", label = "Períodos:",
                                       min = 0, max = length(values$PERIODO_MAT) - 1,
                                       step = 1, sep = "",
                                       value = c(0, length(values$PERIODO_MAT) - 1)))
                )
              )
      ),
      
      
      tabItem(tabName = "tab2",
              h3("Quantos alunos estão aptos a cursar a disciplina X?", align = "center"),
              h5("Possuir pré-requisitos necessários", align = "center"),
              br(),
              fluidRow(
                column(width = 3,
                       box(width = NULL, 
                           selectInput("tab2_selectDisciplina", label = h3("Disciplinas"),
                                       choices = unique(disciplinas_qnt_alunos_aptos$NOME_DISCIPLINA), multiple=T,
                                       selected = c("LAB.DE ORG.E ARQUITETURA DE COMPUTADORES", "ORG.E ARQUITETURA DE COMPUTADORES I")
                           )
                       )
                ),
                column(width = 7,
                       box(width = NULL, highchartOutput("n_alunos_aptos"))
                )
              ) 
      ),
  
      tabItem(tabName = "tab3",
              h3("Quantas matrículas foram efetuadas no período X?", align = "center"),
              br(),
              fluidRow(
                column(width = 3),
                column(width = 7, 
                       box(width = NULL, highchartOutput("matriculas_pelos_periodos"))
                )
              ) 
      )
  
    )
  )
)



server <- function(input, output, session) {
  
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
               PERIODO_MAT <= ((values[["PERIODO_MAT"]][input$target + 1])[2])
      )
    
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
      hc_chart(animation = FALSE) %>% 
      hc_title(text = "Alunos aptos a pagar disciplinas") %>% 
      hc_subtitle(text = "(não cursou a disciplina, mas já cumpriu seus pré-requisitos)") %>%
      hc_xAxis(categories = disciplinas_qnt_alunos_aptos$NOME_DISCIPLINA) %>% 
      hc_plotOptions( column = list(stacking = "normal") ) %>% 
      hc_add_series(
        data = (disciplinas_qnt_alunos_aptos$QNT_ALUNOS_APTOS), 
        name = "Quantidade de alunos aptos a pagar",  
        color = "#00BFA5",
        type = "column"
      ) %>% 
      hc_add_series(
        data = (disciplinas_qnt_alunos_aptos$QNT_ALUNOS_PAGANDO), 
        name = "Quantidade de alunos pagando",  
        color = "#311B92",
        type = "column"
      )
  })
  
  output$matriculas_pelos_periodos <- renderHighchart({
    
    matriculas = get_matriculas() %>%
      filter(PERIODO_MAT > 2001) %>%
      filter(PERIODO_MAT - as.integer(PERIODO_MAT) > 0) %>%
      group_by(PERIODO_MAT) %>%
      summarise(N = n()) %>%
      mutate(
        PERIODO = as.integer(PERIODO_MAT) + (PERIODO_MAT- as.integer(PERIODO_MAT)) * 5 - 0.5
      )

    x <- c("Periodo:", "Quantidade de matrículas: ")
    y <- sprintf("{point.%s}", c("PERIODO_MAT", "N"))
    tltip <- tooltip_table(x, y)

    hchart(matriculas, "spline", hcaes(x = PERIODO, y = N)) %>%
      hc_plotOptions(
        series  = list(
          marker = list(enabled = TRUE, 'x'), 
          color = "#F50057"
        )
      ) %>%
      hc_title(text = "Quantidade de matriculas totais") %>%
      hc_subtitle(text = "(aluno-disciplina)") %>%
      hc_yAxis(title = list(text = "Número de matrículas")) %>%
      hc_xAxis(title = list(text = "Períodos"), min = min(matriculas$PERIODO_MAT)) %>%
      hc_tooltip(table = TRUE, headerFormat = "", pointFormat = tltip)
    
  })
  
}

shinyApp(ui = ui, server = server)