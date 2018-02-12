library(shinydashboard)
library(shinyjs)

tab1 = function(values, matriculas_nome) {
  basicPage(
    h3("Quantas pessoas se matricularam na disciplina X no período letivo Y?", align = "center"),
    br(),
    fluidRow(
      column(width = 3,
             box(width = NULL, selectInput('tab1_selectDisciplina', 'Disciplinas', 
                                           choices = matriculas_nome %>%
                                             select(DIS_DESCRICAO) %>% distinct() %>% na.omit() %>% arrange(DIS_DESCRICAO), 
                                           multiple=F, selectize=TRUE),
                 radioButtons(inputId = "tab1_tipoTurma", label = "Modo de exibição:",
                              choices = c("Por turma", "Agrupada"), selected = "Por turma")
                 #verbatimTextOutput('summary')
                 
             )),
      column(width = 7,
             box(width = NULL, highchartOutput("n_matriculas_periodo")),
             box(width = NULL,uiOutput('selectUI'),
                 sliderInput(inputId = "target", label = "Períodos:",
                             min = 0, max = length(values$PERIODO_MAT) - 1,
                             step = 1, sep = "",
                             value = c(0, length(values$PERIODO_MAT) - 1)))
      )
    )
  )
}

tab2 = function() {
  basicPage(
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
  ) 
}

tab3 = function() {
  basicPage(
    column(width = 3),
    column(width = 7,
           h3("Quantas matrículas foram efetuadas no período X?", align = "center"),
           br(),
           box(width = NULL, highchartOutput("matriculas_pelos_periodos"))
    )
  )
}

tab4 = function(formados_values) {
  fluidPage(
    h3("Quantos alunos se formaram no período X? E no total? E em um intervalo?", align = "center"),
    br(),
    fluidRow(
      column(width = 3),
      column(width = 6,
             box(width = NULL, highchartOutput("formados_pelos_periodos"))
      ),
      column(width = 3)
    ),
    fluidRow(
      column(width = 2),
      column(width = 4,
             box(width = NULL,highchartOutput("formados_total_stack")),
             box(width = NULL,
                 uiOutput('formados_years_select'),
                 sliderInput(inputId = "targetFormados", label = "Períodos:",
                             min = 0, max = length(formados_values$PERIODO_EVASAO) - 1,
                             step = 1, sep = "",
                             value = c(0, length(formados_values$PERIODO_EVASAO) - 1))
             )
      ),
      column(width = 4,
             box(width = NULL, highchartOutput("formados_icon_graph"))
      ),
      column(width = 2)
    ) 
  )
}

tab5 = function() {
  basicPage(
    h3("Qual o número de alunos ativos no curso?", align = "center"),
    br(),
    fluidRow(
      column(width = 3, infoBoxOutput(width = 12,"total_alunos_ativos")),
      column(width = 9, 
             box(width = NULL, highchartOutput("alunos_ativos")),
             box(width = NULL, sliderInput("tab5_selectPeriodoAtivo", label = "Períodos:",
                                           min = 1, max = (alunos %>% filter(ALU_FORMA_EVASAO == 0) %>% select(PERIODO_INGRESSAO) %>% unique() %>% nrow()),
                                           step = 1, sep = "", value = c(1,12)
             ))
      )
    ) 
  )
}