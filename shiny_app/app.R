library(shinydashboard)
library(shinyjs)
library(readr)
library(ggplot2)
library(highcharter)
library(plotly)
library(viridisLite)
source("processa_dados.R")
source("ui_scripts.R")

formados = get_formados() 
formados_group = formados %>% group_by(PERIODO_EVASAO) %>% summarise(N = n())
formados_values = list(PERIODO_EVASAO = (formados %>% select(PERIODO_EVASAO) %>% unique() %>% arrange(PERIODO_EVASAO))$PERIODO_EVASAO %>% na.omit())

matriculas = get_matriculas()
alunos_ativos = get_alunos_ativos() %>% group_by(periodo, PERIODO_INGRESSAO) %>% summarise(n = n())

diciplinas = get_disciplinas()
disciplinas_qnt_alunos_aptos = get_disciplinas_qnt_alunos_aptos()

matriculas_nome = matriculas %>% left_join(disciplinas %>% select(DIS_DISCIPLINA, DIS_DESCRICAO), by = c("MAT_TUR_DIS_DISCIPLINA" = "DIS_DISCIPLINA"))
values = list(PERIODO_MAT = (matriculas %>% select(PERIODO_MAT) %>% unique() %>% arrange(PERIODO_MAT))$PERIODO_MAT %>% na.omit())

ui <- dashboardPage(
  dashboardHeader(title = "Raio-x CC"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(id = "menu",
                menuItem("Matrículas em disciplinas", tabName = "tab1", icon = icon("bookmark")),
                menuItem("Alunos aptos", tabName = "tab2", icon = icon("bookmark")),
                menuItem("Matrículas totais", tabName = "tab3", icon = icon("bookmark")),
                menuItem("Formados", tabName = "tab4", icon = icon("bookmark")),
                menuItem("Matrículas ativas", tabName = "tab5", icon = icon("bookmark"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "tab1",
        tab1(values, matriculas_nome)
      ),
      
      tabItem(tabName = "tab2",
        tab2()
      ),
  
      tabItem(tabName = "tab3",
        tab3()
      ),

      tabItem(tabName = "tab4",
        tab4(formados_values)
      ),

      tabItem(tabName = "tab5",
        tab5()
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$selectUI <- renderUI({
    
    sel_values <- paste(paste0('"', values[["PERIODO_MAT"]], '"'), collapse = ',')
    #print(sel_values)
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
  
  output$n_matriculas_periodo <- renderHighchart({
    
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
       
       #ggplotly(g, tooltip=c("x", "y"))
       highchart() %>%
         hc_xAxis(categories = n_matriculas$Período) %>% 
       hc_add_series(n_matriculas %>% na.omit(), "line", hcaes(x = Período, y = n)) %>%
         hc_title(text = "Número de matrículas por período")
       
       
      }else{
        n_matriculas = n_matriculas %>%
          select(PERIODO_MAT, DIS_DESCRICAO, MAT_TUR_TURMA) %>%
          mutate(PERIODO_MAT = as.character(PERIODO_MAT)) %>%
          group_by(PERIODO_MAT, DIS_DESCRICAO, MAT_TUR_TURMA) %>%
          summarise(n = n())
        
        
        names(n_matriculas)[1:3] = c("Periodo", "Disciplina", "Turma")
        mat  = data_frame(Periodo = as.character(), Turma = as.character())  
        peri = n_matriculas %>% ungroup() %>% select(Periodo) %>% unique()
        tur = n_matriculas %>% ungroup() %>% select(Turma) %>% unique()
        
        
        for(i in 1:(tur %>% nrow())){
          
          
          temp = peri
          temp$Turma = NA
          
          temp$Turma = tur$Turma[i]

          names(temp) = c("Periodo", "Turma")

          mat = mat %>% rbind(temp)
          #print(tur$Turma[i])

        }
        
        #print(mat)
        mat  =  mat %>% left_join(n_matriculas)
        mat$n[is.na(mat$n)] = -1
        #print(mat[11:20,])
        
        n_matriculas$Periodo = as.numeric(n_matriculas$Periodo)
        
      
        
        # g = ggplot(n_matriculas %>% na.omit(), aes(x = Periodo, y = n, group = Turma, color = Turma)) + 
        #   geom_point() +
        #   geom_line() + 
        #   ggtitle("Número de matrículas realizadas por período") + 
        #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
        cols <- viridis(8)
        cols <- substr(cols, 0, 7)
        
        # highchart() %>%
        #   hc_xAxis(categories = as.character(mat$Periodo %>% unique())) %>%
        hchart(n_matriculas %>% na.omit(), "line", hcaes(x = Periodo, y = n, group = Turma)) %>%
          hc_colors(cols) %>%
          hc_title(text = "Número de matrículas por período")
        #   hc_add_series(mat %>% na.omit(), "line", hcaes(x = Periodo, y = n, group = Turma))
        
        #ggplotly(g, tooltip=c("x", "y", "group"))
      }
    }else{
      return(NULL)
    }
    
  })

  output$n_alunos_aptos <- renderHighchart({
    
    disciplinas_qnt_alunos_aptos = disciplinas_qnt_alunos_aptos %>%
      filter(NOME_DISCIPLINA %in% input$tab2_selectDisciplina) %>%
      reorder_by_qnt()
    print(disciplinas_qnt_alunos_aptos)
    
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
  
  output$total_alunos_ativos <- renderInfoBox({
    total = (alunos_ativos %>% ungroup() %>% filter(periodo <= input$tab5_selectPeriodoAtivo[2], periodo > input$tab5_selectPeriodoAtivo[1]-1) %>% select(n)) %>% sum()
    infoBox(width = 12,
      "Total de alunos",
      total,
      color = "maroon",
      icon = icon("info-sign", lib = "glyphicon")
      
    )
  })

  output$alunos_ativos <- renderHighchart({
    
    x <- c("Período de entrada: ", "Número de matrículas: ", "Período atual: ")
    y <- sprintf("{point.%s}", c("PERIODO_INGRESSAO", "n", "periodo"))
    tltip <- tooltip_table(x, y)
    
    highchart() %>%
      hc_add_series(alunos_ativos %>% filter(periodo <= input$tab5_selectPeriodoAtivo[2], periodo > input$tab5_selectPeriodoAtivo[1]-1),
                    showInLegend = FALSE, 
                    "column", 
                    hcaes(x = periodo, y = n))%>%
      hc_yAxis(title = list(text = "Número de matrículas")) %>%
      hc_xAxis(title = list(text = "Período ativo")) %>%
      hc_tooltip(table = TRUE, headerFormat = "", pointFormat = tltip) %>%
      hc_plotOptions(
        series  = list(
          color = "#D81B60"
        )
      )
  })
  
  output$formados_pelos_periodos <- renderHighchart({
    
    x <- c("Formados: ")
    y <- sprintf("{point.%s}", c("N"))
    tltip <- tooltip_table(x, y)
    
    hchart(formados_group, "column", hcaes(x = PERIODO_EVASAO, y = N)) %>%
      hc_title(text = "Quantidade de alunos formados ao longo do tempo") %>%
      hc_subtitle(text = "(a partir de 2001.1)") %>%
      hc_tooltip(table = TRUE, headerFormat = "", pointFormat = tltip) %>%
      hc_plotOptions(
        series  = list(
          color = "#9C27B0"
        )
      )
      
  })
  
  output$formados_total_stack <- renderHighchart({
    
    interval_begin = (formados_values[["PERIODO_EVASAO"]][input$targetFormados + 1])[1]
    interval_end = (formados_values[["PERIODO_EVASAO"]][input$targetFormados + 1])[2]
    
    mm_formados_group = formados_group %>%
      filter(
        PERIODO_EVASAO >= (interval_begin) &
          PERIODO_EVASAO <= (interval_end)
      )
    
    cols <- viridis(33)
    cols <- substr(cols, 0, 7)
    
    total_n_formados = sum(mm_formados_group$N) 
    annotation_y = total_n_formados + 2
    
    title_text = paste("Quantidade de alunos formados entre <i>", interval_begin, "</i>e<i>", interval_end, "</i>")
    subtitle_text = paste("(total de formados desde 2001:<b>", NROW(formados), "</b>)")
    
    tooltip = "<b>Periodo:</b> {point.PERIODO_EVASAO}<br/><b>Qnt formados:</b> {point.y}"

     hchart(mm_formados_group, "column", hcaes(x = 0, y = N, group = PERIODO_EVASAO)) %>%
      hc_title(text = title_text, style = list(fontSize = "14px"), align = "left") %>%
      hc_subtitle(text = subtitle_text, style = list(fontSize = "12px"), align = "left") %>%
      hc_yAxis(title = list(text = "Número de formados")) %>%
      hc_xAxis(labels = list(enabled = FALSE),
               marker = list(enabled = TRUE)) %>%
      hc_plotOptions( column = list(stacking = "normal"),
                      series = list(marker = list(enabled = FALSE))) %>%
      hc_legend(enabled = FALSE) %>%
      hc_tooltip(headerFormat = "", pointFormat = tooltip) %>%
      hc_add_annotation(xValue = 0.3, yValue = annotation_y, 
                        title = list(
                          text = total_n_formados,
                          style = list(
                            color = "#4A148C",
                            fontFamily = 'Tangerine',
                            fontSize = "22"
                          )
                        )                        
      ) %>%
      hc_colors(cols)
  
  })
  
  output$formados_icon_graph <- renderHighchart({
    
    m_formados_group = get_formados()  %>%
      mutate(PERIODO_EVASAO = as.integer(formados$PERIODO_EVASAO)) %>%
      group_by(PERIODO_EVASAO) %>%
      summarise(N = n()) %>%
      ungroup() %>%
      mutate(total = sum(formados_group$N))
    
    colfunc <- colorRampPalette(c("blue4", "grey50", "deeppink", "grey60", "steelblue", 
                                  "turquoise", "blue", "orchid", "lightpink1", "red", 
                                  "gold4", "gold")
                )
    colors = colfunc(NROW(m_formados_group))
    
    N_PERIODOS = NROW(m_formados_group)
    icons = rep("male", N_PERIODOS)
    
    series = m_formados_group$PERIODO_EVASAO
    n = m_formados_group$N %>% sort(decreasing = TRUE)
    
    title_text = paste("Quantidade de alunos formados <i>através dos anos</i>")

    hciconarray(series, n, icons = icons, size = 2.5) %>%
      hc_title(text = title_text, style = list(fontSize = "14px"), align = "left") %>%
      hc_legend(align = "left", verticalAlign = "top", layout = "vertical", y = 30) %>%
      hc_colors(colors)
    
  })

  output$formados_years_select <- renderUI({
    
    m_values <- paste(paste0('"', formados_values[["PERIODO_EVASAO"]], '"'), collapse = ',')
    list(
      (HTML(
        sprintf('
                        <script type="text/javascript">
                        $(document).ready(function() {
                        var vals = [%s];
                        $(\'#targetFormados\').data(\'ionRangeSlider\').update(
                        {values:vals,
                        min: 0,
                        max: %s,
                        from:[0,%s]})
                        })
                        </script>
                        ', m_values,
                length(formados_values[["PERIODO_EVASAO"]]) - 1,
                length(formados_values[["PERIODO_EVASAO"]]) - 1)))
    )
  })
  
}

shinyApp(ui = ui, server = server)