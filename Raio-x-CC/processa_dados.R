library(readr)
library(dplyr)

disciplinas <- read_delim("dados/disciplinas.csv", ";", escape_double = FALSE, trim_ws = TRUE)
matriculas <- read_delim("dados/matriculas.csv", ";", escape_double = FALSE, trim_ws = TRUE)
turmas <- read_delim("dados/turmas.csv", ";", escape_double = FALSE, trim_ws = TRUE)
prerequisitos_raw <- read_delim("dados/prerequisitos_raw.csv", ";", escape_double = FALSE, trim_ws = TRUE)
disciplinas_qnt_alunos_aptos <- read_csv("dados/disciplinas_qnt_alunos_aptos.csv")
alunos <- read_delim("dados/alunos.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Limpando dados incorretos de matriculas e matricula do periodo que não acabou
matriculas = matriculas %>%
  filter(PERIODO_MAT >= 1999.1 & PERIODO_MAT <= 2017.2)

# Trocando NA por 0 em número de turma
matriculas$MAT_TUR_TURMA[is.na(matriculas$MAT_TUR_TURMA)] = 0

matriculas.turma = matriculas %>%
  group_by(PERIODO_MAT, MAT_TUR_DIS_DISCIPLINA, MAT_TUR_TURMA) %>%
  summarise(N = n())

# Filtrando disciplinas por disciplinas ativas ou optativas
disciplinas = disciplinas %>% 
  filter(DIC_REGRA == 'Optativa' | (DIC_REGRA == 'Obrigatoria' & DIC_STATUS == 'A') | (DIC_REGRA == 'Complementar' & DIC_STATUS == 'A'))

#matriculas  = (alunos %>% select(ALU_MATRICULA)) %>% left_join(matriculas, by = c("ALU_MATRICULA" = "MAT_ALU_MATRICULA"))

matriculas = matriculas %>% filter(MAT_ALU_MATRICULA %in% alunos$ALU_MATRICULA)

periodos_matricula = alunos %>% 
  filter(ALU_FORMA_EVASAO == 0) %>% 
  select(PERIODO_INGRESSAO) %>%
  unique() %>%
  arrange(-PERIODO_INGRESSAO)

periodos_matricula$periodo = NA
periodos_matricula$periodo = seq.int(1,nrow(periodos_matricula))

alunos_ativos = alunos %>% 
  filter(ALU_FORMA_EVASAO == 0) %>%
  left_join(periodos_matricula)

get_matriculas = function(){
  return(matriculas)
}

get_disciplinas = function(){
  return(disciplinas)
}

get_turmas = function() {
  return(turmas)
}

get_prerequisitos = function(){
  return(prerequisitos_raw)
}

get_disciplinas_qnt_alunos_aptos = function() {
  return(disciplinas_qnt_alunos_aptos)
}

get_alunos = function(){
  return(alunos)
}

get_formados = function(){
 
  GRADUADO = 1
  N_COLOU_GRAU = 10
  DECISAO_JUDICIAL = 20
  
  se_formou_cod = c(GRADUADO, N_COLOU_GRAU, DECISAO_JUDICIAL)
  
  formados = alunos %>%
    filter(ALU_FORMA_EVASAO %in% se_formou_cod) %>%
    filter(PERIODO_EVASAO > 2001) 
  
   return(formados)
}

get_alunos_ativos = function(){
  return(alunos_ativos)
}


