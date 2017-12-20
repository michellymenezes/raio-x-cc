library(readr)
library(dplyr)

disciplinas <- read_delim("dados/disciplinas.csv", ";", escape_double = FALSE, trim_ws = TRUE)
matriculas <- read_delim("dados/matriculas.csv", ";", escape_double = FALSE, trim_ws = TRUE)
turmas <- read_delim("dados/turmas.csv", ";", escape_double = FALSE, trim_ws = TRUE)
prerequisitos <- read_delim("dados/prerequisitos.csv", ";", escape_double = FALSE, trim_ws = TRUE) 

# Limpando dados incorretos de matriculas e matricula do periodo que não acabou
matriculas = matriculas %>%
  filter(PERIODO_MAT >= 1999.1 & PERIODO_MAT <= 2017.1)

# Trocando NA por 0 em número de turma
matriculas$MAT_TUR_TURMA[is.na(matriculas$MAT_TUR_TURMA)] = 0

matriculas.turma = matriculas %>%
  group_by(PERIODO_MAT, MAT_TUR_DIS_DISCIPLINA, MAT_TUR_TURMA) %>%
  summarise(N = n())

prerequisitos = prerequisitos %>% select(-PRD_DIC_CCU_CUR_COD_CURSO, -PRD_DIC_CCU_COD_CURRICULO)