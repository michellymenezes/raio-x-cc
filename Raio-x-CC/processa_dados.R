library(readr)
library(dplyr)

disciplinas <- read_csv("../dados/disciplinas.csv")
matriculas <- read_csv("../dados/matriculas.csv")
turmas <- read_csv("../dados/turmas.csv")


# Limpando dados incorretos de matriculas
matriculas = matriculas %>%
  filter(PERIODO_MAT >= 1999.1 & PERIODO_MAT <= 2017.2)

# Trocando NA por 0 em número de turma
matriculas$MAT_TUR_TURMA[is.na(matriculas$MAT_TUR_TURMA)] = 0

matriculas.turma = matriculas %>%
  group_by(PERIODO_MAT, MAT_TUR_DIS_DISCIPLINA, MAT_TUR_TURMA) %>%
  summarise(N = n())
