# - Quantos alunos estão aptos a cursar a disciplina X (tem pré-requisito necessário).
# * Gráfico de barras
# * Possível selecionar mais de uma disciplina
# * Cada barra é uma disciplina

disciplinas_qnt_alunos_aptos = get_disciplinas_qnt_alunos_aptos()

reorder_by_qnt = function(df) {
  df = df[order(-df$QNT_ALUNOS_APTOS),] 
  return(df)
}

