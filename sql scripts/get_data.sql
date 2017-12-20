
/* Disciplinas do CC grade de 1999 */
SELECT * 
FROM SCA.ANALYTICS_DISCIPLINAS, 
     SCA.ANALYTIC_DISCIPLINAS_CURRICULO
WHERE DIS_DISCIPLINA = DIC_DIS_DISCIPLINA
  AND DIC_CCU_CUR_COD_CURSO = 14102100 
  AND DIC_CCU_COD_CURRICULO = 1999;
  
/* Turmas para as disciplinas acima*/
SELECT * 
FROM SCA.ANALYTICS_TURMAS
WHERE TUR_DIS_DISCIPLINA IN (
  SELECT DIC_DIS_DISCIPLINA
  FROM SCA.ANALYTIC_DISCIPLINAS_CURRICULO
  WHERE DIC_CCU_CUR_COD_CURSO = 14102100 
    AND DIC_CCU_COD_CURRICULO = 1999
  );
  
/* Matriculas em disciplinas de CC */
SELECT MAT_TUR_ANO || '.' || MAT_TUR_PERIODO as PERIODO_MAT,
  MAT_TUR_DIS_DISCIPLINA, MAT_TUR_TURMA, MAT_ALU_MATRICULA, 
  MAT_TIPO_MATRICULA, MAT_SITUACAO, MAT_MEDIA_FINAL
FROM SCA.ANALYTICS_MATRICULAS
WHERE MAT_TUR_DIS_DISCIPLINA IN (
  SELECT DIC_DIS_DISCIPLINA
  FROM SCA.ANALYTIC_DISCIPLINAS_CURRICULO
  WHERE DIC_CCU_CUR_COD_CURSO = 14102100 
    AND DIC_CCU_COD_CURRICULO = 1999
  );