# Library
require(tidyverse)
require(readxl)

### Abrindo os dados referentes aos aprovados:

pdf_extracao <- read_csv("dados/tabelas_extraidas.csv", skip = 2) |> 
  filter(`SITUAÇÃO NO CARGO` == "Convocado para o curso de formação pelo subitem 1.1 do edital específico nº.2.") |> 
  select(1, 9) |> 
  rename(id = 1, 
         nascimento = 2)


### Relecao entre NOMES e INSCRICAO (DICIONARIO)

dicionario_numero <- read_csv("dados/nomes_codigos-2.csv") |> 
  select(1, 2) |> 
  rename(nome = 1, 
         id = 2)

### Aluno por Turma

por_turma <- read_csv("dados/MATRICULAS CF2/Convocados-Tabela 1.csv") |> 
  select(1,2) |> 
  rename(turma = 1, 
         nome = 2)

### Juntando os dados:

juncao_dados <- pdf_extracao |> 
  left_join(dicionario_numero |> 
              mutate(id = as.character(id))) |> 
  left_join(por_turma) |> 
  mutate(nascimento = dmy(nascimento))

#### AGORA PONDO OS SIGNOS:

### Funcao signos: 

# Função para determinar o signo
signo <- function(data_nascimento) {
  dia <- day(data_nascimento)
  mes <- month(data_nascimento)
  
  if ((mes == 3 && dia >= 21) || (mes == 4 && dia <= 19)) {
    return("Áries")
  } else if ((mes == 4 && dia >= 20) || (mes == 5 && dia <= 20)) {
    return("Touro")
  } else if ((mes == 5 && dia >= 21) || (mes == 6 && dia <= 20)) {
    return("Gêmeos")
  } else if ((mes == 6 && dia >= 21) || (mes == 7 && dia <= 22)) {
    return("Câncer")
  } else if ((mes == 7 && dia >= 23) || (mes == 8 && dia <= 22)) {
    return("Leão")
  } else if ((mes == 8 && dia >= 23) || (mes == 9 && dia <= 22)) {
    return("Virgem")
  } else if ((mes == 9 && dia >= 23) || (mes == 10 && dia <= 22)) {
    return("Libra")
  } else if ((mes == 10 && dia >= 23) || (mes == 11 && dia <= 21)) {
    return("Escorpião")
  } else if ((mes == 11 && dia >= 22) || (mes == 12 && dia <= 21)) {
    return("Sagitário")
  } else if ((mes == 12 && dia >= 22) || (mes == 1 && dia <= 19)) {
    return("Capricórnio")
  } else if ((mes == 1 && dia >= 20) || (mes == 2 && dia <= 18)) {
    return("Aquário")
  } else if ((mes == 2 && dia >= 19) || (mes == 3 && dia <= 20)) {
    return("Peixes")
  }
}

### Aplicando a funcao: 

juncao_dados <- juncao_dados %>%
  mutate(signo = sapply(nascimento, signo)) |> 
  selet(-nome)

write_csv2(juncao_dados, "resultados/signos_atps.csv")
