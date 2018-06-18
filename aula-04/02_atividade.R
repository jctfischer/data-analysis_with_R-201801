library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.
salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

vl_dolar <- 3.2421

salarios %>%
  mutate(remun_total  = REMUNERACAO_REAIS + (REMUNERACAO_DOLARES * vl_dolar)) -> subset_salarios

subset_salarios %>% select(REMUNERACAO_REAIS, REMUNERACAO_DOLARES, remun_total) 



### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 

subset_salarios %>% 
  group_by(DESCRICAO_CARGO) %>%
  summarise(servidores = n()) %>%
  ungroup() %>%
  arrange(desc(servidores)) %>%
  filter(servidores > 200)  %>% 
  pull(DESCRICAO_CARGO) -> cargos_mais_que200

#' 
#' ## Correlograma
#' #' **ggcorrplot** possibilita visualizar as correlações entre variáveis numéricas. 
#' A matriz de correlações deve ser previamente calculada utilizando a função `cor`.
#' 
## ------------------------------------------------------------------------
library(ggcorrplot)

corr <-  subset_salarios %>% 
  mutate( tempo_orgao = year(Sys.Date()) - year(DATA_INGRESSO_ORGAO)
          , tempo_diploma = year(Sys.Date()) - year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO) )%>%
  filter(DESCRICAO_CARGO %in% cargos_mais_que200) %>%
  mutate(tempo_orgao = as.numeric(tempo_orgao)
         , tempo_diploma = as.numeric(tempo_diploma) )%>%
  select(DESCRICAO_CARGO, tempo_orgao, tempo_diploma) %>%
  group_by(DESCRICAO_CARGO) %>%
  select_if(is_numeric) %>%
  cor() %>% round(2)

# ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)



### # ####

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

