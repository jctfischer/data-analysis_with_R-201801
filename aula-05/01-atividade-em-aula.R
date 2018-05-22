# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse

library(tidyverse)
library(lubridate)


# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 

ted_main <- read_csv("C:/Users/alu201830198/data-analysis_with_R-201801/aula-05/data/ted_main.csv.gz")


# Visualize o resumo dos dados do dataframe. 
summary(ted_main)

# Verifique os m�?nimos, máximos, médias e medianas das variáveis numéricas.

min(ted_main$comments) 
max(ted_main$comments) 
mean(ted_main$comments) 
median(ted_main$comments) 

min(ted_main$comments) 
max(ted_main$comments) 
mean(ted_main$comments) 
median(ted_main$comments) 

min(ted_main$duration) 
max(ted_main$duration) 
mean(ted_main$duration) 
median(ted_main$duration) 

min(ted_main$film_date) 
max(ted_main$film_date) 
mean(ted_main$film_date) 
median(ted_main$film_date) 

min(ted_main$languages) 
max(ted_main$languages) 
mean(ted_main$languages) 
median(ted_main$languages) 

min(ted_main$num_speaker) 
max(ted_main$num_speaker) 
mean(ted_main$num_speaker) 
median(ted_main$num_speaker) 

min(ted_main$published_date) 
max(ted_main$published_date) 
mean(ted_main$published_date) 
median(ted_main$published_date) 

min(ted_main$views) 
max(ted_main$views) 
mean(ted_main$views) 
median(ted_main$views) 

# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
# N�o est�o com tipo de dados aprpopriados

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..

ted_main %>%
  mutate(duracao =duration(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))->subset_tedmain_datas


# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

subset_tedmain_datas %>%
  mutate(evento = factor(event)
        ,ocupacao = factor(speaker_occupation)) ->subset_tedmain_fact
summary(subset_tedmain_fact)


# Retire do dataframe a variável name
  subset(subset_tedmain_fact, select = -name)  ->subset_tedmain_noname


# Visualize novamente o resumo dos dados do dataframe. Verifique os m�?nimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas

summary(subset_tedmain_noname)

min(subset_tedmain_noname$duracao) 
max(subset_tedmain_noname$duracao) 
mean(subset_tedmain_noname$duracao) 
median(subset_tedmain_noname$duracao) 

min(subset_tedmain_noname$data_film) 
max(subset_tedmain_noname$data_film) 
mean(subset_tedmain_noname$data_film) 
median(subset_tedmain_noname$data_film) 

min(subset_tedmain_noname$data_public) 
max(subset_tedmain_noname$data_public) 
mean(subset_tedmain_noname$data_public) 
median(subset_tedmain_noname$data_public) 



# Verifique quais registros possuem a menor quantidade de l�?nguas. Corrija para que possuam no m�?nimo 1 idioma.
subset_tedmain_noname %>% arrange(languages)

subset_tedmain_noname %>% mutate(linguagens = if_else( languages == 0, 1L, languages ))-> subset_tedmain_minlang 


# Verifique os 15 registros com menor data de filmagem. 

subset_tedmain_minlang %>% arrange(data_film)%>%select(data_film)%>%head(15)



# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
subset_tedmain_minlang %>% 
  group_by(year(data_film))%>%
  count() -> subset_tedmain_pres_year
subset_tedmain_pres_year

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.

quantile(subset_tedmain_pres_year$n, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90 ))


# Verifique novamente o resumo dos dados do dataframe




# Verifique os 10 registros com maior duração.




# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas




# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil




# Visualize os 10 quantis da quantidade de visualizações




# Compare as seguintes estat�?sticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?




# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de l�?nguas dos seguintes grupos:
#     * 10% de v�?deos com maior número de visualizações
#     * 10% de v�?deos com menor número de visualizações




# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro




# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos v�?deos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de l�?nguas das apresentações
#   * o desvio padrão da quantidade de l�?nguas
#   * o coeficiente de variação da quantidade de l�?nguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES




# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de l�?nguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de l�?nguas




# Descarte os v�?deos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos v�?deos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado




