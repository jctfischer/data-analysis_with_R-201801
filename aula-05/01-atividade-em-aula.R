# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serÃ£o carregadas junto ao tidyverse

library(tidyverse)
library(lubridate)
library(stringr)

# Crie um dataframe com o conteÃºdo do arquivo ted_main.csv.gz. 

ted_main <- read_csv("C:/Users/PC/Documents/data-analysis_with_R-201801-master/aula-05/data/ted_main.csv.gz")


# Visualize o resumo dos dados do dataframe. 
summary(ted_main)

# Verifique os mÃ?nimos, mÃ¡ximos, mÃ©dias e medianas das variÃ¡veis numÃ©ricas.

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

# As variÃ¡veis duration, film_date e published_date estÃ£o no tipo de dados apropriado?
# Não estão com tipo de dados aprpopriados

# Converta as seguintes variÃ¡veis utilizando o pacote Lubridate:
#     * duration, para duraÃ§Ã£o (em segundos). Experimente utilizar as funÃ§Ãµes as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a funÃ§Ã£o as_datetime.
#     * published_date, para data, com a funÃ§Ã£o as_datetime..

ted_main %>%
  mutate(duracao =duration(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))->subset_tedmain_datas


# Converta as seguintes variÃ¡veis character para variÃ¡veis categÃ³ricas com a funÃ§Ã£o factor.
#     * event
#     * speaker_occupation

subset_tedmain_datas %>%
  mutate(evento = factor(event)
        ,ocupacao = factor(speaker_occupation)) ->subset_tedmain_fact
summary(subset_tedmain_fact)


# Retire do dataframe a variÃ¡vel name
  subset(subset_tedmain_fact, select = -name)  ->subset_tedmain_noname


# Visualize novamente o resumo dos dados do dataframe. Verifique os mÃ?nimos, mÃ¡ximos, mÃ©dias e medianas das variÃ¡veis numÃ©ricas. Verifique as contagens das variÃ¡veis categÃ³ricas

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



# Verifique quais registros possuem a menor quantidade de lÃ?nguas. Corrija para que possuam no mÃ?nimo 1 idioma.
subset_tedmain_noname %>% arrange(languages)

subset_tedmain_noname %>% mutate(linguagens = if_else( languages == 0, 1L, languages ))-> subset_tedmain_minlang 


# Verifique os 15 registros com menor data de filmagem. 

subset_tedmain_minlang %>% arrange(data_film)%>%select(data_film)%>%head(15)



# Crie um dataframe com a contagem de apresentaÃ§Ãµes por ano de filmagem e visualize todo o seu conteÃºdo
subset_tedmain_minlang %>% 
  group_by(year(data_film))%>%
  count() -> subset_tedmain_pres_year
subset_tedmain_pres_year

# Analise os 10 quantis da quantidade de apresentaÃ§Ãµes por ano.
quantile(subset_tedmain_pres_year$n, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90 ))

# Descarte, do data frame de apresentaÃ§Ãµes do TED Talks, aqueles cujo ano de filmagem 
# tiver quantidade de apresentaÃ§Ãµes menor ou igual a quantidade do quarto quantil.

subset_tedmain_pres_year%>%
  filter(n > quantile(subset_tedmain_pres_year$n, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90 ))[4][1])%>%
  pull(`year(data_film)`) -> anos

subset_tedmain_pres_year %>% filter( year(data_film) %in% anos )
  
# Verifique novamente o resumo dos dados do dataframe
summary(subset_tedmain_pres_year)


# Verifique os 10 registros com maior duraÃ§Ã£o.
subset_tedmain_datas %>%
  arrange(desc(duracao))%>%
  head(10)
  


# Existem apresentaÃ§Ãµes com duraÃ§Ã£o maior que 3 desvios padrÃ£o acima da mÃ©dia? Liste elas

ted_main %>%
  filter(duration > (3*sd(duration))) %>%
  select( duration, event, film_date, main_speaker)


# Calcule os 4 quartis e o IQR da duraÃ§Ã£o das apresentaÃ§Ãµes. Liste as apresentaÃ§Ãµes cuja duraÃ§Ã£o supera 1.5 * o IQR + o terceiro quartil
quantile(ted_main$duration)


# Visualize os 10 quantis da quantidade de visualizaÃ§Ãµes
quantile(ted_main$views, c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90 ))


# Compare as seguintes estatÃ?sticas descritivas da quantidade de visualizaÃ§Ãµes:
#   * MÃ©dia e Mediana. Qual Ã© maior?
#   * Desvio Absoluto da Mediana e Desvio PadrÃ£o. Qual Ã© maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR Ã© maior que o Desvio Absoluto da Mediana?
#   * Com base na mÃ©dia e na mediana, e na razÃ£o entre o IQR e o Desvio Absoluto da Mediana, 
#     vocÃª conclui que as quantidades de visualizaÃ§Ã£o estÃ£o distribuidas de forma simÃ©trica em torno da mÃ©dia?

mean(ted_main$views)
median(ted_main$views)
# a media eh maior

(dam_visual <- median( abs( ted_main$views - median( ted_main$views ))))
sd(ted_main$views)
# o Desvio Absoluto da Mediana eh maior

quantile(ted_main$views)
IQR <- c(quantile(ted_main$views))[3] - c(quantile(ted_main$views))[1]
IQR/dam_visual

# Calcule a mÃ©dia, o desvio padrÃ£o, a mediana e o IQR da quantidade de lÃ?nguas dos seguintes grupos:
#     * 10% de vÃ?deos com maior nÃºmero de visualizaÃ§Ãµes
#     * 10% de vÃ?deos com menor nÃºmero de visualizaÃ§Ãµes

ted_main%>%
  arrange(desc(languages))%>%
  select(languages)%>%
  head(10) -> dez_maior_visual  

mean(dez_maior_visual$languages)
median(dez_maior_visual$languages)
(dam_visual <- median( abs( dez_maior_visual$languages - median( dez_maior_visual$languages ))))
sd(dez_maior_visual$languages)
IQR <- c(quantile(dez_maior_visual$languages))[3] - c(quantile(dez_maior_visual$languages))[1]

ted_main %>%
  arrange(desclanguages) %>%
  select(languages)%>%
  head(10) -> dez_menor_visual  

mean(dez_menor_visual$languages)
median(dez_menor_visual$languages)
(dam_visual <- median( abs( dez_menor_visual$languages - median( dez_menor_visual$languages ))))
sd(dez_maior_visual$languages)
IQR <- c(quantile(dez_menor_visual$languages))[3] - c(quantile(dez_menor_visual$languages))[1]


# Determine a quantidade de apresentaÃ§Ãµes por evento cujo nome inicie com TED. Utilize a funÃ§Ã£o str_detect para este filtro

ted_main %>%
  filter( str_detect(ted_main$name, pattern = "TED"))%>%
  select(name, event) %>% head(10)

ted_main %>%
  filter(str_detect(ted_main$name, pattern = "TED"))%>%
  group_by(event)%>%
  summarise( qtde_apres = n())%>%
  ungroup()%>%
  arrange(desc(qtde_apres))%>%
  head(20)



# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizaÃ§Ãµes dos vÃ?deos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentaÃ§Ãµes resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicaÃ§Ã£o)
#   * a quantidade mÃ©dia de lÃ?nguas das apresentaÃ§Ãµes
#   * o desvio padrÃ£o da quantidade de lÃ?nguas
#   * o coeficiente de variaÃ§Ã£o da quantidade de lÃ?nguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÃÃES




# Calcule e classifique as seguintes correlaÃ§Ãµes
#     * Quantidade de visualizaÃ§Ãµes e Quantidade de lÃ?nguas
#     * Quantidade de visualizaÃ§Ãµes e DuraÃ§Ã£o
#     * Quantidade de visualizaÃ§Ãµes e Quantidade de ComentÃ¡rios
#     * Quantidade de ComentÃ¡rios e Quantidade de lÃ?nguas

library(ggcorrplot)

corr <-
  ted_main %>% 
  select_if(is_numeric) %>%
  mutate( views = as.numeric(views)
          , languages = as.numeric(languages)) %>%
  select(views, languages) %>%
  cor() %>% 
  round(2)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)


corr <-
  ted_main %>% 
  select_if(is_numeric) %>%
  mutate( views = as.numeric(views)
          , duration = as.numeric(duration)) %>%
  select(views, duration) %>%
  cor() %>% 
  round(2)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)


# Descarte os vÃ?deos cuja duraÃ§Ã£o seja maior que 3 desvios padrÃµes da mÃ©dia. Calcule novamente as 5 correlaÃ§Ãµes solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duraÃ§Ã£o dos vÃ?deos por ano de filmagem. Calcule a correlaÃ§Ã£o entre o ano e a mediana da duraÃ§Ã£o
# e interprete o resultado



# bibliotecas utilizadas
if (!"Hmisc" %in% installed.packages()) install.packages("Hmisc")
if (!"ggcorrplot" %in% installed.packages()) install.packages("ggcorrplot")

library(magrittr)
library(Hmisc)

ted_main %>%
  mutate(duracao =duration(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages )) +
  geom_point( alpha = .3 ) +
  scale_x_continuous( breaks = seq( from = 1970, to = 2020, by = 5 )) +
  theme_bw()


ted_talks_recentes <- ted_main %>%
  mutate(data_film = as_datetime(film_date))%>%
  filter(data_film >= ymd(20050101)) %>%
  mutate(languages = if_else(languages == 0, 1L, languages))


ted_talks_recentes %>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages )) +
  geom_point( alpha = .3 ) +
  scale_x_continuous( breaks = 2005:2017) +
  labs( x = "Ano de filmagem"
      , y = "Quantidade de Línguas"
      , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
      , subtitle = "Período considerado somente a partir de 2005. Dados ajustados para mínimo de 1 língua por apresentação."
      , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


ted_talks_recentes %>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages )) +
  stat_summary(fun.data = mean_sdl) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = -10, to = 60, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2005. Dados ajustados para mínimo de 1 língua por apresentação."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


# ATIVIDADE
# Repetir os gráficos de pontos e de sumário utilizando o ano de publicação no eixo x e a duração no eixo y.


ted_main %>%
  mutate(duracao =as.numeric(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( year = year( data_public )) %>%
  ggplot( aes( x = year, y = duracao )) +
  geom_point( alpha = .3 ) +
  scale_x_continuous( breaks = seq( from = 2000, to = 2020, by = 1 )) +
  theme_bw()

#select(duracao,data_film,data_public,year)

ted_main %>%
  mutate(duracao =as.numeric(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( year = year( data_public )) %>%
  ggplot( aes( x = year, y = duracao )) +
  geom_point( alpha = .3 ) +
  scale_x_continuous( breaks = 2000:2020) +
  labs( x = "Ano de publicacao"
        , y = "Duracao em segundos"
        , title = "Evolução da Duracao por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2000."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


ted_main %>%
  mutate(duracao =as.numeric(duration)) %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( year = year( data_public )) %>%
  ggplot( aes( x = year, y = duracao )) +
  stat_summary(fun.data = mean_sdl) +
  scale_x_continuous( breaks = 2000:2020) +
  scale_y_continuous( breaks = seq(from = 0, to = 3000, by = 500 )) +
  labs( x = "Ano de publicacao"
        , y = "Duracao em segundos"
        , title = "Evolução da Duracao por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2000."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()




#Gráficos de barras

ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  group_by(year) %>%
  summarise(sum_views = sum(views)) %>%
  ungroup() %>%
  ggplot( aes( x = year, y = sum_views )) +
  geom_col(fill="blue", alpha=0.6) +
  scale_x_continuous(breaks = 2005:2017) +
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark=",", scientific = FALSE)) +
  labs( x = "Ano de filmagem"
        , y = "Total de visualizações de apresentações"
        , title = "Exemplo com geom_col"
        , subtitle = "Exibição do total de visualizações de apresentações de um mesmo ano de filmagem") +
  theme_bw()

ggplot(ted_talks_recentes, aes( x = year( data_film ))) +
  geom_bar( fill="blue", color = "blue", alpha=0.6 ) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq( from = 50, to = 300, by = 50 )) +
  labs( x = "Ano de filmagem"
        , y = "Total de apresentações publicadas"
        , title = "Exemplo com geom_bar" ) +
  theme_bw()

ted_talks_recentes %>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( ano = year( data_public ), mes = month( data_public, label = TRUE )) %>%
  ggplot(aes( x = ano, fill = mes )) +
  geom_bar( alpha=0.6, color="black" ) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq( from = 50, to = 300, by = 50 )) +
  labs( x = "Ano de filmagem"
        , y = "Total de apresentações"
        , fill = "Mês do ano"
        , title = "Publicações por mês em cada ano" ) +
  theme_bw()


ted_talks_recentes %>%
  mutate(data_public = as_datetime(published_date))%>%
  mutate( ano = year( data_public ), mes = month( data_public, label = TRUE )) %>%
  ggplot(aes( x = ano )) +
  geom_bar( alpha=0.6 ) +
  scale_x_continuous( breaks = 2005:2017 ) +
  facet_wrap (~ mes, ncol = 3 ) +
  labs( x = "Ano de filmagem"
        , y = "Total de apresentações"
        , fill = "Mês do ano"
        , title = "Publicações por mês em cada ano" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages, group = year )) +
  geom_boxplot() +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = 0, to = 100, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages, group = year )) +
  geom_jitter(alpha = .2, height = 0, width = 0.3) +
  geom_boxplot(outlier.color = "red", outlier.alpha = 0.8, alpha = 0.2) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = 0, to = 100, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()


ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  ggplot( aes( x = year, y = languages )) +
  geom_jitter(alpha = .2, height = 0, width = 0.3) +
  stat_summary(fun.data = mean_sdl, color="red") +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = -10, to = 80, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da Quantidade de Línguas por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2005. Dados ajustados para mínimo de 1 língua por apresentação.\n O ponto é a média no ano e a barra vertical representa o intervalo de 2 desvios acima e abaixo da média."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()

ted_talks_recentes %>%
  mutate(data_film = as_datetime(film_date))%>%
  mutate( year = year( data_film )) %>%
  group_by(year) %>%
  mutate(low = mean(languages) - 2 * sd(languages), hi = mean(languages) + 2 * sd(languages)) %>%
  ungroup() %>%
  ggplot( aes( x = year, y = languages, ymin = low, ymax = hi )) +
  geom_ribbon(fill = "lightgray", alpha = 0.5) +
  geom_jitter(alpha = .2, height = 0, width = 0.5) +
  scale_x_continuous( breaks = 2005:2017 ) +
  scale_y_continuous( breaks = seq(from = -10, to = 80, by = 5 )) +
  labs( x = "Ano de filmagem"
        , y = "Quantidade de Línguas"
        , title = "Evolução da quantidade de línguas por vídeo ao longo dos anos"
        , subtitle = "Período considerado somente a partir de 2005. Dados ajustados para mínimo de 1 língua por apresentação.\n A faixa cinza correponde ao intervalo de 2 desvios padrão acima e abaixo da média, calculados ano a ano."
        , caption = "Dados de TED Talks de https://www.kaggle.com/rounakbanik/ted-talks/data") +
  theme_bw()

library(ggcorrplot)

corr <-
  ted_talks_recentes %>%
  select_if(is_numeric) %>%
  mutate( duration = as.numeric(duration)
          , published_date = as.numeric(published_date)
          , film_date = as.numeric(film_date)) %>%
  select(languages,views,comments,duration,num_speaker,film_date,published_date) %>%
  cor() %>% round(2)

ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)



# geom_histogram
