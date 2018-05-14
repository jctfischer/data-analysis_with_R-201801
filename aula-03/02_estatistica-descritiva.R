#' ---
#' title: "Estat�?stica Descritiva & R"
#' output:
#'   html_document:
#'     df_print: paged
#' ---
#' 
#' ## Medidas de tendência central e de dispersão
#' 
#' Nesta aula trabalharemos com 2 medidas de tendência central e duas medidas de dispersão:
#' 
#' - Medidas de Tendência Central
#'     + Média
#'     + Mediana
#' 
#' - Medidas de Dispersão
#'     + Desvio Padrão
#'     + Desvio Absoluto da Mediana
#' 
#' Na próxima aula continuaremos com mais algumas medidas de tendência central e de dispersão, bem como medidas de associação entre 2 variáveis
#' 
#' ### Dataset para práticas
#' 
#' Dados de remuneração de servidores públicos federais no mês de Fevereiro de 2018, obtidos do Portal da Transparência e pós-processados para uso nesta aula. 
#' 
## ----"Dataset", message=FALSE, warning=FALSE-----------------------------
library(tidyverse)

salarios <- read_csv("C:/Users/alu201830198/data-analysis_with_R-201801/aula-03/data/201802_dados_salarios_servidores.csv.gz")

head(salarios, 20)

#' 
## ------------------------------------------------------------------------
salarios %>%
  select(REMUNERACAO_REAIS, DATA_INGRESSO_ORGAO, DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO) %>%
  summary()

#' 
## ------------------------------------------------------------------------
salarios %>%
  filter(REMUNERACAO_REAIS > 900) %>%
  select(REMUNERACAO_REAIS, DATA_INGRESSO_ORGAO, DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO) %>%
  summary()

#' 
#' 
## ------------------------------------------------------------------------
salarios %>%
  filter(REMUNERACAO_REAIS > 900, !is.na(UF_EXERCICIO)) %>%
  select(ID_SERVIDOR_PORTAL, REMUNERACAO_REAIS, DESCRICAO_CARGO, DATA_INGRESSO_ORGAO, ORGSUP_EXERCICIO, DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO, UF_EXERCICIO) ->
  subset_salarios

#' 
#' 
#' # Medidas
#' 
#' Recapitulando a discussão sobre dados tabulares, temos a definição de uma série de observações (*registros*) que apresentam um mesmo conjunto de variáveis (*features*). 
#' 
#' A quantidade de observações é limitada pela capacidade de coleta (tempo, espaço, custo), de armazenamento, ou pelo tamanho da população. 
#' 
#' > Variáveis, portanto, podem apresentar uma quantidade muito grande de valores.
#' 
#' Utilizamos estat�?sticas descritivas para resumir e descrever o conjunto de valores que uma variável representa.
#' 
#' ```
#' Nota: Não farei distinção entre população e amostra exceto quando indicado
#' ```
#' 
#' ## Medidas de Tendência Central
#' 
#' Medidas de posição apresentam o valor t�?pico de uma variável
#' 
#' > Uma estimativa do valor esperado para a variável
#' 
#' ### Média Aritmética
#' 
#' $$Media = \overline{x} = \frac{\sum _{i=1}^{n}x_{i}}{n}$$
#' 
#' em R:
#' 
## ----eval=FALSE----------------------------------------------------------
## # Implementação padrão
## mean(x, trim = 0, na.rm = FALSE, ...)

#' 
#' ### Exemplos com salário. 
#' 
#' Qual o salário médio de um servidor público federal nos dados obtidos junto ao portal da transparência?
#' 
## ------------------------------------------------------------------------
mean(subset_salarios$REMUNERACAO_REAIS)

#' 
#' E o salário médio por UF?
#' 
## ------------------------------------------------------------------------
subset_salarios %>%
  group_by(UF_EXERCICIO) %>%
  summarise(salario_medio = mean(REMUNERACAO_REAIS), servidores = n()) %>%
  ungroup() %>%
  arrange(desc(salario_medio))

#' 
#' Uma propriedade da média aritmética é ela é a medida de tendência central que minimiza o res�?duo (diferença entre o valor real e o valor estimado) da amostra. A soma dos res�?duos  sempre será zero.
#' 
## ------------------------------------------------------------------------
subset_salarios %>%
  mutate(residuo = REMUNERACAO_REAIS - mean(REMUNERACAO_REAIS)) %>%
  select(residuo) %>%
  mutate(ganho = if_else( sign(residuo) == 1, "ACIMA", "ABAIXO")) %>%
  group_by(ganho) %>%
  summarise(soma_residuo = sum(residuo), servidores = n()) %>%
  ungroup()

#' 
#' > Qual interpretação você dá para a diferença na quantidade de servidores que recebem acima e abaixo da média nacional?
#' 
#' #### Deficiências da Média Aritmética
#' 
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00011.jpg)
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00012.jpg)
#' 
## ------------------------------------------------------------------------
subset_salarios %>%
  mutate(remuneracao = round(REMUNERACAO_REAIS, -2)) %>% # Arredonda para centenas
  count(remuneracao) %>%
  ggplot( aes(x = remuneracao, y = n)) +
  geom_col() +
  geom_vline(xintercept = mean(subset_salarios$REMUNERACAO_REAIS), color="red", size=0.5) +
  geom_point(data = filter(subset_salarios, REMUNERACAO_REAIS > 40000 ), mapping = aes(x=REMUNERACAO_REAIS), inherit.aes = FALSE, y=0, color="darkgreen", size=1, alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) +
  theme_minimal()

#' 
#' >> ATIVIDADE
#' 
#' Utilizando a função `year`, adicione ao dataset o Ano de Ingresso. A partir desta nova variável, determine o tempo médio de trabalho dos servidores, em n�?vel nacional e por UF. Utilizar a data do campo `DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO`. Nos dois casos, utilizar a combinação das funções `summarise` e `mean`.
#' 
  
#' Por fim, determine a média salarial por ano de ingresso.
#' 



## ------------------------------------------------------------------------
print("Atividade")
## Modificar o Dataset para criação de nova variável
subset_salarios %>%
  mutate( ANO_INGRESSO  = year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO) ) -> subset_sal_tempo

## Determine o tempo médio de trabalho em anos, em n�?vel nacional

<<<<<<< HEAD
subset_sal_tempo %>%
  summarise(tempo_medio = mean(year(today()) - ANO_INGRESSO), servidores = n()) %>%
  ungroup()
=======
subset_com_ano <- subset_salarios %>%
  mutate(ano_ingresso = year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO)) 

## Determine o tempo médio de trabalho em anos, em nível nacional
subset_com_ano %>%
  summarise(tempo_medio = mean(year(today()) - ano_ingresso))
>>>>>>> upstream/master

## Determine o tempo médio de trabalho em anos, por UF
subset_com_ano %>%
  group_by(UF_EXERCICIO) %>%
  summarise(tempo_medio = mean(year(today()) - ano_ingresso)) %>%
  arrange(desc(tempo_medio)) %>% View()

subset_sal_tempo %>%
  group_by(UF_EXERCICIO) %>%
  summarise(tempo_medio = mean(year(today()) - ANO_INGRESSO), servidores = n()) %>%
  ungroup() %>%
  arrange(desc(tempo_medio)) -> subset_sal_tempo_medio

## Determine a média salarial por ano de ingresso
subset_com_ano %>%
  group_by(ano_ingresso) %>%
  summarise(media_salarial = mean(REMUNERACAO_REAIS)) %>%
  arrange(desc(media_salarial))


subset_sal_tempo %>%
  group_by(ANO_INGRESSO) %>%
  summarise(sala_medio = mean(REMUNERACAO_REAIS), servidores = n()) %>%
  ungroup() %>%
  arrange(desc(ANO_INGRESSO)) -> subset_media_sal_ano

#' >> FIM DA ATIVIDADE
#' 
#' ### Mediana
#' 
#' A mediana é o elemento central do conjunto (**ordenado**) de valores de uma variável.
#' 
#' * A figura do elemento central só existe quando o número de observações é �?mpar!
#' * Quando o tamanho for par, o elemento central por definicão será a média entre os dois valores mais ao centro. Algumas variações assumem o menor dentre os dois valores, ou o maior dentre os dois valores.
#' 
#' Em R:
## ----eval=FALSE----------------------------------------------------------
## median(x, na.rm = FALSE, ...)

#' 
#' 
#' ### Exemplos com salário. 
#' 
#' Mediana dos salários:
## ------------------------------------------------------------------------
median( subset_salarios$REMUNERACAO_REAIS )

#' 
## ------------------------------------------------------------------------
subset_salarios %>%
  mutate(remuneracao = round(REMUNERACAO_REAIS, -2)) %>% # Arredonda para centenas
  count(remuneracao) %>%
  ggplot( aes(x = remuneracao, y = n)) +
  geom_col() +
  geom_vline(xintercept = mean(subset_salarios$REMUNERACAO_REAIS), color="red", size=0.5, linetype="dashed") +
  geom_vline(xintercept = median(subset_salarios$REMUNERACAO_REAIS), color="blue", size=0.5, linetype="dashed") +
  geom_point(data = filter(subset_salarios, REMUNERACAO_REAIS > 40000 ), mapping = aes(x=REMUNERACAO_REAIS), inherit.aes = FALSE, y=0, color="darkgreen", size=1, alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(subtitle = "Média em vermelho e Mediana em azul") +
  theme_minimal()

#' 
#' >> ATIVIDADE
#' 
#' __Atividade I__
#' 
#' Crie um novo dataset contendo a média e a mediana do salário por UF. Adicione uma nova variável determinando, para cada UF, se a média é maior ou menor que a mediana. Ao final, exiba a quantidade de UFs onde a mediana foi maior que a média.
#' 
## ------------------------------------------------------------------------
print("Atividade")

## Código aqui

#' 
#' __Atividade II__
#' 
#' Qual sua justificativa para a quantidade de casos onde a mediana foi maior que a média? Dica: Observe o gráfico que mostra a média e a mediana. Há cauda longa? Em qual direção?
#' 
#' ``` SUA RESPOSTA AQUI ```
#' 
#' >> FIM DA ATIVIDADE
#' 
#' #### O que a Mediana _não_ nos diz?
#' 
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00013.jpg)
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00014.jpg)
#' 
#' ## Medidas de Variabilidade
#' 
#' ~~Variabilidade~~ Dispersão:
#' 
#' Além de saber o elemento t�?pico do conjunto, é importante conhecer o quanto os valores se aproximam deste elemento t�?pico, ou se estão espalhados/distribu�?dos em posições distantes do elemento central.
#' 
#' A medida de dispersão mais utilizada com Média Aritmética é o Desvio Padrão. 
#' 
#' Já em relação à Mediana, tanto o Desvio Absoluto da Mediana quanto o Intervalo Interquartil são bastante utilizados. Nesta aula utilizaremos o Desvio Absoludo da Mediana.
#' 
#' ### Medidas baseadas na Média
#' 
#' O principal conceito para entendimento do Desvio Padrão está relacionado ao cálculo da média em si.
#' 
#' Como vimos anteriormente, a soma dos res�?duos é zero. Ao tentar sumarizar as distâncias dos valores em relação à média perdemos a própria noção de distância. O cálculo do Desvio Padrão utiliza o conceito de Variância para contornar este problema.
#' 
#' #### Variância
#' 
#' A variância utiliza o quadrado do res�?duo para eliminar o sinal negativo. Além do efeito de transformar todos res�?duos em valores positivos, o uso do quadrado possui propriedades que facilitam os cálculos de otimizações (a derivada do quadrado é de fácil aplicação).
#' 
#' $$Variancia = s^{2} = \frac{1}{n-1} \times \sum_{i=1}^{n}(x_{i} - \overline{x})^{2}$$
#' 
#' ```Notar que a variância é indefinida para amostra de tamanho 1```
#' 
#' Em R:
## ----eval=FALSE----------------------------------------------------------
## var(x, y = NULL, na.rm = FALSE, use)

#' 
## ------------------------------------------------------------------------
var(subset_salarios$REMUNERACAO_REAIS)

#' 
#' Ao elevar os res�?duos ao quadrado estamos também modificando a unidade de medida do res�?duo. Enquanto subtra�?mos a média da observação, como no exemplo dos salários, temos o res�?duo na mesma unidade de medida (R$ no nosso caso). Ao calcular a variância, passamos para uma unidade de medida irreal, que seria algo como $R \$ ^{2}$
#' 
#' #### Desvio padrão
#' 
#' O desvio padrão é definido pela ra�?z quadrada da variância:
#' 
#' $$ s = \sqrt{Variancia} $$
#' em R:
#' 
## ----eval=FALSE----------------------------------------------------------
## sd(x, na.rm = FALSE)

## ------------------------------------------------------------------------
sd( subset_salarios$REMUNERACAO_REAIS )

#' 
#' Ao aplicar a ra�?z quadrada, retornamos para a unidade de medida original, que é em R\$. Temos então um desvio padrão de R\$ 6578,81. Ao comparar com a média, que é de R\$ 9954,77, observamos que um desvio corresponde a 66% da média. Não há uma regra geral para intepretação desta proporção, mas vamos considerar que temos uma grande dispersão de valores em torno da média. Essa medida é chamada de __Coeficiente de Variação__, e tem a caracter�?stica de ser independente de unidade de medida.
#' 
## ------------------------------------------------------------------------
subset_salarios %>%
  mutate(remuneracao = round(REMUNERACAO_REAIS, -2)) %>% # Arredonda para centenas
  count(remuneracao) %>%
  ggplot( aes(x = remuneracao, y = n)) +
  geom_col() +
  geom_vline(xintercept = mean(subset_salarios$REMUNERACAO_REAIS) - sd(subset_salarios$REMUNERACAO_REAIS), color="red", size=0.5, linetype="dashed") +
  geom_vline(xintercept = mean(subset_salarios$REMUNERACAO_REAIS) + sd(subset_salarios$REMUNERACAO_REAIS), color="red", size=0.5, linetype="dashed") +
  geom_vline(xintercept = mean(subset_salarios$REMUNERACAO_REAIS), color="blue", size=0.5, linetype="dashed") +
  geom_point(data = filter(subset_salarios, REMUNERACAO_REAIS > 40000 ), mapping = aes(x=REMUNERACAO_REAIS), inherit.aes = FALSE, y=0, color="darkgreen", size=1, alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(subtitle = "Média em azul e 1 Desvio Padrão da média em vermelho") +
  theme_minimal()

#' 
#' >> ATIVIDADE
#' 
#' __Atividade I__
#' 
#' A [Inequalidade de Chebyshev](https://en.wikipedia.org/wiki/Standard_deviation#Chebyshev's_inequality) afirma que, para distribuições de probabilidade onde o Desvio Padrão é definido, 2 Desvios Padrão da média devem absorver pelo menos 75% do tamanho da amostra.
#' 
#' Verifique a validade deste teorema com os valores calculados.
#' 
## ------------------------------------------------------------------------
print("Atividade")

## Código aqui

#' 
#' __Atividade II__
#' 
#' No dataset de salários temos os diferentes cargos ocupados pelos servidores públicos federais. Liste os 10 cargos de __menor coeficiente de variação__ cujo cargo tenha mais que 100 servidores públicos. A lista deve conter, além do cargo e Coeficiente de Variação, a quantidade de servidores, o menor salário, o maior salário, o salário médio e o desvio padrão.
#' 
## ------------------------------------------------------------------------
print("Atividade")

## Código aqui

#' 
#' __Atividade III__
#' 
#' Repita a Atividade II, mas listando aqueles com __maior coeficiente de variação__.
#' 
## ------------------------------------------------------------------------
print("Atividade")

## Código aqui

#' 
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00021.jpg)
#' ![](https://mathwithbaddrawings.files.wordpress.com/2016/07/20160712085402_00022.jpg)
#' 
#' >> FIM DA ATIVIDADE
#' 
#' ### Medidas baseadas na Mediana
#' 
#' Vimos, nas medidas de Tendência Central, que a Mediana é mais robusta que a média na presença de _Outliers_
#' 
#' Da mesma forma, as medidas de dispersão baseadas na mediana também não sofrem influência de valores extremos.
#' 
#' 
#' #### Desvio absoluto da mediana
#' 
#' O Desvio Absoluto da Mediana é definido como a mediana dos res�?duos absolutos:
#' 
#' No R:
#' 
## ------------------------------------------------------------------------
(dam_salario <- median( abs( subset_salarios$REMUNERACAO_REAIS - median( subset_salarios$REMUNERACAO_REAIS ))))

#' 
#' Lembrando que a mediana é de:
## ------------------------------------------------------------------------
(md_salario <- median( subset_salarios$REMUNERACAO_REAIS ))

#' 
#' O desvio absoluto da mediana corresponde a 
## ------------------------------------------------------------------------
dam_salario / md_salario

#' 
## ------------------------------------------------------------------------
subset_salarios %>%
  mutate(remuneracao = round(REMUNERACAO_REAIS, -2)) %>% # Arredonda para centenas
  count(remuneracao) %>%
  ggplot( aes(x = remuneracao, y = n)) +
  geom_col() +
  geom_vline(xintercept = md_salario - dam_salario, color="red", size=0.5, linetype="dashed") +
  geom_vline(xintercept = md_salario + dam_salario, color="red", size=0.5, linetype="dashed") +
  geom_vline(xintercept = md_salario, color="blue", size=0.5, linetype="dashed") +
  geom_point(data = filter(subset_salarios, REMUNERACAO_REAIS > 40000 ), mapping = aes(x=REMUNERACAO_REAIS), inherit.aes = FALSE, y=0, color="darkgreen", size=1, alpha = 0.3) +
  coord_cartesian(ylim = c(0, 10000)) +
  labs(subtitle = "Mediana em azul e 1 Desvio Absoluto da Mediana em vermelho") +
  theme_minimal()

#' 
#' 
