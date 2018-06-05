#' ---
#' title: "Probabilidade"
#' output:
#'   html_document:
#'     df_print: paged
#' ---
#' 
## ----setup, echo=FALSE, warning=FALSE, message=FALSE, error=FALSE, include=FALSE----
library(tidyverse)
Sys.setlocale("LC_ALL", "pt_BR")
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE, out.width = "600px", out.height="400px")

#' 
#' ## Restante das aulas
#' 
#' 1. 04/06 - Probabilidade, Arquivos Excel
#' 2. 11/06 - Acesso a bancos SQL, Teste de Hipóteses (Paramétricos & Não-paramétricos)
#' 3. 18/06 - Formatos de Arquivos (JSON, XML), Análise de Componentes Principais
#' 
#' ## Distribuições de Probabilidade
#' 
#' - De maneira bastante informal, as distribuições de probabilidade são listas de valores possíveis com suas probabilidades associadas.
#' - O conjunto de valores pode ser discreto quando os possíveis resultados forem contáveis (enumeráveis) ou contínuo caso contrário.
#'     + Distribuições de variáveis aleatórias contínuas são descritas por uma [Função de Densidade de Probabilidade](https://pt.wikipedia.org/wiki/Fun%C3%A7%C3%A3o_densidade).
#'     + Distribuições de variáveis aleatórias discretas são descritas por uma [Função de Massa de Probabilidade](https://pt.wikipedia.org/wiki/Fun%C3%A7%C3%A3o_massa_de_probabilidade).
#' 
#' Distribuições descritas por funções são conhecidas como Distribuições Paramétricas. As suas funções possuem parâmetros que estabelecem os valores possíveis e suas probabilidades.
#' 
#' 
#' ## Distribuições mais conhecidas / utilizadas
#' 
#' #### Uniforme (Variável Discreta e Variável Contínua)
#' - A distribuição Uniforme atribui a mesma probabilidade para cada valor possível entre um mínimo e um máximo. Apesar de não possuir parâmetros, costuma ser definida por $b=max$, $a=min$.
#' 
#' - Valor esperado: (max + min) / 2
#' 
#' ##### Exemplo: 
#' 
#' Simulação de Jogo de Dados com 10.000 repetições. Cada face tem igual probabilidade, de $\dfrac{1}{6} = 0.166\overline6$, e o valor esperado é $\dfrac{6 + 1}{2} = 3.5$
#' 
## ----"Simulação de Jogo de Dados"----------------------------------------
set.seed(201806)
(tb_dados <- table(purrr::rdunif(10000, a=1, b=6)))

#' 
#' Proporcionalmente:
#' 
## ------------------------------------------------------------------------
prop.table(tb_dados)

#' 
#' 
#' ### Variáveis aleatórias discretas
#' 
#' #### Bernoulli
#' 
#' Probabilidade $p$ de Sucesso / Fracasso, onde $p$ é o parâmetro da função. 
#' 
#' - Um jogo de Cara ou Coroa com moeda não viciada é descrito por uma distribuição de Bernoulli com parâmetro $p=0,5$. 
#' - A probabilidade global de uma pessoa ser destra é de 90%, descrita por uma distribuição de Bernoulli com parâmetro $p=0,9$.
#'     + Essa mesma proporção já foi observada em exames ultrassom.
#' - A probabilidade de um brasileiro ser destro é de 96%, descrita por uma distribuição de Bernoulli com parâmetro $p=0,96$.
#' 
#' Em R, simulamos uma distribuição de Bernoulli através da função `rbernoulli`, cujos parâmetros são a quantidade de eventos e a probabilidade do parâmetro $p$. Como o processo de Bernoulli é formado por saídas binárias (acerto/erro, 0/1, cara/coroa) a função retorna um vetor de valores lógicos.
#' 
#' Seu valor esperado é definido por $p$, e a Variância é definida por $p\times(1 - p)$
#' 
## ----"Distribuição Bernoulli", warning=FALSE-----------------------------
x <- 1:7
options(scipen=999)

set.seed(201806)

(bern <- 
  data_frame(Pessoas = 10 ^ x) %>%
  mutate( Destros = map_int(Pessoas, ~ sum(rbernoulli(.x, p = 0.9))) ) %>%
  mutate( Canhotos = Pessoas - Destros))

#' 
#' Considerando o intervalo de 1 desvio padrão em torno da média, a proporção de destros no Brasil está dentro do esperado em relação à proporção global?
#' 
## ----"Variância Bernoulli", warning=FALSE--------------------------------
prob_destro_global <- 0.9
variancia_prob_destro <- prob_destro_global * (1 - prob_destro_global)

lo_int <- prob_destro_global - sqrt( variancia_prob_destro )
hi_int <- prob_destro_global + sqrt( variancia_prob_destro )

between(0.96, lo_int, hi_int)

#' 
#' Essa análise diretamente sobre as proporções faz sentido?
#' 
#' #### Binomial
#' 
#' - Distribuição do número $k$ de sucessos/acertos de um processo de Bernoulli após $n$ eventos.
#' 
#' **Importante:** Os exemplos abaixo assumem que as observações utilizadas são amostras independentes. Canhotismo tem a ver com hereditariedade e mesmo uma insistência familiar/social pelo uso da mão direita.
#' 
## ----"Binomial", warning=FALSE-------------------------------------------
prob_destro_brasil <- 0.96
amostra_populacao <- 100000
variancia_prob_destro <- prob_destro_brasil * (1 - prob_destro_brasil) * amostra_populacao
sd_prob_destro_brasil <- sqrt( variancia_prob_destro )

lo_sd <- 96000 - (2 * sd_prob_destro_brasil)
hi_sd <- 96000 + (2 * sd_prob_destro_brasil)

range_prop_destros <- seq(from=95500, to=96500, by=10)

# Para fechar em 100% tive que multiplicar a densidade (probabilidade) por 10, pois o intervalo foi de 10 em 10
df_binom_probs <- data_frame(x = range_prop_destros, y=dbinom(x, size = 100000, prob = prob_destro_brasil) * 10)

ggplot(df_binom_probs, aes(x, y=cumsum(y))) +
  geom_line() +
  geom_vline(xintercept = lo_sd, alpha = 0.5, linetype = "dashed") +
  geom_label(x = lo_sd, y = 1.0, label = round( lo_sd, 0 )) +
  geom_vline(xintercept = hi_sd, alpha=0.5, linetype = "dashed") +
  geom_label(x = hi_sd, y = 0.0, label = round( hi_sd, 0 )) +
  scale_x_continuous( breaks = seq( from = 95500, to = 96500, by = 100 )) +
  labs(x = "Destros", y = "Probabilidade Acumulada") +
  theme_bw() +
  theme( axis.text.x = element_text( angle = 45, hjust = 1 ))

#' 
#' - Considere uma cidade com 100 mil habitantes. Qual a probabilidade de a cidade ter 96,1 mil destros ou mais quando a probabilidade esperada é de 96%?
#' 
## ----"Probabilidade Binomial"--------------------------------------------
pbinom( 96100, size = 100000, prob = 0.96, lower.tail = FALSE )

#' 
#' ##### Pausa
#' 
#' - Canhoto/Destro é mesmo uma variável categórica? 
#'     + [Edinburgh Handedness Inventory](https://en.wikipedia.org/wiki/Edinburgh_Handedness_Inventory)
#' 
#' #### Poisson
#' 
#' - A distribuição de Poisson descreve o comportamento do número de eventos que ocorrem em um espaço determinado de tempo. 
#'     + Quantidade de carros que passam em uma rodovia por hora, 
#'     + Quantidade de produtos vendidos em um dia, etc. 
#' 
#' - A distribuição de Poisson é definida pelo parâmetro $lambda$, que determina também o valor esperado (média) e a variância de uma distribuição de Poisson.
#' 
#' - Por exemplo, um produto cuja venda diária é descrita por uma distribuição de Poisson com $lambda=18$ vendeu somente 10 unidades no dia de hoje. Qual a probabilidade deste produto vender até o máximo de 10 unidades dado o histórico de vendas do produto?
#' 
## ----"Probabilidade Poisson"---------------------------------------------
paste0(round(ppois(10, lambda = 18) * 100, 2), '%')

#' 
#' - Qual o valor máximo de venda diária esperada para este produto? (100% de probabilidade)
## ----"Quantidade Máxima Poisson Inf"-------------------------------------
qpois(1, lambda = 18, lower.tail = TRUE)

#' - Assume-se uma probabilidade de 99,9%
## ----"Quantidade Máxima Poisson"-----------------------------------------
qpois(1 - 0.001, lambda = 18, lower.tail = TRUE)

#' 
#' - Qual a menor quantidade esperada dada esta mesma possibilidade (agora de 0.1%)?
## ----"Quantidade Mínima Poisson"-----------------------------------------
qpois(0.001, lambda = 18, lower.tail = TRUE)

#' 
#' - Qual a distribuição de probabilidades?
#' 
## ----Poisson-------------------------------------------------------------
df_pois_probs <- data_frame(x = 4:35, y=dpois(4:35, lambda = 18) * 100)

ggplot(df_pois_probs, aes(x=x, y=y)) +
  geom_col() +
  geom_label(aes(label = round(cumsum(y), 1)), size = 2.5, nudge_y = .6) +
  scale_x_continuous(name = "Quantidade vendida", breaks=4:35) +
  scale_y_continuous(name = "Prob (%)", breaks=0:15) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45))

#' 
## ------------------------------------------------------------------------
df_gauss_probs <- data_frame(x = 4:35, y=dnorm( 4:35, mean = 18, sd = sqrt( 18 )) * 100)

ggplot(df_gauss_probs, aes( x = x, y = y )) +
  geom_col() +
  geom_label(aes(label = round(cumsum(y), 1)), size = 2.5, nudge_y = .6) +
  scale_x_continuous(name = "Quantidade vendida", breaks=0:35) +
  scale_y_continuous(name = "Prob (%)", breaks=0:15) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45))

#' 
#' 
#' Agora um produto com venda média representada por $\lambda = 10$.
#' 
## ------------------------------------------------------------------------
df_pois_probs_10 <- data_frame(x = 0:24, y=dpois(0:24, lambda = 10) * 100)

ggplot(df_pois_probs_10, aes(x=x, y=y)) +
  geom_col() +
  geom_label(aes(label = round(cumsum(y), 1)), size = 2.5, nudge_y = .6) +
  scale_x_continuous(name = "Quantidade vendida", breaks=0:24) +
  scale_y_continuous(name = "Prob (%)", breaks=0:15) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45))

#' 
## ------------------------------------------------------------------------
df_gauss_probs_10 <- data_frame(x = -2:24, y=dnorm( -2:24, mean = 10, sd = sqrt( 10 )) * 100)

ggplot(df_gauss_probs_10, aes( x = x, y = y )) +
  geom_col() +
  geom_label(aes(label = round(cumsum(y), 1)), size = 2.5, nudge_y = .6) +
  scale_x_continuous(name = "Quantidade vendida", breaks=-2:24) +
  scale_y_continuous(name = "Prob (%)", breaks=0:15) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45))

#' 
#' 
#' #### Geométrica
#' 
#' - Modela o tempo de espera até o primeiro sucesso de uma sequência de ensaios de Bernoulli. Tal como as distribuições de Bernoulli e Binomial, é parametrizada por $p$. 
#' 
#' - Quantas coroas em sequência até obter uma cara?
#'     + Lembrando que cada arremesso tem igual probabilidade, independente do resultado anterior.
#' 
## ----Geometrica----------------------------------------------------------
df_geom_probs <- data_frame(x = 0:10, y=dgeom(0:10, prob = 0.5) * 100)

ggplot(df_geom_probs, aes(x=x, y=y)) +
  geom_col() +
  scale_x_continuous(name = "Coroas até uma Cara", breaks=0:10) +
  scale_y_continuous(name = "Prob (%)", breaks=seq(from=0, to=50, by=5)) +
  theme_light()

#' 
## ------------------------------------------------------------------------
set.seed(201806)

# Gera uma sequência de 200 eventos de cara / coroa
sample_head_tails <- rbernoulli(500)

# Conta a quantidade de caras em sequência e de coroas em sequência
seq_head_tails <- rle(sample_head_tails)

# Quais as sequências de coroas?
seq_head_tails$lengths[!seq_head_tails$values]

# Vamos comparar a tabela com a distribuição de probabilidades plotada acima
round( prop.table( table(seq_head_tails$lengths[!seq_head_tails$values]) ), 2)

#' 
#' - Comparada com a Distribuição Binomial:
#'     + Binomial: Quantos eventos de sucesso?
#'     + Geométrica: Quantas falhas até o sucesso?
#' 
#' Se você ligar para a operadora em um dia de queda de conectividade de internet você tem 10% de chance de ser atendido a cada ligação. Qual a probabilidade de você ter sua oportunidade de reclamar após 6 ligações frustradas?
#' 
## ------------------------------------------------------------------------
pgeom(6, prob=0.1, lower.tail = TRUE)

#' 
#' >> ATIVIDADE EM AULA
#' 
#' 1. Faça o gráfico da distribuição de probabilidades de chamadas telefônicas até 20 ligações e simule 500 eventos de Bernoulli para esta mesma probabilidade. Nesta simulação, identifique quantas sequências de 6 falhas ocorreram. Use como _seed_ os últimos 5 dígitos da sua matrícula. Veja no exemplo anterior o uso da função `rle`.
df_geom_problig <- data_frame(x = 0:20, y=pgeom(0:20, prob = 0.1) * 100)

ggplot(df_geom_problig, aes(x=x, y=y)) +
  geom_col() +
  scale_x_continuous(name = "Perdas at� atender", breaks=0:20) +
  scale_y_continuous(name = "Prob (%)") +
  theme_light()

set.seed(30198)

# Gera uma sequ�ncia de 500 eventos de atende/perde
sample_fone_tails <- rbernoulli(500, p = 0.1)

# Conta a quantidade de atendes em sequ�ncia e de perdas em sequ�ncia
seq_fone_tails <- rle(sample_fone_tails)

# Quais as sequ�ncias de liga�oes?
seq_fone_tails$lengths[!seq_fone_tails$values]

round( prop.table( table(seq_head_tails$lengths[!seq_head_tails$values]) ), 2)

#' 2. Você criou um sistema para reclamações da demora do atendimento de ligações telefônicas durante quedas de conectividade da Internet, e exige que os usuários acertem um CAPTCHA antes de postarem uma reclamação. Você observou que a probabilidade de um usuário acertar o CAPTCHA exibido no seu sistema é de 70%. 
#' 
#' - Seu sistema de monitoramento identificou que um usuário tentou 5 CAPTCHAS diferentes antes de conseguir reclamar do tempo de atendimento na última queda de conectividade. 
#'     + Qual a probabilidade de um usuário acertar o CAPTCHA após 5 tentativas fracassadas? Qual o mínimo de tentativas para que a probabilidade seja maior que 50%?
#'     
#'     + Você observou que, das últimas 500 _tentativas_ de publicação de reclamações, 340 acertaram a validação de CAPTCHA. Qual a probabilidade de uma quantidade entre 320 e 350 tentativas passarem pela validação de CAPTCHA a cada 500 tentativas? Dada a probabilidade de 70% de sucesso, qual o número esperado de publicações a cada 500 CAPTCHAS? DICA: ESTAMOS TRATANDO DA DISTRIBUIÇÃO BINOMIAL.
#' 
#' >> FIM ATIVIDADE
#' 
#' ### Variáveis aleatórias contínuas
#' 
#' #### Gaussiana
#' 
#' - A mais conhecida dentre as distribuições de probabilidade. Conhecida também como distribuição Normal ou "curva de sino".
#' - O que a torna tão relevante?
#'     + [Teorema Central do Limite](https://pt.wikipedia.org/wiki/Teorema_central_do_limite)
#' - Seus parâmetros, $\mu$ e $\sigma$ conectam-se facilmente com as médias e desvio padrão calculados.
#' 
#' Ranking global de alturas
#' 
#' - [Brasileiro cresce em altura nos últimos cem anos, mas ainda é 'baixinho'; conheça o ranking global](http://g1.globo.com/ciencia-e-saude/noticia/2016/07/brasileiro-cresce-em-altura-nos-ultimos-cem-anos-mas-ainda-e-baixinho-conheca-o-ranking-global.html)
#' - Estudo publicado [A century of trends in adult human height](https://elifesciences.org/articles/13410)
#' - Dados disponíveis [para download](http://ncdrisc.org/data-downloads-height.html)
#' 
## ------------------------------------------------------------------------
br_height <- 
  read_csv("aula-07/data/Brazil.csv") %>% 
  rename(year = Year_of_birth, height = Mean_height, lo_95 = Mean_height_lower_95perc, hi_95 = Mean_height_upper_95perc) %>%
  mutate(Sex = factor(Sex))

ggplot(br_height, aes(x=year, y=height, ymin=lo_95, ymax=hi_95)) +
  geom_line(color="black") +
  geom_ribbon(fill="lightgrey", alpha=.6) +
  facet_wrap(~ Sex) +
  theme_minimal()


#' 
#' >> ATIVIDADE EM AULA
#' 
#' 1. Utilizando o data frame br_height e as operações do pacote __dplyr__ (__tidyverse__), selecione os dados de altura (height), menor altura dentro do IC (lo_95) e maior altura dentro do IC (hi_95) de acordo com o seu sexo e ano de nascença. Crie uma variável que é a divisão de sua altura pela média, e outra que informa se a sua altura está dentro ou fora do intervalo de confiança. Em aula, informe o professor sobre os 2 resultados.
#' 
#' 2. Baixe o relatório do [LEVANTAMENTO DO PERFIL ANTROPOMÉTRICO DA POPULAÇÃO BRASILEIRA USUÁRIA DO TRANSPORTE AÉREO NACIONAL – PROJETO CONHECER](http://www2.anac.gov.br/arquivos/pdf/Relatorio_Final_Projeto_Conhecer.pdf) e obtenha a média e o desvio padrão da amostra deste relatório (página 23).
#' 
#' 3. Considerando que o estudo da ANAC foi realizado entre os anos de 2004 e 2008, e que a média de idade é de 40 anos, com Desvio Padrão de idade de 12 anos, e assumindo como premissa que a altura da pessoa se mantem entre os 20 e os 60 anos, temos um intervalo de aproximadamente 1.65 desvios padrão da média. Utilizando a função `pnorm`, calcule os percentuais de 20 anos e 60 anos com a média (mean), e desvio padrão (sd) obtidos neste relatório. Utilize o parâmtro `lower.tail = FALSE` para 60 anos e `lower.tail = TRUE` para 20 anos. Quais são os valores obtidos? Conclua quanto representa, em percentual, os 1.65 desvios padrão.
#' 
#' 4. Assumindo que a altura aos 18 anos equivale à altura dos 20 aos 60 anos, selecione do data frame br_height a altura média de todas as pessoas que tinham entre 20 e 60 anos entre os anos de 2004 e 2008. Calcule a média de altura de homens e de mulheres neste período. Realize todo este exercício utilizando o __dplyr__. Responda: Com base nas alturas médias obtidas, você acha que mulheres participaram deste estudo?
#' 
#' 5. A altura média dos homens calculada no exercício 4 está quantos desvios-padrão acima/abaixo da média anotada no exercício 2?
#' 
#' 6. Baixe os seguintes arquivos:
#' - [Antropometria e estado nutricional de crianças, adolescentes e adultos no Brasil](https://ww2.ibge.gov.br/home/estatistica/populacao/condicaodevida/pof/2008_2009_encaa/defaulttabzip_brasil.shtm), baixe o arquivo Tabela Completa Brasil.
#' - [link de tabelas por UF](https://ww2.ibge.gov.br/home/estatistica/populacao/condicaodevida/pof/2008_2009_encaa/defaulttabzip_UF.shtm), baixe a tabela dos estados do Rio Grande do Sul e do Sergipe.
#' 
#' 7. Calcule a média ponderada da altura dos homens a partir de 18 anos até o grupo de 45 a 54 anos para os 3 arquivos.
#' 
#' 8. Para cada arquivo, determine a quantidade de grupos de idade que estão abaixo da média obtida no exercício 2.
#' 
#' >> FIM ATIVIDADE
