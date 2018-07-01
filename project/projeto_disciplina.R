# Descrição dos dados: https://tech.instacart.com/3-million-instacart-orders-open-sourced-d40d29ead6f2
# Estamos trabalhando com somente uma amostra do total de pedidos. O dataset abaixo não possui 3 milhões de pedidos ;)
library( tidyverse )

departments <- read_csv("project/departments.csv")                   # Cadastro de Departamentos
aisles <- read_csv("project/aisles.csv")                             # Cadastro de "Corredores"
products <- read_csv("project/products.csv")                         # Cadastro de Produtos

insta_orders <- read_csv( "project/orders_instacart.csv" )           # Amostra de pedidos de usuários
insta_products <- read_csv( "project/order_products_instacart.csv" ) # Produtos que compõe os pedidos

#1 # Quantos dos produtos do cadastro nunca foram comprados?

insta_products %>% 
  group_by(product_id)%>%
  count() -> subset_products_orders

subset_products_orders%>%
  pull(`product_id`) -> produtos_comprados

products %>% 
  filter( !product_id %in% produtos_comprados )%>%count()


#2 # Crie um dataframe com os dados combinados de produtos, corredores e departamentos. 
products %>%
  inner_join( departments, by = "department_id" )%>%
  inner_join( aisles, by = "aisle_id" )-> tb_product_dept_aisle


#3 # Quais as 10 combinações corredor + departamento que possuem mais produtos cadastrados? Use o dataframe da atividade #2.
tb_product_dept_aisle%>%
  group_by(aisle_id, department_id)%>%
  count()%>%ungroup()%>%
  arrange(desc(n))%>%head(10)

tb_product_dept_aisle%>%
  group_by(aisle,department)%>%
  count()%>%ungroup()%>%
  arrange(desc(n))%>%head(10)


#4 # Qual o percentual de pedidos que possuem algum produto dos pares 'corredor + departamento' da atividade anterior?

tb_product_dept_aisle%>%
  group_by(aisle_id, department_id)%>%
  count()%>%ungroup()%>%
  arrange(desc(n))%>%head(10)->dez_mais

products %>%
  inner_join( dez_mais, by = c("department_id","aisle_id") )-> tb_product_dez_mais

insta_products %>%
  inner_join( tb_product_dez_mais, by = "product_id")%>%
group_by(product_id)%>%ungroup()%>%count()->qtd_dez_mais

insta_products %>%count()->qtd_tot_pedidos

print( qtd_dez_mais / qtd_tot_pedidos * 100 , digits = 3)



#5 # Crie um novo dataframe de produtos em pedidos retirando aqueles produtos que não estão categorizados (usar resultado das atividades 3 e 4)

tb_product_dept_aisle%>%
  filter(department_id!=21,aisle_id!=100)->tb_prod_dept_aisle_sem_miss

tb_prod_dept_aisle_sem_miss%>%
  group_by(aisle_id,department_id)%>%
  count()%>%ungroup()%>%
  arrange(desc(n))%>%head(10)->dez_mais

products %>%
  inner_join( dez_mais, by = c("department_id","aisle_id") )-> tb_product_dez_mais

insta_products %>%
  inner_join( tb_prod_dept_aisle_sem_miss, by = "product_id")->insta_product_sem_miss

#6 # Crie um dataframe que combine todos os dataframes através das suas chaves de ligação. Para produtos de pedidos, use o dataframe da atividade 4
   # Transforme as variáveis user_id, department e aisle em factor
   # Transforme a variável order_hour_of_day em um factor ordenado (ordered)

   # Este dataframe deverá ser utilizado em todas as atividades seguintes

insta_orders%>%
  inner_join( insta_product_sem_miss, by = "order_id" )-> tb_tudo

tb_tudo%>%
  mutate( user_id = factor(user_id)
        , department = factor(department)
        , aisle = factor(aisle))->tb_tudo
tb_tudo%>%
  mutate( order_hour_of_day = factor(order_hour_of_day, ordered = TRUE))->tb_tudo
          

#7 # Identifique os 5 horários com maior quantidade de usuários que fizeram pedidos
tb_tudo %>%
  group_by(order_hour_of_day) %>%
  summarise(count_user_id = n_distinct(user_id)) %>%
  arrange(desc(count_user_id)) %>%
  head(n = 5) %>%
  ungroup() -> cinco_hour_day

#8 # Quais os 15 produtos mais vendidos nestes 5 horários? Identifique os produtos e a quantidade total nestes horários (total geral, não por hora)
tb_tudo %>%
  inner_join(cinco_hour_day, by = 'order_hour_of_day') %>%
  group_by(product_id, product_name) %>%
  summarise(qtd = n()) %>%
  arrange(desc(qtd)) %>%
  head(15) -> top_products

#9 # Calcule a média de vendas por hora destes 15 produtos ao longo do dia,
   # e faça um gráfico de linhas mostrando a venda média por hora destes produtos. 
   # Utilize o nome do produto para legenda da cor da linha.
   # Você consegue identificar algum produto com padrão de venda diferente dos demais? 

top_products %>%
  inner_join(tb_tudo, by = 'product_id') %>%
  inner_join(products, by = 'product_id') %>%
  group_by(product_id, product_name, order_hour_of_day, order_dow) %>%
  summarise(qtd_hora = n()) %>%
  ungroup() %>%
  group_by(product_id, product_name, order_hour_of_day) %>%
  summarise(media_qtd_hora = mean(qtd_hora)) %>%
  ungroup() -> ordens_produto_hora 


library(ggplot2)
library(ggcorrplot)

ggplot(data = ordens_produto_hora,
       aes(x = order_hour_of_day,
           y = media_qtd_hora,
           group = product_name) ) +
  geom_line(aes(color = product_name)) +
  geom_point(aes(color = product_name)) +
    labs(x = 'Hora',
       y = 'Quant',
       title = 'Top 15 Produtos por Hora',
       colour = 'Produtos')+
  theme_bw()


#10 # Calcule as seguintes estatísticas descritivas sobre a quantidade de pedidos por dia, para cada hora do dia. O resultado final deve ser exibido para cada hora do dia:
    # Média, Desvio Padrão, Mediana, Mínimo e Máximo
    # Considerando os valores calculados, você acredita que a distribuição por hora é gaussiana? 
tb_tudo %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(qtd_hora = n_distinct(order_id)) %>%
  group_by(order_hour_of_day) %>%
  summarise(media = mean(qtd_hora),
            desvio_padrao = sd(qtd_hora),
            mediana = median(qtd_hora),
            minimo = min(qtd_hora),
            maximo = max(qtd_hora)) %>%
  ungroup()

#11 # Faça um gráfico da média de quantidade de produtos por hora, com 1 desvio padrão para cima e para baixo em forma de gráfico de banda

#/data-analysis_with_R-201801/aula-05/02-graficos-ggplot2.nb.html

tb_tudo %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(qtd_hora = n_distinct(order_id)) %>% 
ggplot( aes( x = order_hour_of_day, y = qtd_hora )) +
  stat_summary(fun.data = mean_sdl) +
labs(x = 'Hora',
     y = 'Quantidade',
     title = 'Media Produtos por Hora') +
  theme_bw()

#12 # Visualize um boxplot da quantidade de pedidos por hora nos 7 dias da semana. O resultado deve ter order_dow como eixo x.

#/data-analysis_with_R-201801/aula-05/02-graficos-ggplot2.nb.html

tb_tudo %>%
  group_by(order_dow, order_hour_of_day) %>%
  summarise(qtd_hora = n_distinct(order_id)) %>% 
  ggplot( aes( x = order_dow, y = qtd_hora, group = order_dow )) +
  geom_boxplot() +
  scale_x_continuous( breaks = 0:6 ) +
  labs(x = 'Dia',
       y = 'Quantidade',
       title = 'Media Pedidos por Dia') +
  theme_bw()


#13 # Identifique, por usuário, o tempo médio entre pedidos
tb_tudo %>%
  group_by(user_id) %>%
  summarise(tempo_medio = mean(days_since_prior_order)) %>%
  ungroup()

#14 # Faça um gráfico de barras com a quantidade de usuários em cada tempo médio calculado

tb_tudo %>%
  group_by(user_id) %>%
  summarise( tempo_medio = mean(days_since_prior_order)) %>%
  group_by(tempo_medio) %>%
  summarise(qtd_usuarios = n_distinct(user_id)) %>%
  ungroup()%>%
ggplot( aes( x = tempo_medio, y = qtd_usuarios )) +
  geom_col(fill="blue", alpha=0.6) +
labs(x = 'Tempo Medio',
     y = 'Quantidade Usuarios',
     title = 'Quantidade de usuarios em cada tempo medio')
  
#15 # Faça um gráfico de barras com a quantidade de usuários em cada número de dias desde o pedido anterior. Há alguma similaridade entre os gráficos das atividades 14 e 15? 

tb_tudo %>%
  group_by(days_since_prior_order) %>%
  summarise(qtd_usuarios = n_distinct(user_id)) %>%
  ungroup()%>%
  ggplot( aes( x = days_since_prior_order, y = qtd_usuarios )) +
  geom_col(fill="blue", alpha=0.6) +
  labs(x = 'Tempo Entre Pedidos',
       y = 'Quantidade Usuarios',
       title = 'quantidade de usuários em cada tempo médio')


#16 # Repita o gráfico da atividade 14 mantendo somente os usuários com no mínimo 5 pedidos. O padrão se mantém?
insta_orders %>%
  group_by(user_id) %>% 
  summarise(qtd_pedidos = n_distinct(user_id)) %>% filter(qtd_pedidos>1)->qtd_ped_user
  
# cada usuario tem apenas 1 pedido !!!!!!!!!!!!!!!!!!!!!!!!!!!!

#  summarise( tempo_medio = mean(days_since_prior_order)) %>%
#  group_by(tempo_medio) %>%
#  summarise(qtd_usuarios = n_distinct(user_id)) %>%
#  ungroup()%>%
#  ggplot( aes( x = tempo_medio, y = qtd_usuarios )) +
#  geom_col(fill="blue", alpha=0.6) +
#  labs(x = 'Tempo Medio',
#       y = 'Quantidade Usuarios',
#       title = 'quantidade de usuários em cada tempo médio')


#17 # O vetor abaixo lista todos os IDs de bananas maduras em seu estado natural.
    # Utilizando este vetor, identifique se existem pedidos com mais de um tipo de banana no mesmo pedido.
bananas <- c(24852, 13176, 39276, 37067, 29259)
insta_products%>%
  filter(product_id %in% bananas)%>%
  group_by(order_id)%>%
  summarise(qtde_banana = n_distinct(product_id)) %>%
  ungroup()%>%
  filter(qtde_banana>1)%>%
  head(20)

#18 # Se existirem, pedidos resultantes da atividade 17, conte quantas vezes cada tipo de banana aparece nestes pedidos com mais de um tipo de banana.
    # Após exibir os tipos de banana, crie um novo vetor de id de bananas contendo somente os 3 produtos de maior contagem de ocorrências
tb_tudo%>%
  filter(product_id %in% bananas)%>%
  group_by(order_id)%>%
  summarise(qtde_banana = n_distinct(product_id)) %>%
  ungroup()%>%
  filter(qtde_banana>1)%>%
  pull(`order_id`) -> pedido_banana

tb_tudo%>%
  filter(product_id %in% bananas)%>%
  filter(order_id %in% pedido_banana)%>%
    group_by(product_id)%>%
  summarise( cada_banana = n())%>%
  arrange(desc(cada_banana))->mais_bananas

mais_bananas

mais_bananas %>% 
pull(`product_id`)-> bananas
bananas<-bananas[1:3]


#19 # Com base no vetor criado na atividade 18, conte quantos pedidos de, em média, são feitos por hora em cada dia da semana. 


#20 # Faça um gráfico dos pedidos de banana da atividade 19. O gráfico deve ter o dia da semana no eixo X, a hora do dia no eixo Y, 
    # e pontos na intersecção dos eixos, onde o tamanho do ponto é determinado pela quantidade média de pedidos de banana 
    # nesta combinação de dia da semana com hora


#21 # Faça um histograma da quantidade média calculada na atividade 19, facetado por dia da semana


#22 # Teste se há diferença nas vendas por hora entre os dias 3 e 4 usando o teste de wilcoxon e utilizando a simulação da aula de testes

