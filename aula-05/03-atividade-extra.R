# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serÃ£o carregadas junto ao tidyverse

library(tidyverse)
library(lubridate)
library(stringr)
library(magrittr)
library(Hmisc)

# Crie um dataframe com o conteÃºdo do arquivo ted_main.csv.gz. 

ted_main <- read_csv("C:/Users/PC/Documents/data-analysis_with_R-201801-master/aula-05/data/ted_main.csv.gz")

# Visualize o resumo dos dados do dataframe. 
summary(ted_main)

# quantidade de visualizações por ano de publicação, restrito aos anos entre 2012 e 2017
ted_talks_recentes <- ted_main %>%
  mutate( data_public = as_datetime(published_date)
        , views = if_else(views == 0, 1L, views)
        , year = year(data_public)) %>%
filter(data_public >= ymd(20120101))

ggplot(ted_talks_recentes, aes( x = year )) +
  geom_bar( aes(weight = views),fill="blue", color = "blue", alpha=0.8 ) +
  scale_x_continuous( breaks = 2012:2017 ) +
  theme_bw()

# histograma da quantidade de visualizações por ano de publicação, restrito aos anos entre 2012 e 2017
  ggplot( ted_talks_recentes, aes( year )) +
  geom_histogram(aes(weight = views),binwidth = 1,breaks = 2012:2017,boundary = 1) +
  ylab("views")


# http://flowingdata.com/2014/02/27/how-to-read-histograms-and-use-them-in-r/

