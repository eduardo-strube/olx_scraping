library(magrittr) # não vivo sem esse pacote
library(tidyr) # data cleaning
library(dplyr) # data cleaning
library(ggplot2) # graficos
library(RColorBrewer)
library(stringr)
library(plotly)

install.packages('ggplot2')

azul = "#01a2d9" # definir cor para usar nos gráficos

# limitando o preco do imovel para abaixo de 12 Milhões
df2 <- df %>% filter(preco < 12000000) %>% mutate(preco = preco/1000)


# histograma com intervalos de 25 mil
ggplot(df2, aes(preco)) +
  geom_histogram(breaks  = seq(0, 13000, 100), fill = azul) +
  coord_cartesian(xlim = c(0, 13000)) +
  scale_x_continuous(breaks  = seq(0, 12000, 1000)) +
  labs(x = "Preço de Venda (Mil R$)", y = "Quantidade de apartamentos")


# histograma com limite até 5 milhoes e intervalos de 25 mil
ggplot(df2, aes(preco)) +
  geom_histogram(breaks  = seq(0, 5000, 25), fill = azul) +
  coord_cartesian(xlim = c(0, 2000)) +
  scale_x_continuous(breaks  = seq(0, 5000, 100)) +
  labs(x = "Preço de Venda (Mil R$)", y = "Quantidade de apartamentos")

# histograma para apto de 3 qtos
ggplot(filter(df2, qtd_quarto == 3), aes(preco)) +
  geom_histogram(breaks  = seq(0, 5000, 25), fill = azul) +
  coord_cartesian(xlim = c(0, 2000)) +
  scale_x_continuous(breaks  = seq(0, 5000, 100)) +
  labs(x = "Preço de Venda (Mil R$)", y = "Quantidade de apartamentos")

# quantos quartos (anuncios?)
ggplot(df, aes(x = qtd_quarto)) +
  geom_bar(fill = azul) +
  labs(x = "Quartos", y = "Quantidade de apartamentos")
