library(magrittr) # não vivo sem esse pacote
library(rvest) # principal pacote para web-scraping
library(readr) # usado para extrair numeros de texto
library(stringr) # usado para o data cleaning
library(curl) # usado como suporte para o rvest
library(tidyr) # data cleaning
library(dplyr) # data cleaning
library(purrr)

source("src/funcoes_olx.R")

url_apt <- "https://rj.olx.com.br/rio-de-janeiro-e-regiao/niteroi/icarai/imoveis/venda"
number_pages <- 100 #hard coded

# Criar vetor com todos os urls para as páginas do olx
lista_urls <- paste0(url_apt, "?o=", 1:number_pages, "&sf=1")

url_teste <- lista_urls[9]
system.time(df <- extrairAnuncios(url_teste, info_adicional = TRUE))

df <- map_df(lista_urls, extrairAnuncios_tryCatch, info_adicional = TRUE)

## LIMPEZA DOS DADOS

  # Separar cidade e bairro
    df %<>% separate(bairro, c("cidade", "bairro"), sep = ",")

  # Para facilitar a limpeza  
  # substituir 'quartos' por 'quarto' e 'ou mais' por ''
    df$adicional %<>% str_replace_all("quartos", "quarto")
    df$adicional %<>% str_replace_all("ou mais ", "")

  df %<>% mutate(
    tem_quarto = str_detect(adicional, "quarto"),
    tem_area = str_detect(adicional, "m²"),
    tem_taxa = str_detect(adicional, "Condomínio"),
    tem_garagem = str_detect(adicional, "vaga")
  )

  # quantos anuncios tem info de quarto, area, condominio e garagem?
    x <- round(apply(df[, 7:10], 2, mean), 3) * 100
    print(x)

  # Construindo Coluna de Quantidade de Quartos
    # Quarto: pegar posicao inicial e final do string quarto
    # Localizar trecho dentro do string referente a quartos
      matriz_posicao <- str_locate(df$adicional, "quarto")
    # Voltar 2 posições no string para pegar o número (ex: 2 quarto)
      matriz_posicao[,1] <- matriz_posicao[,1] - 2
    # extrair string com posições iniciais e finais
      vetor_quartos <- str_sub(df$adicional, matriz_posicao[,1], matriz_posicao[,2])
    # extrair apenas número (primeiro caractere do string) e converter para numeric
      vetor_quartos <- str_sub(vetor_quartos, 1, 1)
      vetor_quartos %<>% as.numeric()
    # adicionar ao data frame
      df$qtd_quarto <- vetor_quartos

  # Construindo Coluna valor da Taxa de Condominio
    # retirar cifrao pra ficar mais facil
      df$adicional %<>% str_replace_all("\\$", "S")
      matriz_posicao <- str_locate(df$adicional, "Condomínio: RS ")
    # mover cinco posicoes para pegar algarismos após o RS
      vetor_taxa <- str_sub(df$adicional, matriz_posicao[, 2], matriz_posicao[, 2] + 5)
      vetor_taxa %<>% str_replace_all("\\|", "")
      vetor_taxa %<>% str_replace_all("\\.", "")
    # extrair apenas numeros
      vetor_taxa %<>% readr::parse_number()
    # vendo se funcionou
      data.frame(df$adicional, vetor_taxa) %>% head(20)    
    # Funcionou! Incorporar vetor ao data frame
      df$taxa_condominio <- vetor_taxa
      
  # Construindo Coluna valor de Area
      matriz_posicao <- str_locate(df$adicional, "m²")
      # voltar quatro posições
      vetor_area <- str_sub(df$adicional, matriz_posicao[,1] - 4, matriz_posicao[, 1])
      # converter para numerico
      vetor_area %<>% readr::parse_number()
      vetor_area %<>% na_if(0)
      # vendo se funcionou
      data.frame(df$adicional, vetor_area) %>% head(20)
      df$area_condominio <- vetor_area
      
  # Construindo Coluna para Garagem
      matriz_posicao <- str_locate(df$adicional, " vaga")
      # voltar quatro posições
      vetor_garagem <- str_sub(df$adicional, matriz_posicao[,1] - 2, matriz_posicao[, 1])
      # converter para numerico
      vetor_garagem %<>% readr::parse_number()
      vetor_garagem %<>% replace_na(0)
      # vendo se funcionou
      data.frame(df$adicional, vetor_garagem) %>% head(20)
      # Funcionou! Incorporar ao data frame
      df$garagem <- vetor_garagem
      
  # Remover objetos desnecessários
    rm(matriz_posicao, vetor_adicional, vetor_area, vetor_garagem, vetor_quartos, vetor_taxa)
    
  write.csv(df, 'dados/olx_data.csv')
  