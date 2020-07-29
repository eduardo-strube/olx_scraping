library(magrittr) # não vivo sem esse pacote
library(rvest) # principal pacote para web-scraping
library(readr) # usado para extrair numeros de texto
library(stringr) # usado para o data cleaning
library(curl) # usado como suporte para o rvest
library(tidyr) # data cleaning
library(dplyr) # data cleaning

url_apt <- "https://rj.olx.com.br/rio-de-janeiro-e-regiao/niteroi/icarai/imoveis/venda"
number_pages <- 88 #hard coded
# Criar vetor com todos os urls para as páginas do olx
lista_urls <- paste0(url_apt, "?o=", 1:number_pages, "&ros=3")

url_teste <- lista_urls[2]
system.time(df <- extrairAnuncios(url_teste, info_adicional = TRUE))

# Separar cidade e bairro
df %<>% separate(bairro, c("cidade", "bairro"), sep = ",")
head(df) %>% knitr::kable()

# substituir quartos por quarto
df$adicional %<>% str_replace_all("quartos", "quarto")
df$adicional %<>% str_replace_all("ou mais ", "")
df %<>% mutate(
  tem_quarto = str_detect(adicional, "quarto"),
  tem_area = str_detect(adicional, "m²"),
  tem_taxa = str_detect(adicional, "Condomínio"),
  tem_garagem = str_detect(adicional, "vaga")
)

x <- round(apply(df[, 7:10], 2, mean), 3) * 100
print(x)

# COLUNA DE QUANTIDADE DE QUARTOS
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

# COLUNA DE VALOR DE CONDOMINIO
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
    
    # Área
    matriz_posicao <- str_locate(df$adicional, "m²")
    # voltar quatro posições
    vetor_area <- str_sub(df$adicional, matriz_posicao[,1] - 4, matriz_posicao[, 1])
    # converter para numerico
    vetor_area %<>% readr::parse_number()
    # vendo se funcionou
    data.frame(df$adicional, vetor_area) %>% head(20)
    df$area_condominio <- vetor_area
    
    # Garagem
    matriz_posicao <- str_locate(df$adicional, " vaga")
    # voltar quatro posições
    vetor_garagem <- str_sub(df$adicional, matriz_posicao[,1] - 2, matriz_posicao[, 1])
    # converter para numerico
    vetor_garagem %<>% readr::parse_number()
    # vendo se funcionou
    data.frame(df$adicional, vetor_garagem) %>% head(20)
    # Funcionou! Incorporar ao data frame
    df$garagem <- vetor_garagem
    
    # Remover objetos desnecessários
    rm(matriz_posicao, vetor_adicional, vetor_area, vetor_garagem, vetor_quartos, vetor_taxa)
    