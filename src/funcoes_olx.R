
# definir função para limpar strings coletadas
limparString <- function(x) {
  # x = string coletado do olx
  x %<>% str_replace_all("[\t]", "")
  x %<>% str_replace_all("[\n]", "")
  x %<>% str_replace_all("Apartamentos", "")
  x %<>% str_replace_all("Anúncio Profissional", "")
  x %<>% str_replace("-", "")
  x %<>% str_replace_all("[R$]", "")
  x %<>% str_replace_all("[.]", "")
  x %<>% str_trim()
  return(x)
}


extrairAnuncios <- function(url_pagina, info_adicional) {
  ### INPUTS:
  # url_pagina: url de uma pagina do olx com uma lista de links de anúncios.
  # info_adicional: variavel booleana. se verdadeiro, faz o scraping de dados adicionais do anuncio
  # ... deve ser usado apenas para apartamentos, pois a sintaxe do html para quartos é diferente
  mycurl <- curl(url_pagina)
  mycurl <- read_html(mycurl, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  
  x <- mycurl %>% html_nodes(".fnmrjs-0")
  
  # extrair link do anuncio
  col_links <- mycurl %>% html_nodes(".fnmrjs-0") %>% html_attr("href")
  # extrair titulo do anuncio
  col_titles <- mycurl %>% html_nodes(".fnmrjs-0") %>% html_attr("title")
  
  # extrair preço
  precos <- mycurl %>% html_nodes(".fnmrjs-16")
  precos %<>% lapply(html_text)
  precos %<>% unlist()
  precos %<>% limparString()
  precos %<>% as.numeric()
  col_precos <- precos
  
  # extrair bairros
  bairros <- mycurl %>% html_nodes(".fnmrjs-13") %>% html_text()
  col_bairros <- bairros
  
  # extrair informações adicionais de apartamento
  
  if (info_adicional) {
    adicional <- mycurl %>% html_nodes(".jm5s8b-0") %>% html_text()
    adicional %<>% str_replace_all("[\t]", "")
    adicional %<>% str_replace_all("[\n]", "")
    col_adicionais <- adicional
    
  }
  return(data.frame(link = col_links,
                    titulo = col_titles,
                    preco = col_precos,
                    bairro = col_bairros,
                    adicional = col_adicionais,
                    stringsAsFactors = FALSE))
}
