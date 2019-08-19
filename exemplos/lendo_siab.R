library(tidyverse)
library(sidrar)
library(readxl)
library(scales)
library(lubridate)

#pegando o código dos municipios

municipios_ibge <- read_excel("dados/ibge/municipios.xlsx", 
                              col_names = c("cod_municipio", "municipio")
                              ) %>% 
    separate(municipio, 
             into = c("municipio", "uf"),
             sep = "\\(", 
             extra = "merge"
             ) %>%
    mutate(uf = str_replace(uf, "\\)", "")) %>% 
    mutate(
        cod_municipio = as.numeric(cod_municipio)
    )

#pegando o número de famílias cadastradas no siab

siab_num_familias <- read_csv2(
    "dados/siab/cadastro_numero_familias.csv", 
    skip = 3, 
    locale = locale(encoding = "latin1" )
    ) %>% 
    rename(municipio = "Município") %>% 
    separate(municipio, into = c("cod_municipio", "municipio"), sep = " ", extra = "merge" ) %>% 
    gather(ano, n_familias, -municipio, -cod_municipio) %>% 
    mutate(n_familias =  str_replace(n_familias, "-", "0")) %>% 
    mutate(
        n_familias = as.numeric(n_familias),
        cod_municipio = as.numeric(cod_municipio) 
        ) %>%
    filter(!is.na(cod_municipio)) 

#pegando o número de famílias estimadas no IBGE


ibge_num_familias <- get_sidra(api = "/t/1940/n1/all/n3/all/v/155/p/all/c1/6795/c12057/99568/d/v155%200") %>%
    select(
        nivel = "Nível Territorial (Código)",
        cod_uf = "Brasil e Unidade da Federação (Código)",
        uf = "Brasil e Unidade da Federação",
        ano =  "Ano",
        n_familias = "Valor"
    ) %>% 
    mutate(nivel = as.numeric(nivel)) %>% 
    mutate(cod_uf = as.numeric(cod_uf) ) %>% 
    mutate(n_familias = n_familias*1000 ) %>% 
    filter(nivel == 3)


#O número de municípios no IBGE vem com um dígito verificador. Vamos tirar
municipios_ibge  <-  municipios_ibge %>% 
    mutate(cod_municipio = cod_municipio %/% 10 ) %>% 
    mutate(cod_uf = cod_municipio %/% 10000 ) 


siab_num_familias_por_uf <- siab_num_familias %>% 
    left_join(municipios_ibge, by = c("cod_municipio")) %>% 
    group_by(ano, uf, cod_uf) %>% 
    summarise(n_familias = sum(n_familias)) 


compara_ibge_siab <- siab_num_familias_por_uf %>% 
    inner_join(ibge_num_familias, by = c("cod_uf", "ano"), suffix = c("_siab","_ibge")) %>% 
    select(-uf_ibge, -nivel, - cod_uf, uf = uf_siab) %>% 
    gather(escopo, n_familias, -ano, -uf ) %>% 
    mutate(escopo = str_replace(escopo,"n_familias_", ""))


compara_ibge_siab_brasil <- compara_ibge_siab %>% 
    group_by(ano, escopo) %>% 
    summarise(n_familias = sum(n_familias)/1000000)

fracao_ibge_siab_brasil <- siab_num_familias_por_uf %>% 
    inner_join(ibge_num_familias, by = c("cod_uf", "ano"), suffix = c("_siab","_ibge")) %>% 
    select(-uf_ibge, -nivel, - cod_uf, uf = uf_siab) %>% 
    group_by(ano) %>% 
    summarise(cobertura = sum(n_familias_siab)/sum(n_familias_ibge))

             
ggplot(compara_ibge_siab_brasil) +
    geom_col(aes(x = ano, y = n_familias, fill = escopo ), position = "dodge") +
    labs(y = "milhões de famílias")

ggplot(fracao_ibge_siab_brasil) +
    geom_col(aes(x = ano, y = cobertura)) +
    geom_text(aes(x = ano, y = cobertura, label = percent(cobertura)  ), nudge_y = 0.05, size = 3 ) +
    scale_y_continuous(labels = percent_format(accuracy = 2)) 



mes <- tibble(n_mes = 1:12, mes = c("Jan","Fev","Mar","Abr","Mai","Jun", "Jul", "Ago", "Set","Out", "Nov", "Dez"))

siab_hanseniase <- read_csv2(
    "dados/siab/atendimento_hanseniase.csv", 
    skip = 3, 
    locale = locale(encoding = "latin1" )
) %>% 
    rename(municipio = "Município") %>% 
    separate(municipio, into = c("cod_municipio", "municipio"), sep = " ", extra = "merge" ) %>% 
    gather(ano, atend_hanseniase, -municipio, -cod_municipio) %>% 
    mutate(atend_hanseniase =  str_replace(atend_hanseniase, "-", "0")) %>% 
    mutate(
        atend_hanseniase = as.numeric(atend_hanseniase),
        cod_municipio = as.numeric(cod_municipio) 
    ) %>%
    filter(!is.na(cod_municipio)) %>% 
    filter(ano != "Total") %>% 
    separate(ano, sep = "/", into = c("ano", "mes")) %>% 
    inner_join(mes, by = c("mes")) %>% 
    mutate(data = make_datetime(as.numeric(ano), as.numeric(n_mes),1)) %>% 
    select(-mes, -n_mes, -ano)


siab_hanseniase_brasil <- siab_hanseniase %>% 
    group_by(data) %>% 
    summarise(atend_hanseniase = sum(atend_hanseniase)/1000)


ggplot(siab_hanseniase_brasil ) +
    geom_line(aes(x = data, y = atend_hanseniase))
    

ggplot(siab_hanseniase_brasil %>% filter(atend_hanseniase < 100) ) +
    geom_line(aes(x = data, y = atend_hanseniase))






