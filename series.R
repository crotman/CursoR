
library(tidyverse)
library(tsibble)
library(lubridate)

dados_gd <- read_rds("dados/gd/gd.rds")

meses <- tibble(mes = seq.Date(from = make_date(2009,1,1), to = make_date(2020,3,1), by = "month" ) %>%  yearmonth())




dados_gd_cum <- dados_gd %>% 
    rename(
        uf = USER_SigUF
    ) %>% 
    crossing(meses) %>% 
    filter(DthConexao < mes) %>%
    group_by(
        uf,
        mes
    ) %>% 
    summarise(
        n = n(),
        potencia = sum(USER_MdaPo)
    )


weather <- nycflights13::weather %>% 
    select(origin, time_hour, temp, humid, precip)

weather_tsbl <- as_tsibble(weather, key = origin, index = time_hour ) 

dados_gd_tsbl <- as_tsibble(dados_gd_cum, key = uf, index = mes ) 


full_weather <- weather_tsbl %>%
    fill_gaps(precip = 0) %>% 
    group_by_key() %>% 
    tidyr::fill(temp, humid, .direction = "down")

full_dados_gd <- dados_gd_tsbl %>%
    fill_gaps(
        n = 0,
        potencia = 0,
        .full = TRUE
    )

gd_anual <- full_dados_gd %>% 
    group_by_key() %>% 
    index_by(ano = ~year(.)) %>% 
    summarise(
        n = last(n),
        potencia = last(potencia)
    )

gd_trimestre <- full_dados_gd %>% 
    group_by_key() %>% 
    index_by(ano = ~yearquarter(.)) %>% 
    summarise(
        n = last(n), 
        potencia = last(potencia)
    )



























