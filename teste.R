library(gapminder)
library(gganimate)


fundos_escolhidos <- tribble(  
    ~cnpj,                       ~nome,
    "07.455.507/0001-89",        "Verde",
    "12.798.221/0001-36",        "Nimitz",
    "17.087.932/0001-16",        "Maraú",
    "31.666.755/0001-53",        "Legacy",
    "23.884.632/0001-60",        "Adam",
    "11.419.627/0001-06",         "Itaú"
)

cores <- c(
    "Nimitz" = "seashell4", 
    "Verde" = "seagreen", 
    "Maraú" = "darkred",
    "Itaú" = "darkorange3",
    "Legacy" = "lightgoldenrod4",
    "Adam" = "lightblue"
)


dados_fundos_escolhidos <- todos_os_fundos %>% 
    inner_join(
        fundos_escolhidos,
        by = c("CNPJ_FUNDO" = "cnpj")
    ) %>% 
    filter(
        !is.na(VL_PATRIM_LIQ) & VL_PATRIM_LIQ > 1000000000
    )



ggplot(dados_fundos_escolhidos) +
    geom_line(
        aes(
            x = DT_COMPTC,
            y = VL_PATRIM_LIQ,
            color = nome,
            group = nome
        )
    )



dados_fundos_escolhidos_rank <- dados_fundos_escolhidos %>%  
    group_by(DT_COMPTC) %>% 
    mutate(
        casas = log10(max(VL_PATRIM_LIQ)) %>%  floor(),
        VL_PATRIM_LIQ = VL_PATRIM_LIQ / 10^(casas - 9),
        rank = rank(VL_PATRIM_LIQ),
        rank = (6 - max(rank)) + rank,
        pl = VL_PATRIM_LIQ/max(VL_PATRIM_LIQ)
    ) %>% 
    select(
        casas,
        everything() 
    ) %>%
    ungroup() %>% 
    mutate(
        PL_texto = number(VL_PATRIM_LIQ / 1000000, accuracy = 1, big.mark = ".", decimal.mark = ","),
        label = str_glue("{nome}: {PL_texto}")
    )
    



my_plot <- ggplot(dados_fundos_escolhidos_rank, aes(group = nome, y = nome, x = rank, label = nome, fill = nome) ) +
    geom_tile(
        aes(
            y = pl/2,
            height = pl,
            width = 0.9,
            fill = nome
        )
    ) +
    geom_text(
        aes( 
            y = pl,
            label = label,
            hjust = ifelse(pl > 0.5, 1, 0),
            size = 3
        ) 
    ) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_fill_manual(values = cores) +
    # Here comes the gganimate specific bits
    labs(title = 'Data: {frame_time}', x = '', y = '') +
    transition_time(DT_COMPTC) +
    ease_aes("cubic-in-out") +
    theme_minimal() +
    theme(
        plot.title = element_text(color = "#01807e", face = "bold", hjust = 0, size = 30),
        axis.ticks.y = element_blank(), #removes axis ticks
        axis.text.y = element_blank(), #removes axis ticks
        axis.ticks.x = element_blank(), #removes axis ticks
        axis.text.x = element_blank(), #removes axis ticks
        panel.grid.major = element_blank(), #removes grid lines
        panel.grid.minor = element_blank(), #removes grid lines
        legend.position = "none",
    )

animate(my_plot, nframes = 500, fps = 15, width = 400, height = 400, renderer = gifski_renderer())

anim_save(filename = "teste2.gif")

