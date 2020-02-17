library(shiny)
library(tidyverse)
library(reactable)
library(sf)
library(waiter)
library(ggmap)
library(RColorBrewer)
library(scales)

#### LEITURA DE DADOS ####

gd <- read_rds("dados/gd/gd.rds") %>% 
    mutate(
        unidade = 1
    )

#### VARIÁVEIS DE ESTADO GLOBAIS ####

ultimos_pontos <- gd
ultimo_grafico <- NA
ultimo_n_amostra <- 0

#### OPÇÕES DOS SELECTS ####

agrupadores <- c(
    "Setor econômico" = "USER_DscCl", 
    "Tarifa" = "USER_DscGr",  
    "Tipo de geração" = "USER_DscMo", 
    "Município" =  "USER_NomMu", 
    "Região" = "USER_NomRe", 
    "UF" = "USER_SigUF",
    "Tipo de fonte" = "USER_SigTi", 
    "Combustível" = "USER_DscCo",
    "Agente" = "USER_SigAg"
)


somas <- c(
    
    "Quantidade" = "unidade",
    "Potência (MW)" = "USER_MdaPo"
)

#### ELEMENTOS DE UI ####


mapa <- plotOutput(
    "mapa", 
    height = "600px",
    brush = brushOpts(
        id = "brush_mapa",
        #resetOnNew = TRUE,
        opacity = 0.2,
        stroke = "black",
        fill = "white",
        clip = FALSE,
        delay = 5000
    )
    
)


selectUFs <- selectInput(
    "ufs",
    "UFs:",
    choices = unique(gd$USER_SigUF) %>% sort(),
    multiple = TRUE
)

slider_n_amostra <- sliderInput(
    "n_amostra",
    label = "% amostrado no gráfico",
    min = 10,
    max = 100,
    step = 10,
    value = 10,
    post = "%"
    
)

slider_tamanho_ponto <- sliderInput(
    "tamanho_ponto",
    "Tamanho do ponto",
    min = 0,
    max = 2,
    step = 0.05,
    value = 0.05
    
)


agrupador <- selectInput(
    "campo_grupo",
    "Agrupar por:",
    choices = agrupadores
)


soma_por <- selectInput(
    "campo_soma",
    "Somar por:",
    choices = somas
)


tabela_count <- reactableOutput("info")

grafico_barras <- plotOutput("grafico_barras")

#### DIAGRAMAÇÃO DA UI ####


ui <- fluidPage(
    theme = "bootstrap.css",
    use_waiter(),
    titlePanel(
        fluidRow(
            column(width = 1, img(height = 40, src = "logo-epe-azul-15-anos.gif")),
            column(
                width = 11,
                tags$div(style = "height:10px"),
                h3("Dashboard Geração Distribuída")
                
            )
        )    
    ),
    tags$hr(),
    sidebarLayout(
        sidebarPanel(
            h4("Filtros" %>% tags$b()),
            wellPanel(
                selectUFs 
            ),
            h4("Configurações do gráfico" %>% tags$b()),
            wellPanel(
                agrupador,
                soma_por,
                slider_n_amostra,
                slider_tamanho_ponto
            ),
            width = 3
        ),
        mainPanel(
            width = 9,
            splitLayout(
                cellWidths =  c("60%","40%"),
                div(style = 'overflow: hidden',mapa),
                div(style = 'overflow: hidden',
                    verticalLayout(
                        grafico_barras,
                        tabela_count
                    )
                )
            )
        ) 
    )
)




server <- function(input, output, session) {
    
    #### MAPA ####
    
    
    output$mapa <- renderPlot({
        
        
        pontos_selecionados <- pontos()

        if(isTRUE(all_equal(ultimos_pontos, pontos_selecionados)) & ultimo_n_amostra == input$n_amostra)
        {
            resposta <- ultimo_grafico
        }
        else
        {
            print("atualiza")

            xmin <- min(pontos_selecionados$X)
            ymin <- min(pontos_selecionados$Y)
            xmax <- max(pontos_selecionados$X)
            ymax <- max(pontos_selecionados$Y)
            
            margem_x <- min((ymax - ymin)/7,2)
            margem_y <- min((xmax - xmin)/7,2)
            
            xmin <- xmin - margem_x 
            ymin <- ymin - margem_y
            xmax <- xmax + margem_x 
            ymax <- ymax + margem_y
            
            
    
            if (is.na(xmin)){
                local <- c(-67.85551, -31.76278, -34.80921, -1.198088)
            }
            else{
                local <- c(xmin, ymin, xmax, ymax)    
            }
            
            map = get_map(local,source = "stamen", maptype = "toner-lite") 
            
            
            resposta <- ggmap(ggmap = map) +
                stat_density_2d(
                    aes(x = X, y = Y, fill = ..level.. ), 
                    bins = 30,
                    geom = "polygon", 
                    data = pontos_selecionados ,
                    alpha = .1
                ) +
                geom_point(
                    data = pontos_selecionados, 
                    aes(
                        x = X,
                        y = Y
                    ),
                    alpha = 0.01,
                    color = "darkblue",
                    size = input$tamanho_ponto
                ) +
                scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd")) +  
                theme_inset()
            
            ultimo_grafico <<- resposta 
            ultimos_pontos <<- pontos_selecionados
            ultimo_n_amostra <<- input$n_amostra 
        
        }

        resposta 
        
    })

    #### PONTOS DO MAPA SELECIONADOS ####

        
    pontos <- reactive({
        
        
        waiter <- Waiter$new(id = "mapa")$show()

        resposta <- brushedPoints(gd_sample(), input$brush_mapa, xvar = "X", yvar = "Y") 
        

        if (nrow(resposta) == 0){
            resposta <- gd_sample()
        }
        

        resposta
    })

    #### DADOS SELECIONADOS PELO MAPA####
    
        
    dados <- reactive({

        resposta <- brushedPoints(gd_filtro(), input$brush_mapa, xvar = "X", yvar = "Y") 
        
        if (nrow(resposta) == 0){
            resposta <- gd_filtro()
        }

        campo_grupo <- names(agrupadores)[agrupadores == input$campo_grupo]
        campo_soma <- names(somas)[somas == input$campo_soma]
        
        resposta %>% 
            group_by_at(input$campo_grupo) %>% 
            summarise_at(input$campo_soma, sum) %>% 
            ungroup() %>% 
            mutate(
                Parcela = .data[[input$campo_soma]]/sum(.data[[input$campo_soma]])
            ) %>% 
            arrange(desc(.data[[input$campo_soma]])) %>% 
            rename(
                !!campo_grupo := 1,
                !!campo_soma := 2
            )
        
    })

    #### TABELA ####
    
    
    output$info <- renderReactable({
        

        
        dados() %>% 
            reactable(
                pagination = FALSE,
                defaultColDef = colDef(
                    format = colFormat(
                        digits = 0,
                        separators = TRUE
                    )
                ),
                columns = list(
                    Parcela = colDef(
                        format = colFormat(
                            percent = TRUE,
                            digits = 0
                        )
                    )
                )
                    
            )
        
    })

    #### GRÁFICO DE BARRAS HORIZONTAIS ####
    
    
    output$grafico_barras <- renderPlot({

        campo_grupo <- names(agrupadores)[agrupadores == input$campo_grupo]
        campo_soma <- names(somas)[somas == input$campo_soma]

        dados() %>% 
            mutate(
                !!campo_grupo := 
                    fct_lump(
                        f = .data[[campo_grupo]], 
                        prop = 0.05, 
                        w = .data[[campo_soma]] ,
                        other_level = "Outros"
                    )
            ) %>% 
            mutate(
                !!campo_grupo :=
                    fct_reorder(
                        .f = .data[[campo_grupo]],
                        .x = .data[[campo_soma]],
                        .fun = sum
                    )
            ) %>% 
            ggplot() +
            geom_col(
                aes(
                    x = .data[[campo_grupo]],
                    y = .data[[campo_soma]],
                    fill = .data[[campo_grupo]]
                )
            ) +
            labs(
                y = campo_soma,
                x = campo_grupo,
                fill = campo_grupo
            ) +
            coord_flip() +
            theme_minimal() +
            scale_y_continuous(
                labels = number_format(
                    big.mark = ".",
                    accuracy = 1
                )
            ) +
            theme(
                legend.position =  "top"
            ) +
            NULL
        
        
        
    })
    
    

    gd_filtro <- reactive({
        
        gd %>%
            filter(USER_SigUF %in% input$ufs | length(input$ufs) == 0 )
        
    })
    
    gd_sample <- reactive({
        gd_filtro() %>% 
            sample_frac(input$n_amostra/100)
    })


}


shinyApp(ui, server)