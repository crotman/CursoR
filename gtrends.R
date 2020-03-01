library(gtrendsR)
library(tidyverse)
library(BETS)
library(tsibble)
library(feasts)


#ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/



caged <- read_csv2("d:\\CAGEDEST_122019\\CAGEDEST_122019.txt")



ind <- BETSsearch("unemployment")

viagra <- gtrends(c("seguro desemprego"), time = "2005-01-01 2020-02-01", onlyInterest = TRUE, geo = "BR"  )

# desemp <- BETSget(10777)

desemp_series <- as_tsibble(desemp) 

tudo <- bind_rows(viagra)
    
ggplot(tudo) + 
      geom_line(
        aes(
            x = date,
            y = hits,
        ) +
            
    ) +
    facet_grid(keyword~., scales = "free" )



gg_tsdisplay(desemp_series)
