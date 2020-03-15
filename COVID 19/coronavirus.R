library(magrittr)
library(data.table)
library(ggplot2)

fread('ccaa_covid19_casos.csv') %>% 
  melt( id.vars=c("cod_ine",  "CCAA")) ->
  covid 

setnames(covid, 'variable', 'dia')
covid[, dia := as.Date(dia, format='%d/%m/%Y')]
covid <- covid[CCAA != 'Total'] 
setorder(covid, CCAA, dia)

last_day <- covid[, max(dia)]
annotations <- covid[dia == last_day, .(value, CCAA)]
log_annotations <- covid[dia == last_day, .(log_value=log(value), CCAA)]

covid %>% 
  ggplot() +
  geom_line(aes(dia, value, group=CCAA, color=CCAA), show.legend = FALSE) +
  annotate('text', x= last_day, y = annotations[, value], label=annotations[, CCAA]) +
  theme(legend.title = element_blank())

covid %>% 
  ggplot() +
  geom_line(aes(dia, log(value), group=CCAA, color=CCAA), show.legend = FALSE) +
  annotate('text', x= last_day, y = log_annotations[, log_value], label=log_annotations[, CCAA]) +
  theme(legend.title = element_blank())


ccaa = 'Madrid'
covid[CCAA == ccaa]

  
