library(magrittr)
library(data.table)
library(ggplot2)

data_type <- 'fallecidos' # casos
ccaas <- c('Madrid', 'Cataluña')
next_days <- 1:7

'ccaa_covid19_' %>% 
  paste0(data_type) %>% 
  paste0('.csv') %>% 
  fread() %>% 
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
  theme(legend.title = element_blank()) +
  ggtitle(data_type)

covid %>% 
  ggplot() +
  geom_line(aes(dia, log(value), group=CCAA, color=CCAA), show.legend = FALSE) +
  annotate('text', x= last_day, y = log_annotations[, log_value], label=log_annotations[, CCAA]) +
  theme(legend.title = element_blank()) +
  ggtitle(data_type)

df_prediction <- data.frame()
for(ccaa in ccaas){
  data_ccaa <- covid[value>0 & CCAA == ccaa] # From when there was a first death
  min_day <- data_ccaa[, min(dia)] 
  data_ccaa %>% 
    .[, diff_dias := as.numeric(dia - min_day)] %>% 
    .[, log_value := log(value)]
  linear <- lm(log_value~diff_dias, data_ccaa)
  max_day_diff <- data_ccaa[, max(diff_dias)]
  max_day <- data_ccaa[, max(dia)]
  
  predict_df <- data.frame(diff_dias = as.numeric(max_day_diff + next_days))
  predict_value <- exp(predict(linear, predict_df))
  predict_dia <- max_day + next_days
  
  df_prediction_ccaa_original <- data.frame(
    CCAA = ccaa, 
    dia = data_ccaa[, dia],
    value = data_ccaa[, value],
    type='original',
    group=paste(ccaa, 'original')
  )
  
  df_prediction_ccaa_prediction <-  data.frame(
    CCAA = ccaa, 
    dia = predict_dia,
    value = predict_value,
    type="prediction",
    group=paste(ccaa, 'prediction')
  )
    
  df_prediction_ccaa <- rbind(
    df_prediction_ccaa_original,
    df_prediction_ccaa_prediction
  )
  
  df_prediction <- rbind(
    df_prediction,
    df_prediction_ccaa
  )
}


df_prediction %>% 
  ggplot(aes(dia, value, group=group, linetype = type)) +
  geom_line() +
  facet_grid(CCAA~., scales="free") +
  ggtitle(data_type)
  















