### cargar paquetes
library(tidyverse)
library(writexl)
library(survey)
library(srvyr)


###leer los archivos
a <- read_csv(paste(input, 'enva_principal.csv', sep = '/'))

b <- read_csv(paste(input, 'enva_delitos.csv', sep = '/'))

dic <- read_csv('diccionario_de_datos.csv')


###poblaciones de hogares o individuos
b_hog <- sum(b$fac_hog_cdmx_del)

a_hog <- sum(a$fac_hog_cdmx_ajuste)

a_pob <- sum(a$fac_pob_cdmx_ajuste)


###especificar disenios muestrales

#disenio muestral para instrumento a (pregunta generales)
dis_a_pob <- a %>% mutate(N = a_pob) %>% 
  as_survey_design(weights = fac_pob_cdmx_ajuste, 
                              fpc = N, id = 1)

#disenio muestral para instrumento a (preguntas de victimizacion)
dis_a_hog <- a %>% mutate(N = a_hog) %>% 
  as_survey_design(weights = fac_hog_cdmx_ajuste, 
                   fpc = N, id = 1)

#disenio muestral para instrumento b 
dis_b  <- b %>% mutate(N = b_hog) %>% 
  as_survey_design(weights = fac_hog_cdmx_del, 
                   fpc = N, id = 1)



################## INSTRUMENTO A PREGUNTAS GENERALES

#preguntas generales del instrumento a
columnas <- names(a)[9:223]


#lista vacia para loop
loop_list_a_general <- list()

#loop para calcular resultados por pregunta
for(i in 1:length(columnas)) {
  
  loop_list_a_general[[i]] <- dis_a_hog %>% 
    group_by(!!as.symbol(columnas[[i]])) %>% 
    summarise(est = survey_total(vartype = 'ci', level = .95)) %>% 
    mutate(pregunta = columnas[[i]],
           respuesta = .[[1]]) %>% 
    select(pregunta, respuesta, 2:5)
  
}

#unir lista de preguntas en un dataframe, hacer join con diccionario de datos y guardar
do.call(rbind, loop_list_a_general) %>% 
  left_join(dic) %>% 
  write_csv('enva_survey_general_a.csv')



######################### INSTRUMENTO A PREGUNTAS DE INCIDENCIA

#preguntas de incidencia
columnas_incidencia <- columnas[170:215]

#lista vacia para loop
loop_list_vic_a <- list()

#loop para calcular resultados por pregunta
for(i in 1:length(columnas_incidencia)) {
  
  loop_list_vic_a[[i]] <- dis_a_hog %>% 
    group_by(!!as.symbol(columnas_incidencia[[i]])) %>% 
    summarise(est = survey_total(vartype = 'ci', level = .95)) %>% 
    mutate(pregunta = columnas_incidencia[[i]],
           respuesta = .[[1]]) %>% 
    select(pregunta, respuesta, 2:5)
  
}

#unir lista de preguntas/resultados en un dataframe y hacer join con diccionario y guardar
do.call(rbind, loop_list_vic_a) %>% 
  left_join(dic) %>% 
  write_csv('enva_survey_vic_a.csv')


############################################
############################################
#                Instrumento B
############################################
############################################




#extraer unicamente preguntas de opcion multiple al filtrar "txt"
names_df <- data.frame(c_name = names(b))

names_vec <- names_df %>% tibble() %>% 
  mutate(c_name = as.character(c_name)) %>% 
filter(str_detect(c_name, 'txt', negate = TRUE)) %>% 
  pull()


#definir preguntas para el loop
columnas_b <- names_vec[10:101]

#lista vacia para el loop
loop_list_b <- list()

#loop para calcular resultados por pregunta del instrumento b
for(i in 1:length(columnas_b)) {
  
  loop_list_b [[i]] <- dis_b %>% 
    group_by(!!as.symbol(columnas_b[[i]])) %>% 
    summarise(est = survey_total(vartype = 'ci', level = .95)) %>% 
    mutate(pregunta = columnas_b[[i]],
           respuesta = .[[1]]) %>% 
    select(pregunta, respuesta, 2:5)
  
}

#unir la lista de pregutnas/respuestas y guardar
do.call(rbind, loop_list_b) %>% 
  write_csv('enva_survey_b.csv')


#unir las listas en un archivo completo
rbind(do.call(rbind, loop_list_a_general),
      do.call(rbind, loop_list_vic_a),
      do.call(rbind, loop_list_b)) %>% 
  write_csv('enva_full_srvyr.csv')