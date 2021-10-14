### Estimados de ENVIPE para comparar con ENVA  
###
### Código elaborado por Pablo Paras Ochoa

### Paquetes & Setup ----
# Instala paquetes -
library(pacman)
p_load(tidyverse, openxlsx, zip, readxl, broom, infer, janitor, lubridate, cowplot, officer, writexl, reshape, viridis, showtext, scales, ggridges, ggplot2, scales, survey, srvyr, foreign)

# Ajusta encoding y notacion cientifica -
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999) 

### Importar bases ----
# Importa la base, hace ajustes a tipos de variables para que esten correctos y filtra para cdmx-
TPer_Vic <- read_csv("Datos/bd_envipe_2021_csv/TPer_Vic1.csv")  %>%
  clean_names() %>%
  mutate(upm_dis = as.numeric(upm_dis),
         est_dis = as.numeric(est_dis),
         fac_ele = as.numeric(fac_ele),
         cve_ent = as.numeric(cve_ent),
         sexo = as.numeric(sexo),
         cve_mun = as.numeric(cve_mun),
         ap4_8_4 = as.numeric(ap4_8_4),
         ap4_8_5 = as.factor(ap4_8_5),
         ap4_10_01 = as.factor(ap4_10_01),
         ap4_10_02 = as.factor(ap4_10_02),
         ap4_10_03 = as.factor(ap4_10_03),
         ap4_10_04 = as.factor(ap4_10_04),
         ap4_10_05 = as.factor(ap4_10_05),
         ap4_10_06 = as.factor(ap4_10_06),
         ap4_10_07 = as.factor(ap4_10_07),
         ap4_10_08 = as.factor(ap4_10_08),
         ap4_10_09 = as.factor(ap4_10_09),
         ap4_10_10 = as.factor(ap4_10_10),
         ap4_10_11 = as.factor(ap4_10_11),
         ap4_10_12 = as.factor(ap4_10_12),
         ap4_10_13 = as.factor(ap4_10_13),
         ap4_10_14 = as.factor(ap4_10_14),
         ap4_10_15 = as.factor(ap4_10_15),
         ap4_10_16 = as.factor(ap4_10_16),
         ap4_11_01 = as.factor(ap4_11_01),
         ap4_11_02 = as.factor(ap4_11_02),
         ap4_11_03 = as.factor(ap4_11_03),
         ap4_11_04 = as.factor(ap4_11_04),
         ap4_11_05 = as.factor(ap4_11_05),
         ap4_11_06 = as.factor(ap4_11_04),
         ap4_11_04 = as.factor(ap4_11_06),
         ap4_11_07 = as.factor(ap4_11_07),
         ap4_11_08 = as.factor(ap4_11_08),
         ap4_11_09 = as.factor(ap4_11_09),
         ap4_11_10 = as.factor(ap4_11_10),
         ap4_11_11 = as.factor(ap4_11_11)) %>%
  filter(cve_ent == 09)

# Importa la base, hace ajustes a tipos de variables para que esten correctos y filtra para cdmx-
TPer_Vic2 <- read_csv("Datos/bd_envipe_2021_csv/TPer_Vic2.csv") %>%
  clean_names() %>%
  mutate(cve_ent = as.numeric(cve_ent),
         ap7_2 = as.numeric(ap7_2),
         ap7_4_05 = as.numeric(ap7_4_05),
         ap7_4_05 = replace_na(ap7_4_05, 0),
         ap7_4_06 = as.numeric(ap7_4_06),
         ap7_4_06 = replace_na(ap7_4_06, 0),
         ap7_4_07 = as.numeric(ap7_4_07),
         ap7_4_07 = replace_na(ap7_4_07, 0),
         ap7_4_08 = as.numeric(ap7_4_08),
         ap7_4_08 = replace_na(ap7_4_08, 0),
         ap7_4_09 = as.numeric(ap7_4_09),
         ap7_4_09 = replace_na(ap7_4_09, 0),
         ap7_4_10 = as.numeric(ap7_4_10),
         ap7_4_10 = replace_na(ap7_4_10, 0),
         ap7_4_11 = as.numeric(ap7_4_11),
         ap7_4_11 = replace_na(ap7_4_11, 0),
         ap7_4_12 = as.numeric(ap7_4_12),
         ap7_4_12 = replace_na(ap7_4_12, 0),
         ap7_4_13 = as.numeric(ap7_4_13),
         ap7_4_13 = replace_na(ap7_4_13, 0),
         ap7_4_14 = as.numeric(ap7_4_14),
         ap7_4_14 = replace_na(ap7_4_14, 0),
         ap7_4_15 = as.numeric(ap7_4_15),
         ap7_4_15 = replace_na(ap7_4_15, 0)) %>%
  filter(cve_ent == 09) 

# Importa la base, hace ajustes a tipos de variables para que esten correctos y filtra para cdmx y delitos que ocurrieron antes o durante septiembre-
TMod_Vic <- read.dbf("Datos/TMod_Vic.dbf") %>%
  clean_names() %>%
  filter(bp1_2c == "09") %>%
  mutate(upm = as.double(upm),
         upm_dis = as.double(upm_dis),
         est_dis = as.double(est_dis),
         bp1_1 = as.numeric(bp1_1)) %>%
  filter(bp1_1 <= 9)

### Colonia y habitos ----
# Crea el diseño muestral -
design <- 
  TPer_Vic %>%
  as_survey_design(id = upm_dis, strata = est_dis, weights = fac_ele)

# Realiza un estimado y un tibble por cada pregunta el cual contiene el total estimado por cada respuesta, los limites inferiores y superiores, el numero de pregunta y un identificador de la respuesta para la cual se esta estimando
ap4_8_4 <-
  design %>%
  group_by(ap4_8_4) %>%
  summarise(ap4_8_4 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_8_4, total_low = ap4_8_4_low, total_upp = ap4_8_4_upp) %>%
  mutate(pregunta = "ap4_8_4") %>% 
  mutate(respuesta = row_number())

ap4_8_5 <-
  design %>%
  group_by(ap4_8_5) %>%
  summarise(ap4_8_5 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_8_5, total_low = ap4_8_5_low, total_upp = ap4_8_5_upp) %>%
  mutate(pregunta = "ap4_8_5") %>% 
  mutate(respuesta = row_number())

ap4_10_01 <-
  design %>%
  group_by(ap4_10_01) %>%
  summarise(ap4_10_01 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_01, total_low = ap4_10_01_low, total_upp = ap4_10_01_upp) %>%
  mutate(pregunta = "ap4_10_01") %>% 
  mutate(respuesta = row_number())

ap4_10_02 <-
  design %>%
  group_by(ap4_10_02) %>%
  summarise(ap4_10_02 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_02, total_low = ap4_10_02_low, total_upp = ap4_10_02_upp) %>%
  mutate(pregunta = "ap4_10_02") %>% 
  mutate(respuesta = row_number())

ap4_10_03 <-
  design %>%
  group_by(ap4_10_03) %>%
  summarise(ap4_10_03 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_03, total_low = ap4_10_03_low, total_upp = ap4_10_03_upp) %>%
  mutate(pregunta = "ap4_10_03") %>% 
  mutate(respuesta = row_number())

ap4_10_04 <-
  design %>%
  group_by(ap4_10_04) %>%
  summarise(ap4_10_04 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_04, total_low = ap4_10_04_low, total_upp = ap4_10_04_upp) %>%
  mutate(pregunta = "ap4_10_04") %>% 
  mutate(respuesta = row_number())

ap4_10_05 <-
  design %>%
  group_by(ap4_10_05) %>%
  summarise(ap4_10_05 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_05, total_low = ap4_10_05_low, total_upp = ap4_10_05_upp) %>%
  mutate(pregunta = "ap4_10_05") %>% 
  mutate(respuesta = row_number())

ap4_10_06 <-
  design %>%
  group_by(ap4_10_06) %>%
  summarise(ap4_10_06 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_06, total_low = ap4_10_06_low, total_upp = ap4_10_06_upp) %>%
  mutate(pregunta = "ap4_10_06") %>% 
  mutate(respuesta = row_number())

ap4_10_07 <-
  design %>%
  group_by(ap4_10_07) %>%
  summarise(ap4_10_07 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_07, total_low = ap4_10_07_low, total_upp = ap4_10_07_upp) %>%
  mutate(pregunta = "ap4_10_07") %>% 
  mutate(respuesta = row_number())

ap4_10_08 <-
  design %>%
  group_by(ap4_10_08) %>%
  summarise(ap4_10_08 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_08, total_low = ap4_10_08_low, total_upp = ap4_10_08_upp) %>%
  mutate(pregunta = "ap4_10_08") %>% 
  mutate(respuesta = row_number())

ap4_10_09 <-
  design %>%
  group_by(ap4_10_09) %>%
  summarise(ap4_10_09 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_09, total_low = ap4_10_09_low, total_upp = ap4_10_09_upp) %>%
  mutate(pregunta = "ap4_10_09") %>% 
  mutate(respuesta = row_number())

ap4_10_10 <-
  design %>%
  group_by(ap4_10_10) %>%
  summarise(ap4_10_10 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_10, total_low = ap4_10_10_low, total_upp = ap4_10_10_upp) %>%
  mutate(pregunta = "ap4_10_10") %>% 
  mutate(respuesta = row_number())

ap4_10_11 <-
  design %>%
  group_by(ap4_10_11) %>%
  summarise(ap4_10_11 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_11, total_low = ap4_10_11_low, total_upp = ap4_10_11_upp) %>%
  mutate(pregunta = "ap4_10_11") %>% 
  mutate(respuesta = row_number())

ap4_10_12 <-
  design %>%
  group_by(ap4_10_12) %>%
  summarise(ap4_10_12 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_12, total_low = ap4_10_12_low, total_upp = ap4_10_12_upp) %>%
  mutate(pregunta = "ap4_10_12") %>% 
  mutate(respuesta = row_number())

ap4_10_13 <-
  design %>%
  group_by(ap4_10_13) %>%
  summarise(ap4_10_13 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_13, total_low = ap4_10_13_low, total_upp = ap4_10_13_upp) %>%
  mutate(pregunta = "ap4_10_13") %>% 
  mutate(respuesta = row_number())

ap4_10_14 <-
  design %>%
  group_by(ap4_10_14) %>%
  summarise(ap4_10_14 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_14, total_low = ap4_10_14_low, total_upp = ap4_10_14_upp) %>%
  mutate(pregunta = "ap4_10_14") %>% 
  mutate(respuesta = row_number())

ap4_10_15 <-
  design %>%
  group_by(ap4_10_15) %>%
  summarise(ap4_10_15 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_15, total_low = ap4_10_15_low, total_upp = ap4_10_15_upp) %>%
  mutate(pregunta = "ap4_10_15") %>% 
  mutate(respuesta = row_number())

ap4_10_16 <-
  design %>%
  group_by(ap4_10_16) %>%
  summarise(ap4_10_16 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_10_16, total_low = ap4_10_16_low, total_upp = ap4_10_16_upp) %>%
  mutate(pregunta = "ap4_10_16") %>% 
  mutate(respuesta = row_number())

ap4_11_01 <-
  design %>%
  group_by(ap4_11_01) %>%
  summarise(ap4_11_01 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_01, total_low = ap4_11_01_low, total_upp = ap4_11_01_upp) %>%
  mutate(pregunta = "ap4_11_01") %>% 
  mutate(respuesta = row_number())

ap4_11_02 <-
  design %>%
  group_by(ap4_11_02) %>%
  summarise(ap4_11_02 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_02, total_low = ap4_11_02_low, total_upp = ap4_11_02_upp) %>%
  mutate(pregunta = "ap4_11_02") %>% 
  mutate(respuesta = row_number())

ap4_11_03 <-
  design %>%
  group_by(ap4_11_03) %>%
  summarise(ap4_11_03 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_03, total_low = ap4_11_03_low, total_upp = ap4_11_03_upp) %>%
  mutate(pregunta = "ap4_11_03") %>% 
  mutate(respuesta = row_number())

ap4_11_04 <-
  design %>%
  group_by(ap4_11_04) %>%
  summarise(ap4_11_04 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_04, total_low = ap4_11_04_low, total_upp = ap4_11_04_upp) %>%
  mutate(pregunta = "ap4_11_04") %>% 
  mutate(respuesta = row_number())

ap4_11_05 <-
  design %>%
  group_by(ap4_11_05) %>%
  summarise(ap4_11_05 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_05, total_low = ap4_11_05_low, total_upp = ap4_11_05_upp) %>%
  mutate(pregunta = "ap4_11_05") %>% 
  mutate(respuesta = row_number())

ap4_11_06 <-
  design %>%
  group_by(ap4_11_06) %>%
  summarise(ap4_11_06 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_06, total_low = ap4_11_06_low, total_upp = ap4_11_06_upp) %>%
  mutate(pregunta = "ap4_11_06") %>% 
  mutate(respuesta = row_number())

ap4_11_07 <-
  design %>%
  group_by(ap4_11_07) %>%
  summarise(ap4_11_07 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_07, total_low = ap4_11_07_low, total_upp = ap4_11_07_upp) %>%
  mutate(pregunta = "ap4_11_07") %>% 
  mutate(respuesta = row_number())

ap4_11_08 <-
  design %>%
  group_by(ap4_11_08) %>%
  summarise(ap4_11_08 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_08, total_low = ap4_11_08_low, total_upp = ap4_11_08_upp) %>%
  mutate(pregunta = "ap4_11_08") %>% 
  mutate(respuesta = row_number())

ap4_11_09 <-
  design %>%
  group_by(ap4_11_09) %>%
  summarise(ap4_11_09 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_09, total_low = ap4_11_09_low, total_upp = ap4_11_09_upp) %>%
  mutate(pregunta = "ap4_11_09") %>% 
  mutate(respuesta = row_number())

ap4_11_10 <-
  design %>%
  group_by(ap4_11_10) %>%
  summarise(ap4_11_10 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_10, total_low = ap4_11_10_low, total_upp = ap4_11_10_upp) %>%
  mutate(pregunta = "ap4_11_10") %>% 
  mutate(respuesta = row_number())

ap4_11_11 <-
  design %>%
  group_by(ap4_11_11) %>%
  summarise(ap4_11_11 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap4_11_11, total_low = ap4_11_11_low, total_upp = ap4_11_11_upp) %>%
  mutate(pregunta = "ap4_11_11") %>% 
  mutate(respuesta = row_number())

# Junta los tibbles a un solo tibble que contiene todas las preguntas de este apartado -
Respuestas1 <-
  bind_rows(ap4_8_4, ap4_8_5, ap4_10_01, ap4_10_02, ap4_10_03, ap4_10_04, ap4_10_05, ap4_10_06, ap4_10_07, ap4_10_08, ap4_10_09, ap4_10_10, ap4_10_11, ap4_10_12, ap4_10_13, ap4_10_14, ap4_10_15, ap4_10_16, ap4_11_01, ap4_11_02, ap4_11_03, ap4_11_04, ap4_11_05, ap4_11_06, ap4_11_07, ap4_11_08, ap4_11_09, ap4_11_10, ap4_11_11)

# Genera un excel con el tibble creado anteriormente -
write.xlsx(Respuestas1, file = "Datos/Respuestas.xlsx", sheetName = "Respuestas")

### Parte victimización 1 ----

# Crea el diseño muestral -
design <- 
  TPer_Vic2 %>%
  as_survey_design(id = upm_dis, strata = est_dis, weights = fac_ele)

# Realiza un estimado y un tibble por cada pregunta el cual contiene el total estimado por cada respuesta, los limites inferiores y superiores, el numero de pregunta y un identificador de la respuesta para la cual se esta estimando
ap7_1 <-
  design %>%
  group_by(ap7_1) %>%
  summarise(ap7_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_1, total_low = ap7_1_low, total_upp = ap7_1_upp) %>%
  mutate(pregunta = "ap7_1") %>% 
  mutate(respuesta = row_number())

ap7_2 <-
  design %>%
  group_by(ap7_2) %>%
  summarise(ap7_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_2, total_low = ap7_2_low, total_upp = ap7_2_upp) %>%
  mutate(pregunta = "ap7_2") %>% 
  mutate(respuesta = row_number())

ap7_3_05 <-
  design %>%
  group_by(ap7_3_05) %>%
  summarise(ap7_3_05 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_05, total_low = ap7_3_05_low, total_upp = ap7_3_05_upp) %>%
  mutate(pregunta = "ap7_3_05") %>% 
  mutate(respuesta = row_number())

ap7_3_06 <-
  design %>%
  group_by(ap7_3_06) %>%
  summarise(ap7_3_06 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_06, total_low = ap7_3_06_low, total_upp = ap7_3_06_upp) %>%
  mutate(pregunta = "ap7_3_06") %>% 
  mutate(respuesta = row_number())

ap7_3_07 <-
  design %>%
  group_by(ap7_3_07) %>%
  summarise(ap7_3_07 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_07, total_low = ap7_3_07_low, total_upp = ap7_3_07_upp) %>%
  mutate(pregunta = "ap7_3_07") %>% 
  mutate(respuesta = row_number())

ap7_3_08 <-
  design %>%
  group_by(ap7_3_08) %>%
  summarise(ap7_3_08 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_08, total_low = ap7_3_08_low, total_upp = ap7_3_08_upp) %>%
  mutate(pregunta = "ap7_3_08") %>% 
  mutate(respuesta = row_number())

ap7_3_09 <-
  design %>%
  group_by(ap7_3_09) %>%
  summarise(ap7_3_09 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_09, total_low = ap7_3_09_low, total_upp = ap7_3_09_upp) %>%
  mutate(pregunta = "ap7_3_09") %>% 
  mutate(respuesta = row_number())

ap7_3_10 <-
  design %>%
  group_by(ap7_3_10) %>%
  summarise(ap7_3_10 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_10, total_low = ap7_3_10_low, total_upp = ap7_3_10_upp) %>%
  mutate(pregunta = "ap7_3_10") %>% 
  mutate(respuesta = row_number())

ap7_3_11 <-
  design %>%
  group_by(ap7_3_11) %>%
  summarise(ap7_3_11 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_11, total_low = ap7_3_11_low, total_upp = ap7_3_11_upp) %>%
  mutate(pregunta = "ap7_3_11") %>% 
  mutate(respuesta = row_number())

ap7_3_12 <-
  design %>%
  group_by(ap7_3_12) %>%
  summarise(ap7_3_12 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_12, total_low = ap7_3_12_low, total_upp = ap7_3_12_upp) %>%
  mutate(pregunta = "ap7_3_12") %>% 
  mutate(respuesta = row_number())

ap7_3_13 <-
  design %>%
  group_by(ap7_3_13) %>%
  summarise(ap7_3_13 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_13, total_low = ap7_3_13_low, total_upp = ap7_3_13_upp) %>%
  mutate(pregunta = "ap7_3_13") %>% 
  mutate(respuesta = row_number())

ap7_3_14 <-
  design %>%
  group_by(ap7_3_14) %>%
  summarise(ap7_3_14 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_14, total_low = ap7_3_14_low, total_upp = ap7_3_14_upp) %>%
  mutate(pregunta = "ap7_3_14") %>% 
  mutate(respuesta = row_number())

ap7_3_15 <-
  design %>%
  group_by(ap7_3_15) %>%
  summarise(ap7_3_15 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_3_15, total_low = ap7_3_15_low, total_upp = ap7_3_15_upp) %>%
  mutate(pregunta = "ap7_3_15") %>% 
  mutate(respuesta = row_number())

ap7_4_05 <-
  design %>%
  summarise(ap7_4_05 = survey_total(ap7_4_05, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_05, total_low = ap7_4_05_low, total_upp = ap7_4_05_upp) %>%
  mutate(pregunta = "ap7_4_05") %>% 
  mutate(respuesta = row_number())

ap7_4_06 <-
  design %>%
  summarise(ap7_4_06 = survey_total(ap7_4_06, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_06, total_low = ap7_4_06_low, total_upp = ap7_4_06_upp) %>%
  mutate(pregunta = "ap7_4_06") %>% 
  mutate(respuesta = row_number())

ap7_4_07 <-
  design %>%
  summarise(ap7_4_07 = survey_total(ap7_4_07, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_07, total_low = ap7_4_07_low, total_upp = ap7_4_07_upp) %>%
  mutate(pregunta = "ap7_4_07") %>% 
  mutate(respuesta = row_number())

ap7_4_08 <-
  design %>%
  summarise(ap7_4_08 = survey_total(ap7_4_08, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_08, total_low = ap7_4_08_low, total_upp = ap7_4_08_upp) %>%
  mutate(pregunta = "ap7_4_08") %>% 
  mutate(respuesta = row_number())

ap7_4_09 <-
  design %>%
  summarise(ap7_4_09 = survey_total(ap7_4_09, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_09, total_low = ap7_4_09_low, total_upp = ap7_4_09_upp) %>%
  mutate(pregunta = "ap7_4_09") %>% 
  mutate(respuesta = row_number())

ap7_4_10 <-
  design %>%
  summarise(ap7_4_10 = survey_total(ap7_4_10, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_10, total_low = ap7_4_10_low, total_upp = ap7_4_10_upp) %>%
  mutate(pregunta = "ap7_4_10") %>% 
  mutate(respuesta = row_number())

ap7_4_11 <-
  design %>%
  summarise(ap7_4_11 = survey_total(ap7_4_11, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_11, total_low = ap7_4_11_low, total_upp = ap7_4_11_upp) %>%
  mutate(pregunta = "ap7_4_11") %>% 
  mutate(respuesta = row_number())

ap7_4_12 <-
  design %>%
  summarise(ap7_4_12 = survey_total(ap7_4_12, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_12, total_low = ap7_4_12_low, total_upp = ap7_4_12_upp) %>%
  mutate(pregunta = "ap7_4_12") %>% 
  mutate(respuesta = row_number())

ap7_4_13 <-
  design %>%
  summarise(ap7_4_13 = survey_total(ap7_4_13, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_13, total_low = ap7_4_13_low, total_upp = ap7_4_13_upp) %>%
  mutate(pregunta = "ap7_4_13") %>% 
  mutate(respuesta = row_number())

ap7_4_14 <-
  design %>%
  summarise(ap7_4_14 = survey_total(ap7_4_14, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_14, total_low = ap7_4_14_low, total_upp = ap7_4_14_upp) %>%
  mutate(pregunta = "ap7_4_14") %>% 
  mutate(respuesta = row_number())

ap7_4_15 <-
  design %>%
  summarise(ap7_4_15 = survey_total(ap7_4_15, vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap7_4_15, total_low = ap7_4_15_low, total_upp = ap7_4_15_upp) %>%
  mutate(pregunta = "ap7_4_15") %>% 
  mutate(respuesta = row_number())

# Junta los tibbles a un solo tibble que contiene todas las preguntas de este apartado -
Respuestas2 <-
  bind_rows(ap7_1, ap7_2, ap7_3_05, ap7_3_06, ap7_3_07, ap7_3_08, ap7_3_09, ap7_3_10, ap7_3_11, ap7_3_12, ap7_3_13, ap7_3_14, ap7_3_15, ap7_4_05, ap7_4_06, ap7_4_07, ap7_4_08, ap7_4_09, ap7_4_10, ap7_4_11, ap7_4_12, ap7_4_13, ap7_4_14, ap7_4_15)

# Genera un excel con el tibble creado anteriormente -

write.xlsx(Respuestas2, file = "Datos/Respuestas2.xlsx", sheetName = "Respuestas2")

### Parte victimización 2 ----

# Crea el diseño muestral -

design <- 
  TMod_Vic %>%
  as_survey_design(id = upm_dis, strata = est_dis, weights = fac_del)

options(survey.lonely.psu='adjust')

# Realiza un estimado y un tibble por cada pregunta el cual contiene el total estimado por cada respuesta, los limites inferiores y superiores, el numero de pregunta y un identificador de la respuesta para la cual se esta estimando

bpcod <-
  design %>%
  group_by(bpcod) %>%
  summarise(bpcod = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bpcod, total_low = bpcod_low, total_upp = bpcod_upp) %>%
  mutate(pregunta = "bpcod") %>% 
  mutate(respuesta = row_number())

bp1_1 <-
  design %>%
  group_by(bp1_1) %>%
  summarise(bp1_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_1, total_low = bp1_1_low, total_upp = bp1_1_upp) %>%
  mutate(pregunta = "bp1_1") %>% 
  mutate(respuesta = row_number())

bp1_4 <-
  design %>%
  group_by(bp1_4) %>%
  summarise(bp1_4 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_4, total_low = bp1_4_low, total_upp = bp1_4_upp) %>%
  mutate(pregunta = "bp1_4") %>% 
  mutate(respuesta = row_number())

bp1_5 <-
  design %>%
  group_by(bp1_5) %>%
  summarise(bp1_5 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_5, total_low = bp1_5_low, total_upp = bp1_5_upp) %>%
  mutate(pregunta = "bp1_5") %>% 
  mutate(respuesta = row_number())

bp1_6 <-
  design %>%
  group_by(bp1_6) %>%
  summarise(bp1_6 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_6, total_low = bp1_6_low, total_upp = bp1_6_upp) %>%
  mutate(pregunta = "bp1_6") %>% 
  mutate(respuesta = row_number())

bp1_7 <-
  design %>%
  group_by(bp1_7) %>%
  summarise(bp1_7 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_7, total_low = bp1_7_low, total_upp = bp1_7_upp) %>%
  mutate(pregunta = "bp1_7") %>% 
  mutate(respuesta = row_number())

bp1_8 <-
  design %>%
  group_by(bp1_8) %>%
  summarise(bp1_8 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_8, total_low = bp1_8_low, total_upp = bp1_8_upp) %>%
  mutate(pregunta = "bp1_8") %>% 
  mutate(respuesta = row_number())

bp1_9 <-
  design %>%
  group_by(bp1_9) %>%
  summarise(bp1_9 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_9, total_low = bp1_9_low, total_upp = bp1_9_upp) %>%
  mutate(pregunta = "bp1_9") %>% 
  mutate(respuesta = row_number())

bp1_10_1 <-
  design %>%
  group_by(bp1_10_1) %>%
  summarise(bp1_10_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_10_1, total_low = bp1_10_1_low, total_upp = bp1_10_1_upp) %>%
  mutate(pregunta = "bp1_10_1") %>% 
  mutate(respuesta = row_number())


bp1_10_2 <-
  design %>%
  group_by(bp1_10_2) %>%
  summarise(bp1_10_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_10_2, total_low = bp1_10_2_low, total_upp = bp1_10_2_upp) %>%
  mutate(pregunta = "bp1_10_2") %>% 
  mutate(respuesta = row_number())


bp1_10_9 <-
  design %>%
  group_by(bp1_10_9) %>%
  summarise(bp1_10_9 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_10_9, total_low = bp1_10_9_low, total_upp = bp1_10_9_upp) %>%
  mutate(pregunta = "bp1_10_9") %>% 
  mutate(respuesta = row_number())

bp1_12_1 <-
  design %>%
  group_by(bp1_12_1) %>%
  summarise(bp1_12_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_12_1, total_low = bp1_12_1_low, total_upp = bp1_12_1_upp) %>%
  mutate(pregunta = "bp1_12_1") %>% 
  mutate(respuesta = row_number())

bp1_12_2 <-
  design %>%
  group_by(bp1_12_2) %>%
  summarise(bp1_12_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_12_2, total_low = bp1_12_2_low, total_upp = bp1_12_2_upp) %>%
  mutate(pregunta = "bp1_12_2") %>% 
  mutate(respuesta = row_number())

bp1_12_3 <-
  design %>%
  group_by(bp1_12_3) %>%
  summarise(bp1_12_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_12_3, total_low = bp1_12_3_low, total_upp = bp1_12_3_upp) %>%
  mutate(pregunta = "bp1_12_3") %>% 
  mutate(respuesta = row_number())

bp1_12_4 <-
  design %>%
  group_by(bp1_12_4) %>%
  summarise(bp1_12_4 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_12_4, total_low = bp1_12_4_low, total_upp = bp1_12_4_upp) %>%
  mutate(pregunta = "bp1_12_4") %>% 
  mutate(respuesta = row_number())

bp1_12_5 <-
  design %>%
  group_by(bp1_12_5) %>%
  summarise(bp1_12_5 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_12_5, total_low = bp1_12_5_low, total_upp = bp1_12_5_upp) %>%
  mutate(pregunta = "bp1_12_5") %>% 
  mutate(respuesta = row_number())

bp1_12_9 <-
  design %>%
  group_by(bp1_12_9) %>%
  summarise(bp1_12_9 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_12_9, total_low = bp1_12_9_low, total_upp = bp1_12_9_upp) %>%
  mutate(pregunta = "bp1_12_9") %>% 
  mutate(respuesta = row_number())

bp1_14_1 <-
  design %>%
  group_by(bp1_14_1) %>%
  summarise(bp1_14_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_14_1, total_low = bp1_14_1_low, total_upp = bp1_14_1_upp) %>%
  mutate(pregunta = "bp1_14_1") %>% 
  mutate(respuesta = row_number())

bp1_14_2 <-
  design %>%
  group_by(bp1_14_2) %>%
  summarise(bp1_14_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_14_2, total_low = bp1_14_2_low, total_upp = bp1_14_2_upp) %>%
  mutate(pregunta = "bp1_14_2") %>% 
  mutate(respuesta = row_number())

bp1_14_3 <-
  design %>%
  group_by(bp1_14_3) %>%
  summarise(bp1_14_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_14_3, total_low = bp1_14_3_low, total_upp = bp1_14_3_upp) %>%
  mutate(pregunta = "bp1_14_3") %>% 
  mutate(respuesta = row_number())

bp1_14_4 <-
  design %>%
  group_by(bp1_14_4) %>%
  summarise(bp1_14_4 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_14_4, total_low = bp1_14_4_low, total_upp = bp1_14_4_upp) %>%
  mutate(pregunta = "bp1_14_4") %>% 
  mutate(respuesta = row_number())

bp1_14_9 <-
  design %>%
  group_by(bp1_14_9) %>%
  summarise(bp1_14_9 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_14_9, total_low = bp1_14_9_low, total_upp = bp1_14_9_upp) %>%
  mutate(pregunta = "bp1_14_9") %>% 
  mutate(respuesta = row_number())

bp1_15 <-
  design %>%
  group_by(bp1_15) %>%
  summarise(bp1_15 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_15, total_low = bp1_15_low, total_upp = bp1_15_upp) %>%
  mutate(pregunta = "bp1_15") %>% 
  mutate(respuesta = row_number())

bp1_16_1 <-
  design %>%
  group_by(bp1_16_1) %>%
  summarise(bp1_16_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_16_1, total_low = bp1_16_1_low, total_upp = bp1_16_1_upp) %>%
  mutate(pregunta = "bp1_16_1") %>% 
  mutate(respuesta = row_number())

bp1_16_2 <-
  design %>%
  group_by(bp1_16_2) %>%
  summarise(bp1_16_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_16_2, total_low = bp1_16_2_low, total_upp = bp1_16_2_upp) %>%
  mutate(pregunta = "bp1_16_2") %>% 
  mutate(respuesta = row_number())

bp1_16_3 <-
  design %>%
  group_by(bp1_16_3) %>%
  summarise(bp1_16_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_16_3, total_low = bp1_16_3_low, total_upp = bp1_16_3_upp) %>%
  mutate(pregunta = "bp1_16_3") %>% 
  mutate(respuesta = row_number())

bp1_16_4 <-
  design %>%
  group_by(bp1_16_4) %>%
  summarise(bp1_16_4 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_16_4, total_low = bp1_16_4_low, total_upp = bp1_16_4_upp) %>%
  mutate(pregunta = "bp1_16_4") %>% 
  mutate(respuesta = row_number())

bp1_16_9 <-
  design %>%
  group_by(bp1_16_9) %>%
  summarise(bp1_16_9 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_16_9, total_low = bp1_16_9_low, total_upp = bp1_16_9_upp) %>%
  mutate(pregunta = "bp1_16_9") %>% 
  mutate(respuesta = row_number())

bp1_17 <-
  design %>%
  group_by(bp1_17) %>%
  summarise(bp1_17 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_17, total_low = bp1_17_low, total_upp = bp1_17_upp) %>%
  mutate(pregunta = "bp1_17") %>% 
  mutate(respuesta = row_number())

bp1_18 <-
  design %>%
  group_by(bp1_18) %>%
  summarise(bp1_18 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_18, total_low = bp1_18_low, total_upp = bp1_18_upp) %>%
  mutate(pregunta = "bp1_18") %>% 
  mutate(respuesta = row_number())

bp1_19_1 <-
  design %>%
  group_by(bp1_19_1) %>%
  summarise(bp1_19_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_19_1, total_low = bp1_19_1_low, total_upp = bp1_19_1_upp) %>%
  mutate(pregunta = "bp1_19_1") %>% 
  mutate(respuesta = row_number())

bp1_19_2 <-
  design %>%
  group_by(bp1_19_2) %>%
  summarise(bp1_19_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_19_2, total_low = bp1_19_2_low, total_upp = bp1_19_2_upp) %>%
  mutate(pregunta = "bp1_19_2") %>% 
  mutate(respuesta = row_number())

bp1_19_3 <-
  design %>%
  group_by(bp1_19_3) %>%
  summarise(bp1_19_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_19_3, total_low = bp1_19_3_low, total_upp = bp1_19_3_upp) %>%
  mutate(pregunta = "bp1_19_3") %>% 
  mutate(respuesta = row_number())

bp1_19_4 <-
  design %>%
  group_by(bp1_19_4) %>%
  summarise(bp1_19_4 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_19_4, total_low = bp1_19_4_low, total_upp = bp1_19_4_upp) %>%
  mutate(pregunta = "bp1_19_4") %>% 
  mutate(respuesta = row_number())

bp1_19_5 <-
  design %>%
  group_by(bp1_19_5) %>%
  summarise(bp1_19_5 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_19_5, total_low = bp1_19_5_low, total_upp = bp1_19_5_upp) %>%
  mutate(pregunta = "bp1_19_5") %>% 
  mutate(respuesta = row_number())

bp1_19_6 <-
  design %>%
  group_by(bp1_19_6) %>%
  summarise(bp1_19_6 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_19_6, total_low = bp1_19_6_low, total_upp = bp1_19_6_upp) %>%
  mutate(pregunta = "bp1_19_6") %>% 
  mutate(respuesta = row_number())

bp1_19_7 <-
  design %>%
  group_by(bp1_19_7) %>%
  summarise(bp1_19_7 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_19_7, total_low = bp1_19_7_low, total_upp = bp1_19_7_upp) %>%
  mutate(pregunta = "bp1_19_7") %>% 
  mutate(respuesta = row_number())

bp1_19_8 <-
  design %>%
  group_by(bp1_19_8) %>%
  summarise(bp1_19_8 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_19_8, total_low = bp1_19_8_low, total_upp = bp1_19_8_upp) %>%
  mutate(pregunta = "bp1_19_8") %>% 
  mutate(respuesta = row_number())

bp1_20 <-
  design %>%
  group_by(bp1_20) %>%
  summarise(bp1_20 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_20, total_low = bp1_20_low, total_upp = bp1_20_upp) %>%
  mutate(pregunta = "bp1_20") %>% 
  mutate(respuesta = row_number())

bp1_22 <-
  design %>%
  group_by(bp1_22) %>%
  summarise(bp1_22 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_22, total_low = bp1_22_low, total_upp = bp1_22_upp) %>%
  mutate(pregunta = "bp1_22") %>% 
  mutate(respuesta = row_number())

bp1_23 <-
  design %>%
  group_by(bp1_23) %>%
  summarise(bp1_23 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_23, total_low = bp1_23_low, total_upp = bp1_23_upp) %>%
  mutate(pregunta = "bp1_23") %>% 
  mutate(respuesta = row_number())

bp1_24 <-
  design %>%
  group_by(bp1_24) %>%
  summarise(bp1_24 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_24, total_low = bp1_24_low, total_upp = bp1_24_upp) %>%
  mutate(pregunta = "bp1_24") %>% 
  mutate(respuesta = row_number())

bp1_25 <-
  design %>%
  group_by(bp1_25) %>%
  summarise(bp1_25 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_25, total_low = bp1_25_low, total_upp = bp1_25_upp) %>%
  mutate(pregunta = "bp1_25") %>% 
  mutate(respuesta = row_number())

bp1_26 <-
  design %>%
  group_by(bp1_26) %>%
  summarise(bp1_26 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_26, total_low = bp1_26_low, total_upp = bp1_26_upp) %>%
  mutate(pregunta = "bp1_26") %>% 
  mutate(respuesta = row_number())

bp1_27 <-
  design %>%
  group_by(bp1_27) %>%
  summarise(bp1_27 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_27, total_low = bp1_27_low, total_upp = bp1_27_upp) %>%
  mutate(pregunta = "bp1_27") %>% 
  mutate(respuesta = row_number())

bp1_28 <-
  design %>%
  group_by(bp1_28) %>%
  summarise(bp1_28 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_28, total_low = bp1_28_low, total_upp = bp1_28_upp) %>%
  mutate(pregunta = "bp1_28") %>% 
  mutate(respuesta = row_number())

bp1_29 <-
  design %>%
  group_by(bp1_29) %>%
  summarise(bp1_29 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_29, total_low = bp1_29_low, total_upp = bp1_29_upp) %>%
  mutate(pregunta = "bp1_29") %>% 
  mutate(respuesta = row_number())

bp1_30 <-
  design %>%
  group_by(bp1_30) %>%
  summarise(bp1_30 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_30, total_low = bp1_30_low, total_upp = bp1_30_upp) %>%
  mutate(pregunta = "bp1_30") %>% 
  mutate(respuesta = row_number())

bp1_31_01 <-
  design %>%
  group_by(bp1_31_01) %>%
  summarise(bp1_31_01 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_01, total_low = bp1_31_01_low, total_upp = bp1_31_01_upp) %>%
  mutate(pregunta = "bp1_31_01") %>% 
  mutate(respuesta = row_number())

bp1_31_02 <-
  design %>%
  group_by(bp1_31_02) %>%
  summarise(bp1_31_02 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_02, total_low = bp1_31_02_low, total_upp = bp1_31_02_upp) %>%
  mutate(pregunta = "bp1_31_02") %>% 
  mutate(respuesta = row_number())

bp1_31_03 <-
  design %>%
  group_by(bp1_31_03) %>%
  summarise(bp1_31_03 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_03, total_low = bp1_31_03_low, total_upp = bp1_31_03_upp) %>%
  mutate(pregunta = "bp1_31_03") %>% 
  mutate(respuesta = row_number())

bp1_31_04 <-
  design %>%
  group_by(bp1_31_04) %>%
  summarise(bp1_31_04 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_04, total_low = bp1_31_04_low, total_upp = bp1_31_04_upp) %>%
  mutate(pregunta = "bp1_31_04") %>% 
  mutate(respuesta = row_number())

bp1_31_05 <-
  design %>%
  group_by(bp1_31_05) %>%
  summarise(bp1_31_05 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_05, total_low = bp1_31_05_low, total_upp = bp1_31_05_upp) %>%
  mutate(pregunta = "bp1_31_05") %>% 
  mutate(respuesta = row_number())

bp1_31_06 <-
  design %>%
  group_by(bp1_31_06) %>%
  summarise(bp1_31_06 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_06, total_low = bp1_31_06_low, total_upp = bp1_31_06_upp) %>%
  mutate(pregunta = "bp1_31_06") %>% 
  mutate(respuesta = row_number())

bp1_31_07 <-
  design %>%
  group_by(bp1_31_07) %>%
  summarise(bp1_31_07 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_07, total_low = bp1_31_07_low, total_upp = bp1_31_07_upp) %>%
  mutate(pregunta = "bp1_31_07") %>% 
  mutate(respuesta = row_number())

bp1_31_08 <-
  design %>%
  group_by(bp1_31_08) %>%
  summarise(bp1_31_08 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_08, total_low = bp1_31_08_low, total_upp = bp1_31_08_upp) %>%
  mutate(pregunta = "bp1_31_08") %>% 
  mutate(respuesta = row_number())

bp1_31_99 <-
  design %>%
  group_by(bp1_31_99) %>%
  summarise(bp1_31_99 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_31_99, total_low = bp1_31_99_low, total_upp = bp1_31_99_upp) %>%
  mutate(pregunta = "bp1_31_99") %>% 
  mutate(respuesta = row_number())

bp1_32_1 <-
  design %>%
  group_by(bp1_32_1) %>%
  summarise(bp1_32_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_32_1, total_low = bp1_32_1_low, total_upp = bp1_32_1_upp) %>%
  mutate(pregunta = "bp1_32_1") %>% 
  mutate(respuesta = row_number())

bp1_32_2 <-
  design %>%
  group_by(bp1_32_2) %>%
  summarise(bp1_32_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_32_2, total_low = bp1_32_2_low, total_upp = bp1_32_2_upp) %>%
  mutate(pregunta = "bp1_32_2") %>% 
  mutate(respuesta = row_number())

bp1_32_3 <-
  design %>%
  group_by(bp1_32_3) %>%
  summarise(bp1_32_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_32_3, total_low = bp1_32_3_low, total_upp = bp1_32_3_upp) %>%
  mutate(pregunta = "bp1_32_3") %>% 
  mutate(respuesta = row_number())

bp1_32_4 <-
  design %>%
  group_by(bp1_32_4) %>%
  summarise(bp1_32_4 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_32_4, total_low = bp1_32_4_low, total_upp = bp1_32_4_upp) %>%
  mutate(pregunta = "bp1_32_4") %>% 
  mutate(respuesta = row_number())

bp1_32_5 <-
  design %>%
  group_by(bp1_32_5) %>%
  summarise(bp1_32_5 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_32_5, total_low = bp1_32_5_low, total_upp = bp1_32_5_upp) %>%
  mutate(pregunta = "bp1_32_5") %>% 
  mutate(respuesta = row_number())

bp1_32_9 <-
  design %>%
  group_by(bp1_32_9) %>%
  summarise(bp1_32_9 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_32_9, total_low = bp1_32_9_low, total_upp = bp1_32_9_upp) %>%
  mutate(pregunta = "bp1_32_9") %>% 
  mutate(respuesta = row_number())

bp1_33 <-
  design %>%
  group_by(bp1_33) %>%
  summarise(bp1_33 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp1_33, total_low = bp1_33_low, total_upp = bp1_33_upp) %>%
  mutate(pregunta = "bp1_33") %>% 
  mutate(respuesta = row_number())

# Junta los tibbles a un solo tibble que contiene todas las preguntas de este apartado -

Respuestas3 <-
  bind_rows(bp1_1, bp1_4, bp1_5, bp1_6, bp1_7, bp1_8, bp1_9, bp1_10_1, bp1_10_2, bp1_10_9, bp1_12_1, bp1_12_2, bp1_12_3, bp1_12_4, bp1_12_5, bp1_12_9, bp1_14_1, bp1_14_2, bp1_14_3, bp1_14_4, bp1_14_9, bp1_15, bp1_16_1, bp1_16_2, bp1_16_3, bp1_16_4, bp1_16_9, bp1_17, bp1_18, bp1_19_1, bp1_19_2, bp1_19_3, bp1_19_4, bp1_19_5, bp1_19_6, bp1_19_7, bp1_19_8, bp1_20, bp1_22, bp1_23, bp1_24, bp1_25, bp1_26, bp1_27, bp1_28, bp1_29, bp1_30, bp1_31_01, bp1_31_02, bp1_31_03, bp1_31_04, bp1_31_05, bp1_31_06, bp1_31_07, bp1_31_08, bp1_31_99, bp1_32_1, bp1_32_2, bp1_32_3, bp1_32_4, bp1_32_5, bp1_32_9, bp1_33)

# Genera un excel con el tibble creado anteriormente -

write.xlsx(Respuestas3, file = "Datos/Respuestas3.xlsx", sheetName = "Respuestas3")

### Victimización parte 3 ----

# Crea el diseño muestral -

design <- 
  TMod_Vic %>%
  as_survey_design(id = upm_dis, strata = est_dis, weights = fac_del)

# Realiza un estimado y un tibble por cada pregunta el cual contiene el total estimado por cada respuesta, los limites inferiores y superiores, el numero de pregunta y un identificador de la respuesta para la cual se esta estimando

bp2_1 <-
  design %>%
  group_by(bp2_1) %>%
  summarise(bp2_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp2_1, total_low = bp2_1_low, total_upp = bp2_1_upp) %>%
  mutate(pregunta = "bp2_1") %>% 
  mutate(respuesta = row_number())

bp3_1_01 <-
  design %>%
  group_by(bp3_1_01) %>%
  summarise(bp3_1_01 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_01, total_low = bp3_1_01_low, total_upp = bp3_1_01_upp) %>%
  mutate(pregunta = "bp3_1_01") %>% 
  mutate(respuesta = row_number())

bp3_1_02 <-
  design %>%
  group_by(bp3_1_02) %>%
  summarise(bp3_1_02 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_02, total_low = bp3_1_02_low, total_upp = bp3_1_02_upp) %>%
  mutate(pregunta = "bp3_1_02") %>% 
  mutate(respuesta = row_number())

bp3_1_03 <-
  design %>%
  group_by(bp3_1_03) %>%
  summarise(bp3_1_03 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_03, total_low = bp3_1_03_low, total_upp = bp3_1_03_upp) %>%
  mutate(pregunta = "bp3_1_03") %>% 
  mutate(respuesta = row_number())

bp3_1_04 <-
  design %>%
  group_by(bp3_1_04) %>%
  summarise(bp3_1_04 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_04, total_low = bp3_1_04_low, total_upp = bp3_1_04_upp) %>%
  mutate(pregunta = "bp3_1_04") %>% 
  mutate(respuesta = row_number())

bp3_1_05 <-
  design %>%
  group_by(bp3_1_05) %>%
  summarise(bp3_1_05 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_05, total_low = bp3_1_05_low, total_upp = bp3_1_05_upp) %>%
  mutate(pregunta = "bp3_1_05") %>% 
  mutate(respuesta = row_number())

bp3_1_06 <-
  design %>%
  group_by(bp3_1_06) %>%
  summarise(bp3_1_06 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_06, total_low = bp3_1_06_low, total_upp = bp3_1_06_upp) %>%
  mutate(pregunta = "bp3_1_06") %>% 
  mutate(respuesta = row_number())

bp3_1_07 <-
  design %>%
  group_by(bp3_1_07) %>%
  summarise(bp3_1_07 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_07, total_low = bp3_1_07_low, total_upp = bp3_1_07_upp) %>%
  mutate(pregunta = "bp3_1_07") %>% 
  mutate(respuesta = row_number())

bp3_1_08 <-
  design %>%
  group_by(bp3_1_08) %>%
  summarise(bp3_1_08 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_08, total_low = bp3_1_08_low, total_upp = bp3_1_08_upp) %>%
  mutate(pregunta = "bp3_1_08") %>% 
  mutate(respuesta = row_number())

bp3_1_09 <-
  design %>%
  group_by(bp3_1_09) %>%
  summarise(bp3_1_09 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_09, total_low = bp3_1_09_low, total_upp = bp3_1_09_upp) %>%
  mutate(pregunta = "bp3_1_09") %>% 
  mutate(respuesta = row_number())

bp3_1_10 <-
  design %>%
  group_by(bp3_1_10) %>%
  summarise(bp3_1_10 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_10, total_low = bp3_1_10_low, total_upp = bp3_1_10_upp) %>%
  mutate(pregunta = "bp3_1_10") %>% 
  mutate(respuesta = row_number())

bp3_1_11 <-
  design %>%
  group_by(bp3_1_11) %>%
  summarise(bp3_1_11 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_11, total_low = bp3_1_11_low, total_upp = bp3_1_11_upp) %>%
  mutate(pregunta = "bp3_1_11") %>% 
  mutate(respuesta = row_number())

bp3_1_12 <-
  design %>%
  group_by(bp3_1_12) %>%
  summarise(bp3_1_12 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_12, total_low = bp3_1_12_low, total_upp = bp3_1_12_upp) %>%
  mutate(pregunta = "bp3_1_12") %>% 
  mutate(respuesta = row_number())

bp3_1_99 <-
  design %>%
  group_by(bp3_1_99) %>%
  summarise(bp3_1_99 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_1_99, total_low = bp3_1_99_low, total_upp = bp3_1_99_upp) %>%
  mutate(pregunta = "bp3_1_99") %>% 
  mutate(respuesta = row_number())

bp3_2 <-
  design %>%
  group_by(bp3_2) %>%
  summarise(bp3_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp3_2, total_low = bp3_2_low, total_upp = bp3_2_upp) %>%
  mutate(pregunta = "bp3_2") %>% 
  mutate(respuesta = row_number())

bp4_1 <-
  design %>%
  group_by(bp4_1) %>%
  summarise(bp4_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp4_1, total_low = bp4_1_low, total_upp = bp4_1_upp) %>%
  mutate(pregunta = "bp4_1") %>% 
  mutate(respuesta = row_number())

bp5_1 <-
  design %>%
  group_by(bp5_1) %>%
  summarise(bp5_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp5_1, total_low = bp5_1_low, total_upp = bp5_1_upp) %>%
  mutate(pregunta = "bp5_1") %>% 
  mutate(respuesta = row_number())

bp5_2_1 <-
  design %>%
  group_by(bp5_2_1) %>%
  summarise(bp5_2_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp5_2_1, total_low = bp5_2_1_low, total_upp = bp5_2_1_upp) %>%
  mutate(pregunta = "bp5_2_1") %>% 
  mutate(respuesta = row_number())

bp5_2_2 <-
  design %>%
  group_by(bp5_2_2) %>%
  summarise(bp5_2_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp5_2_2, total_low = bp5_2_2_low, total_upp = bp5_2_2_upp) %>%
  mutate(pregunta = "bp5_2_2") %>% 
  mutate(respuesta = row_number())

bp5_2_3 <-
  design %>%
  group_by(bp5_2_3) %>%
  summarise(bp5_2_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp5_2_3, total_low = bp5_2_3_low, total_upp = bp5_2_3_upp) %>%
  mutate(pregunta = "bp5_2_3") %>% 
  mutate(respuesta = row_number())

bp5_2_4 <-
  design %>%
  group_by(bp5_2_4) %>%
  summarise(bp5_2_4 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp5_2_4, total_low = bp5_2_4_low, total_upp = bp5_2_4_upp) %>%
  mutate(pregunta = "bp5_2_4") %>% 
  mutate(respuesta = row_number())


bp5_3 <-
  design %>%
  group_by(bp5_3) %>%
  summarise(bp5_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp5_3, total_low = bp5_3_low, total_upp = bp5_3_upp) %>%
  mutate(pregunta = "bp5_3") %>% 
  mutate(respuesta = row_number())

bp6_1 <-
  design %>%
  group_by(bp6_1) %>%
  summarise(bp6_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp6_1, total_low = bp6_1_low, total_upp = bp6_1_upp) %>%
  mutate(pregunta = "bp6_1") %>% 
  mutate(respuesta = row_number())

bp6_2 <-
  design %>%
  group_by(bp6_2) %>%
  summarise(bp6_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp6_2, total_low = bp6_2_low, total_upp = bp6_2_upp) %>%
  mutate(pregunta = "bp6_2") %>% 
  mutate(respuesta = row_number())

bp6_3 <-
  design %>%
  group_by(bp6_3) %>%
  summarise(bp6_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp6_3, total_low = bp6_3_low, total_upp = bp6_3_upp) %>%
  mutate(pregunta = "bp6_3") %>% 
  mutate(respuesta = row_number())

bp7_1 <-
  design %>%
  group_by(bp7_1) %>%
  summarise(bp7_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = bp7_1, total_low = bp7_1_low, total_upp = bp7_1_upp) %>%
  mutate(pregunta = "bp7_1") %>% 
  mutate(respuesta = row_number())

# Junta los tibbles a un solo tibble que contiene todas las preguntas de este apartado -

Respuestas4 <-
  bind_rows(bp2_1, bp3_1_01, bp3_1_02, bp3_1_03, bp3_1_04, bp3_1_05, bp3_1_06, bp3_1_07, bp3_1_08, bp3_1_09, bp3_1_10, bp3_1_11, bp3_1_12, bp3_1_99, bp3_2, bp4_1, bp5_1, bp5_2_1, bp5_2_2, bp5_2_3, bp5_2_4, bp5_3, bp6_1, bp6_2, bp6_3, bp7_1)

# Genera un excel con el tibble creado anteriormente -

write.xlsx(Respuestas4, file = "Datos/Respuestas4.xlsx", sheetName = "Respuestas4")

### Victimización por hogar ----

# Crea el diseño muestral -

design <- 
  TPer_Vic2 %>%
  as_survey_design(id = upm_dis, strata = est_dis, weights = fac_hog)

# Realiza un estimado y un tibble por cada pregunta el cual contiene el total estimado por cada respuesta, los limites inferiores y superiores, el numero de pregunta y un identificador de la respuesta para la cual se esta estimando

ap6_2 <-
  design %>%
  group_by(ap6_2) %>%
  summarise(ap6_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_2, total_low = ap6_2_low, total_upp = ap6_2_upp) %>%
  mutate(pregunta = "ap6_2") %>% 
  mutate(respuesta = row_number())

ap6_3 <-
  design %>%
  group_by(ap6_3) %>%
  summarise(ap6_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_3, total_low = ap6_3_low, total_upp = ap6_3_upp) %>%
  mutate(pregunta = "ap6_3") %>% 
  mutate(respuesta = row_number())

ap6_4_01 <-
  design %>%
  group_by(ap6_4_01) %>%
  summarise(ap6_4_01 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_4_01, total_low = ap6_4_01_low, total_upp = ap6_4_01_upp) %>%
  mutate(pregunta = "ap6_4_01") %>% 
  mutate(respuesta = row_number())

ap6_4_02 <-
  design %>%
  group_by(ap6_4_02) %>%
  summarise(ap6_4_02 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_4_02, total_low = ap6_4_02_low, total_upp = ap6_4_02_upp) %>%
  mutate(pregunta = "ap6_4_02") %>% 
  mutate(respuesta = row_number())

ap6_4_03 <-
  design %>%
  group_by(ap6_4_03) %>%
  summarise(ap6_4_03 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_4_03, total_low = ap6_4_03_low, total_upp = ap6_4_03_upp) %>%
  mutate(pregunta = "ap6_4_03") %>% 
  mutate(respuesta = row_number())

ap6_4_04 <-
  design %>%
  group_by(ap6_4_04) %>%
  summarise(ap6_4_04 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_4_04, total_low = ap6_4_04_low, total_upp = ap6_4_04_upp) %>%
  mutate(pregunta = "ap6_4_04") %>% 
  mutate(respuesta = row_number())

ap6_5_01 <-
  design %>%
  group_by(ap6_5_01) %>%
  summarise(ap6_5_01 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_5_01, total_low = ap6_5_01_low, total_upp = ap6_5_01_upp) %>%
  mutate(pregunta = "ap6_5_01") %>% 
  mutate(respuesta = row_number())

ap6_5_02 <-
  design %>%
  group_by(ap6_5_02) %>%
  summarise(ap6_5_02 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_5_02, total_low = ap6_5_02_low, total_upp = ap6_5_02_upp) %>%
  mutate(pregunta = "ap6_5_02") %>% 
  mutate(respuesta = row_number())

ap6_6_01 <-
  design %>%
  group_by(ap6_6_01) %>%
  summarise(ap6_6_01 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_6_01, total_low = ap6_6_01_low, total_upp = ap6_6_01_upp) %>%
  mutate(pregunta = "ap6_6_01") %>% 
  mutate(respuesta = row_number())

ap6_6_02 <-
  design %>%
  group_by(ap6_6_02) %>%
  summarise(ap6_6_02 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_6_02, total_low = ap6_6_02_low, total_upp = ap6_6_02_upp) %>%
  mutate(pregunta = "ap6_6_02") %>% 
  mutate(respuesta = row_number())

ap6_6_03 <-
  design %>%
  group_by(ap6_6_03) %>%
  summarise(ap6_6_03 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_6_03, total_low = ap6_6_03_low, total_upp = ap6_6_03_upp) %>%
  mutate(pregunta = "ap6_6_03") %>% 
  mutate(respuesta = row_number())

ap6_6_04 <-
  design %>%
  group_by(ap6_6_04) %>%
  summarise(ap6_6_04 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_6_04, total_low = ap6_6_04_low, total_upp = ap6_6_04_upp) %>%
  mutate(pregunta = "ap6_6_04") %>% 
  mutate(respuesta = row_number())

ap6_7 <-
  design %>%
  group_by(ap6_7) %>%
  summarise(ap6_7 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_7, total_low = ap6_7_low, total_upp = ap6_7_upp) %>%
  mutate(pregunta = "ap6_7") %>% 
  mutate(respuesta = row_number())

ap6_8 <-
  design %>%
  group_by(ap6_8) %>%
  summarise(ap6_8 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_8, total_low = ap6_8_low, total_upp = ap6_8_upp) %>%
  mutate(pregunta = "ap6_8") %>% 
  mutate(respuesta = row_number())

ap6_9 <-
  design %>%
  group_by(ap6_9) %>%
  summarise(ap6_9 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_9, total_low = ap6_9_low, total_upp = ap6_9_upp) %>%
  mutate(pregunta = "ap6_9") %>% 
  mutate(respuesta = row_number())

ap6_10_1 <-
  design %>%
  group_by(ap6_10_1) %>%
  summarise(ap6_10_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_10_1, total_low = ap6_10_1_low, total_upp = ap6_10_1_upp) %>%
  mutate(pregunta = "ap6_10_1") %>% 
  mutate(respuesta = row_number())

ap6_10_2 <-
  design %>%
  group_by(ap6_10_2) %>%
  summarise(ap6_10_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_10_2, total_low = ap6_10_2_low, total_upp = ap6_10_2_upp) %>%
  mutate(pregunta = "ap6_10_2") %>% 
  mutate(respuesta = row_number())

ap6_11_1 <-
  design %>%
  group_by(ap6_11_1) %>%
  summarise(ap6_11_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_11_1, total_low = ap6_11_1_low, total_upp = ap6_11_1_upp) %>%
  mutate(pregunta = "ap6_11_1") %>% 
  mutate(respuesta = row_number())

ap6_12_1 <-
  design %>%
  group_by(ap6_12_1) %>%
  summarise(ap6_12_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_12_1, total_low = ap6_12_1_low, total_upp = ap6_12_1_upp) %>%
  mutate(pregunta = "ap6_12_1") %>% 
  mutate(respuesta = row_number())

ap6_13_1_1 <-
  design %>%
  group_by(ap6_13_1_1) %>%
  summarise(ap6_13_1_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_13_1_1, total_low = ap6_13_1_1_low, total_upp = ap6_13_1_1_upp) %>%
  mutate(pregunta = "ap6_13_1_1") %>% 
  mutate(respuesta = row_number())

ap6_14 <-
  design %>%
  group_by(ap6_14) %>%
  summarise(ap6_14 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_14, total_low = ap6_14_low, total_upp = ap6_14_upp) %>%
  mutate(pregunta = "ap6_14") %>% 
  mutate(respuesta = row_number())

ap6_15_1 <-
  design %>%
  group_by(ap6_15_1) %>%
  summarise(ap6_15_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_15_1, total_low = ap6_15_1_low, total_upp = ap6_15_1_upp) %>%
  mutate(pregunta = "ap6_15_1") %>% 
  mutate(respuesta = row_number())

ap6_15_2 <-
  design %>%
  group_by(ap6_15_2) %>%
  summarise(ap6_15_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_15_2, total_low = ap6_15_2_low, total_upp = ap6_15_2_upp) %>%
  mutate(pregunta = "ap6_15_2") %>% 
  mutate(respuesta = row_number())

ap6_16_1 <-
  design %>%
  group_by(ap6_16_1) %>%
  summarise(ap6_16_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_16_1, total_low = ap6_16_1_low, total_upp = ap6_16_1_upp) %>%
  mutate(pregunta = "ap6_16_1") %>% 
  mutate(respuesta = row_number())

ap6_16_2 <-
  design %>%
  group_by(ap6_16_2) %>%
  summarise(ap6_16_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_16_2, total_low = ap6_16_2_low, total_upp = ap6_16_2_upp) %>%
  mutate(pregunta = "ap6_16_2") %>% 
  mutate(respuesta = row_number())

ap6_16_3 <-
  design %>%
  group_by(ap6_16_3) %>%
  summarise(ap6_16_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_16_3, total_low = ap6_16_3_low, total_upp = ap6_16_3_upp) %>%
  mutate(pregunta = "ap6_16_3") %>% 
  mutate(respuesta = row_number())

ap6_17_1 <-
  design %>%
  group_by(ap6_17_1) %>%
  summarise(ap6_17_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_17_1, total_low = ap6_17_1_low, total_upp = ap6_17_1_upp) %>%
  mutate(pregunta = "ap6_17_1") %>% 
  mutate(respuesta = row_number())

ap6_17_2 <-
  design %>%
  group_by(ap6_17_2) %>%
  summarise(ap6_17_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_17_2, total_low = ap6_17_2_low, total_upp = ap6_17_2_upp) %>%
  mutate(pregunta = "ap6_17_2") %>% 
  mutate(respuesta = row_number())

ap6_17_3 <-
  design %>%
  group_by(ap6_17_3) %>%
  summarise(ap6_17_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_17_3, total_low = ap6_17_3_low, total_upp = ap6_17_3_upp) %>%
  mutate(pregunta = "ap6_17_3") %>% 
  mutate(respuesta = row_number())

ap6_18_1 <-
  design %>%
  group_by(ap6_18_1) %>%
  summarise(ap6_18_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_18_1, total_low = ap6_18_1_low, total_upp = ap6_18_1_upp) %>%
  mutate(pregunta = "ap6_18_1") %>% 
  mutate(respuesta = row_number())

ap6_18_2 <-
  design %>%
  group_by(ap6_18_2) %>%
  summarise(ap6_18_2 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_18_2, total_low = ap6_18_2_low, total_upp = ap6_18_2_upp) %>%
  mutate(pregunta = "ap6_18_2") %>% 
  mutate(respuesta = row_number())

ap6_18_3 <-
  design %>%
  group_by(ap6_18_3) %>%
  summarise(ap6_18_3 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_18_3, total_low = ap6_18_3_low, total_upp = ap6_18_3_upp) %>%
  mutate(pregunta = "ap6_18_3") %>% 
  mutate(respuesta = row_number())

ap6_19 <-
  design %>%
  group_by(ap6_19) %>%
  summarise(ap6_19 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_19, total_low = ap6_19_low, total_upp = ap6_19_upp) %>%
  mutate(pregunta = "ap6_19") %>% 
  mutate(respuesta = row_number())

ap6_20_1 <-
  design %>%
  group_by(ap6_20_1) %>%
  summarise(ap6_20_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_20_1, total_low = ap6_20_1_low, total_upp = ap6_20_1_upp) %>%
  mutate(pregunta = "ap6_20_1") %>% 
  mutate(respuesta = row_number())

ap6_20_1 <-
  design %>%
  group_by(ap6_20_1) %>%
  summarise(ap6_20_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_20_1, total_low = ap6_20_1_low, total_upp = ap6_20_1_upp) %>%
  mutate(pregunta = "ap6_20_1") %>% 
  mutate(respuesta = row_number())

ap6_21_1 <-
  design %>%
  group_by(ap6_21_1) %>%
  summarise(ap6_21_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_21_1, total_low = ap6_21_1_low, total_upp = ap6_21_1_upp) %>%
  mutate(pregunta = "ap6_21_1") %>% 
  mutate(respuesta = row_number())

ap6_22_1 <-
  design %>%
  group_by(ap6_22_1) %>%
  summarise(ap6_22_1 = survey_total(vartype = "ci")) %>%
  as_tibble() %>%
  select(total = ap6_22_1, total_low = ap6_22_1_low, total_upp = ap6_22_1_upp) %>%
  mutate(pregunta = "ap6_22_1") %>% 
  mutate(respuesta = row_number())

#Junta los tibbles a un solo tibble que contiene todas las preguntas de este apartado -

Respuestas5 <-
  bind_rows(ap6_2, ap6_3, ap6_4_01, ap6_4_02, ap6_4_03, ap6_5_01, ap6_5_02, ap6_6_01, ap6_6_02, ap6_6_03, ap6_6_04, ap6_7, ap6_8, ap6_9, ap6_10_1, ap6_10_2, ap6_11_1, ap6_12_1, ap6_13_1_1, ap6_14, ap6_15_1, ap6_15_2, ap6_16_1, ap6_16_2, ap6_16_3, ap6_17_1, ap6_17_2, ap6_17_3, ap6_18_1, ap6_18_2, ap6_18_3, ap6_19, ap6_20_1, ap6_21_1)

# Genera un excel con el tibble creado anteriormente -

write.xlsx(Respuestas5, file = "Datos/Respuestas5.xlsx", sheetName = "Respuestas5")

### Base de respuestas final ----

# Crea una sola base de datos con los estimados para todas las preguntas relevantes -
Respuestas <-
  bind_rows(Respuestas1, Respuestas2, Respuestas3, Respuestas4, Respuestas5)

# Genera un excel con la base de datos generada anteriormente -
write.xlsx(Respuestas, file = "Datos/Respuestas.xlsx", sheetName = "Respuestas")