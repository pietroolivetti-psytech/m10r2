
library(dplyr)
library(ggplot2)
library(lubridate)             

##########################################################################################
# FUNCTIONS

avgbycos <- function(df, year){
  res <- df %>%
    group_by(Id_cliente) %>%
    summarise(avg = mean(Importe_tiquet)) %>%
    summarise(all_avg = mean(avg)) %>%
    unlist() %>%
    round(2)
  
  res_fis <- df %>%
    filter(Origen_tiquet == 'Tienda física') %>%
    group_by(Id_cliente) %>%
    summarise(avg = mean(Importe_tiquet)) %>%
    summarise(all_avg = mean(avg)) %>%
    unlist() %>%
    round(2)
  
  res_onl <- df %>%
    filter(Origen_tiquet == 'Tienda online') %>%
    group_by(Id_cliente) %>%
    summarise(avg = mean(Importe_tiquet)) %>%
    summarise(all_avg = mean(avg)) %>%
    unlist() %>%
    round(2)
  
  cat("\nMean Purchase by costumer", year, ":",res)
  cat("\nMean Purchase by costumer in Store", year, ":",res_fis)
  cat("\nMean Purchase by costumer Online", year, ":",res_onl)  
  cat("\n")
  
  
  allres <- c(res, res_fis, res_onl)
  cols <- c("Total", "Solo Física", "Solo Online")
  df <- cbind(cols, allres)
  return(df)
} 



frec_rec <- function(df, yr){
  nj <- df %>%
    select(Id_cliente, Fecha_tiquet) %>%
    arrange(Id_cliente, Fecha_tiquet) %>% 
    group_by(Id_cliente) %>%
    mutate(dias_entre_compras = as.numeric(difftime(Fecha_tiquet, lag(Fecha_tiquet), units = "days"))) %>%
    arrange(Id_cliente, Fecha_tiquet) %>% 
    ungroup()
  
  
  # boxplot usando ggplot2
  bplot <- ggplot(nj, aes(x = "", y = dias_entre_compras)) + geom_boxplot(fill = "lightblue", color = "darkblue") + labs(title = paste("Boxplot of Days Between Purchases -", yr), x = "", y = "Days Between Purchases") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))  
  
  plot(bplot)  
  
  nj <- nj %>%
    mutate(Periodo = floor_date(Fecha_tiquet, "month"))
  
  nj <- nj %>%
    group_by(Id_cliente) %>%
    mutate(Comprado_proximo_mes = lead(Periodo) == Periodo + months(1)) %>%
    ungroup()
  
  prob_rec <- nj %>%
    group_by(Periodo) %>%
    summarize(probabilidad = mean(Comprado_proximo_mes, na.rm = TRUE))
  
  tiempo_medio_por_cliente <- nj %>%
    group_by(Id_cliente) %>%
    summarize(tiempo_medio_compras = mean(dias_entre_compras, na.rm = TRUE))
  
  # Gráfico de barras para visualizar la probabilidad de recurrencia por periodo
  barplot(prob_rec$probabilidad,
          names.arg = as.character(prob_rec$Periodo),
          main = "Probabilidad de Recurrencia por Mes",
          ylab = "Probabilidad de Recurrencia",
          col = "lightgreen",
          las = 2)
  
  qt75 <- quantile(tiempo_medio_por_cliente$tiempo_medio_compras, 0.75, na.rm = T)
  
  hist(tiempo_medio_por_cliente$tiempo_medio_compras, 
       breaks = 20, 
       main = "Distribución del Tiempo Medio entre Compras", 
       xlab = "Días entre Compras", 
       col = "lightblue")
  abline(v = qt75, col = "red", lwd = 2)
  legend("topright", legend = paste("75th Percentile: ", qt75), lty = 1, lwd = 2, col = "red")
  
  
  print(prob_rec)
  print(tiempo_medio_por_cliente)
  head(nj)
  
  print(summary(nj))
  print(summary(tiempo_medio_por_cliente))
  print(summary(prob_rec))
  
}

##########################################################################################

# LOAD
cli <- read.csv('Maestro de Clientes.csv', sep = ';')
tiq <- read.csv('Maestro Tiquets Cliente.csv', sep = ';')

##########################################################################################

# CLEANING MAESTRO TIQUETS CLIENTE

# Id_cliente: Eliminemos el unico NA
tiq <- tiq %>% filter(!is.na(tiq$Id_cliente))

# Origen_cliente y Id_tiquet no parecen requerer cambios

#Fecha
tiq$Fecha_tiquet <- as.Date(tiq$Fecha_tiquet, format = "%d/%m/%Y")

# Importe: Cambiando coma por punto para convertir en numeric
tiq$Importe_tiquet <- gsub(",", ".", tiq$Importe_tiquet)
tiq$Importe_tiquet  <- as.numeric(tiq$Importe_tiquet)

# Articulos. Eliminamos los 2 valores que son invalidos y parecen tratarse de un equivoco.


tiq <- tiq %>% filter(!Articulos_tiquet == 14942330 & !Articulos_tiquet == -14942330)

##########################################################################################

# CLEANING MAESTRO DE CLIENTES

# Eliminando la variable Género
cli$Genero <- NULL

#Edad
mean_age <- round(mean(cli$Edad_actual, na.rm = T))


# Añadiendo la media para valores inválidos

cli <- cli %>%
  mutate(Edad_actual =  ifelse(Edad_actual < 18 | Edad_actual > 120, mean_age, Edad_actual)) 


# Cuento de NAs en Edad tinene que ver con si la tienda es física o online?

onl_age_na <- sum(cli$Tienda_CaptacionCRM == 'Tienda online' & is.na(cli$Edad_actual))
fis_age_na <- sum(cli$Tienda_CaptacionCRM == 'Tienda física' & is.na(cli$Edad_actual))


# Fecha
cli$Fecha_CaptacionCRM <- as.Date(cli$Fecha_CaptacionCRM, format = "%d/%m/%Y")

# Promedio de edad de tienda online
avg_onl <- cli %>%
  filter(Tienda_CaptacionCRM == 'Tienda online') %>%
  summarize(mean(Edad_actual, na.rm = T))
avg_onl <- round(avg_onl[[1]])

# Promedio de edad de tienda física
avg_fis <- cli %>%
  filter(Tienda_CaptacionCRM == 'Tienda física') %>%
  summarize(mean(Edad_actual, na.rm = T))
avg_fis <- round(avg_fis[[1]])


# Borrando todos os valores vacios:
cli <- cli[cli$Tienda_CaptacionCRM != '', ]

# Añadiremos el promedio de cada tipo de Establecimiento 

cli <- cli %>%
  mutate(Edad_actual = case_when(Tienda_CaptacionCRM == 'Tienda física' & is.na(Edad_actual) ~ avg_fis, 
                                 Tienda_CaptacionCRM == 'Tienda online' & is.na(Edad_actual) ~ avg_onl, 
                                 TRUE ~ Edad_actual))

cli <- cli %>%
  mutate(Pais = case_when(Pais == 'Spain' ~ 'ES', 
                          Pais == 'PORTUGAL' ~ 'PT',
                          Pais == 'REPÚBLICA CHECA'  ~ 'CZ',
                          Pais == 'ITALIA' ~ 'IT',
                          Pais == 'HOLANDA' ~ 'NL',
                          Pais == 'GRECIA' ~ 'GR',
                          Pais == 'ESTONIA' ~ 'EE',
                          Pais == 'IRLANDA' ~ 'IE',
                          Pais == 'ESLOVAQUIA' ~ 'SK',
                          Pais == 'REINO UNIDO' ~ 'GB',
                          Pais == 'FRANCIA' ~ 'FR',
                          Pais == 'AUSTRIA' ~ 'AT',
                          Pais == 'CHIPRE' ~ 'CY',
                          Pais == 'ALEMANIA' ~ 'DE',
                          Pais == 'MALTA' ~ 'MT',
                          Pais == 'BÉLGICA' ~ 'BE',
                          Pais == 'LUXEMBURGO' ~ 'LU',
                          Pais == 'SUECIA' ~ 'SE',
                          Pais == 'DINAMARCA' ~ 'DK',
                          Pais == '10' ~ "",
                          Pais == 'HUNGRÍA' ~ 'HU',
                          Pais == '11' ~ "",
                          Pais == 'BULGARIA' ~ 'BG',
                          Pais == 'RUMANIA' ~ 'RO',
                          Pais == 'ESLOVENIA' ~ 'SI',
                          Pais == 'LITUANIA' ~ 'LT',
                          Pais == 'POLONIA' ~ 'PL',
                          Pais == 'FINLANDIA' ~ 'FI',
                          Pais == 'CROACIA' ~ 'HR',
                          Pais == 'PO' ~ "",
                          Pais == 'LETONIA' ~ 'LV',
                          Pais == '5' ~ "",
                          Pais == 'ANDORRA' ~ 'AD',
                          TRUE ~ Pais))



cli <- cli[cli$Pais != '', ]

# IDs

cli <- cli %>% filter(!is.na(Id_cliente))
##########################################################################################


## Clientes Vivos: Número total de clientes registrados en el programa de fidelización. 
#2023: 32576
#2022: 81793
#2021: 75302

# DFs por año
cli2023 <- cli %>% filter(format(Fecha_CaptacionCRM, '%Y') == '2023') 
cli2022 <- cli %>% filter(format(Fecha_CaptacionCRM, '%Y') == '2022') 
cli2021 <- cli %>% filter(format(Fecha_CaptacionCRM, '%Y') == '2021') 

#DFs correspondientes a los datos de 2023
cli2022_05 <- cli %>% filter(format(Fecha_CaptacionCRM, '%Y-%m') >= '2022-01' & format(Fecha_CaptacionCRM, '%Y-%m') <= '2022-05')
cli2021_05 <- cli %>% filter(format(Fecha_CaptacionCRM, '%Y-%m') >= '2021-01' & format(Fecha_CaptacionCRM, '%Y-%m') <= '2021-05')

# N de clientes vivos (inscritos en el programa de fidelización)
cv2023 <- length(unique(cli2023$Id_cliente))
cv2022 <- length(unique(cli2022$Id_cliente))
cv2021 <- length(unique(cli2021$Id_cliente))
cv2022_05 <- length(unique(cli2022_05$Id_cliente))
cv2021_05 <- length(unique(cli2021_05$Id_cliente))

# Variaciones
ncv_var2021_2022 <- round(((cv2022 - cv2021) / cv2021) * 100, 2)
ncv_var2021_2022_05 <- round(((cv2022_05 - cv2021_05) / cv2021_05) * 100, 2)
ncv_var2022_2023_05 <- round(((cv2023 - cv2022_05) / cv2022_05) * 100, 2)

##########################################################################################

#Clientes Activos: Filtrar los clientes con compras en el período de análisis y 
#calcular el número de clientes únicos en ese subconjunto.

# Juntando las dos tablas con inner_join elimino a los clientes del programa de fidelización que nunca compraron
joined <- inner_join(cli, tiq, by = "Id_cliente", relationship = "many-to-many")


# Dfs por periodos
joined2023 <- joined %>% filter(format(Fecha_tiquet, '%Y') == '2023') 
joined2022 <- joined %>% filter(format(Fecha_tiquet, '%Y') == '2022') 
joined2021 <- joined %>% filter(format(Fecha_tiquet, '%Y') == '2021') 
joined2022_05 <- joined %>% filter(format(Fecha_tiquet, '%Y-%m') >= '2022-01' & format(Fecha_tiquet, '%Y-%m') <= '2022-05') 
joined2021_05 <- joined %>% filter(format(Fecha_tiquet, '%Y-%m') >= '2021-01' & format(Fecha_tiquet, '%Y-%m') <= '2021-05') 

joined_crm_2023 <- joined %>% filter(format(Fecha_CaptacionCRM, '%Y') == '2023') 
joined_crm_2022 <- joined %>% filter(format(Fecha_CaptacionCRM, '%Y') == '2022') 
joined_crm_2021 <- joined %>% filter(format(Fecha_CaptacionCRM, '%Y') == '2021') 
joined_crm_old <- joined %>% filter(format(Fecha_CaptacionCRM, '%Y') < '2021')
joined_crm_2022_05 <- joined %>% filter(format(Fecha_CaptacionCRM, '%Y-%m') >= '2022-01' & format(Fecha_tiquet, '%Y-%m') <= '2022-05') 
joined_crm_2021_05 <- joined %>% filter(format(Fecha_CaptacionCRM, '%Y-%m') >= '2021-01' & format(Fecha_tiquet, '%Y-%m') <= '2021-05') 

joined_yrs <- bind_rows(
  joined2021 %>% mutate(yr = 2021),
  joined2022 %>% mutate(yr = 2022),
  joined2023 %>% mutate(yr = 2023))


# N de clientes activos por periodo
# calculo el número de identificadores unicos(Id_cliente) en joined202X (gracias al inner_join solo hay quienes tienen tiquets de compras asociados) que también están presentes en cli202X.
nac2023 <- joined2023 %>% select(Id_cliente) %>%  filter(Id_cliente %in% cli2023$Id_cliente) %>% summarise(n_distinct(Id_cliente)) %>% unlist()

nac2022 <- joined2022 %>% select(Id_cliente) %>%  filter(Id_cliente %in% cli2022$Id_cliente) %>% summarise(n_distinct(Id_cliente)) %>% unlist()

nac2021 <- joined2021 %>% select(Id_cliente) %>%  filter(Id_cliente %in% cli2021$Id_cliente) %>% summarise(n_distinct(Id_cliente)) %>% unlist()

nac2022_05 <- joined2022_05 %>% select(Id_cliente) %>%  filter(Id_cliente %in% cli2022_05$Id_cliente) %>% summarise(n_distinct(Id_cliente)) %>% unlist()

nac2021_05 <- joined2021_05 %>% select(Id_cliente) %>%  filter(Id_cliente %in% cli2021_05$Id_cliente) %>% summarise(n_distinct(Id_cliente)) %>% unlist()

#VARIACIONES
ac_var2021_2022 <- round(((nac2022 - nac2021) / nac2021) * 100, 2)
ac_var2021_2022_05 <- round(((nac2022_05 - nac2021_05) / nac2021_05) * 100, 2)
ac_var2022_2023_05 <- round(((nac2023 - nac2022_05) / nac2022_05) * 100, 2)

##########################################################################################

# PROMEDIO COMPRAS

meantiq <- joined %>%
  summarize(
    Min = min(Importe_tiquet),
    Q1 = quantile(Importe_tiquet, 0.25),
    Median = median(Importe_tiquet),
    Mean = mean(Importe_tiquet),
    Q3 = quantile(Importe_tiquet, 0.75),
    Max = max(Importe_tiquet)) %>%
  t()

meantiq2023 <-  joined2023 %>%
  summarize(
    Min = min(Importe_tiquet),
    Q1 = quantile(Importe_tiquet, 0.25),
    Median = median(Importe_tiquet),
    Mean = mean(Importe_tiquet),
    Q3 = quantile(Importe_tiquet, 0.75),
    Max = max(Importe_tiquet)) %>%
  t()

meantiq2022 <-  joined2022 %>%
  summarize(
    Min = min(Importe_tiquet),
    Q1 = quantile(Importe_tiquet, 0.25),
    Median = median(Importe_tiquet),
    Mean = mean(Importe_tiquet),
    Q3 = quantile(Importe_tiquet, 0.75),
    Max = max(Importe_tiquet)) %>%
  t()

meantiq2022_05 <-  joined2022_05 %>%
  summarize(
    Min = min(Importe_tiquet),
    Q1 = quantile(Importe_tiquet, 0.25),
    Median = median(Importe_tiquet),
    Mean = mean(Importe_tiquet),
    Q3 = quantile(Importe_tiquet, 0.75),
    Max = max(Importe_tiquet)) %>%
  t()

meantiq2021 <-  joined2021 %>%
  summarize(
    Min = min(Importe_tiquet),
    Q1 = quantile(Importe_tiquet, 0.25),
    Median = median(Importe_tiquet),
    Mean = mean(Importe_tiquet),
    Q3 = quantile(Importe_tiquet, 0.75),
    Max = max(Importe_tiquet)) %>%
  t()

meantiq2021_05 <-  joined2021_05 %>%
  summarize(
    Min = min(Importe_tiquet),
    Q1 = quantile(Importe_tiquet, 0.25),
    Median = median(Importe_tiquet),
    Mean = mean(Importe_tiquet),
    Q3 = quantile(Importe_tiquet, 0.75),
    Max = max(Importe_tiquet)) %>%
  t()

data.frame(meantiq, meantiq2023, meantiq2022_05, meantiq2021_05, meantiq2022, meantiq2021)

# Promedio
mtiq2023 <- round(meantiq2023[4], 2)
mtiq2022 <- round(meantiq2022[4], 2)
mtiq2021 <- round(meantiq2021[4], 2)
mtiq2021_05 <- round(meantiq2021_05[4], 2)
mtiq2022_05 <- round(meantiq2022_05[4], 2)

#Variaciones
mtiq_var_2122 <- round(((mtiq2022 - mtiq2021) / mtiq2021) * 100, 2)
mtiq_var_2122_05 <- round(((mtiq2022_05 - mtiq2021_05) / mtiq2021_05) * 100, 2)
mtiq_var_2223_05 <- round(((mtiq2023 - mtiq2022_05) / mtiq2022_05) * 100, 2)

##########################################################################################
#Compra media por cliente

bycost21 <- avgbycos(joined2021, "2021")
mbycost21 <- bycost21[1,2]
bycost22 <- avgbycos(joined2022, "2022")
mbycost22 <- bycost22[1,2]
bycost23 <- avgbycos(joined2023, "2023")
mbycost23 <- bycost23[1,2]
bycost21_05 <- avgbycos(joined2021_05, "2021")
mbycost21_05 <- bycost21_05[1,2]
bycost22_05 <- avgbycos(joined2022_05, "2021")
mbycost22_05 <- bycost22_05[1,2]


bycost_var_2122 <- round(((as.numeric(mbycost22) - as.numeric(mbycost21)) / as.numeric(mbycost21))*100, 2)
bycost_var_2122_05 <- round(((as.numeric(mbycost22_05) - as.numeric(mbycost21_05)) / as.numeric(mbycost21_05))*100, 2)
bycost_var_2223_05 <- round(((as.numeric(mbycost23) - as.numeric(mbycost22_05)) / as.numeric(mbycost22_05))*100, 2)

##########################################################################################
# UPT o Promedio de articulos

meanart <- joined %>%
  summarize(
    Min = min(Articulos_tiquet),
    Q1 = quantile(Articulos_tiquet, 0.25),
    Median = median(Articulos_tiquet),
    Mean = mean(Articulos_tiquet),
    Q3 = quantile(Articulos_tiquet, 0.75),
    Max = max(Articulos_tiquet)) %>%
  t()

meanart2023 <-  joined2023 %>%
  summarize(
    Min = min(Articulos_tiquet),
    Q1 = quantile(Articulos_tiquet, 0.25),
    Median = median(Articulos_tiquet),
    Mean = mean(Articulos_tiquet),
    Q3 = quantile(Articulos_tiquet, 0.75),
    Max = max(Articulos_tiquet)) %>%
  t()

meanart2022 <-  joined2022 %>%
  summarize(
    Min = min(Articulos_tiquet),
    Q1 = quantile(Articulos_tiquet, 0.25),
    Median = median(Articulos_tiquet),
    Mean = mean(Articulos_tiquet),
    Q3 = quantile(Articulos_tiquet, 0.75),
    Max = max(Articulos_tiquet)) %>%
  t()

meanart2022_05 <-  joined2022_05 %>%
  summarize(
    Min = min(Articulos_tiquet),
    Q1 = quantile(Articulos_tiquet, 0.25),
    Median = median(Articulos_tiquet),
    Mean = mean(Articulos_tiquet),
    Q3 = quantile(Articulos_tiquet, 0.75),
    Max = max(Articulos_tiquet)) %>%
  t()

meanart2021 <-  joined2021 %>%
  summarize(
    Min = min(Articulos_tiquet),
    Q1 = quantile(Articulos_tiquet, 0.25),
    Median = median(Articulos_tiquet),
    Mean = mean(Articulos_tiquet),
    Q3 = quantile(Articulos_tiquet, 0.75),
    Max = max(Articulos_tiquet)) %>%
  t()

meanart2021_05 <-  joined2021_05 %>%
  summarize(
    Min = min(Articulos_tiquet),
    Q1 = quantile(Articulos_tiquet, 0.25),
    Median = median(Articulos_tiquet),
    Mean = mean(Articulos_tiquet),
    Q3 = quantile(Articulos_tiquet, 0.75),
    Max = max(Articulos_tiquet)) %>%
  t()

#data.frame(meanart, meanart2023, meanart2022_05, meanart2021_05, meanart2022, meanart2021)

upt21 <- round(meanart2021[4], 2)
upt22 <- round(meanart2022[4], 2)
upt23 <- round(meanart2023[4], 2)
upt21_05 <- round(meanart2021_05[4], 2)
upt22_05 <- round(meanart2022_05[4], 2)

var_upt_2122 <- round( ((upt22 - upt21) / upt21)  * 100 ,2)
var_upt_2122_05 <- round( ((upt22_05 - upt21_05) / upt21_05)  * 100 ,2)
var_upt_2223_05 <- round( ((upt23 - upt22_05) / upt22_05)  * 100 ,2)

##########################################################################################

# bargraph de importes totales
tiqtot2023graph <- ggplot(joined2023, aes(x = month(Fecha_tiquet), y = Importe_tiquet)) + 
  stat_summary(fun = sum, geom = "bar", fill = "steelblue") + labs(x = "Mes", y = "Importe Total de Tiquetes ($)", title = "Importe Total de Tiquetes por Mes en 2023") + scale_fill_brewer(palette = "Set1") + theme_classic() + scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

tiqtot2022graph <- ggplot(joined2022, aes(x = month(Fecha_tiquet), y = Importe_tiquet)) + 
  stat_summary(fun = sum, geom = "bar", fill = "darkgreen") + scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + labs(x = "Mes", y = "Importe Total de Tiquetes ($)", title = "Importe Total de Tiquetes por Mes en 2022") + theme_classic()

tiqtot2021graph <- ggplot(joined2021, aes(x = month(Fecha_tiquet), y = Importe_tiquet)) + 
  stat_summary(fun = sum, geom = "bar", fill = "pink4") + labs(x = "Mes", y = "Importe Total de Tiquetes ($)", title = "Importe Total de Tiquetes por Mes en 2021") + theme_classic() + scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

tiqtot2022_05graph <- ggplot(joined2022_05, aes(x = month(Fecha_tiquet), y = Importe_tiquet)) + 
  stat_summary(fun = sum, geom = "bar", fill = "limegreen") + scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) + labs(x = "Mes", y = "Importe Total de Tiquetes ($)", title = "Importe Total de Tiquetes por Mes en 2022 hasta mayo") + theme_classic()

tiqtot2021_05graph <- ggplot(joined2021_05, aes(x = month(Fecha_tiquet), y = Importe_tiquet)) + 
  stat_summary(fun = sum, geom = "bar", fill = "pink1") + labs(x = "Mes", y = "Importe Total de Tiquetes ($)", title = "Importe Total de Tiquetes por Mes en 2021 hasta mayo") + theme_classic() + scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))

##########################################################################################

# Haciendo porbabilidad de recurrencia y tiempo medio gasto por cliente

joined_yrs_frecrec <- joined_yrs %>%
  select(Id_cliente, Fecha_tiquet, Id_tiquet, yr) %>%
  arrange(Id_cliente, Fecha_tiquet) %>% 
  group_by(Id_cliente) %>%
  mutate(dias_entre_compras = as.numeric(difftime(Fecha_tiquet, lag(Fecha_tiquet), units = "days"))) %>%
  arrange(Id_cliente, Fecha_tiquet) %>% 
  ungroup()




joined_yrs_frecrec <- joined_yrs_frecrec %>%
  mutate(Periodo = floor_date(Fecha_tiquet, "month"))



# Crear una matriz de transición: ver si el cliente compró en el mes siguiente
joined_yrs_frecrec <- joined_yrs_frecrec %>%
  group_by(Id_cliente) %>%
  mutate(Comprado_proximo_mes = lead(Periodo) == Periodo + months(1)) %>%
  ungroup()



joined_yrs_frecrec2021 <- joined_yrs_frecrec %>%  filter(yr == 2021)
joined_yrs_frecrec2022 <- joined_yrs_frecrec %>%  filter(yr == 2022)
joined_yrs_frecrec2023 <- joined_yrs_frecrec %>%  filter(yr == 2023)

prob_rec <- joined_yrs_frecrec %>% group_by(Periodo) %>% summarize(probabilidad = mean(Comprado_proximo_mes, na.rm = TRUE))
prob_rec2021 <- joined_yrs_frecrec2021 %>% group_by(Periodo) %>% summarize(probabilidad = mean(Comprado_proximo_mes, na.rm = TRUE))
prob_rec2022 <- joined_yrs_frecrec2022 %>% group_by(Periodo) %>% summarize(probabilidad = mean(Comprado_proximo_mes, na.rm = TRUE))
prob_rec2023 <- joined_yrs_frecrec2023 %>% group_by(Periodo) %>% summarize(probabilidad = mean(Comprado_proximo_mes, na.rm = TRUE))


tiempo_medio_por_cliente <- joined_yrs_frecrec %>% group_by(Id_cliente) %>% summarize(tiempo_medio_compras = mean(dias_entre_compras, na.rm = TRUE))

tiempo_medio_por_cliente2023 <- joined_yrs_frecrec2023 %>% group_by(Id_cliente) %>% summarize(tiempo_medio_compras = mean(dias_entre_compras, na.rm = TRUE))

tiempo_medio_por_cliente2022 <- joined_yrs_frecrec2022 %>% group_by(Id_cliente) %>% summarize(tiempo_medio_compras = mean(dias_entre_compras, na.rm = TRUE))

tiempo_medio_por_cliente2021 <- joined_yrs_frecrec2021 %>% group_by(Id_cliente) %>% summarize(tiempo_medio_compras = mean(dias_entre_compras, na.rm = TRUE))


probabilidad_recurrencia <- joined_yrs_frecrec %>%
  group_by(Periodo) %>%
  summarize(probabilidad = mean(Comprado_proximo_mes, na.rm = TRUE))

#probabilidad_recurrencia

tiempo_medio_por_cliente <- joined_yrs_frecrec %>%
  group_by(Id_cliente) %>%
  summarize(tiempo_medio_compras = mean(dias_entre_compras, na.rm = TRUE))

#summary(tiempo_medio_por_cliente$tiempo_medio_compras)

##########################################################################################




nj <- joined %>%
  select(Id_cliente, Importe_tiquet ,Fecha_tiquet, Id_tiquet) %>%
  arrange(Id_cliente, Fecha_tiquet) %>% 
  group_by(Id_cliente) %>%
  mutate(dias_entre_compras = as.numeric(difftime(Fecha_tiquet, lag(Fecha_tiquet), units = "days"))) %>%
  arrange(Id_cliente, Fecha_tiquet) %>% 
  ungroup()




# Calcular el Ticket Medio (valor promedio gastado por compra)
clientes2122 <- df2122 %>%
  group_by(Id_cliente) %>%
  summarise(Ticket_Medio = mean(Importe_tiquet, na.rm = TRUE))

#clientes

# Calcular la Frecuencia de Compra (número de compras en el período de 12 meses)
frecuencia2122 <- df2122 %>%
  group_by(Id_cliente) %>%
  summarise(Frecuencia_Compra = n())  # Contar las compras en 2022

#frecuencia

# Calcular el Tiempo de Vida en años
tiempo_vida2122 <- df2122 %>%
  group_by(Id_cliente) %>%
  summarise(Fecha_Primera_Compra = min(Fecha_tiquet), Fecha_Ult_Compra = max(Fecha_tiquet)) %>%
  mutate(Tiempo_Vida = as.numeric(difftime('2023-05-31', Fecha_Primera_Compra, units = "weeks")) / 52)

fechacrm_cltv2122 <- df_cltv2122 %>% select(Id_cliente, Fecha_CaptacionCRM)

# Unir todas las métricas y calcular el CLTV
clientes_cltv2122 <- clientes2122 %>%
  left_join(frecuencia2122, by = "Id_cliente") %>%
  left_join(tiempo_vida2122, by = "Id_cliente") %>%
  left_join(fechacrm_cltv2122, by = "Id_cliente") %>%
  mutate(CLTV = Ticket_Medio * Frecuencia_Compra * Tiempo_Vida)



# Calcular el Ticket Medio (valor promedio gastado por compra)
clientes2223 <- df_cltv2223 %>%
  group_by(Id_cliente) %>%
  summarise(Ticket_Medio = mean(Importe_tiquet, na.rm = TRUE))


# Calcular la Frecuencia de Compra (número de compras en el período de 12 meses)
frecuencia2223 <- df_cltv2223 %>%
  group_by(Id_cliente) %>%
  summarise(Frecuencia_Compra = n())  # Contar las compras en 2022



# Calcular el Tiempo de Vida en años
tiempo_vida2223 <- df_cltv2223 %>%
  group_by(Id_cliente) %>%
  summarise(Fecha_Primera_Compra = min(Fecha_tiquet), Fecha_Ult_Compra = max(Fecha_tiquet)) %>%
  mutate(Tiempo_Vida = as.numeric(difftime('2023-05-31', Fecha_Primera_Compra, units = "weeks")) / 52)

#tiempo_vida

fechacrm_cltv2223 <- df_cltv2223 %>% select(Id_cliente, Fecha_CaptacionCRM)
#fechacrm_cltv2223

# Unir todas las métricas y calcular el CLTV
clientes_cltv2223 <- clientes2223 %>%
  left_join(frecuencia2223, by = "Id_cliente") %>%
  left_join(tiempo_vida2223, by = "Id_cliente") %>%
  left_join(fechacrm_cltv2223, by = "Id_cliente") %>%
  mutate(CLTV = Ticket_Medio * Frecuencia_Compra * Tiempo_Vida)
























# Calculate the median CLTV for each cluster
kpi_median <- kpi2123 %>%
  group_by(cluster) %>%
  summarise(Mediana_CLTV = median(CLTV))

# Bar graph
ggplot(kpi_median, aes(x = factor(cluster), y = Mediana_CLTV, fill = factor(cluster))) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  scale_fill_manual(values = c("red", "blue", "green", "orange"),
                    labels = c("Clientes Premium ", "Clientes Frecuentes", "Clientes Potenciales", "Clientes Dormidos")) +
  labs(
    title = "CLTV por Cluster de Clientes",
    x = "Grupo de Clientes",
    y = "Mediana de CLTV",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "top"
  )








# Nombres de los clusters y valores
clusters <- c( "Clientes Potenciales", "Clientes Dormidos", "Clientes Frecuentes", "Clientes Premium")
values <- c(rn2, rn4, rn3, rn1)

# Crear un data frame para el gráfico
data <- data.frame(Cluster = clusters, Conteo = values)

data <- data %>%
  mutate(Porcentaje = round(Conteo / sum(Conteo) * 100, 1))

# Crear el gráfico de barras horizontal
ggplot(data, aes(x = Conteo, y = Cluster, fill = Cluster)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Porcentaje, "%")), hjust = -0.1) +
  labs(title = "Conteo de Clientes por Cluster", x = "Cantidad de Clientes", y = NULL) +
  scale_fill_manual(values = c("orange", "green", "blue", "red")) +
  theme_minimal() +
  theme(legend.position = "none") + xlim(0, 63000) 




kmeans_result <- kmeans(kpi2123_scaled, centers = 4)

kpi2123$cluster <- kmeans_result$cluster

cl2123_gjitter <- ggplot(data = kpi2123, aes(x = recencia_dias, y = valor_total, 
                           color = as.factor(cluster), size = log(Frecuencia_Compra))) + 
  geom_jitter(alpha = 0.5) +
  scale_color_manual(values = c("red", "blue", "green", "orange"), 
                     name = "Grupos de clientes", labels = c("Clientes Premium  (1)", "Clientes Frecuentes (2)", "Clientes Dormidos (3)","Clientes Potenciales(4)")) + scale_size_continuous(name = "Frecuencia (log)", breaks = c(0, 2, 4, 6)) + theme_minimal() + ylim(0, 1000) + 
  labs(title = "Clustering de Clientes basado entre 2021 a 2023",
       subtitle = "Grupos diferenciados por Recencia de Compra y Valor Total",
       x = "Recencia (Días desde la Última Compra)",
       y = "Valor Monetario (Gasto Total)") + 
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "right",           # Position the legend on the right
    plot.title = element_text(size = 14, face = "bold"),  # Title styling
    plot.subtitle = element_text(size = 10),             # Subtitle styling
    strip.text = element_text(size = 10, face = "bold")  # Facet label styling
  ) + 
  facet_wrap(~cluster, 
             labeller = labeller(cluster = c("1" = "Clientes Premium ", "2" = "Clientes Frecuentes ", 
                                             "3" = "Clientes Potenciales", "4" = "Clientes Dormidos")))  
