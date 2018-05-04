# El script genera:
# 100 muestras completas
# 100 faltantes a nivel casilla 
# 100 faltantes estrato

## Marco muestral
# Se ubica en la carpeta de datos_procesados/Presidente2012_20180430 para 
# crearlo se tomó como base el archivo Presidente2012_completo.csv (creado por 
# Paty) y se actualizaron estratos, conservando el nombre de variables de la 
# versión anterior.
## Selección de muestras
# Se consideran 3 escenarios y para cada uno se generan 100 muestras:
# 1) Muestra completa: se seleccionan 100 muestras usando la estratificación 
# y tamaños de muestra de acuerdo al archivo info_estratos.csv
# 2) Muestra faltantes_casilla: se seleccionaron 100 muestras censurando las 
# muestras completas del inciso anterior, donde la probabilidad de que una 
# casilla sea censurada considera los patrones observados en las remesas 2012 
# con corte a las 22:00 horas.
# 3) Muestra faltantes_estrato: se seleccionaron 100 muestras censurando las 
# muestras completas del inciso 1), en este caso se eliminan 10 estratos por 
# muestra. Los estratos a censurar se seleccionan con probabilidad proporcional 
# a los votos recibidos por el PAN en el estrato.


library(quickcountmx)
library(tidyverse)
library(lubridate)

# 1. 100 muestras completas
marco <- read_csv("../datos/Presidente2012_20180430/Presidente2012_completo.csv")
tamanos <- read_csv("../datos/Presidente2012_20180430/info_estratos.csv") %>% 
    select(estrato = ESTRATO, nh)

marco_rural <- marco %>% 
    mutate(rural = (
        Tipo_seccion_8feb2012 == "R") * 1, 
        estrato_faltantes = str_c(iD_ESTADO, DISTRITO_FEDERAL_2012, rural, 
            sep = "-")
    ) 

seleccionar_muestras_completas <- function(i){
    muestra <- select_sample_str(marco_rural, tamanos, sample_size = nh, 
        stratum = estrato)
    write_csv(select(muestra, -estrato_faltantes, -rural), path = stringr::str_c("../datos/muestras/completas/completa_", 
        i, ".csv"))
    return(muestra)
}
set.seed(895428)
completas <- map(1:100, seleccionar_muestras_completas)

# 2. 100 faltantes a nivel casilla
# tomaremos submuestras de la muestra para simular las remesas
# la simulación de las remesas se hará de acuerdo a los datos de la elección 
# presidencial 2012

remesas_2012 <- readxl::read_excel("../datos/Muestras de conteos rápidos 2012_2017/ConRemesaFinal/Base_Conteo_Presidencial 2012_final.xlsx")

# veamos los cortes cada media hora entre 20:30 y 23:30
remesas_tidy <- remesas_2012 %>% 
    mutate(
        iD_ESTADO = base_pres_id_ESTADO,
        DISTRITO_FEDERAL_2012 = base_pres_ID_DISTRITO, 
        remesas,
        rural = (tsecc_DEOE == "Rural") * 1,
        hora_transmision = strftime(hora_transmision, format="%H:%M:%S"),
        dia_hora = FECHA_HORA
        ) %>% 
    mutate(
        remesa_2030 = dia_hora < ymd_hms("2012-07-01 20:30:00 UTC"), 
        remesa_2100 = dia_hora < ymd_hms("2012-07-01 21:00:00 UTC"),
        remesa_2130 = dia_hora < ymd_hms("2012-07-01 21:30:00 UTC"),
        remesa_2200 = dia_hora < ymd_hms("2012-07-01 22:00:00 UTC"), 
        remesa_2230 = dia_hora < ymd_hms("2012-07-01 22:30:00 UTC"),
        remesa_2300 = dia_hora < ymd_hms("2012-07-01 23:00:00 UTC"),
        remesa_2330 = dia_hora < ymd_hms("2012-07-01 23:30:00 UTC")
    ) %>% 
    mutate_at(vars(remesa_2030:remesa_2330), funs(ifelse(is.na(.), FALSE, .)))
glimpse(remesas_tidy)

# usaremos las proporciones recibidas por edo, distrto federal y rural/urbano
llegada_distrito <- remesas_tidy %>% 
    group_by(iD_ESTADO, DISTRITO_FEDERAL_2012, rural) %>% 
    summarise_at(vars(remesa_2030:remesa_2330), mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(estrato_faltantes = str_c(iD_ESTADO, DISTRITO_FEDERAL_2012, rural, 
        sep = "-")) %>% 
    select(estrato_faltantes, rural, remesa_2030:remesa_2330)

# La gráfica muestra como llegaban las remesas y que a partir de las 22:30 se
# tenía todo
llegada_distrito %>% 
    gather(horario, p_llegada, remesa_2030:remesa_2330) %>% 
    ggplot(aes(x = reorder(horario, p_llegada), y = p_llegada,  color = factor(rural))) +
    geom_boxplot() 

# usamos corte de las 22:00
seleccionar_muestras_2200 <- function(i, muestra){
    llegada_distrito_muestra <- muestra %>% 
        left_join(llegada_distrito) %>% 
        distinct(iD_ESTADO, estrato_faltantes, remesa_2200) %>% 
        group_by(iD_ESTADO) %>% 
        mutate(
            remesa_2200 = ifelse(is.na(remesa_2200), mean(remesa_2200, 
                na.rm = TRUE), remesa_2200)
            ) %>% 
        ungroup() %>% 
        select(-iD_ESTADO)
    muestra_f <- select_sample_str(sampling_frame = muestra, 
        allocation = llegada_distrito_muestra, 
        sample_size = remesa_2200, stratum = estrato_faltantes, is_frac = TRUE) %>% 
        select(-estrato_faltantes, -rural)
        
    write_csv(muestra_f, 
        path = stringr::str_c("../datos/muestras/faltantes_casilla/faltantes_casilla_", 
        i, ".csv"))
    return(muestra_f)
}
set.seed(98713)
muestras_2200 <- map2(1:100, completas, ~seleccionar_muestras_2200(.x, .y))

# 3. 100 muestras, MAR 0.1 & faltantes en promedio 10
seleccionar_muestras_sesgo <- function(i, muestra){
    estratos_eliminar <- sample(unique(estrato_pan$estrato), size = 15, prob = estrato_pan$p)
    muestra_s <- filter(muestra, !(estrato %in% estratos_eliminar)) %>% 
        select(-rural, -estrato_faltantes)
    write_csv(muestra_s, 
        path = stringr::str_c("../datos/muestras/faltantes_estrato/faltantes_estrato_", 
            i, ".csv"))
    return(muestra_s)
}
estrato_pan <- marco %>% 
    group_by(estrato) %>% 
    summarise(CPAN = sum(CPAN), TOTAL_VOTOS = sum(TOTAL_VOTOS)) %>% 
    ungroup() %>% 
    mutate(p = CPAN / TOTAL_VOTOS)

set.seed(4654678)
muestras_sesgo <- map2(1:100, completas, ~seleccionar_muestras_sesgo(.x, .y))

map_dbl(muestras_2200, ~n_distinct(.$estrato)) %>% table()
map_dbl(muestras_sesgo, ~n_distinct(.$estrato)) %>% table()

map_dbl(muestras_sesgo, nrow) %>% mean()
map_dbl(muestras_2200, nrow) %>% mean()
map_dbl(completas, nrow) %>% mean()

compara <- data_frame(
    casilla = map_dbl(muestras_2200, ~sum(.$CPAN) / sum(.$TOTAL_VOTOS)), 
    completa = map_dbl(completas, ~sum(.$CPAN) / sum(.$TOTAL_VOTOS)), 
    sesgo = map_dbl(muestras_sesgo, ~sum(.$CPAN) / sum(.$TOTAL_VOTOS)))

compara <- data_frame(
    casilla = map_dbl(muestras_2200, ~sum(.$CPAN) / sum(.$TOTAL_VOTOS)), 
    completa = map_dbl(completas, ~sum(.$CPAN) / sum(.$TOTAL_VOTOS)), 
    sesgo = map_dbl(muestras_sesgo, ~sum(.$CPAN) / sum(.$TOTAL_VOTOS)))

ggplot(compara) +
    geom_point(aes(x = completa, y = casilla, color = "Casilla")) +
    geom_abline() +
    coord_equal() +
    geom_point(aes(x = completa, y = sesgo, color = "Sesgo")) +
    ylim(0.245, 0.263) + 
    xlim(0.245, 0.263) +
    geom_point(aes(x = sum(marco$CPAN) / sum(marco$TOTAL_VOTOS), 
        y = sum(marco$CPAN) / sum(marco$TOTAL_VOTOS)), color = "red", size = 2) 


