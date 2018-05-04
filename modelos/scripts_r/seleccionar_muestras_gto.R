# El script genera:
# 100 muestras completas
# 100 faltantes a nivel casilla 
# 100 faltantes estrato

# Detalles:
## Marco muestral: gto_2012 incluído en el paquete
# quickcountmx (devotos::github_install("tereom/quickcountmx")
## Selección de muestras
# Se consideran 3 escenarios y para cada uno se generan 100 muestras:
# 1) Muestra completa: se seleccionan 100 muestras usando la estratificación
#   de acuerdo a distrito local 2017.
# 2) Muestra faltantes_casilla: se seleccionaron 100 muestras censurando las 
#   muestras completas del inciso anterior, donde la probabilidad de que una 
#   casilla sea censurada considera los patrones observados en las remesas 2012 
#   con corte a las 22:00 horas.
# 3) Muestra faltantes_estrato: se seleccionaron 100 muestras censurando las 
#   muestras completas del inciso 1), en este caso se eliminan 3 estratos por 
#   muestra. Los estratos a censurar se seleccionan con probabilidad 
#   proporcional a los votos recibidos por el PAN en el estrato.

library(quickcountmx)
library(tidyverse)
library(lubridate)

# 1. 100 muestras completas
gto_2012

marco <- gto_2012 %>% 
    mutate(
        distrito_rural = str_c(distrito_fed_12, rural, sep = "-"), 
        distrito_rural = ifelse(distrito_rural == "5-1", "5-0", distrito_rural)
    )

seleccionar_muestras_completas <- function(i){
    muestra <- select_sample_prop(marco, stratum = distrito_loc_17, 
        frac = 0.075)
    write_csv(select(muestra, -distrito_rural), 
        path = stringr::str_c("datos/muestras_gto/completas/completa_", 
        i, ".csv"))
    return(muestra)
}
set.seed(895428)
completas <- map(1:100, seleccionar_muestras_completas)

# 2. 100 faltantes a nivel casilla
# tomaremos submuestras de la muestra para simular las remesas
# la simulación de las remesas se hará de acuerdo a los datos de la elección 
# presidencial 2012

remesas_2012 <- readxl::read_excel("datos/Muestras de conteos rápidos 2012_2017/ConRemesaFinal/Base_Conteo_Presidencial 2012_final.xlsx")

# veamos los cortes cada media hora entre 20:30 y 23:30
remesas_gto_2012 <- remesas_2012 %>% 
    filter(base_pres_id_ESTADO == "11") %>% 
    mutate(
        distrito_fed_12 = base_pres_ID_DISTRITO, 
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
    mutate_at(vars(remesa_2030:remesa_2330), funs(ifelse(is.na(.), 
        FALSE, .)))

glimpse(remesas_gto_2012)

# usaremos las proporciones de datos recibidas por distrto federal y rural/urbano
llegada_distrito <- remesas_gto_2012 %>% 
    group_by(distrito_fed_12, rural) %>% 
    summarise_at(vars(remesa_2030:remesa_2330), mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(distrito_rural = str_c(distrito_fed_12, rural, sep = "-")) %>% 
    select(distrito_rural, rural, remesa_2030:remesa_2330)

# La gráfica muestra como llegaban las remesas y que a partir de las 22:30 se
# tenía todo
llegada_distrito %>% 
    gather(horario, p_llegada, remesa_2030:remesa_2330) %>% 
    ggplot(aes(x = reorder(horario, p_llegada), y = p_llegada,  color = factor(rural))) +
    geom_boxplot() 

# usamos corte de las 22:00
seleccionar_muestras_2200 <- function(i, muestra){
    muestra_f <- select_sample_str(sampling_frame = muestra, 
        allocation = select(llegada_distrito, distrito_rural, remesa_2200), 
        sample_size = remesa_2200, stratum = distrito_rural, is_frac = TRUE)
    write_csv(select(muestra_f, -distrito_rural), 
        path = stringr::str_c("datos/muestras_gto/faltantes_casilla/faltantes_casilla_", 
            i, ".csv"))
    return(muestra_f)
}
set.seed(98713)
muestras_2200 <- map2(1:100, completas, ~seleccionar_muestras_2200(.x, .y))

# 3. 100 muestras faltantes 3 estratos
seleccionar_muestras_sesgo <- function(i, muestra){
    estratos_eliminar <- sample(unique(estrato_pan_na$distrito_loc_17), size = 3, 
        prob = estrato_pan_na$p)
    muestra_s <- filter(muestra, !(distrito_loc_17 %in% estratos_eliminar))
    write_csv(select(muestra_s, -distrito_rural), 
        path = stringr::str_c("datos/muestras_gto/faltantes_estrato/faltantes_estrato_", 
            i, ".csv"))
    return(muestra_s)
}
estrato_pan_na <- marco %>% 
    group_by(distrito_loc_17) %>% 
    summarise(pan_na = sum(pan_na), total = sum(total)) %>% 
    ungroup() %>% 
    mutate(p = pan_na / total)

set.seed(4654678)
muestras_sesgo <- map2(1:100, completas, ~seleccionar_muestras_sesgo(.x, .y))

map_dbl(completas, ~n_distinct(.$distrito_loc_17)) %>% table()
map_dbl(muestras_2200, ~n_distinct(.$distrito_loc_17)) %>% table()
map_dbl(muestras_sesgo, ~n_distinct(.$distrito_loc_17)) %>% table()

map_dbl(muestras_sesgo, nrow) %>% mean()
map_dbl(muestras_2200, nrow) %>% mean()
map_dbl(completas, nrow) %>% mean()

compara <- data_frame(
    casilla = map_dbl(muestras_2200, ~sum(.$pan_na) / sum(.$total)), 
    completa = map_dbl(completas, ~sum(.$pan_na) / sum(.$total)), 
    sesgo = map_dbl(muestras_sesgo, ~sum(.$pan_na) / sum(.$total)))

ggplot(compara) +
    geom_point(aes(x = completa, y = casilla, color = "Casilla")) +
    geom_abline() +
    coord_equal() +
    geom_point(aes(x = completa, y = sesgo, color = "Sesgo")) +
    geom_point(aes(x = sum(marco$pan_na) / sum(marco$total), 
        y = sum(marco$pan_na) / sum(marco$total)), color = "red", size = 2)
