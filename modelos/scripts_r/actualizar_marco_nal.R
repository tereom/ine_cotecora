# Objetivo: actualizar marco muestral por cambio en estratificación 
# Veracruz (colapso estratos) tomando como base el marco muestra generado por 
# Paty
# Se actualizaron los archivos info_estratos.csv e info_estatal.csv para 
# acomodar los cambios en la estratificación, nuevamente se preservaron nombres 
# de variables de los archivos de Paty.

library(tidyverse)
nal_estratos <- readr::read_delim(
    "../datos/Presidente2012/Presidente2012_completo.csv",
    ",", escape_double = FALSE, trim_ws = TRUE, 
    locale = readr::locale(encoding = "ISO-8859-1"))

glimpse(nal_estratos)

# estratos anteriores veracruz con distrito federal 2012
# table(ver_estratos$DISTRITO_FEDERAL_2012, ver_estratos$estrato)

# nuevos estratos con distrito federal 2017 y colapso
ver_2017 <- read_delim("../datos/veracruz/Computos_Distritales_Presidente_2012_VER(2017).txt", 
    "|", escape_double = FALSE, trim_ws = TRUE) %>% 
    arrange(DISTRITO_FEDERAL_2017)

ver_2017_estratos <- ver_2017 %>% 
    filter(ID_ESTADO == 30, !is.na(ESTRATO)) %>%  # NAs en extranjero 
    mutate(PARAESTRATO = as.integer(factor(ESTRATO, levels = unique(ver_2017$ESTRATO)))) %>% 
    select(iD_ESTADO = ID_ESTADO, DISTRITO_FEDERAL_2017:EXT_CONTIGUA,
        ID_MUNICIPIO, ESTRATO, PARAESTRATO)

# cambiamos en base nal_estratos
nal_aux <- nal_estratos %>% 
    left_join(ver_2017_estratos) %>% 
    mutate(
        estrato_aux = ifelse(iD_ESTADO == 30, ESTRATO, estrato), 
        paraestrato = ifelse(iD_ESTADO == 30, PARAESTRATO, paraestrato)
    )

# Nuevo csv completo    
nal_estratos_act <- nal_aux %>% 
    mutate(
        estrato = as.integer(factor(estrato_aux, levels = unique(nal_aux$estrato_aux)))
        )

# actualizar tamaños de muestra
n_estratos <- read_csv("../datos/Presidente2012/Info_estratos.csv")
glimpse(n_estratos)

# csv estratos Gabriel
n_ver <- read_csv("../datos/veracruz/Nhs_Veracruz2.csv")
glimpse(n_ver)

# asignar los nuevos números de estrato a Veracruz
n_ver_aux <- nal_estratos_act %>% 
    filter(iD_ESTADO == 30) %>% 
    select(iD_ESTADO, paraestrato, PARAESTRATO, estrato, ESTRATO) %>% 
    distinct() %>% 
    left_join(n_ver) %>% 
    select(ESTADO = iD_ESTADO, ESTRATIFICACION = paraestrato, ESTRATO = estrato, 
        Nh)

# falta variable nh usamos la info en la tabla anterior
frac <- n_estratos %>% 
    filter(ESTADO == 30) %>% 
    summarise(frac = sum(nh) / sum(Nh)) %>% 
    pull(frac)

n_ver_aux$nh <- as.integer(round(n_ver_aux$Nh * frac))

# actualizar los números de estratos del resto
n_aux <- nal_estratos_act %>% 
    filter(iD_ESTADO != 30) %>% 
    select(iD_ESTADO, paraestrato, estrato, estrato_aux) %>% 
    distinct() %>% 
    mutate(estrato_aux = as.integer(estrato_aux)) %>% 
    left_join(n_estratos, by = c("estrato_aux" = "ESTRATO", 
        "iD_ESTADO" = "ESTADO")) %>% 
    select(ESTADO = iD_ESTADO, ESTRATIFICACION, ESTRATO = estrato, 
        Nh, nh)    

# actualizado
n_estratos_act <- bind_rows(n_aux, n_ver_aux) %>% 
    arrange(ESTADO, ESTRATIFICACION, ESTRATO)
glimpse(n_estratos_act)

# revisión
sum(n_estratos$nh)
sum(n_estratos_act$nh)

sum(n_estratos_act$Nh)
sum(n_estratos$Nh)

write_csv(n_estratos_act, path = "../datos/Presidente2012_20180430/info_estratos.csv")

# resumen estatal
info_estatal <- n_estratos_act %>% 
    group_by(ESTADO) %>% 
    summarise(
        Num_estratos = n(), 
        Num_casillas = sum(Nh), 
        Num_muestra = sum(nh)
        )

write_csv(info_estatal, path = "../datos/Presidente2012_20180430/info_estatal.csv")

# Base completa
completo <- nal_estratos_act %>% 
    select(-estrato_aux, -ESTRATO, -PARAESTRATO)

write_csv(completo, path = "../datos/Presidente2012_20180430/Presidente2012_completo.csv")

