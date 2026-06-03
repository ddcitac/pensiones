#### Tablas de Mujeres ####

tabla_nueva_F <-  Tabla_nueva %>% mutate(qx = Mujeres) %>% select(edad, qx) %>% 
  mutate(px = 1 - qx, lx = 1000000)

for (i in 2:nrow(tabla_nueva_F)) {
  tabla_nueva_F$lx[i] <-
    (tabla_nueva_F$px[i - 1] * tabla_nueva_F$lx[i - 1]) %>% round(0)
}

t <- 60:1351

tabla_nueva_F_int <- data.frame(Num_mes = t)

tabla_nueva_F_int$mes[1] <- 12

for (j in 2:nrow(tabla_nueva_F_int)) {
  tabla_nueva_F_int$mes[j] <-
    ifelse(tabla_nueva_F_int$mes[j - 1] == 12, 1,
           tabla_nueva_F_int$mes[j - 1] + 1)
}


tabla_nueva_F_int <-
  tabla_nueva_F_int %>% mutate(edad_mes = Num_mes / 12, edad = edad_mes %>% floor())


tabla_nueva_F_int <-
  tabla_nueva_F_int %>% left_join(tabla_nueva_F, by = "edad") %>% 
  mutate(factor = edad_mes - edad, lx = ifelse(is.na(lx), 0, lx))

for (i in 1:nrow(tabla_nueva_F_int)) {
  tabla_nueva_F_int$lx_real[i] <-
    (1 - tabla_nueva_F_int$factor[i]) * tabla_nueva_F_int$lx[i] +
    tabla_nueva_F_int$factor[i] *  tabla_nueva_F_int$lx[i + 12]
}

tabla_nueva_F_int <-
  tabla_nueva_F_int %>% select(edad, Num_mes, lx_real) %>% 
  filter(edad <=110)

colnames(tabla_nueva_F_int) <- c("Edad años", "Edad Meses", "lx")

# Hombres

tabla_nueva_M <-
  Tabla_nueva %>% mutate(qx = Hombres) %>% select(edad, qx) %>%
  mutate(px = 1 - qx, lx = 1000000)

for (i in 2:nrow(tabla_nueva_M)) {
  tabla_nueva_M$lx[i] <-
    (tabla_nueva_M$px[i - 1] * tabla_nueva_M$lx[i - 1]) %>% round(0)
}


t <- 60:1351

tabla_nueva_M_int <- data.frame(Num_mes = t)

tabla_nueva_M_int$mes[1] <- 12

for (j in 2:nrow(tabla_nueva_M_int)) {
  tabla_nueva_M_int$mes[j] <-
    ifelse(tabla_nueva_M_int$mes[j - 1] == 12,
           1,
           tabla_nueva_M_int$mes[j - 1] + 1)
}


tabla_nueva_M_int <-
  tabla_nueva_M_int %>% mutate(edad_mes = Num_mes / 12, edad = edad_mes %>% floor())


tabla_nueva_M_int <-
  tabla_nueva_M_int %>% left_join(tabla_nueva_M, by = "edad") %>%
  mutate(factor = edad_mes - edad, lx = ifelse(is.na(lx), 0, lx))

for (i in 1:nrow(tabla_nueva_M_int)) {
  tabla_nueva_M_int$lx_real[i] <-
    (1 - tabla_nueva_M_int$factor[i]) * tabla_nueva_M_int$lx[i] + 
    tabla_nueva_M_int$factor[i] * tabla_nueva_M_int$lx[i + 12]
}

tabla_nueva_M_int <-
  tabla_nueva_M_int %>% select(edad, Num_mes, lx_real) %>% 
  filter(edad <= 110)

colnames(tabla_nueva_M_int) <- c("Edad años", "Edad Meses", "lx")
