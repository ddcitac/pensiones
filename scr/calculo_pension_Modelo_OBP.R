rm(list = ls());rm();gc()
#librerias
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
######################PARAMETROS################################################
SMLV <-  1300000 
#SMLV <-  1160000
ahno_valuacion<-2024
fecha_calculo = as.Date("2024-01-31")
inf = (((3*0.0928)+(2*0.1312)+(0.0562))/6)*100
SML = 1300000       # Salario al 2023+1
cambio_bene = 0.006 # Factor de seguridad
infobp = 0.0928     # IPC año

#tipo<- "sin_desl_fch_corte"#"con_desl_fch_corte"###" 
tipo<- "con_desliz_OBP"#
#tipo<- "con_desl_IPC"
#"base"##"con_desliza"#"sin_desl_IPC"##"con_desl_IPC"

######################INSUMOS###################################################
base_formato_394 <- read_excel("data/FT394_ENE_2024.xlsx" , sheet = "Sheet 1",
                               #base_formato_394 <- read_excel("data/FT394_ENE_2024.xlsx" , sheet = "fecha_corte",
                               #### ------- #####
                               col_types = c("text", "text", "text",
                                             "date", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text",
                                             "text", "text", "text"
                                             ,"numeric", "numeric"
                               )
)#%>% filter(COMPANIA%in%c("ALFA"), RAMO == "39")#[1:1,] 
#base_formato_394 %>% nrow()
#base_formato_394 %>% filter(NRO_IDENTIF_A ==19232092) %>% View()
#### ------- #####
Tabla_validos_M <- read_excel("data/Tabla_validos_M_mejora.xlsx", sheet = "2008") #hoja 2008 Sin mejoras
Tabla_validos_F <- read_excel("data/Tabla_validos_F_mejora.xlsx", sheet = "2008") #hoja 2008 Sin mejoras
Tabla_SMLV  <- read_excel("data/SMLV.xlsx")
Tabla_nueva <- read_excel("data/tabla_nueva_2020_final.xlsx")
######################CALCULO TABLA INVALIDOS###################################
invalidos_M <- read_excel("data/Tablas_Mortalidad_Validos_e_Invalidos_mensual.xlsx", sheet = "TMM-invalidos-mensual")
invalidos_F <- read_excel("data/Tablas_Mortalidad_Validos_e_Invalidos_mensual.xlsx", sheet = "TMF-invalidos-mensual")
#########################FIX MORTALITY##########################################
mujeres_invalidos_func <- 
  invalidos_F %>%
  as_tibble() %>% 
  mutate(edad_meses = `Edad Meses`) %>%
  select(edad_meses, lx)
hombres_invalidos_func <- 
  invalidos_M %>%
  as_tibble() %>% 
  mutate(edad_meses = `Edad Meses`) %>%
  select(edad_meses, lx)
hombres_validos_func <- 
  Tabla_validos_M %>%
  as_tibble() %>% 
  mutate(edad_meses = `Edad Meses`) %>%
  select(edad_meses, lx)
mujeres_validos_func <- 
  Tabla_validos_F %>%
  as_tibble() %>% 
  mutate(edad_meses = `Edad Meses`) %>%
  select(edad_meses, lx)
tibble_tabla_mort <- tribble(~"sexo",~"estado", ~"tabla_mort",
                             1, 2,hombres_invalidos_func, #estado 2 es invalido
                             1, 1,hombres_validos_func,
                             2, 2,mujeres_invalidos_func,
                             2, 1,mujeres_validos_func
) %>% 
  as_tibble()
######################TABLA_INCREMENTO_SLM######################################
deslizamiento_esc0 = 0.13/100#0/100 #(aumento SLM)
deslizamiento_esc1 = deslizamiento_esc0 - 0.001 #(disminucion SLM)
deslizamiento_esc2 = deslizamiento_esc0 + 0.001 #(aumento SLM)
tabla_incremento_slmv <- tibble(ahno = ahno_valuacion:3000) %>%
  mutate(crecimiento_anual_IPC =
           case_when(ahno == ahno_valuacion    ~ 1 ,
                     ahno == ahno_valuacion +1 ~ 1 + inf /100,#3.91/100,#,#3.91/100,#7.39,#4.07/100,#4.63/100,#6.74/100, # corte sep 2022 # 7.39/100,#oct,#
                     ahno == ahno_valuacion +2 ~ 1 + inf /100,#3.90/100,#,#3.90/100,#4.01,# 3.15/100,#3.41/100,#3.95/100, # corte sep 2022 #4.01/100,#oct
                     TRUE ~  1+inf /100
           ),
         crecimiento_anual_SLM_ESC0 = case_when(
           ahno == ahno_valuacion ~ 1 ,
           TRUE ~ crecimiento_anual_IPC *(1+deslizamiento_esc0)
         ),
         crecimiento_anual_SLM_ESC1 = case_when(
           ahno == ahno_valuacion ~ 1 ,
           TRUE ~ crecimiento_anual_IPC *(1+deslizamiento_esc1) 
         ) ,
         crecimiento_anual_SLM_ESC2 = case_when(
           ahno == ahno_valuacion ~ 1 ,
           TRUE ~ crecimiento_anual_IPC * (1+deslizamiento_esc2) 
         )
  ) %>% 
  # mutate(crecimiento_acum_IPC = cumprod(crecimiento_anual_IPC),
  #        crecimiento_acum_SLM_ESC0 = cumprod(crecimiento_anual_SLM_ESC0),
  #        crecimiento_acum_SLM_ESC1 = cumprod(crecimiento_anual_SLM_ESC1),
  #        crecimiento_acum_SLM_ESC2 = cumprod(crecimiento_anual_SLM_ESC2)
  # ) %>% 
  mutate(crecimiento_acum_IPC      = (crecimiento_anual_IPC),
         crecimiento_acum_SLM_ESC0 = (crecimiento_anual_SLM_ESC0),
         crecimiento_acum_SLM_ESC1 = (crecimiento_anual_SLM_ESC1),
         crecimiento_acum_SLM_ESC2 = (crecimiento_anual_SLM_ESC2)
  ) %>% 
  select(ahno, crecimiento_anual_IPC,contains("acum"))
gc()
#View(tabla_incremento_slmv)
######################CALCULO PENSION###########################################
## Pension por sustitución?? pensiones_RL$ORIGEN_PENSION %>% unique()
pensiones_RL <- ##pensiones con riesgo de deslizamiento
  base_formato_394 %>%
  filter(RAMO == "39") %>%
  #filter(TIPO_CREC == "4") %>%  #crecimiento anual del ipc
  select(
    -CODCIA,
    -CODFORMATO,
    -UNIDAD_CAPTURA,
    -CODSUBCTA,
    -RAMO,
    -UNIDAD,-EQUIVALENCIA,
    -TIPO_CREC,
    -TIEMPO_DIFERIMIENTO ,
    -OBSERVACIONES,-PORC_AMORT_EXCESO,
    -FECH_AVISO_SINIESTRO,
    -FECH_INI_RENTA_DIF
  ) %>%
  mutate(FECHACORTE = as_date(FECHACORTE)) %>%
  mutate(across(.cols = c(INTERES_TEC, TASA_CREC, NRO_MESADAS, MESADA_PER, 
                          FECH_CAUSA_DERECHO, GASTOS, starts_with("ESTADO_"),INTERES_TECNICO,
                          starts_with("SEXO_"), starts_with("PARENT_")
  ), parse_number
  ),
  across(.cols = starts_with("FECH_"), dmy)
  ) %>% 
  mutate(GASTOS_FIX = if_else(is.na(GASTOS),0,GASTOS/100)) %>% 
  mutate(GASTOS_FIX = if_else(COMPANIA == "COLMENA",GASTOS_FIX/100,GASTOS_FIX)) %>% 
  mutate(across(.cols = c(TASA_CREC,INTERES_TEC, GASTOS_FIX),
                .fns = ~if_else(.x >= 1,.x/100,.x, missing = NA_real_)
  )
  ) %>% 
  select(COMPANIA, FECHACORTE,TASA_CREC,INTERES_TEC,RENT_ESP, CLASE_PENSION,
         TEMPORALIDAD, MESADA_PER,NRO_MESADAS, GASTOS_FIX,
         starts_with("FECH_NACI_"), starts_with("PARENT_"),
         starts_with("ESTADO_"), starts_with("SEXO_"),
         INVALIDEZ_VEJEZ, SOBREVIVENCIA, AUX_FUNERARIO, 
         TOTAL_RESERVA, TOT_RESERVA_EST_FINANCIEROS
  )
colnames(base_formato_394) %>% sort()
pensiones_RL$TEMPORALIDAD <- as.numeric(pensiones_RL$TEMPORALIDAD)
pensiones_RL %>% 
  filter(MESADA_PER <= SMLV)

11675/17423
gc()
###########################validaciones#########################################
pensiones_menos_minimo <- pensiones_RL %>%
  filter(MESADA_PER <  SMLV) %>% 
  nrow()
pensiones_igual_minimo <- pensiones_RL %>%
  filter(MESADA_PER ==  SMLV) %>% 
  nrow()
paste("# Pensiones con menos del SLM en año valuación: ", pensiones_menos_minimo)
SMLV <- SMLV
################################################################################
## PARENT_A-->HIJO: Valido o invalido?
crear_tablas_beneficiarios <- function(data, endvar = "A")
{
  # data = pensiones_RL; endvar = "A"
  ##############################################################################
  FECH_NACI = paste0("FECH_NACI_",endvar)  
  PARENT = paste0("PARENT_",endvar)  
  ESTADO = paste0("ESTADO_",endvar) 
  SEXO = paste0("SEXO_",endvar) 
  name_var <- paste0("difcalc_",endvar)
  name_edad_meses <- paste0("edadmeses_",endvar)
  name_edad_meses_hasta <- paste0("edadmeseshasta_",endvar)
  sub_tabla_beneficiario <- 
    data %>% 
    #select({{SEXO}}, {{PARENT}}, {{FECH_NACI}}, {{ESTADO}}, FECHACORTE) %>% 
    mutate(temp_naci = if_else(is.na(!!sym(FECH_NACI)),FECHACORTE,!!sym(FECH_NACI))) %>% #para usar el calculo de edades
    mutate(EDAD_CALC = if_else(is.na(!!sym(FECH_NACI)) | !!sym(ESTADO)==3, NA_real_, eeptools::age_calc(temp_naci, FECHACORTE, units = "years", precise = TRUE)),
           EDAD_MESES_CALC = if_else(is.na(!!sym(FECH_NACI)) | !!sym(ESTADO)==3, NA_real_, eeptools::age_calc(temp_naci, enddate = FECHACORTE, units = "months", precise = TRUE) %>% floor()),
           EDAD_HASTA_CALC = case_when(
             !!sym(PARENT) %in% c(2,3) & !!sym(ESTADO)==1 ~ 25,
             !!sym(PARENT) %in% c(7) ~ EDAD_CALC + TEMPORALIDAD/12, ##¿Esto esta extraño?
             is.na(!!sym(FECH_NACI)) | !!sym(ESTADO)==3 ~ 0,
             TRUE ~ 110
           ),
           EDAD_MESES_HASTA_CALC = EDAD_HASTA_CALC*12,
           LIMITE_INF_CALC = ifelse(is.na(EDAD_MESES_CALC) | !!sym(ESTADO)==3, 0, EDAD_MESES_CALC), # la condicion no importa (el beneficiario esta vivo)
           LIMITE_SUP_CALC = ifelse(is.na(EDAD_MESES_HASTA_CALC) | !!sym(ESTADO)==3, 0, EDAD_MESES_HASTA_CALC),
           {{ name_var }} := LIMITE_SUP_CALC-LIMITE_INF_CALC
    ) %>% 
    rename({{ name_edad_meses }} := EDAD_MESES_CALC,
           {{ name_edad_meses_hasta }} := EDAD_MESES_HASTA_CALC
    ) %>% 
    select(-ends_with("_CALC"), -temp_naci)
  sub_tabla_beneficiario
}
crear_tabla_flujos <- function(max_dif_calc, fec_corte, interes_tecnico,
                               tasa_crecimiento, mesada, Num_mesadas_para,
                               gastos, estado, edadmeses_A,
                               edadmeses_B,edadmeses_C,edadmeses_D,
                               edadmeseshasta_A,edadmeseshasta_B,
                               edadmeseshasta_C, edadmeseshasta_D
)
{
  # max_dif_calc = 100
  # fec_corte = pensiones_RL$FECHACORTE[1]
  # tasa_crecimiento = 0.04
  # interes_tecnico = 0.04
  # mesada = 1e6
  # gastos = 0.02
  # Num_mesadas_para = 14
  retorno <- 
    tibble(t = 0:(max_dif_calc+1)) %>%
    mutate(temp2 = fec_corte %m+% months(t)) %>%
    mutate(mes = month(temp2), ahno = year(temp2)) %>%
    select(t, mes, ahno, temp2)  %>% 
    mutate(tasa_crecimiento = tasa_crecimiento,
           gastos = gastos
    ) %>% 
    left_join(tabla_incremento_slmv, by = "ahno") %>% 
    mutate(SLM_REF = SMLV, # como crece el salario minimo de acuerdo a los escenarios
           SLM_t_ESC_IPC = SLM_REF*crecimiento_acum_IPC,
           SLM_t_ESC0    = SLM_REF*crecimiento_acum_SLM_ESC0,
           SLM_t_ESC1    = SLM_REF*crecimiento_acum_SLM_ESC1,
           SLM_t_ESC2    = SLM_REF*crecimiento_acum_SLM_ESC2
    ) %>% 
    mutate(
      # Ajuste al interes técnico
      interes_tecnico  =
             case_when( interes_tecnico < 0.04 ~ interes_tecnico,
                        0.04 < interes_tecnico ~ 0.04,
                        TRUE ~ 0.04
                        ),
           interes_tec_mens = ((1+inf)*(1 + interes_tecnico))^(1/12),
           ipc_mens = (crecimiento_anual_IPC)^(1/12),   
           ipc_mens_lagged = lead(ipc_mens),
           # Interes tecnico mensual     
           inf_tec = 1/(interes_tec_mens)
    ) %>% 
    mutate(v = (inf_tec)) %>% 
    filter(!is.na(ipc_mens_lagged)) %>% 
    mutate(pot = t %/% 12) %>% 
    mutate(
      Num_mesadas = Num_mesadas_para,
      mesada_1 = (mesada * (1 + tasa_crecimiento) ^ pot),
      mesada_adicional =
        case_when(
          (Num_mesadas == 13 | Num_mesadas == 1) & mes == 12 ~ mesada_1,
          (Num_mesadas == 14 |
             Num_mesadas == 2) & (mes == 6 | mes == 12) ~ mesada_1,
          TRUE ~ 0
        ),
      mesada_total = if_else(
        Num_mesadas == 1 | Num_mesadas == 2,
        mesada_adicional,
        mesada_1 + mesada_adicional
      ),
      mesada_total_gatos = (mesada_total / (1 - gastos)) 
    ) %>% 
    mutate(mesada_1_ipc = (mesada * (crecimiento_acum_IPC))) %>% 
    mutate(mesada_adicional_ipc = if_else(mesada_adicional!=0,mesada_1_ipc,0)) %>% 
    mutate(mesada_1_cont_SLM_ESC0 = if_else(mesada_1_ipc<SLM_t_ESC0,SLM_t_ESC0,mesada_1_ipc),
           mesada_1_cont_SLM_ESC1 = if_else(mesada_1_ipc<SLM_t_ESC1,SLM_t_ESC1,mesada_1_ipc),
           mesada_1_cont_SLM_ESC2 = if_else(mesada_1_ipc<SLM_t_ESC2,SLM_t_ESC2,mesada_1_ipc),
           mesada_1_cont_SLM_ESC_IPC = if_else(mesada_1_ipc<SLM_t_ESC_IPC,SLM_t_ESC_IPC,mesada_1_ipc)
    ) %>% 
    mutate(mesada_adicional_cont_SLM_ESC0 = if_else(mesada_adicional_ipc<SLM_t_ESC0 & mesada_adicional!=0,
                                                    SLM_t_ESC0,
                                                    mesada_adicional_ipc),
           mesada_adicional_cont_SLM_ESC1 = if_else(mesada_adicional_ipc<SLM_t_ESC1 & mesada_adicional!=0,
                                                    SLM_t_ESC1,
                                                    mesada_adicional_ipc),
           mesada_adicional_cont_SLM_ESC2 = if_else(mesada_adicional_ipc<SLM_t_ESC2 & mesada_adicional!=0,
                                                    SLM_t_ESC2,
                                                    mesada_adicional_ipc),
           mesada_adicional_cont_SLM_ESC_IPC = if_else(mesada_adicional_ipc<SLM_t_ESC_IPC & mesada_adicional!=0,
                                                       SLM_t_ESC_IPC,
                                                       mesada_adicional_ipc)
    ) %>% 
    mutate(mesada_total_SLM_ESC0 = mesada_1_cont_SLM_ESC0 + mesada_adicional_cont_SLM_ESC0,
           mesada_total_SLM_ESC1 = mesada_1_cont_SLM_ESC1 + mesada_adicional_cont_SLM_ESC1,
           mesada_total_SLM_ESC2 = mesada_1_cont_SLM_ESC2 + mesada_adicional_cont_SLM_ESC2,
           mesada_total_SLM_ESC_IPC = mesada_1_cont_SLM_ESC_IPC + mesada_adicional_cont_SLM_ESC_IPC
    ) %>%    
    mutate(mesada_total_gatos_SLM_ESC0 = (mesada_total_SLM_ESC0 / (1 - gastos)),
           mesada_total_gatos_SLM_ESC1 = (mesada_total_SLM_ESC1 / (1 - gastos)),
           mesada_total_gatos_SLM_ESC2 = (mesada_total_SLM_ESC2 / (1 - gastos)),
           mesada_total_gatos_SLM_ESC_IPC = (mesada_total_SLM_ESC_IPC / (1 - gastos))
    ) %>% 
    select(t, mes, ahno, tasa_crecimiento, gastos, crecimiento_anual_IPC, crecimiento_acum_IPC,
           crecimiento_acum_SLM_ESC0, crecimiento_acum_SLM_ESC1, crecimiento_acum_SLM_ESC2,
           interes_tec_mens, ipc_mens, inf_tec,ipc_mens_lagged,
           v, pot, Num_mesadas, mesada_total_gatos_SLM_ESC0, mesada_total_gatos_SLM_ESC1,
           mesada_total_gatos_SLM_ESC2, mesada_total_gatos_SLM_ESC_IPC, 
           mesada_1,mesada_1_cont_SLM_ESC0,mesada_1_cont_SLM_ESC1,mesada_1_cont_SLM_ESC2,mesada_1_cont_SLM_ESC_IPC
           
    ) %>% 
    mutate(estado = estado,
           edadmeses_A = edadmeses_A,
           edadmeses_B = edadmeses_B,
           edadmeses_C = edadmeses_C,
           edadmeses_D = edadmeses_D,
           edadmeseshasta_A = edadmeseshasta_A,
           edadmeseshasta_B = edadmeseshasta_B,
           edadmeseshasta_C = edadmeseshasta_C,
           edadmeseshasta_D = edadmeseshasta_D,
           edadmeses_A_fin = if_else(estado == 3,0,edadmeses_A+t),
           edadmeses_B_fin = if_else(is.na(edadmeses_B), 0, edadmeses_B+t),
           edadmeses_C_fin = if_else(is.na(edadmeses_C), 0, edadmeses_C+t),
           edadmeses_D_fin = if_else(is.na(edadmeses_D), 0, edadmeses_D+t),
    ) %>% 
    mutate(
      edadmeses_A_fin = if_else(edadmeses_A_fin > edadmeseshasta_A, 0, edadmeses_A_fin),
      edadmeses_B_fin = if_else(edadmeses_B_fin > edadmeseshasta_B, 0, edadmeses_B_fin),
      edadmeses_C_fin = if_else(edadmeses_C_fin > edadmeseshasta_C, 0, edadmeses_C_fin),
      edadmeses_D_fin = if_else(edadmeses_D_fin > edadmeseshasta_D, 0, edadmeses_D_fin)
    )
  retorno
}



gc()
################################################################################
pensiones_RL$TEMPORALIDAD <- as.numeric(pensiones_RL$TEMPORALIDAD)
tictoc::tic()
sub_tabla_2 <-
  pensiones_RL %>%
  crear_tablas_beneficiarios(data = ., endvar = "A") %>%
  crear_tablas_beneficiarios(data = ., endvar = "B") %>%
  crear_tablas_beneficiarios(data = ., endvar = "C") %>%
  crear_tablas_beneficiarios(data = ., endvar = "D") %>%
  mutate(max_dif_calc = purrr::pmap_dbl(select(., starts_with("difcalc_")),
                                        pmax, 
                                        na.rm = TRUE)
  ) %>% 
  #slice(10:100) %>% 
  mutate(tabla = pmap(
    list(max_dif_calc, FECHACORTE, INTERES_TEC, TASA_CREC, MESADA_PER,
         NRO_MESADAS, GASTOS_FIX, ESTADO_A, edadmeses_A, edadmeses_B,edadmeses_C,
         edadmeses_D, edadmeseshasta_A,edadmeseshasta_B,edadmeseshasta_C,edadmeseshasta_D),
    ~crear_tabla_flujos(max_dif_calc = ..1,fec_corte = ..2,interes_tecnico = ..3,
                        tasa_crecimiento = ..4, mesada = ..5,Num_mesadas_para = ..6,
                        gastos = ..7,estado = ..8,edadmeses_A = ..9,
                        edadmeses_B = ..10,edadmeses_C = ..11,edadmeses_D = ..12,
                        edadmeseshasta_A = ..13,edadmeseshasta_B = ..14,
                        edadmeseshasta_C = ..15, edadmeseshasta_D = ..16
    )
  )) %>% 
  left_join(tibble_tabla_mort, by = c("SEXO_A"="sexo","ESTADO_A"="estado")) %>% 
  rename(mortalidad_A = tabla_mort) %>% 
  left_join(tibble_tabla_mort, by = c("SEXO_B"="sexo","ESTADO_B"="estado")) %>% 
  rename(mortalidad_B = tabla_mort) %>%
  left_join(tibble_tabla_mort, by = c("SEXO_C"="sexo","ESTADO_C"="estado")) %>% 
  rename(mortalidad_C = tabla_mort) %>%
  left_join(tibble_tabla_mort, by = c("SEXO_D"="sexo","ESTADO_D"="estado")) %>% 
  rename(mortalidad_D = tabla_mort) %>% 
  mutate(tabla_final_A = 
           map2(.x = tabla, .y = mortalidad_A, 
                ~if(is.null(.y))
                {
                  .x %>% mutate(lx_A = 0)
                }
                else
                {
                  .x %>% 
                    left_join(.y, by=c("edadmeses_A_fin"="edad_meses")) %>% 
                    rename(lx_A = lx)
                }
           ),
         tabla_final_B =
           map2(.x = tabla_final_A, .y = mortalidad_B,
                ~ if (is.null(.y))
                {
                  .x %>% mutate(lx_B = 0)
                }
                else
                {
                  .x %>%
                    left_join(.y, by = c("edadmeses_B_fin" = "edad_meses")) %>%
                    rename(lx_B = lx)
                }),
         tabla_final_C =
           map2(.x = tabla_final_B, .y = mortalidad_C,
                ~ if (is.null(.y))
                {
                  .x %>% mutate(lx_C = 0)
                }
                else
                {
                  .x %>%
                    left_join(.y, by = c("edadmeses_C_fin" = "edad_meses")) %>%
                    rename(lx_C = lx)
                }),
         tabla_final_D =
           map2(.x = tabla_final_C, .y = mortalidad_D,
                ~ if (is.null(.y))
                {
                  .x %>% mutate(lx_D = 0)
                }
                else
                {
                  .x %>%
                    left_join(.y, by = c("edadmeses_D_fin" = "edad_meses")) %>%
                    rename(lx_D = lx)
                })
  ) %>% 
  select(-tabla,-mortalidad_A,-mortalidad_B,-mortalidad_C,-mortalidad_D,
         -tabla_final_A,-tabla_final_B,-tabla_final_C
  ) %>% 
  mutate(tabla_for_calculate = 
           map(tabla_final_D,
               ~.x %>% 
                 mutate(
                   lx_A = if_else(edadmeses_A_fin == 0, 0, lx_A),
                   lx_B = if_else(edadmeses_B_fin == 0, 0, lx_B),
                   lx_C = if_else(edadmeses_C_fin == 0, 0, lx_C),
                   lx_D = if_else(edadmeses_D_fin == 0, 0, lx_D)
                 ) %>% 
                 mutate(
                   lx_A0 = first(lx_A) ,
                   lx_B0 = first(lx_B),
                   lx_C0 = first(lx_C),
                   lx_D0 = first(lx_D)
                 ) %>% 
                 mutate(t_p_A=lx_A/lx_A0, 
                        t_p_B=lx_B/lx_B0, 
                        t_p_C=lx_C/lx_C0, 
                        t_p_D=lx_D/lx_D0
                 ) %>% 
                 mutate(t_p_A= ifelse(is.na(t_p_A) | is.nan(t_p_A), 0, t_p_A), 
                        t_p_B= ifelse(is.na(t_p_B) | is.nan(t_p_B), 0, t_p_B), 
                        t_p_C= ifelse(is.na(t_p_C) | is.nan(t_p_C), 0, t_p_C), 
                        t_p_D= ifelse(is.na(t_p_D) | is.nan(t_p_D), 0, t_p_D), 
                        t_p_AB=t_p_A*t_p_B, 
                        t_p_AC=t_p_A*t_p_C,
                        t_p_AD=t_p_A*t_p_D, 
                        t_p_BC=t_p_B*t_p_C, 
                        t_p_BD=t_p_B*t_p_D, 
                        t_p_CD=t_p_C*t_p_D, 
                        t_p_ABC=t_p_A*t_p_B*t_p_C,
                        t_p_ABD=t_p_A*t_p_B*t_p_D,
                        t_p_ACD=t_p_A*t_p_C*t_p_D, 
                        t_p_BCD=t_p_B*t_p_C*t_p_D, 
                        t_p_ABCD=t_p_A*t_p_B*t_p_C*t_p_D, 
                        t_p1=t_p_A,
                        t_p2=t_p_B-t_p_AB, 
                        t_p3=t_p_C-t_p_AC-t_p_BC+t_p_ABC,
                        t_p4=t_p_D-t_p_AD-t_p_BD-t_p_CD+t_p_ABD+t_p_ACD+t_p_BCD-t_p_ABCD,
                        pi_sob=t_p2+t_p3+t_p4
                 ) %>% 
                 mutate(Fact_AF_A = t_p_A*(1-lead(lx_A)/lx_A)) %>% 
                 mutate(Fact_AF_A = if_else(is.nan(Fact_AF_A), 0, Fact_AF_A)) %>% 
                 mutate(
                   monto_Aux_Fun=   min(10*SMLV,max(mesada_1, 5*SMLV)),
                   monto_Aux_Fun_ESC0    =  case_when(estado == 2 ~ min(10*SMLV,max(mesada_1_cont_SLM_ESC0, 5*SMLV)),TRUE ~0 ),
                   monto_Aux_Fun_ESC1    =  case_when(estado == 2 ~  min(10*SMLV,max(mesada_1_cont_SLM_ESC1, 5*SMLV)),TRUE ~0),
                   monto_Aux_Fun_ESC2    =  case_when(estado == 2 ~  min(10*SMLV,max(mesada_1_cont_SLM_ESC2, 5*SMLV)),TRUE ~0),
                   monto_Aux_Fun_ESC_IPC =  case_when(estado == 2 ~  min(10*SMLV,max(mesada_1_cont_SLM_ESC_IPC, 5*SMLV)),TRUE ~0),
                   #
                   monto_Aux_Fun_1         = monto_Aux_Fun * (1 + tasa_crecimiento) ^ pot,
                   monto_Aux_Fun_ESC0_1    = monto_Aux_Fun_ESC0* (1 + tasa_crecimiento) ^ pot,
                   monto_Aux_Fun_ESC1_1    = monto_Aux_Fun_ESC1 * (1 + tasa_crecimiento) ^ pot,
                   monto_Aux_Fun_ESC2_1    = monto_Aux_Fun_ESC2 * (1 + tasa_crecimiento) ^ pot,
                   monto_Aux_Fun_ESC_IPC_1 = monto_Aux_Fun_ESC_IPC * (1 + tasa_crecimiento) ^ pot,
                   
                   Monto_AF_Gastos         = monto_Aux_Fun_1 / (1 - gastos),
                   Monto_AF_Gastos_ESC0    = monto_Aux_Fun_ESC0_1 / (1 - gastos),
                   Monto_AF_Gastos_ESC1    = monto_Aux_Fun_ESC1_1 / (1 - gastos),
                   Monto_AF_Gastos_ESC2    = monto_Aux_Fun_ESC2_1 / (1 - gastos),
                   Monto_AF_Gastos_ESC_IPC = monto_Aux_Fun_ESC_IPC_1 / (1 - gastos),
                   producto_aux_fun=Fact_AF_A*Monto_AF_Gastos*v
                 )
           )
  ) %>% 
  mutate(resumen_final_pension = map(tabla_for_calculate,
                                     ~.x %>% 
                                       slice(-1) %>% 
                                       summarise(reser_vejez_ESC0 = sum(t_p1*mesada_total_gatos_SLM_ESC0*v, na.rm = T),
                                                 reser_vejez_ESC1 = sum(t_p1*mesada_total_gatos_SLM_ESC1*v, na.rm = T),  
                                                 reser_vejez_ESC2 = sum(t_p1*mesada_total_gatos_SLM_ESC2*v, na.rm = T),
                                                 reser_vejez_ESC_IPC = sum(t_p1*mesada_total_gatos_SLM_ESC_IPC*v, na.rm = T),
                                                 reser_sobre_ESC0 = sum(pi_sob*mesada_total_gatos_SLM_ESC0*v, na.rm = T),
                                                 reser_sobre_ESC1 = sum(pi_sob*mesada_total_gatos_SLM_ESC1*v, na.rm = T), 
                                                 reser_sobre_ESC2 = sum(pi_sob*mesada_total_gatos_SLM_ESC2*v, na.rm = T),
                                                 reser_sobre_ESC_IPC = sum(pi_sob*mesada_total_gatos_SLM_ESC_IPC*v, na.rm = T),
                                                 #reser_auxfun_ESC0 = sum(Fact_AF_A*Monto_AF_Gastos*v, na.rm = T), # todos los escenarios iguales
                                                 reser_auxfun_ESC0 = sum(Fact_AF_A*Monto_AF_Gastos_ESC0*v, na.rm = T),
                                                 reser_auxfun_ESC1 = sum(Fact_AF_A*Monto_AF_Gastos_ESC1*v, na.rm = T),  
                                                 reser_auxfun_ESC2 = sum(Fact_AF_A*Monto_AF_Gastos_ESC2*v, na.rm = T),
                                                 reser_auxfun_ESC_IPC = sum(Fact_AF_A*Monto_AF_Gastos_ESC_IPC*v, na.rm = T)
                                       ) 
  )
  )
tictoc::toc()
#beep(8)
#### Guardar Flujo de pensiones
#sub_tabla_2 %>% colnames()
#sub_tabla_2 %>% head(20) %>% View()
#write_rds(sub_tabla_2, paste0 ("outputs/flujos/flujo_pens_RL_0101",ahno_valuacion,"_",tipo,".rds") )

################################################################################
resumenes <- sub_tabla_2 %>% 
  select(COMPANIA, resumen_final_pension) %>% 
  unnest(resumen_final_pension)
uu = resumenes %>% 
  group_by(COMPANIA) %>% 
  summarise(ipc_vejez = sum(reser_vejez_ESC_IPC)/1e6,
            ipc_sobre = sum(reser_sobre_ESC_IPC)/1e6,
            ipc_AUX_FUN = sum(reser_auxfun_ESC_IPC)/1e6,
            ESC0_vejez = sum(reser_vejez_ESC0)/1e6,
            ESC0_sobre = sum(reser_sobre_ESC0)/1e6,
            ESC0_AUX_FUN = sum(reser_auxfun_ESC0)/1e6,
            ESC1_vejez = sum(reser_vejez_ESC1)/1e6,
            ESC1_sobre = sum(reser_sobre_ESC1)/1e6,
            ESC1_AUX_FUN = sum(reser_auxfun_ESC1)/1e6,
            ESC2_vejez = sum(reser_vejez_ESC2)/1e6,
            ESC2_sobre = sum(reser_sobre_ESC2)/1e6,
            ESC2_AUX_FUN = sum(reser_auxfun_ESC2)/1e6
  ) %>% 
  mutate(ipc_TOTAL = rowSums(select(.,starts_with("ipc"))),
         ESC0_TOTAL = rowSums(select(.,starts_with("ESC0"))),
         ESC1_TOTAL = rowSums(select(.,starts_with("ESC1"))),
         ESC2_TOTAL = rowSums(select(.,starts_with("ESC2")))
  )
uu_t<-uu%>% 
  summarise(across(where(is.numeric), sum)) %>% # Suma solo las columnas numéricas
  bind_rows(uu, .)
print(uu$ESC0_TOTAL)
#View(uu)

variable <- c("INVALIDEZ_VEJEZ","SOBREVIVENCIA","AUX_FUNERARIO","TOTAL_RESERVA")
pensiones_RL <- pensiones_RL %>%  mutate(across(.cols = c(INVALIDEZ_VEJEZ, SOBREVIVENCIA, AUX_FUNERARIO, TOTAL_RESERVA),
                                                parse_number)
)

yy = pensiones_RL %>% 
  group_by(COMPANIA) %>% 
  summarise(ipc_vejez = sum(INVALIDEZ_VEJEZ,na.rm = T)/1e6,
            ipc_sobre = sum(SOBREVIVENCIA,na.rm = T)/1e6,
            ipc_AUX_FUN = sum(AUX_FUNERARIO,na.rm = T)/1e6,
            ipc_TOTAL = sum(TOTAL_RESERVA,na.rm = T)/1e6,
            sum_pond_tasa_crec = sum(TASA_CREC*TOTAL_RESERVA,na.rm = T)/sum(TOTAL_RESERVA,na.rm = T),
            mean_tasa_crec = mean(TASA_CREC),
            median_tasa_crec = median(TASA_CREC)
  ) 
yy_t<-yy%>% 
  summarise(across(where(is.numeric), sum)) %>% # Suma solo las columnas numéricas
  bind_rows(yy, .)

# View(yy %>%       left_join(uu, by ="COMPANIA", suffix=c("_394","_CALC")) )

#### Exportar resultados
zz<-yy %>% 
  left_join(uu, by ="COMPANIA", suffix=c("_394","_CALC")) 

zz_t<-zz%>% 
  summarise(across(where(is.numeric), sum)) %>% # Suma solo las columnas numéricas
  bind_rows(zz, .)

####
#### COMPARACIÓN DE RESERVAS POR IAS Y ESCENACIOS
write_delim(zz,paste0("outputs/txt/comparacion_ias_reserva_total_",ahno_valuacion,"_",tipo,".txt"),delim = "|") 
write_delim(uu,paste0("outputs/txt/comparacion_esc_reserva_total_",ahno_valuacion,"_",tipo,".txt"),delim = "|")

hojas <- list("IAS" = zz_t, 
              "ESC" = uu_t
)
write.xlsx(hojas,paste0("outputs/excel/comparacion_reserva_total_",ahno_valuacion,"_",tipo,"correcion.xlsx"), overwrite = TRUE)
#########
set.seed(123)
colmena = pensiones_RL %>% 
  filter(COMPANIA == "COLMENA") %>% 
  #slice_sample(n = 5) %>% 
  mutate(across(starts_with("ESTADO_"),.fns = ~case_when(.x == 1 ~ "valido",
                                                         .x == 2 ~ "invalido",
                                                         .x == 3 ~ "muerto",
                                                         TRUE ~ as.character(.x)
  )
  )) %>% 
  mutate(across(starts_with("PARENT_"),.fns = ~case_when(.x == 1 ~ "afiliado",
                                                         .x == 2 ~ "hijo",
                                                         .x == 3 ~ "hijo",
                                                         .x == 4 ~ "conyuge",
                                                         .x == 5 ~ "padre",
                                                         .x == 6 ~ "madre",
                                                         .x == 7 ~ "compañera < 30",
                                                         .x == 8 ~ "hermano",
                                                         TRUE ~ as.character(.x)
  )
  )) %>% 
  mutate(across(starts_with("SEXO_"),.fns = ~case_when(.x == 1 ~ "masculino",
                                                       .x == 2 ~ "femenino",
                                                       TRUE ~ as.character(.x)
  )
  ))
colmena$GASTOS_FIX %>% summary()
0.45/100

suma_ipc = sum(resumenes$reser_vejez_ESC_IPC) + 
  sum(resumenes$reser_sobre_ESC_IPC) + sum(resumenes$reser_auxfun_ESC_IPC)
scales::dollar(suma_ipc/1e6,big.mark = ",")


tt = sub_tabla_2 %>% 
  filter(COMPANIA == "COLMENA") %>% 
  filter(FECH_NACI_A == ymd("1939-05-16"))
#write_delim(tt[[41]][[1]],"outputs/validacion_flujos_colmena.txt",delim = "|")
colnames(sub_tabla_2)
################################################################################

pensiones_RL$TOTAL_RESERVA %>% sum(na.rm = T)

cc = sub_tabla_2 %>% 
  select(COMPANIA,tabla_for_calculate) %>% 
  mutate(temp = map(tabla_for_calculate,~.x %>% slice(-1))) %>% 
  unnest(temp)

dd = cc %>% 
  group_by(COMPANIA, ahno) %>% 
  summarise(reser_vejez_ESC0 = sum(t_p1*mesada_total_gatos_SLM_ESC0*v, na.rm = T),
            reser_vejez_ESC1 = sum(t_p1*mesada_total_gatos_SLM_ESC1*v, na.rm = T),  
            reser_vejez_ESC2 = sum(t_p1*mesada_total_gatos_SLM_ESC2*v, na.rm = T),
            reser_vejez_ESC_IPC = sum(t_p1*mesada_total_gatos_SLM_ESC_IPC*v, na.rm = T),
            reser_sobre_ESC0 = sum(pi_sob*mesada_total_gatos_SLM_ESC0*v, na.rm = T),
            reser_sobre_ESC1 = sum(pi_sob*mesada_total_gatos_SLM_ESC1*v, na.rm = T), 
            reser_sobre_ESC2 = sum(pi_sob*mesada_total_gatos_SLM_ESC2*v, na.rm = T),
            reser_sobre_ESC_IPC = sum(pi_sob*mesada_total_gatos_SLM_ESC_IPC*v, na.rm = T),
            reser_auxfun_ESC0 = sum(Fact_AF_A*Monto_AF_Gastos_ESC0*v, na.rm = T),
            reser_auxfun_ESC1 = sum(Fact_AF_A*Monto_AF_Gastos_ESC1*v, na.rm = T),  
            reser_auxfun_ESC2 = sum(Fact_AF_A*Monto_AF_Gastos_ESC2*v, na.rm = T),
            reser_auxfun_ESC_IPC = sum(Fact_AF_A*Monto_AF_Gastos_ESC_IPC*v, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(ipc_TOTAL  = rowSums(select(.,contains("IPC"))),
         ESC0_TOTAL = rowSums(select(.,contains("ESC0"))),
         ESC1_TOTAL = rowSums(select(.,contains("ESC1"))),
         ESC2_TOTAL = rowSums(select(.,contains("ESC2")))
  )
#### GUARDAR FLUJOS ANUALES
write_delim(dd, paste0("outputs/flujo_anual_pensiones",ahno_valuacion,"_",tipo,".txt"),delim = "|")

write.xlsx(dd, paste0("outputs/flujo_anual_pensiones_dd",ahno_valuacion,"_",tipo,"_correcion.xlsx"))
#colnames(dd)
resumen_flujo_ahno <- dd %>% group_by(ahno) %>% 
  summarise(ipc_TOTAL = sum(ipc_TOTAL,  na.rm = T),
            ESC0_TOTAL= sum(ESC0_TOTAL, na.rm = T),
            ESC1_TOTAL= sum(ESC1_TOTAL, na.rm = T),
            ESC2_TOTAL= sum(ESC2_TOTAL, na.rm = T)
  ) %>% ungroup() %>% arrange(ahno)

#colnames(sub_tabla_2)
gc()
print(uu$ESC0_TOTAL)



##### FLUJOS SIN TRAER A VALOR PRESENTE
# realizar el calculo de sin multiplicar por *v 

vr = cc %>% 
  group_by(COMPANIA, ahno) %>% 
  summarise(reser_vejez_ESC0 = sum(t_p1*mesada_total_gatos_SLM_ESC0, na.rm = T),
            reser_vejez_ESC1 = sum(t_p1*mesada_total_gatos_SLM_ESC1, na.rm = T),  
            reser_vejez_ESC2 = sum(t_p1*mesada_total_gatos_SLM_ESC2, na.rm = T),
            reser_vejez_ESC_IPC = sum(t_p1*mesada_total_gatos_SLM_ESC_IPC, na.rm = T),
            reser_sobre_ESC0 = sum(pi_sob*mesada_total_gatos_SLM_ESC0, na.rm = T),
            reser_sobre_ESC1 = sum(pi_sob*mesada_total_gatos_SLM_ESC1, na.rm = T), 
            reser_sobre_ESC2 = sum(pi_sob*mesada_total_gatos_SLM_ESC2, na.rm = T),
            reser_sobre_ESC_IPC = sum(pi_sob*mesada_total_gatos_SLM_ESC_IPC, na.rm = T),
            reser_auxfun_ESC0 = sum(Fact_AF_A*Monto_AF_Gastos_ESC0, na.rm = T),
            reser_auxfun_ESC1 = sum(Fact_AF_A*Monto_AF_Gastos_ESC1, na.rm = T),  
            reser_auxfun_ESC2 = sum(Fact_AF_A*Monto_AF_Gastos_ESC2, na.rm = T),
            reser_auxfun_ESC_IPC = sum(Fact_AF_A*Monto_AF_Gastos_ESC_IPC, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(ipc_TOTAL  = rowSums(select(.,contains("IPC"))),
         ESC0_TOTAL = rowSums(select(.,contains("ESC0"))),
         ESC1_TOTAL = rowSums(select(.,contains("ESC1"))),
         ESC2_TOTAL = rowSums(select(.,contains("ESC2")))
  )
#### GUARDAR FLUJOS ANUALES
write.xlsx(vr, paste0("outputs/flujo_anual_pensiones_vr",ahno_valuacion,"_",tipo,"_correcion.xlsx"))
#colnames(dd)
resumen_flujo_ahno_vr <- vr %>% group_by(ahno) %>% 
  summarise(ipc_TOTAL = sum(ipc_TOTAL,  na.rm = T),
            ESC0_TOTAL= sum(ESC0_TOTAL, na.rm = T),
            ESC1_TOTAL= sum(ESC1_TOTAL, na.rm = T),
            ESC2_TOTAL= sum(ESC2_TOTAL, na.rm = T)
  ) %>% ungroup() %>% arrange(ahno)

#colnames(sub_tabla_2)







