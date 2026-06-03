# -----------------------------------------------------------------------------
# 03_modelamiento.R
# Modelamiento actuarial de reservas y modelos estadisticos auxiliares.
# -----------------------------------------------------------------------------

source(file.path("scripts", "00_config.R"))
source(file.path("R", "funciones_modelos.R"))

proyectar_mesada_constante <- function(valor_mesada, meses, crecimiento_anual = 0) {
  periodo_meses <- seq_len(meses)
  crecimiento_mensual <- (1 + crecimiento_anual)^(1 / 12) - 1
  data.frame(
    periodo_meses = periodo_meses,
    valor = valor_mesada * (1 + crecimiento_mensual)^(periodo_meses - 1)
  )
}

calcular_reserva_escenarios <- function(valor_mesada, meses, escenarios, tasa_anual = config$tasa_tecnica_anual) {
  resultados <- lapply(names(escenarios), function(escenario) {
    flujos <- proyectar_mesada_constante(valor_mesada, meses, escenarios[[escenario]])
    data.frame(
      escenario = escenario,
      reserva = calcular_reserva_mensual(flujos, tasa_anual),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, resultados)
}

escenarios_deslizamiento <- c(
  ipc = 0.00,
  escenario_0 = 0.03,
  escenario_1 = 0.04,
  escenario_2 = 0.02
)
