# -----------------------------------------------------------------------------
# Funciones de modelamiento actuarial y predictivo.
# -----------------------------------------------------------------------------

calcular_factor_descuento <- function(tasa_anual, periodo_meses) {
  tasa_mensual <- (1 + tasa_anual)^(1 / 12) - 1
  (1 + tasa_mensual)^(-periodo_meses)
}

calcular_reserva_mensual <- function(flujos, tasa_anual = 0.03) {
  stopifnot(all(c("periodo_meses", "valor") %in% names(flujos)))
  flujos$factor_descuento <- calcular_factor_descuento(tasa_anual, flujos$periodo_meses)
  flujos$valor_presente <- flujos$valor * flujos$factor_descuento
  sum(flujos$valor_presente, na.rm = TRUE)
}

ajustar_modelo_glmnet <- function(x, y, family = "gaussian", alpha = 1) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Instale glmnet para usar ajustar_modelo_glmnet().", call. = FALSE)
  }
  glmnet::cv.glmnet(as.matrix(x), y, family = family, alpha = alpha)
}

ajustar_modelo_gam <- function(formula, data) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("Instale mgcv para usar ajustar_modelo_gam().", call. = FALSE)
  }
  mgcv::gam(formula, data = data, method = "REML")
}

ajustar_modelo_supervisado <- function(formula, data, metodo = "rf") {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Instale caret para usar ajustar_modelo_supervisado().", call. = FALSE)
  }
  caret::train(formula, data = data, method = metodo)
}
