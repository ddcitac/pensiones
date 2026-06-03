# -----------------------------------------------------------------------------
# 02_limpieza.R
# Estandarizacion y limpieza de bases cargadas.
# -----------------------------------------------------------------------------

source(file.path("scripts", "00_config.R"))

limpiar_nombres <- function(nombres) {
  nombres <- tolower(iconv(nombres, to = "ASCII//TRANSLIT"))
  nombres <- gsub("[^a-z0-9]+", "_", nombres)
  nombres <- gsub("(^_|_$)", "", nombres)
  make.unique(nombres, sep = "_")
}

normalizar_base <- function(datos) {
  names(datos) <- limpiar_nombres(names(datos))
  datos
}

validar_columnas_minimas <- function(datos, columnas) {
  faltantes <- setdiff(columnas, names(datos))
  if (length(faltantes) > 0) {
    stop("Columnas faltantes: ", paste(faltantes, collapse = ", "), call. = FALSE)
  }
  TRUE
}

limpiar_formato_394 <- function(datos) {
  datos <- normalizar_base(datos)
  columnas_clave <- intersect(c("compania", "ramo", "sexo", "fecha_nacimiento", "valor_mesada"), names(datos))
  datos <- datos[stats::complete.cases(datos[, columnas_clave, drop = FALSE]), , drop = FALSE]
  datos
}
