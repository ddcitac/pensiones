# -----------------------------------------------------------------------------
# 00_config.R
# Configuracion central del proyecto de reservas de pensiones.
# Ejecute este archivo al inicio de cualquier flujo en RStudio.
# -----------------------------------------------------------------------------

options(stringsAsFactors = FALSE, scipen = 999)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

# Paquetes requeridos por el estandar analitico del proyecto. Se usa el nombre
# real del paquete en R: shiny, quarto y rmarkdown se escriben en minuscula.
paquetes_requeridos <- c(
  "tidyverse", "dplyr", "data.table", "purrr", "stringr", "lubridate",
  "readxl", "openxlsx", "arrow", "duckdb", "DBI", "sparklyr", "ggplot2",
  "plotly", "DT", "flexdashboard", "shiny", "quarto", "rmarkdown",
  "actuar", "ChainLadder", "survival", "MASS", "glmmTMB", "glmnet",
  "mgcv", "forecast", "fable", "tidymodels", "caret", "xgboost",
  "randomForest", "renv"
)

# Parametros del ejercicio. Ajuste estos valores antes de ejecutar el pipeline.
config <- list(
  proyecto = "pensiones",
  ahno_valuacion = 2024,
  smlv = 1300000,
  escenario_base = "con_deslizamiento",
  moneda = "COP",
  tasa_tecnica_anual = 0.03,
  horizonte_proyeccion = 3000,
  hoja_formato_394 = "Sheet 1"
)

# Rutas canonicas del proyecto.
root_dir <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% getwd()), ".."), mustWork = FALSE)
paths <- list(
  root = root_dir,
  data = file.path(root_dir, "data"),
  scripts = file.path(root_dir, "scripts"),
  funciones = file.path(root_dir, "R"),
  outputs = file.path(root_dir, "outputs"),
  tablas = file.path(root_dir, "outputs", "tablas"),
  img = file.path(root_dir, "outputs", "img"),
  reportes = file.path(root_dir, "outputs", "reportes"),
  docs = file.path(root_dir, "docs"),
  parametros = file.path(root_dir, "parametros"),
  legado = file.path(root_dir, "scr")
)

crear_estructura <- function(paths_obj = paths) {
  dirs <- unlist(paths_obj[c("data", "scripts", "funciones", "tablas", "img", "reportes", "docs")])
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

cargar_librerias_requeridas <- function(paquetes = paquetes_requeridos, instalar_faltantes = FALSE) {
  faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]

  if (length(faltantes) > 0 && isTRUE(instalar_faltantes)) {
    install.packages(faltantes, dependencies = TRUE)
    faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
  }

  disponibles <- setdiff(paquetes, faltantes)
  invisible(lapply(disponibles, library, character.only = TRUE))

  if (length(faltantes) > 0) {
    warning(
      "Paquetes faltantes: ", paste(faltantes, collapse = ", "),
      ". Instale con install.packages() o renv::restore().",
      call. = FALSE
    )
  }

  data.frame(
    paquete = paquetes,
    disponible = paquetes %in% disponibles,
    stringsAsFactors = FALSE
  )
}

crear_estructura()
