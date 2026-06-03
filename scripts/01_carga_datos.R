# -----------------------------------------------------------------------------
# 01_carga_datos.R
# Carga insumos del proyecto desde data/ y parametros/.
# -----------------------------------------------------------------------------

source(file.path("scripts", "00_config.R"))
source(file.path("R", "funciones_validacion.R"))

leer_excel_seguro <- function(ruta, sheet = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Instale readxl para leer archivos Excel.", call. = FALSE)
  }
  if (is.null(sheet)) readxl::read_excel(ruta) else readxl::read_excel(ruta, sheet = sheet)
}

listar_insumos <- function(paths_obj = paths) {
  fuentes <- c(paths_obj$data, paths_obj$parametros)
  archivos <- unlist(lapply(fuentes[dir.exists(fuentes)], list.files, recursive = TRUE, full.names = TRUE), use.names = FALSE)
  data.frame(
    archivo = archivos,
    nombre = basename(archivos),
    extension = tools::file_ext(archivos),
    stringsAsFactors = FALSE
  )
}

cargar_parametros_deslizamiento <- function(paths_obj = paths) {
  candidatos <- list.files(paths_obj$parametros, pattern = "deslizamiento.*\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
  if (length(candidatos) == 0) return(NULL)
  leer_excel_seguro(candidatos[[1]])
}

insumos_disponibles <- listar_insumos(paths)
validacion_insumos <- validar_archivos_entrada(paths)

if (interactive()) {
  print(insumos_disponibles)
  print(validacion_insumos)
}
