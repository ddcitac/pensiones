# -----------------------------------------------------------------------------
# 04_validacion.R
# Validacion integral de estructura, dependencias, insumos y sintaxis R.
# -----------------------------------------------------------------------------

source(file.path("scripts", "00_config.R"))
source(file.path("R", "funciones_validacion.R"))

resultados_validacion <- rbind(
  validar_estructura_proyecto(paths),
  validar_paquetes(paquetes_requeridos),
  validar_archivos_entrada(paths),
  validar_sintaxis_r(c("R", "scripts", "scr"))
)

resumen <- resumen_validacion(resultados_validacion)

salida_csv <- file.path(paths$tablas, "validacion_proyecto.csv")
utils::write.csv(resultados_validacion, salida_csv, row.names = FALSE, fileEncoding = "UTF-8")

if (requireNamespace("openxlsx", quietly = TRUE)) {
  salida_xlsx <- file.path(paths$tablas, "validacion_proyecto.xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "detalle")
  openxlsx::addWorksheet(wb, "resumen")
  openxlsx::writeData(wb, "detalle", resultados_validacion)
  openxlsx::writeData(wb, "resumen", resumen)
  openxlsx::saveWorkbook(wb, salida_xlsx, overwrite = TRUE)
}

print(resumen)
