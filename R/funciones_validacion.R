# -----------------------------------------------------------------------------
# Funciones de validacion del proyecto.
# -----------------------------------------------------------------------------

validar_paquetes <- function(paquetes) {
  data.frame(
    componente = "paquete",
    nombre = paquetes,
    estado = ifelse(vapply(paquetes, requireNamespace, logical(1), quietly = TRUE), "OK", "FALTA"),
    detalle = ifelse(
      vapply(paquetes, requireNamespace, logical(1), quietly = TRUE),
      "Instalado y disponible",
      "No instalado en la libreria actual"
    ),
    stringsAsFactors = FALSE
  )
}

validar_estructura_proyecto <- function(paths_obj) {
  esperados <- c("data", "scripts", "funciones", "tablas", "img", "reportes", "docs")
  data.frame(
    componente = "directorio",
    nombre = esperados,
    estado = ifelse(dir.exists(unlist(paths_obj[esperados])), "OK", "FALTA"),
    detalle = unlist(paths_obj[esperados]),
    stringsAsFactors = FALSE
  )
}

validar_sintaxis_r <- function(directorios = c("R", "scripts", "scr")) {
  archivos <- unlist(lapply(directorios[dir.exists(directorios)], function(d) {
    list.files(d, pattern = "\\.[rR]$", full.names = TRUE, recursive = TRUE)
  }), use.names = FALSE)

  if (length(archivos) == 0) {
    return(data.frame(
      componente = "sintaxis_r",
      nombre = NA_character_,
      estado = "FALTA",
      detalle = "No se encontraron archivos R",
      stringsAsFactors = FALSE
    ))
  }

  resultados <- lapply(archivos, function(archivo) {
    detalle <- tryCatch({
      parse(archivo, keep.source = FALSE)
      "Sintaxis valida"
    }, error = function(e) conditionMessage(e))

    data.frame(
      componente = "sintaxis_r",
      nombre = archivo,
      estado = ifelse(identical(detalle, "Sintaxis valida"), "OK", "ERROR"),
      detalle = detalle,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, resultados)
}

validar_archivos_entrada <- function(paths_obj) {
  patrones <- c(
    formato_394 = "FT394|FORMATO.?394|SISS_FORMATO394",
    validos_m = "validos.*M|Tabla_validos_M",
    validos_f = "validos.*F|Tabla_validos_F",
    invalidos = "invalidos|Tablas_Mortalidad",
    smlv = "SMLV|salario"
  )

  fuentes <- c(paths_obj$data, paths_obj$parametros)
  archivos <- unlist(lapply(fuentes[dir.exists(fuentes)], list.files, recursive = TRUE, full.names = TRUE), use.names = FALSE)

  resultados <- lapply(names(patrones), function(nombre) {
    encontrados <- archivos[grepl(patrones[[nombre]], basename(archivos), ignore.case = TRUE)]
    data.frame(
      componente = "insumo",
      nombre = nombre,
      estado = ifelse(length(encontrados) > 0, "OK", "FALTA"),
      detalle = ifelse(length(encontrados) > 0, paste(encontrados, collapse = "; "), "No encontrado"),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, resultados)
}

resumen_validacion <- function(resultados) {
  aggregate(nombre ~ componente + estado, data = resultados, FUN = length)
}
