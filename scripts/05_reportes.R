# -----------------------------------------------------------------------------
# 05_reportes.R
# Generacion de reportes R Markdown / Quarto.
# -----------------------------------------------------------------------------

source(file.path("scripts", "00_config.R"))

reporte_rmd <- file.path(paths$docs, "reporte_validacion.Rmd")
reporte_qmd <- file.path(paths$docs, "reporte_validacion.qmd")

if (requireNamespace("rmarkdown", quietly = TRUE) && file.exists(reporte_rmd)) {
  rmarkdown::render(reporte_rmd, output_dir = paths$reportes, quiet = TRUE)
}

if (requireNamespace("quarto", quietly = TRUE) && file.exists(reporte_qmd)) {
  quarto::quarto_render(reporte_qmd, output_dir = paths$reportes, quiet = TRUE)
}
