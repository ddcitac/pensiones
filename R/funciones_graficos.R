# -----------------------------------------------------------------------------
# Funciones para visualizacion y tableros.
# -----------------------------------------------------------------------------

graficar_flujos <- function(flujos) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Instale ggplot2 para usar graficar_flujos().", call. = FALSE)
  }
  ggplot2::ggplot(flujos, ggplot2::aes(x = periodo_meses, y = valor)) +
    ggplot2::geom_line(color = "#1f77b4") +
    ggplot2::labs(x = "Periodo (meses)", y = "Flujo", title = "Flujos proyectados") +
    ggplot2::theme_minimal()
}

graficar_reserva_escenarios <- function(reservas) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Instale ggplot2 para usar graficar_reserva_escenarios().", call. = FALSE)
  }
  ggplot2::ggplot(reservas, ggplot2::aes(x = escenario, y = reserva, fill = escenario)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::labs(x = "Escenario", y = "Reserva", title = "Reserva por escenario") +
    ggplot2::theme_minimal()
}

convertir_plotly <- function(grafico) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Instale plotly para usar convertir_plotly().", call. = FALSE)
  }
  plotly::ggplotly(grafico)
}

crear_tabla_dt <- function(datos) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Instale DT para usar crear_tabla_dt().", call. = FALSE)
  }
  DT::datatable(datos, options = list(pageLength = 10, scrollX = TRUE))
}
