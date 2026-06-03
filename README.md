# Proyecto reservas de pensiones

Este repositorio contiene una estructura modular para validar, limpiar, modelar y reportar reservas de mesadas del Formato 394. La carpeta `scr/` conserva los scripts historicos y la carpeta `scripts/` contiene el pipeline profesional listo para ejecutar desde RStudio.

## Estructura del proyecto

```text
proyecto/
  data/                     # Insumos operativos no versionados o controlados por el equipo
  parametros/               # Parametros historicos y archivos auxiliares existentes
  scripts/
    00_config.R             # Configuracion, rutas y librerias requeridas
    01_carga_datos.R        # Inventario y carga segura de insumos
    02_limpieza.R           # Limpieza y estandarizacion de bases
    03_modelamiento.R       # Proyeccion de flujos y reservas por escenario
    04_validacion.R         # Validacion integral del proyecto
    05_reportes.R           # Render de reportes RMarkdown / Quarto
  R/
    funciones_validacion.R  # Funciones reutilizables de validacion
    funciones_modelos.R     # Funciones actuariales y modelos estadisticos
    funciones_graficos.R    # Funciones de visualizacion
  outputs/
    tablas/                 # Resultados tabulares
    img/                    # Imagenes y graficos
    reportes/               # Reportes renderizados
  docs/                     # Fuentes de reportes y documentacion
  proyecto.Rproj            # Proyecto RStudio
  renv.lock                 # Semilla de reproducibilidad con renv
```

## Librerias requeridas

El archivo `scripts/00_config.R` declara el set base de librerias solicitado para el proyecto:

- tidyverse, dplyr, data.table, purrr, stringr, lubridate.
- readxl, openxlsx, arrow, duckdb, DBI, sparklyr.
- ggplot2, plotly, DT, flexdashboard, shiny, quarto, rmarkdown.
- actuar, ChainLadder, survival, MASS, glmmTMB, glmnet, mgcv.
- forecast, fable, tidymodels, caret, xgboost, randomForest, renv.

Para validar disponibilidad de paquetes sin instalar automaticamente:

```r
source("scripts/00_config.R")
cargar_librerias_requeridas(instalar_faltantes = FALSE)
```

Para instalar faltantes desde CRAN:

```r
source("scripts/00_config.R")
cargar_librerias_requeridas(instalar_faltantes = TRUE)
renv::snapshot()
```

## Ejecucion recomendada en RStudio

1. Abra `proyecto.Rproj`.
2. Coloque los insumos del Formato 394, tablas de mortalidad y SMLV en `data/` o `parametros/`.
3. Ejecute los scripts en este orden:

```r
source("scripts/00_config.R")
source("scripts/01_carga_datos.R")
source("scripts/02_limpieza.R")
source("scripts/03_modelamiento.R")
source("scripts/04_validacion.R")
source("scripts/05_reportes.R")
```

## Validacion integral

El script `scripts/04_validacion.R` revisa:

- Existencia de la estructura profesional de carpetas.
- Disponibilidad de todas las librerias requeridas.
- Existencia de insumos esperados: Formato 394, tablas de validos, tablas de invalidos y SMLV.
- Sintaxis de los scripts nuevos y de los scripts historicos en `scr/`.

Los resultados se escriben en `outputs/tablas/validacion_proyecto.csv` y, si `openxlsx` esta disponible, tambien en `outputs/tablas/validacion_proyecto.xlsx`.

## Notas de migracion

- Los scripts historicos en `scr/` no fueron eliminados para preservar trazabilidad.
- La nueva capa modular permite incorporar progresivamente la logica historica dentro de funciones reutilizables en `R/`.
- `renv.lock` se entrega como semilla; despues de instalar las dependencias en el equipo objetivo ejecute `renv::snapshot()` para congelar versiones exactas del entorno.
