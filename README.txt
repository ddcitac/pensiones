# Deslizamiento

En el proyecto actual se realiza la estimación de las reservas de las mesadas contenidas en el formato 394 de la Circular Externa 041 de 2016. Tarda aproximadamente entre 35 a 50 mínutos la ejecución.


Recibe como insumo las siguientes bases:

* **SISS_FORMATO394**: Base a un corte específico con los ramos correspondientes
* **Tabla_validos_M**: [TCMR RV 08 Resolución número 1555 de 2010](https://www.superfinanciera.gov.co/jsp/10103719), de manera mensual utilizando distribución uniforme de decesos 
* **Tabla_validos_F**: TCMR RV 08 Resolución número 1555 de 2010, de manera mensual utilizando distribución uniforme de decesos
* **Tabla_invalidos_M**: 
* **Tabla_invalidos_F**: 
* **SMLV**: Histórico del salario mínimo.


Los siguientes parámetros deben ser actualizados:
* **deslizamiento_esc0**: Estimación del deslizamiento, suele utilizarse el IPC y la variación del salario mínimo, promediando los ultimos 5, 10 o 20 años (incluso toda la historia). En la carpeta *Estimaciones* se encuentra el archivo *Estimación deslizamiento*, el cual puede actualizar y realizar el calculo del deslizamiento.
* **deslizamiento_esc1** y **deslizamiento_esc2**: Se le adiciona o resta unos puntos porcentuales para tener escenarios de variación.
* **tabla_incremento_slmv**: Periodo de ejecución del ejercicio, se toma a partir del año de proyección que se quiere realizar hasta 3000; donde el primer año tendrá un incremento de IPC de 1, los dos siguientes años se toma con los resultados mensuales de la [encuesta de expectativas de analistas económicos](https://www.banrep.gov.co/es/resultados-mensuales-expectativas-analistas-economicos?field_date_format_value=All&page=0%22), Hoja "Resumen", última tabla en la cual se encuentra el % anual en diciembre de los proximos 2 años. Para el resto de los años, se deja un 3% constante.

## Ramos 

Los ramos contenidos en el formato 394 son:
* 39 := Riesgos Profesionales 
* 40 := Pensiones Ley 100
* 41 := Pensiones Voluntarias
* 42 := Pensiones con Conmutación Pensional
* 43 := Rentas Voluntarias
(de conformidad  con el capítulo segundo del título VI de la Circular Externa 007 de 1996)

## Salidas

* Flujo anual de Pensiones
* Reserva Total por compañia, discriminado en sobrevivencia, invalidez y auxilio funerario.
* Reserva Total por compañia en 4 escenarios: IPC, escenario 0, escenario 1 y escenario 2

## Ejemplo de implementación

