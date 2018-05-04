# ine_cotecora
Conteo rápido elecciones 2018: Nacional y Guanajuato

El [conteo rápido](http://www.ine.mx/wp-content/uploads/2017/07/DERFE-explicacionconteonayarit2017.pdf) es un ejercicio que realiza el Instituto Nacional Electoral (INE) y por un Comité Técnico Asesor para el Conteo Rápido (COTECORA) con el fin de estimar los resultado de elecciones presidenciales y estatales.

Este repositorio contiene el trabajo realizado por el equipo de Michelle Anzarut, cuya responsabilidad primordial fue seleccionar el tamaño de muestra y método de estimación a utilizar en Guanajuato. Adicionalmente se trabajó en la elección nacional y en el estado de Morelos. 

# Muestreo probabilístico
Se selecciona una muestra de casillas a partir de las cuales se estimarán los resultados. Para esta selección se utiliza muestro probabilístico (estratificado). El tamaño de muestra se selecciona de tal manera que los intervalos de 95% de confianza, para la proporción de votos obtenidos por los candidatos, tengan precisiones de aproximadamente 1 punto porcentual.

Para determinar los tamaños de muestra se realizaron ejercicios de simulación, los scripts y reportes de éstos ejercicios están en las carpetas: 

* tamano_muestra/scripts_rmd
  
  - /explora_disenio_muestral_2012.Rmd: Se consideran distintas estratificaciones y se utiliza simulación para elegir la estratificación que resulte en la mayor precisión con el menor tamaño de muestra. Un criterio adicional a considerar es la carga de trabajo para los CAES (encargados de enviar la información de las casillas en la muestra) de tal manera que se minimicen los faltantes.  
  - /explora_disenio_muestral_faltantes_2012.Rmd: Se exploran las consecuencias (en sesgo y precisión) de recibir muestras incompletas.
  - /remesas.Rmd: Exploración (incompleta) de factores que atrasan el reporte de las casillas, diferencias entre ámbito rural/urbano, huso horario,... El análisis quedó incompleto a falta de datos.
  
* tamano_muestra/reportes: Los reportes correspondientes a los archivos Rmarkdown mencionados.

# Modelo
Desarrollo y validación del modelo a utilizar.
