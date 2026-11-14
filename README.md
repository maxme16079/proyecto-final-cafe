# El Niño y el café colombiano

Proyecto final de Business Analytics 2026-3, Pontificia Universidad Javeriana.
Integrantes: Maximo van Fulpen Nigrinis y Santiago Rubio.

## Pregunta

¿Qué efecto tiene el fenómeno de El Niño sobre la producción y las exportaciones de café de Colombia entre 2000 y 2026, una vez se controla por el precio internacional, la tasa de cambio, la estacionalidad y los choques conocidos del periodo?

## Estructura

| Ruta | Contenido |
|---|---|
| `datos/crudos/` | Archivos originales tal como se descargaron, sin modificar |
| `datos/base_cafe.csv` | Base mensual final, 321 meses de enero de 2000 a septiembre de 2026 |
| `R/01_base.R` | Lee los archivos crudos, los une por mes y escribe `datos/base_cafe.csv` |
| `R/02_vistazo.R` | Primer vistazo a la base, con mínimos, correlaciones y la gráfica `f01` |
| `R/03_modelos.R` | Tabla descriptiva, gráficas, regresiones y chequeo de robustez |
| `output/` | Gráficas en png que generan los scripts |
| `output/tablas/` | Tablas de resultados en csv que genera `03_modelos.R` |

## Cómo replicar

1. Instalar R 4.4 o superior y los paquetes.

```r
install.packages(c("tidyverse", "readxl", "scales"))
```

2. Abrir `proyecto-final-cafe.Rproj` en RStudio, o fijar el directorio de trabajo en la carpeta raíz del repositorio. Todas las rutas son relativas a esa carpeta.

3. Correr los scripts en orden.

```r
source("R/01_base.R")
source("R/02_vistazo.R")
source("R/03_modelos.R")
```

`01_base.R` vuelve a construir la base a partir de `datos/crudos/` y el resultado es idéntico a `datos/base_cafe.csv`. `03_modelos.R` vuelve a generar todas las gráficas y tablas de `output/`.

## Fuentes

Los archivos crudos se descargaron el 4 de noviembre de 2026.

| Archivo | Variables | Fuente |
|---|---|---|
| `fnc_precios_produccion.xlsx` | Producción, precio interno, precio OIC de suaves colombianos | Federación Nacional de Cafeteros, [estadísticas cafeteras](https://federaciondecafeteros.org/wp/estadisticas-cafeteras/) |
| `fnc_exportaciones.xlsx` | Exportaciones | Federación Nacional de Cafeteros, misma página |
| `fred_precio_cafe.csv` | Precio de otros suaves | FRED, serie [PCOFFOTMUSDM](https://fred.stlouisfed.org/series/PCOFFOTMUSDM) |
| `trm_diaria.csv` | TRM diaria | Banco de la República en [datos.gov.co](https://www.datos.gov.co/resource/mcec-87by.csv), conjunto `mcec-87by` |
| `noaa_oni.txt` | Índice ONI de El Niño y La Niña | NOAA, [oni.ascii.txt](https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt) |

## Variables de la base

| Variable | Descripción |
|---|---|
| `fecha` | Primer día del mes |
| `produccion` | Producción registrada, miles de sacos de 60 kg |
| `exportaciones` | Exportaciones, miles de sacos de 60 kg |
| `precio_interno` | Precio interno de referencia, pesos por carga de 125 kg |
| `precio_colombianos` | Precio OIC de suaves colombianos, centavos de dólar por libra |
| `precio_otros_suaves` | Precio de otros suaves (FRED), centavos de dólar por libra |
| `trm` | Promedio mensual de la TRM diaria, pesos por dólar |
| `oni` | Anomalía de temperatura del Pacífico. Cada trimestre móvil se asigna a su mes central |
| `cosecha_principal` | 1 en octubre, noviembre y diciembre |
| `mitaca` | 1 en abril, mayo y junio |
| `nino` | 1 si el ONI es mayor o igual a 0,5 |
| `nina` | 1 si el ONI es menor o igual a menos 0,5 |

## Modelo principal

ln(Y) = c + b · Niño hace 12 meses + ln(precio OIC hace 6 meses) + ln(TRM) + dummies de mes + tendencia + roya + pandemia + paro + error

Y es la producción o las exportaciones. También se estima la versión con solo El Niño y la versión en niveles. La robustez compara niveles contra logaritmos con una partición temporal (entrenamiento hasta 2021 y prueba de 2022 a 2026), revisa los residuos y cambia el rezago de El Niño entre 0 y 12 meses.
