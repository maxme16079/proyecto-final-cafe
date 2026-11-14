#### Proyecto final. Base mensual de cafe en Colombia
#### Une FNC, FRED, Banco de la Republica (TRM) y NOAA en una sola tabla mensual

library(tidyverse)
library(readxl)

crudos <- "datos/crudos"
desde <- as.Date("2000-01-01")
fecha_excel <- function(x) as.Date(as.numeric(x), origin = "1899-12-30")

#### Series de la Federacion Nacional de Cafeteros
leer_fnc <- function(archivo, hoja, col_fecha, col_valor, nombre) {
  read_excel(file.path(crudos, archivo), hoja, col_names = FALSE, .name_repair = "minimal") |>
    suppressMessages() |>
    (\(x) tibble(fecha = fecha_excel(x[[col_fecha]]), valor = as.numeric(x[[col_valor]])))() |>
    suppressWarnings() |>
    filter(!is.na(fecha), !is.na(valor)) |>
    mutate(fecha = floor_date(fecha, "month")) |>
    rename(!!nombre := valor)
}

produccion <- leer_fnc("fnc_precios_produccion.xlsx", "8. Producción mensual", 2, 3, "produccion")
exportaciones <- leer_fnc("fnc_exportaciones.xlsx", "1. Total_Volumen", 3, 4, "exportaciones")
precio_interno <- leer_fnc("fnc_precios_produccion.xlsx", "2. Precio Interno Mensual", 2, 3, "precio_interno")
precio_oic <- leer_fnc("fnc_precios_produccion.xlsx", "6. Precio OIC Mensual", 1, 5, "precio_colombianos")

#### Precio internacional de otros suaves (FRED)
precio_fred <- read_csv(file.path(crudos, "fred_precio_cafe.csv"), show_col_types = FALSE) |>
  transmute(fecha = as.Date(observation_date), precio_otros_suaves = PCOFFOTMUSDM)

#### TRM: promedio mensual de los valores diarios
trm <- read_csv(file.path(crudos, "trm_diaria.csv"), show_col_types = FALSE) |>
  transmute(fecha = floor_date(as.Date(vigenciadesde), "month"), valor) |>
  group_by(fecha) |>
  summarise(trm = mean(valor), .groups = "drop")

#### ONI: cada trimestre movil se asigna a su mes central (DJF es enero)
oni <- read_table(file.path(crudos, "noaa_oni.txt"), show_col_types = FALSE) |>
  group_by(YR) |>
  mutate(mes = row_number()) |>
  ungroup() |>
  transmute(fecha = make_date(YR, mes, 1), oni = ANOM)

#### Base final
base <- produccion |>
  inner_join(exportaciones, by = "fecha") |>
  left_join(precio_interno, by = "fecha") |>
  left_join(precio_oic, by = "fecha") |>
  left_join(precio_fred, by = "fecha") |>
  left_join(trm, by = "fecha") |>
  left_join(oni, by = "fecha") |>
  filter(fecha >= desde) |>
  drop_na() |>
  mutate(mes = month(fecha),
         cosecha_principal = as.integer(mes %in% 10:12),
         mitaca = as.integer(mes %in% 4:6),
         nino = as.integer(oni >= 0.5),
         nina = as.integer(oni <= -0.5)) |>
  select(-mes) |>
  arrange(fecha)

write_csv(base, "datos/base_cafe.csv")
cat("Filas", nrow(base), "desde", format(min(base$fecha)), "hasta", format(max(base$fecha)), "\n")
print(summary(base))
