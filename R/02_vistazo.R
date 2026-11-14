#### Proyecto final. Primer vistazo a la base

library(tidyverse)
base <- read_csv("datos/base_cafe.csv", show_col_types = FALSE)

base |> filter(produccion == min(produccion) | exportaciones == min(exportaciones)) |> select(fecha, produccion, exportaciones) |> print()

base |>
  select(produccion, exportaciones, precio_colombianos, precio_otros_suaves, trm, oni) |>
  cor() |> round(2) |> print()

grafica <- base |>
  select(fecha, Produccion = produccion, Exportaciones = exportaciones) |>
  pivot_longer(-fecha, names_to = "serie", values_to = "valor") |>
  ggplot(aes(fecha, valor, colour = serie)) +
  geom_line(linewidth = 0.4) +
  scale_colour_manual(values = c(Produccion = "#6B3E26", Exportaciones = "#2E7D32")) +
  labs(x = NULL, y = "Miles de sacos de 60 kg", colour = NULL,
       title = "Produccion y exportaciones mensuales de cafe en Colombia, 2000 a 2026",
       caption = "Fuente: Federacion Nacional de Cafeteros") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top")
ggsave("output/f01_produccion.png", grafica, width = 7, height = 3.2, dpi = 200, bg = "white")
