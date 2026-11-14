#### Proyecto final. Datos, modelos y chequeo de robustez
#### Efecto de El Nino sobre la produccion y las exportaciones de cafe en Colombia

library(tidyverse)
library(scales)
options(scipen = 999)
if (!interactive()) options(device = function(...) pdf(NULL))

dir.create("output/tablas", showWarnings = FALSE, recursive = TRUE)
rojo <- "#C0392B"
azul <- "#1F77B4"
fuente <- "Fuente: FNC, FRED, Banco de la República y NOAA"

#### Base y variables nuevas
# El Nino se mide con la dummy nino (ONI mayor o igual a 0,5). No se meten nino, nina y oni juntas
# porque nino y nina salen del mismo ONI. La categoria base son los meses sin Nino (neutros y Nina).
# El Nino entra con 12 meses de rezago porque el clima afecta la floracion y la cosecha sale despues.
base <- read_csv("datos/base_cafe.csv", show_col_types = FALSE) |>
  arrange(fecha) |>
  mutate(mes = factor(month(fecha)),
         tendencia = row_number(),
         nino_12 = lag(nino, 12),
         log_precio_6 = lag(log(precio_colombianos), 6),
         precio_6 = lag(precio_colombianos, 6),
         roya = as.integer(fecha >= as.Date("2008-01-01") & fecha <= as.Date("2012-12-01")),
         covid = as.integer(fecha >= as.Date("2020-03-01") & fecha <= as.Date("2020-06-01")),
         paro = as.integer(fecha >= as.Date("2021-04-01") & fecha <= as.Date("2021-06-01")))

modelo_base <- base |> drop_na(nino_12, log_precio_6)
cat("Meses en los modelos:", nrow(modelo_base), "desde", format(min(modelo_base$fecha)), "\n")

#### 1. Tabla descriptiva
descriptiva <- base |>
  select(Produccion = produccion, Exportaciones = exportaciones,
         `Precio interno` = precio_interno, `Precio OIC colombianos` = precio_colombianos,
         TRM = trm, ONI = oni, Nino = nino, Nina = nina) |>
  pivot_longer(everything(), names_to = "Variable") |>
  group_by(Variable = fct_inorder(Variable)) |>
  summarise(Media = mean(value), `Desv. estandar` = sd(value), Minimo = min(value),
            Mediana = median(value), Maximo = max(value), .groups = "drop")
print(descriptiva)
write_csv(descriptiva, "output/tablas/t1_descriptiva.csv")

#### 2. Graficas de la serie con los episodios de Nino y Nina
# Cada mes de Nino se sombrea en rojo y cada mes de Nina en azul
episodios <- base |>
  filter(nino == 1 | nina == 1) |>
  transmute(inicio = fecha, fin = fecha %m+% months(1), fase = if_else(nino == 1, "El Nino", "La Nina"))

grafica_serie <- function(variable, titulo, subtitulo) {
  ggplot(base) +
    geom_rect(data = episodios, aes(xmin = inicio, xmax = fin, ymin = -Inf, ymax = Inf, fill = fase),
              alpha = 0.18, inherit.aes = FALSE) +
    geom_line(aes(fecha, .data[[variable]]), colour = "grey25", linewidth = 0.35) +
    scale_fill_manual(values = c("El Nino" = rojo, "La Nina" = azul), labels = c("El Niño", "La Niña")) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(x = NULL, y = "Miles de sacos de 60 kg", fill = NULL, title = titulo, subtitle = subtitulo,
         caption = "Fuente: FNC y NOAA (ONI mayor o igual a 0,5 es Niño y menor o igual a -0,5 es Niña)") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"))
}

f02 <- grafica_serie("exportaciones", "Exportaciones mensuales de café de Colombia, 2000 a 2026",
                     "El tramo más bajo de la serie, de 2010 a 2012, coincide con La Niña y con la roya")
f03 <- grafica_serie("produccion", "Producción mensual de café de Colombia, 2000 a 2026",
                     "Doce meses después de los episodios de El Niño la producción suele estar por encima de lo normal")
ggsave("output/f02_exportaciones_nino.png", f02, width = 7.5, height = 3.3, dpi = 200, bg = "white")
ggsave("output/f03_produccion_nino.png", f03, width = 7.5, height = 3.3, dpi = 200, bg = "white")

#### 3. Matriz de correlacion de todas las variables
nombres <- c(produccion = "Producción", exportaciones = "Exportaciones", precio_interno = "Precio interno",
             precio_colombianos = "Precio OIC", trm = "TRM", oni = "ONI", nino = "Niño", nina = "Niña",
             cosecha_principal = "Cosecha", mitaca = "Mitaca")
correlaciones <- base |> select(all_of(names(nombres))) |> cor()
write_csv(as_tibble(round(correlaciones, 2), rownames = "variable"), "output/tablas/t2_correlaciones.csv")

f04 <- as_tibble(correlaciones, rownames = "fila") |>
  pivot_longer(-fila, names_to = "columna", values_to = "r") |>
  mutate(fila = factor(nombres[fila], levels = rev(nombres)), columna = factor(nombres[columna], levels = nombres)) |>
  ggplot(aes(columna, fila, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = number(r, accuracy = 0.01, decimal.mark = ",")), size = 2.6) +
  scale_fill_gradient2(low = azul, mid = "white", high = rojo, limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, fill = "r", title = "Correlación entre las variables de la base",
       subtitle = "Las dependientes se mueven juntas y el precio interno repite al precio internacional",
       caption = fuente) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), panel.grid = element_blank(),
        plot.title = element_text(face = "bold"))
ggsave("output/f04_correlaciones.png", f04, width = 6.5, height = 5, dpi = 200, bg = "white")

#### 4. Modelos
# Para cada dependiente se estiman tres modelos sobre el mismo periodo
# (1) logaritmo con solo El Nino, (2) logaritmo con controles, (3) niveles con controles
controles_log <- "log_precio_6 + log(trm) + mes + tendencia + roya + covid + paro"
controles_nivel <- "precio_6 + trm + mes + tendencia + roya + covid + paro"

formulas <- list(
  prod_log_simple = "log(produccion) ~ nino_12",
  prod_log = paste("log(produccion) ~ nino_12 +", controles_log),
  prod_nivel = paste("produccion ~ nino_12 +", controles_nivel),
  expo_log_simple = "log(exportaciones) ~ nino_12",
  expo_log = paste("log(exportaciones) ~ nino_12 +", controles_log),
  expo_nivel = paste("exportaciones ~ nino_12 +", controles_nivel)
)
modelos <- map(formulas, \(f) lm(as.formula(f), data = modelo_base))
walk2(modelos, names(modelos), \(m, n) { cat("\n====", n, "\n"); print(summary(m)) })

# Tabla de resultados: coeficiente con su valor p entre parentesis
etiquetas <- c("(Intercept)" = "Constante", nino_12 = "El Niño hace 12 meses",
               log_precio_6 = "Precio OIC hace 6 meses", precio_6 = "Precio OIC hace 6 meses",
               "log(trm)" = "TRM", trm = "TRM", tendencia = "Tendencia",
               roya = "Roya 2008 a 2012", covid = "Pandemia 2020", paro = "Paro 2021")
celda <- function(b, p) sprintf("%s (%s)", number(b, accuracy = 0.001, big.mark = ".", decimal.mark = ","),
                                 if_else(p < 0.001, "<0,001", number(p, accuracy = 0.001, decimal.mark = ",")))

resultados <- imap(modelos, \(m, n) {
  s <- summary(m)
  coefs <- as_tibble(s$coefficients, rownames = "termino") |>
    filter(!str_detect(termino, "^mes")) |>
    transmute(Variable = etiquetas[termino], valor = celda(Estimate, `Pr(>|t|)`))
  extras <- tibble(Variable = c("Dummies de mes", "R cuadrado", "R cuadrado ajustado", "Observaciones"),
                   valor = c(if_else(str_detect(n, "simple"), "No", "Sí"),
                             number(s$r.squared, accuracy = 0.01, decimal.mark = ","),
                             number(s$adj.r.squared, accuracy = 0.01, decimal.mark = ","),
                             as.character(nobs(m))))
  bind_rows(coefs, extras) |> mutate(modelo = n)
}) |>
  bind_rows() |>
  pivot_wider(names_from = modelo, values_from = valor, values_fill = "")
orden <- c("Constante", "El Niño hace 12 meses", "Precio OIC hace 6 meses", "TRM", "Tendencia",
           "Roya 2008 a 2012", "Pandemia 2020", "Paro 2021", "Dummies de mes", "R cuadrado",
           "R cuadrado ajustado", "Observaciones")
resultados <- resultados |> arrange(match(Variable, orden))
print(resultados, width = 200)
write_csv(resultados, "output/tablas/t3_resultados.csv")

# Efecto de El Nino en porcentaje para los modelos en logaritmo
efecto_nino <- imap_dfr(modelos[str_detect(names(modelos), "log")], \(m, n) {
  b <- coef(m)["nino_12"]
  tibble(modelo = n, coef = b, efecto_pct = 100 * (exp(b) - 1), p = summary(m)$coefficients["nino_12", 4])
})
print(efecto_nino)
write_csv(efecto_nino, "output/tablas/t4_efecto_nino.csv")

#### 5. Chequeo de robustez
# 5.1 Particion temporal. Se entrena hasta diciembre de 2021 y se predice desde enero de 2022
# Las predicciones del modelo en logaritmo se devuelven a miles de sacos con exp()
entrena <- modelo_base |> filter(fecha <= as.Date("2021-12-01"))
prueba <- modelo_base |> filter(fecha >= as.Date("2022-01-01"))

evaluar <- function(nombre, dependiente) {
  m <- lm(as.formula(formulas[[nombre]]), data = entrena)
  pred <- predict(m, newdata = prueba)
  if (str_detect(nombre, "log")) pred <- exp(pred)
  real <- prueba[[dependiente]]
  tibble(modelo = nombre, MAE = mean(abs(real - pred)), RMSE = sqrt(mean((real - pred)^2)),
         shapiro_p = shapiro.test(residuals(modelos[[nombre]]))$p.value)
}
comparacion <- bind_rows(evaluar("prod_nivel", "produccion"), evaluar("prod_log", "produccion"),
                         evaluar("expo_nivel", "exportaciones"), evaluar("expo_log", "exportaciones"))
print(comparacion)
write_csv(comparacion, "output/tablas/t5_niveles_vs_log.csv")

# 5.2 Predichos contra reales en el periodo de prueba (modelos en logaritmo)
predichos <- map_dfr(c(prod_log = "produccion", expo_log = "exportaciones"), \(dep) {
  nombre <- if_else(dep == "produccion", "prod_log", "expo_log")
  m <- lm(as.formula(formulas[[nombre]]), data = entrena)
  tibble(fecha = prueba$fecha, Real = prueba[[dep]], Predicho = exp(predict(m, newdata = prueba)),
         serie = if_else(dep == "produccion", "Producción", "Exportaciones"))
})
f05 <- predichos |>
  pivot_longer(c(Real, Predicho), names_to = "tipo", values_to = "valor") |>
  ggplot(aes(fecha, valor, colour = tipo)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ serie, ncol = 1) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_colour_manual(values = c(Real = "grey30", Predicho = rojo)) +
  labs(x = NULL, y = "Miles de sacos de 60 kg", colour = NULL,
       title = "Predicho contra real fuera de la muestra, enero 2022 a septiembre 2026",
       subtitle = "Modelos en logaritmo con controles entrenados con datos hasta 2021",
       caption = fuente) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))
ggsave("output/f05_predicho_vs_real.png", f05, width = 7.5, height = 4.6, dpi = 200, bg = "white")

# 5.3 Residuos de los modelos en logaritmo contra el valor ajustado
residuos <- map_dfr(c("prod_log", "expo_log"), \(n) {
  tibble(ajustado = fitted(modelos[[n]]), residuo = residuals(modelos[[n]]),
         serie = if_else(n == "prod_log", "Producción", "Exportaciones"))
})
f06 <- ggplot(residuos, aes(ajustado, residuo)) +
  geom_hline(yintercept = 0, colour = rojo) +
  geom_point(colour = "grey40", size = 0.9, alpha = 0.7) +
  facet_wrap(~ serie, scales = "free_x") +
  scale_x_continuous(labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
  scale_y_continuous(labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
  labs(x = "Valor ajustado (logaritmo)", y = "Residuo", title = "Residuos de los modelos en logaritmo",
       subtitle = "Los residuos oscilan alrededor de cero sin abrirse a medida que crece el valor ajustado",
       caption = fuente) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))
ggsave("output/f06_residuos.png", f06, width = 7.5, height = 3.2, dpi = 200, bg = "white")
cat("\nCorrelacion residuo con su valor anterior\n")
print(map_dbl(modelos[c("prod_log", "expo_log")], \(m) cor(head(residuals(m), -1), tail(residuals(m), -1))))

# 5.4 Sensibilidad al rezago de El Nino (modelo en logaritmo con controles)
rezagos <- map_dfr(c(0, 3, 6, 9, 12), \(k) {
  d <- base |> mutate(nino_k = lag(nino, k)) |> drop_na(nino_k, log_precio_6) |>
    filter(fecha >= min(modelo_base$fecha))
  map_dfr(c(Produccion = "produccion", Exportaciones = "exportaciones"), \(dep) {
    m <- lm(as.formula(paste0("log(", dep, ") ~ nino_k + ", controles_log)), data = d)
    s <- summary(m)$coefficients["nino_k", ]
    tibble(rezago = k, dependiente = dep, coef = s[1], efecto_pct = 100 * (exp(s[1]) - 1), p = s[4])
  })
})
print(rezagos)
write_csv(rezagos, "output/tablas/t6_rezagos_nino.csv")

#### 6. Anexos
a1 <- base |>
  mutate(mes = factor(month(fecha), labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))) |>
  select(mes, Producción = produccion, Exportaciones = exportaciones) |>
  pivot_longer(-mes) |>
  ggplot(aes(mes, value)) +
  geom_boxplot(fill = "grey85", outlier.size = 0.6) +
  facet_wrap(~ name, ncol = 1) +
  labs(x = NULL, y = "Miles de sacos de 60 kg", title = "Estacionalidad por mes",
       subtitle = "La producción tiene su pico de octubre a enero y un segundo pico de mayo a julio con la mitaca",
       caption = "Fuente: FNC") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))
ggsave("output/a1_estacionalidad.png", a1, width = 7.5, height = 4.6, dpi = 200, bg = "white")

a2 <- base |>
  select(fecha, `Precio OIC colombianos (centavos de dólar por libra)` = precio_colombianos, `TRM (pesos por dólar)` = trm) |>
  pivot_longer(-fecha) |>
  ggplot(aes(fecha, value)) +
  geom_line(colour = "grey25", linewidth = 0.4) +
  facet_wrap(~ name, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(x = NULL, y = NULL, title = "Precio internacional y tasa de cambio, 2000 a 2026",
       subtitle = "Los dos controles suben en el tiempo y por eso los modelos incluyen una tendencia",
       caption = "Fuente: FNC con datos de la OIC y Banco de la República") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))
ggsave("output/a2_precio_trm.png", a2, width = 7.5, height = 4.6, dpi = 200, bg = "white")

a3 <- map_dfr(c("prod_nivel", "prod_log", "expo_nivel", "expo_log"), \(n) {
  r <- residuals(modelos[[n]])
  tibble(residuo = (r - mean(r)) / sd(r),
         modelo = recode(n, prod_nivel = "Producción en niveles", prod_log = "Producción en logaritmo",
                         expo_nivel = "Exportaciones en niveles", expo_log = "Exportaciones en logaritmo"))
}) |>
  ggplot(aes(sample = residuo)) +
  stat_qq(size = 0.6, colour = "grey40") +
  stat_qq_line(colour = rojo) +
  facet_wrap(~ fct_inorder(modelo), ncol = 2) +
  labs(x = "Cuantil teórico normal", y = "Residuo estandarizado", title = "Normalidad de los residuos",
       subtitle = "Si los puntos siguen la línea roja el residuo se parece a una normal",
       caption = fuente) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))
ggsave("output/a3_normalidad.png", a3, width = 7.5, height = 5, dpi = 200, bg = "white")
