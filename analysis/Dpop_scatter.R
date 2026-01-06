library(data.table)
library(ggplot2)

base_dir <- "inst/data_files"
fig_dir  <- "inst/figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
}

# Se leen los ficheros de pares de asignaturas con conteo (sin ponderar y ponderado)
pairs_unw <- fread(
  file.path(base_dir, "pairs_unweighted.csv"),
  sep = ";", encoding = "UTF-8"
)

pairs_w <- fread(
  file.path(base_dir, "pairs_weighted.csv"),
  sep = ";", encoding = "UTF-8"
)

# Se renombran las columnas de score para distinguirlas
setnames(pairs_unw, "n", "N_unw")
setnames(pairs_w,   "w", "N_w")

# Se unen las columnas por pareja de asignaturas
merged_counts <- merge(
  pairs_unw[, .(ASSIG_A, ASSIG_B, N_unw)],
  pairs_w[,   .(ASSIG_A, ASSIG_B, N_w)],
  by = c("ASSIG_A", "ASSIG_B"),
  all = FALSE
)

# Se quita la diagonal
merged_counts <- merged_counts[ASSIG_A != ASSIG_B]

# Se filtra por el umbral mínimo de co-matriculaciones (>= 10)
merged_counts <- merged_counts[N_unw >= 10 & N_w >= 10]

# Se aplica transformación logarítmica para suavizar el rango usando log10
merged_counts[, logN_unw := log10(N_unw)]
merged_counts[, logN_w   := log10(N_w)]

# Se genera scatter plot en escala logarítmica
p_counts_log <- ggplot(merged_counts, aes(x = logN_unw, y = logN_w)) +
  geom_point(alpha = 0.4, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  coord_equal() +
  labs(
    title = "Co-matriculaciones sin ponderar vs ponderadas (escala log10)",
    x     = "log10(N co-matriculaciones sin ponderar)",
    y     = "log10(N co-matriculaciones ponderadas)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(fig_dir, "Dpop_counts_scatter_log.png"),
  plot     = p_counts_log,
  width    = 8, height = 6, dpi = 120
)