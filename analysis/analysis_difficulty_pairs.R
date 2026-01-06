library(data.table)
library(ggplot2)

# Se definen las rutas a utilizar, suponiendo que se parte de TFM/VisualEnrollment/R
base_dir <- "inst/data_files"
rep_dir  <- "inst/reports"
fig_dir  <- "inst/figures"

for (d in c(rep_dir, fig_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Ficheros de entrada
pairs_file    <- file.path(base_dir, "expedient_pairs.csv")
subjects_file <- file.path(base_dir, "subjects_INFORMATICA.csv")

# Se define funciones auxiliares a utilizar en el código
# Función que normaliza los códigos de asignaturas. Formato XX.YYY
normalize_code <- function(x) {
  x <- as.character(x)
  parts <- tstrsplit(x, "\\.")
  sprintf("%02d.%03d",
          suppressWarnings(as.integer(parts[[1]])),
          suppressWarnings(as.integer(parts[[2]])))
}

# Se realiza la lectura del fichero CSV generado en expedients_crossed.R
pairs <- fread(
  pairs_file,
  sep      = ";",
  encoding = "UTF-8"
)
pairs <- pairs[!(is.na(ASSIG_A) | ASSIG_A == ""), ]

# Se fuerza la normalización de los códigos de asignatura a formato XX.YYY
pairs[, ASSIG_A := normalize_code(ASSIG_A)]
pairs[, ASSIG_B := normalize_code(ASSIG_B)]

# Se hace una validación de códigos de calificación conocidos
valid_cod <- c("A","C","CE","EX","M","NO","NP","PQ","SU","NA")
cod_A <- unique(trimws(toupper(pairs$QUAL_A)))
cod_B <- unique(trimws(toupper(pairs$QUAL_B)))
cod_total <- sort(unique(c(cod_A, cod_B)))
# Se excluyen cadenas vacías y NAs (casos sin información)
cod_total <- cod_total[cod_total != "" & !is.na(cod_total)]
unknown <- setdiff(cod_total, valid_cod)

if (length(unknown) > 0) {
  warning("Se han detectado códigos de calificación desconocidos: ",
          paste(unknown, collapse=", "),
          ". Se debe revisar la función is_fail()")
} else {
  message("Validación correcta: no hay códigos de calificación desconocidos")
}

# Se crea un data.table con los datos agregados por pareja de número de veces que
# se cursan conjuntamente, y el número de veces que se suspende una de ellas, o ambas
agg <- pairs[
  ,
  .(
    n       = .N,
    any_f   = sum(fail_A | fail_B, na.rm = TRUE),
    both_f  = sum(fail_A & fail_B, na.rm = TRUE)
  ),
  by = .(ASSIG_A, ASSIG_B)
]

# Se calculan las probabilidades de dificultad (proporciones de suspenso):
# dif_any: dificultad en tanto por uno de suspender alguna de las dos
# dif_both: dificultad en tanto por uno de suspender ambas
agg[
  ,
  ':='(
    dif_any  = ifelse(n > 0, any_f  / n, NA_real_),
    dif_both = ifelse(n > 0, both_f / n, NA_real_)
  )
]

# Se lee el fichero de asignaturas subjects_INFORMATICA.csv
subjects <- fread(
  subjects_file,
  sep      = ";",
  encoding = "UTF-8"
)

# Se fuerza la normalización de los códigos de asignatura a formato XX.YYY
subjects[, subject_code := normalize_code(subject_code)]

# Se construye la cuadrícula completa SxS, siendo S el número de asignaturas únicas
# Se realiza CJ Cross Join para obtener la matriz SxS. grid es la matriz completa,
# incluyendo pares de asignaturas que nunca se han cursado juntas
S <- sort(unique(subjects$subject_code))
grid <- CJ(ASSIG_A = S, ASSIG_B = S)

# Se indican las columnas clave de cada tabla/matriz
setkey(agg,   ASSIG_A, ASSIG_B)
setkey(grid,  ASSIG_A, ASSIG_B)

# A la matriz completa grid se añaden los datos de agg para aquellos pares de
# asignaturas que tengan información en agg, es decir, que se hayan cursado juntas.
# Para el resto de casos, se rellena con NA
grid <- agg[grid]

# Se aplica simetría a la matriz: si existe (A,B) pero no (B,A) se copian los valores
# calculados en agg para ese caso inverso; si no es así, se mantienen los NA
cols_to_sym <- c("n", "any_f", "both_f", "dif_any", "dif_both")
for (col in cols_to_sym) {
  miss <- is.na(grid[[col]])
  if (any(miss)) {
    swap <- grid[miss, .(ASSIG_A = ASSIG_B, ASSIG_B = ASSIG_A)]
    setkey(swap, ASSIG_A, ASSIG_B)
    tmp <- agg[swap][[col]]
    grid[[col]][miss] <- tmp
  }
}

# Para el caso de la diagonal, la dificultad no aplica (se fija a 0). No aplica
# porque no se está valorando la dificultad de cursar A con A
grid[ASSIG_A == ASSIG_B, ':='(
  n         = 0L,
  any_f     = 0L,
  both_f    = 0L,
  dif_any  = 0,
  dif_both = 0
)]

# De igual forma, para aquellos pares de asignaturas que nunca han aparecido juntas,
# se fijan a 0 en las matrices
for (col in c("dif_any", "dif_both")) {
  grid[[col]][is.na(grid[[col]])] <- 0
}
grid[is.na(n), n := 0L]
grid[is.na(any_f), any_f := 0L]
grid[is.na(both_f), both_f := 0L]

# Se exportan los resultados para los casos de "any" y "both". Aunque, a nivel
# estadístico, se excluirán los casos de diagonales y los casos de n=0, se exporta
# el valor de n también para posteriores usos de estos ficheros
out_any <- data.table(
  subject_code_1       = grid$ASSIG_A,
  subject_code_2       = grid$ASSIG_B,
  n_cooc               = grid$n,
  difficulty_score_any = grid$dif_any
)

out_both <- data.table(
  subject_code_1        = grid$ASSIG_A,
  subject_code_2        = grid$ASSIG_B,
  n_cooc               = grid$n,
  difficulty_score_both = grid$dif_both
)

fwrite(out_any,  file.path(base_dir, "Ddif_any_INFORMATICA.csv"),
       sep = ";", bom = TRUE)
fwrite(out_both, file.path(base_dir, "Ddif_both_INFORMATICA.csv"),
       sep = ";", bom = TRUE)

# Se crean data.table con los datos de any y both para crear histogramas y
# calcular estadísticas. Sólo se tienen en cuenta parejas con co-matrícula
pairs_any  <- grid[ASSIG_A != ASSIG_B & n > 0, .(
  ASSIG_A, ASSIG_B, n, dif_any
)]
pairs_both <- grid[ASSIG_A != ASSIG_B & n > 0, .(
  ASSIG_A, ASSIG_B, n, dif_both
)]

# Se crean histogramas para cada caso
png(file.path(fig_dir, "difficulty_any_hist.png"),
    width = 800, height = 500, res = 120)
ggplot(pairs_any, aes(x = dif_any)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribución de dificultad (suspende A o B)",
    x     = "Proporción de co-matrículas con al menos un suspenso",
    y     = "Número de parejas"
  )
dev.off()

png(file.path(fig_dir, "difficulty_both_hist.png"),
    width = 800, height = 500, res = 120)
ggplot(pairs_both, aes(x = dif_both)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribución de dificultad (suspenden A y B)",
    x     = "Proporción de co-matrículas con doble suspenso",
    y     = "Número de parejas"
  )
dev.off()

# Se calculan estadísticas para cada caso
stat_any <- pairs_any[
  ,
  .(
    min     = min(dif_any, na.rm = TRUE),
    q1      = quantile(dif_any, 0.25, na.rm = TRUE),
    median  = median(dif_any, na.rm = TRUE),
    mean    = mean(dif_any, na.rm = TRUE),
    q3      = quantile(dif_any, 0.75, na.rm = TRUE),
    max     = max(dif_any, na.rm = TRUE),
    n_pairs = .N
  )
]

stat_both <- pairs_both[
  ,
  .(
    min     = min(dif_both, na.rm = TRUE),
    q1      = quantile(dif_both, 0.25, na.rm = TRUE),
    median  = median(dif_both, na.rm = TRUE),
    mean    = mean(dif_both, na.rm = TRUE),
    q3      = quantile(dif_both, 0.75, na.rm = TRUE),
    max     = max(dif_both, na.rm = TRUE),
    n_pairs = .N
  )
]

# Se añade la columna 'metric' a cada caso indicando qué métrica es la que se
# está teniendo en cuenta en ese fichero. Se junta todo en un único fichero
summary_difficulty <- rbind(
  cbind(metric = "difficulty_any",  stat_any),
  cbind(metric = "difficulty_both", stat_both)
)

# Se exporta fichero
fwrite(
  summary_difficulty,
  file.path(rep_dir, "summary_difficulty_INFORMATICA.csv"),
  sep = ";", bom = TRUE
)

# Se calculan las 20 parejas más difícil, incluyendo los nombres de las asignaturas
subjects_names <- subjects[, .(
  subject_code,
  subject_name = name_es
)]

# Signo negativo para indicar de mayor a menor. Se ordenan por probabilidad, luego
# por número de ocurrencias y, por último, por nombre de asignaturas
worst_any <- pairs_any[
  order(-dif_any, -n, ASSIG_A, ASSIG_B)
][1:20]

worst_both <- pairs_both[
  order(-dif_both, -n, ASSIG_A, ASSIG_B)
][1:20]

# Se hace el siguiente renombrado para que el merge se cruce por "ASIG_A"
setnames(subjects_names, "subject_code", "ASSIG_A")
worst_any  <- merge(worst_any,  subjects_names, by = "ASSIG_A", all.x = TRUE)
worst_both <- merge(worst_both, subjects_names, by = "ASSIG_A", all.x = TRUE)
setnames(worst_any,  "subject_name", "subject_name_1")
setnames(worst_both, "subject_name", "subject_name_1")

# Se hace el siguiente renombrado para que el merge se cruce por "ASIG_B"
setnames(subjects_names, "ASSIG_A", "ASSIG_B")    
worst_any  <- merge(worst_any,  subjects_names, by = "ASSIG_B", all.x = TRUE)
worst_both <- merge(worst_both, subjects_names, by = "ASSIG_B", all.x = TRUE)
setnames(worst_any,  "subject_name", "subject_name_2")
setnames(worst_both, "subject_name", "subject_name_2")

# Se exportan los ficheros
fwrite(
  worst_any,
  file.path(rep_dir, "top20_difficulty_any_INFORMATICA.csv"),
  sep = ";", bom = TRUE
)

fwrite(
  worst_both,
  file.path(rep_dir, "top20_difficulty_both_INFORMATICA.csv"),
  sep = ";", bom = TRUE
)