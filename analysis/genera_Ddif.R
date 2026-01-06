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

# Se define función auxiliar a utilizar en el código
# Función que normaliza los códigos de asignaturas. Formato XX.YYY
normalize_code <- function(x) {
  x <- as.character(x)
  x <- sub("^75\\.", "05.", x)
  x[x == "22.500"] <- "22.400"
  
  # Se detectan pares iguales que aparecen separados (ej: “05.616” vs “5.616”).
  # Se normaliza: %02d.%03d (se admite variantes como "5.6", "05.006", "5.616"...)
  parts <- tstrsplit(x, "\\.")
  left  <- suppressWarnings(as.integer(parts[[1]]))
  right <- suppressWarnings(as.integer(parts[[2]]))
  code  <- sprintf("%02d.%03d", left, right)
  
  code[code == "05.672"] <- "05.615"
  code[code %in% c("05.606", "05.610")] <- NA_character_
  code
}

# Se definen parámetros de ponderación y redondeo
# MIN_COOC = 10, umbral mínimo de co-matriculaciones
MIN_COOC <- 10L
ROUND_DIGITS <- 6L

# Se definen variables para las diferentes configuraciones a ejecutar de la matriz
min_sem_new <- 20201L
scopes      <- c("all", "new")
variants    <- c("any", "both")

# Se leen los registros de pares de asignaturas, traído del expediente completo
# de los estudiantes. Se hace verificación previa de existencia del fichero
if (!file.exists(pairs_file)) {
  stop("No se encuentra ", pairs_file, 
       ". Asegúrate de haber ejecutado antes expedients_crossed.R.")
}
pairs <- fread(pairs_file, sep=";", encoding="UTF-8",
               colClasses = list(character = c("ASSIG_A","ASSIG_B")))

# Se eliminan filas sin código de asignatura
pairs_all <- pairs[!(is.na(ASSIG_A) | ASSIG_A == ""), ]

# Se fuerza la normalización de los códigos de asignatura a formato XX.YYY
pairs_all[, ASSIG_A := normalize_code(ASSIG_A)]
pairs_all[, ASSIG_B := normalize_code(ASSIG_B)]

# Se hace una validación de códigos de calificación conocidos
valid_cod <- c("A","C","CE","EX","M","NO","NP","PQ","SU","NA")
cod_A <- unique(trimws(toupper(pairs_all$QUAL_A)))
cod_B <- unique(trimws(toupper(pairs_all$QUAL_B)))
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

# Leer catálogo si existe
subjects_ord <- NULL
subjects     <- NULL

if (file.exists(subjects_file)) {
  subjects <- fread(
    subjects_file,
    sep = ";",
    encoding = "UTF-8",
    colClasses = list(character = "subject_code")
  )
  subjects[, subject_code := trimws(subject_code)]
  subjects_ord <- subjects$subject_code
  
  if (!all(grepl("^\\d{2}\\.\\d{3}$", subjects_ord))) {
    subjects_ord <- normalize_code(subjects_ord)
    subjects[, subject_code := subjects_ord]
  }
  
  codes_pairs <- sort(unique(c(pairs_all$ASSIG_A, pairs_all$ASSIG_B)))
  if (!all(codes_pairs %in% subjects_ord)) {
    warning("Existen códigos en expedient_pairs que no están en subjects_INFORMATICA.csv")
  }
} else {
  warning("No se encuentra fichero subjects_INFORMATICA.csv. ",
          "Se usa el listado de códigos presentes en expedient_pairs.csv.")
  subjects_ord <- sort(unique(c(pairs_all$ASSIG_A, pairs_all$ASSIG_B)))
  if (!all(grepl("^\\d{2}\\.\\d{3}$", subjects_ord))) {
    subjects_ord <- normalize_code(subjects_ord)
  }
  subjects <- data.table(
    subject_code = subjects_ord,
    name_es      = subjects_ord
  )
}

# Conjunto ordenado de asignaturas
S <- sort(unique(subjects$subject_code))

# Se incluyen los nombres de las asignaturas para los ficheros resultantes de ranking
subjects_names <- subjects[, .(
  subject_code,
  subject_name = name_es
)]

# Se recorre bucle doble para hacer el cálculo de la matriz x 4: tomando en cuenta
# las 2 variantes (any, both) y los dos scopes (all, new)
for (scope in scopes) {
  for (variant in variants) {
    
    cat("\n=============================\n")
    cat("Generando Ddif | variant =", variant, "| scope =", scope, "\n")
    cat("=============================\n")
    
    # Se aplica el scope correspondiente
    pairs_scope <- copy(pairs_all)
    if (scope == "new") {
      pairs_scope <- pairs_scope[SEMESTRE_ABSOLUT >= min_sem_new]
    }
    
    # Se crea un data.table con los datos agregados por pareja de número de veces que
    # se cursan conjuntamente, y el número de veces que se suspende una de ellas, o ambas
    agg <- pairs_scope[
      ,
      .(
        n       = .N,
        any_f   = sum(fail_A | fail_B, na.rm = TRUE),
        both_f  = sum(fail_A & fail_B, na.rm = TRUE)
      ),
      by = .(ASSIG_A, ASSIG_B)
    ]
    
    # Se calcula la dificultad correspondiente a esta iteración
    # dif_any: dificultad en tanto por uno de suspender alguna de las dos
    # dif_both: dificultad en tanto por uno de suspender ambas
    # No se tienen en cuenta los casos de n (número co-matriculacioneS) < 10
    if (variant == "any") {
      agg[, dif := ifelse(n >= MIN_COOC, any_f / n, NA_real_)]
    } else {
      agg[, dif := ifelse(n >= MIN_COOC, both_f / n, NA_real_)]
    }
    
    # Se construye la cuadrícula completa SxS, siendo S el número de asignaturas únicas
    # Se realiza CJ Cross Join para obtener la matriz SxS. grid es la matriz completa,
    # incluyendo pares de asignaturas que nunca se han cursado juntas
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
    cols_to_sym <- c("n", "any_f", "both_f", "dif")
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
      dif = 0
    )]
    
    # De igual forma, para aquellos pares de asignaturas que nunca han aparecido juntas,
    # se fijan a 0 en las matrices
    grid[is.na(dif), dif := 0]
    grid[is.na(n),   n   := 0L]
    grid[is.na(any_f),  any_f := 0L]
    grid[is.na(both_f), both_f := 0L]
    
    # Se exporta matriz final con nombre esperado según la iteración
    ddif_file <- file.path(
      base_dir,
      sprintf("Ddif_INFORMATICA_%s_%s.csv", variant, scope)
    )
    
    if (variant == "any") {
      out <- data.table(
        subject_code_1       = grid$ASSIG_A,
        subject_code_2       = grid$ASSIG_B,
        n_cooc               = grid$n,
        difficulty_score_any = round(grid$dif, ROUND_DIGITS)
      )
    } else {
      out <- data.table(
        subject_code_1        = grid$ASSIG_A,
        subject_code_2        = grid$ASSIG_B,
        n_cooc                = grid$n,
        difficulty_score_both = round(grid$dif, ROUND_DIGITS)
      )
    }
    
    fwrite(out, ddif_file, sep = ";", bom = TRUE)
    
    cat("Escrito:", ddif_file,
        "| filas:", nrow(out),
        "| asignaturas:", length(S),
        "| MIN_COOC:", MIN_COOC, "\n")
    
    # Se crean data.table con los datos de any y both para crear histogramas y
    # calcular estadísticas. Sólo se tienen en cuenta parejas con co-matrícula
    pairs_metric <- grid[ASSIG_A != ASSIG_B & n >= MIN_COOC, .(ASSIG_A, ASSIG_B, n, dif)]
    
    # Se crea histograma para cada iteración
    hist_file <- file.path(fig_dir, sprintf("difficulty_%s_hist_%s.png", variant, scope))
    png(hist_file, width = 800, height = 500, res = 120)
    ggplot(pairs_metric, aes(x = dif)) +
      geom_histogram(bins = 50) +
      labs(
        title = if (variant == "any") {
          sprintf("Distribución de dificultad (suspende A o B) | scope=%s", scope)
        } else {
          sprintf("Distribución de dificultad (suspenden A y B) | scope=%s", scope)
        },
        x = "Proporción de co-matrículas con suspenso",
        y = "Número de parejas"
      )
    dev.off()
    
    # Se calculan estadísticas para cada caso
    stat <- pairs_metric[, .(
      min     = min(dif, na.rm = TRUE),
      q1      = quantile(dif, 0.25, na.rm = TRUE),
      median  = median(dif, na.rm = TRUE),
      mean    = mean(dif, na.rm = TRUE),
      q3      = quantile(dif, 0.75, na.rm = TRUE),
      max     = max(dif, na.rm = TRUE),
      n_pairs = .N
    )]
    
    # Se exportan las estadísticas según scope y variante
    summary_file <- file.path(rep_dir, sprintf("summary_difficulty_INFORMATICA_%s_%s.csv", variant, scope))
    fwrite(
      cbind(metric = sprintf("difficulty_%s", variant), stat),
      summary_file,
      sep = ";", bom = TRUE
    )
    
    cat("\nEstadísticas Ddif (", variant, ", scope=", scope, "):\n", sep = "")
    print(stat)
    
    # Se calculan las 20 parejas más difíciles, incluyendo los nombres de las asignaturas
    worst <- pairs_metric[order(-dif, -n, ASSIG_A, ASSIG_B)][1:20]
    
    # Se hace el siguiente renombrado para que el merge se cruce por "ASIG_A"
    tmp_names <- copy(subjects_names)
    setnames(tmp_names, "subject_code", "ASSIG_A")
    worst <- merge(worst, tmp_names, by = "ASSIG_A", all.x = TRUE)
    setnames(worst, "subject_name", "subject_name_1")
    
    # Se hace el siguiente renombrado para que el merge se cruce por "ASIG_B"
    tmp_names2 <- copy(subjects_names)
    setnames(tmp_names2, "subject_code", "ASSIG_B")
    worst <- merge(worst, tmp_names2, by = "ASSIG_B", all.x = TRUE)
    setnames(worst, "subject_name", "subject_name_2")
    
    # Se exportan los resultados
    top20_file <- file.path(rep_dir, sprintf("top20_difficulty_%s_INFORMATICA_%s.csv", variant, scope))
    fwrite(worst, top20_file, sep = ";", bom = TRUE)
    
  }
}