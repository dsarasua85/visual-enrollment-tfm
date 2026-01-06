library(data.table)
library(ggplot2)

# Se definen las rutas a utilizar, suponiendo que se parte de TFM/VisualEnrollment/R
base_dir <- "inst/data_files"
rep_dir  <- "inst/reports"
fig_dir  <- "inst/figures"

for (d in c(rep_dir, fig_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# se define parámetro de número de decimales
ROUND_DIGITS <- 6L

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

# Se lee el fichero de asignaturas subjects_INFORMATICA.csv
subjects_file <- file.path(base_dir, "subjects_INFORMATICA.csv")
subjects <- fread(
  subjects_file,
  sep = ";",
  encoding = "UTF-8",
  colClasses = list(character = "subject_code")
)

# Se fuerza la normalización de los códigos de asignatura a formato XX.YYY
subjects[, subject_code := trimws(subject_code)]
subjects[, subject_code := normalize_code(subject_code)]

# Se crea el universo ordenado de asignaturas
S <- sort(unique(subjects$subject_code))

# Se lee el fichero de actividades activitat.csv
act_file <- file.path(base_dir, "activitat.csv")
activitats <- fread(
  act_file,
  sep = ";",
  encoding = "UTF-8",
  colClasses = list(character = "CODI_ASSIGNATURA",
                    character = "CODI_ASSIGNATURA_DOCENCIA",
                    integer = "SEMESTRE_ABSOLUT")
  #colClasses = list(SEMESTRE_ABSOLUT = "integer")
)

# Se fuerza la normalización de los códigos de asignatura a formato XX.YYY
activitats[, subject_code := normalize_code(CODI_ASSIGNATURA)]

# Se convierten las fechas a formato date, conservando solo la parte de fecha
activitats[, ':='(
    date_start = as.Date(substr(DATA_PUBLICACIO_INI_ACTIVITAT, 1, 10)),
    date_end   = as.Date(substr(DATA_ENTREGA_FIN_ACTIVITAT, 1, 10))
  )
]

# Se hace limpieza de filas sin fechas o con rangos invertidos
activitats <- activitats[!is.na(date_start) & !is.na(date_end) & date_end >= date_start]

# Se filtran solo las actividades evaluables.
activitats <- activitats[AVALUABLE == TRUE]

# Se averiguan qué asignaturas aparecen en el calendario pero no en el catálogo
cod_act <- sort(unique(activitats$subject_code))
cod_no_catalogo <- setdiff(cod_act, S)

if (length(cod_no_catalogo) > 0) {
  warning(
    sprintf(
      "Existen códigos en activitat.csv que no están en subjects_INFORMATICA.csv: %s",
      paste(cod_no_catalogo, collapse = ", ")
    )
  )
}

# Se filtran las asignaturas que están en el catálogo
activitats <- activitats[subject_code %in% S]

# Se definen otras funciones auxiliares necesarias para el código
# Función para calcular los días totales del semestre (D) y span temporal
get_D_and_span <- function(dt) {
  if (nrow(dt) == 0L) {
    return(list(D = 0L, start = NA, end = NA))
  }
  D_start <- min(dt$date_start, na.rm = TRUE)
  D_end   <- max(dt$date_end,   na.rm = TRUE)
  list(
    D     = as.integer(D_end - D_start + 1L),
    start = D_start,
    end   = D_end
  )
}

# Función para expandir actividades a días (un día activo por fila) y así poder
# hacer la comparación día a día entre calendarios de actividades. Se cuenta número
# de actividades distintas por día, ignorando el aula
expand_to_days <- function(dt) {
  # Si no hay actividades, se devuelve tabla vacía coherente
  if (nrow(dt) == 0L){
    return(data.table(
      subject_code  = character(),
      day           = as.Date(character()),
      n_activities  = integer()
    ))
  }
  
  # Se obtienen las filas únicas por asignatura + actividad + rango de fechas
  dt_unique <- unique(dt[, .(subject_code, ACTIVITAT, date_start, date_end)])
  
  # Si dt_unique se queda vacío, se devuelve tabla vacía coherente
  if (nrow(dt_unique) == 0L) {
    return(data.table(
      subject_code = character(),
      day          = as.Date(character()),
      n_activities = integer()
    ))
  }
  
  # Se expande cada actividad a días
  dt_expanded <- dt_unique[
    ,
    .(day = seq(date_start, date_end, by = "day")),
    by = .(subject_code, ACTIVITAT, date_start, date_end)
  ]
  
  # Si no hay días, se devuelve tabla vacía coherente
  if (nrow(dt_expanded) == 0L) {
    return(data.table(
      subject_code = character(),
      day          = as.Date(character()),
      n_activities = integer()
    ))
  }
  
  # Se cuentan cuántas actividades distintas están activas en cada día
  dt_days <- dt_expanded[
    ,
    .(n_activities = .N),
    by = .(subject_code, day)
  ]
  
  dt_days[]
}

# Función para calcular la matriz de solapamiento para un semestre
overlap_matrix <- function(days_active, span, subjects_universe) {
  # Se genera la matriz completa S x S mediante Crossed Join
  grid <- CJ(ASSIG_A = subjects_universe, ASSIG_B = subjects_universe)
  setkey(grid, ASSIG_A, ASSIG_B)
  
  # Si no hay actividades, se devuelve todo 0
  if (nrow(days_active) == 0L || is.na(span$D) || span$D <= 0L) {
    grid[, overlap_days := 0L]
    grid[, overlap_score := 0]
    return(grid[])
  }
  
  # Se define, de forma binaria, si al menos hay una actividad ese día
  days_bin <- unique(
    days_active[
      n_activities > 0,
      .(
        subject_code = as.character(subject_code),
        day          = as.Date(day)
      )
    ]
  )
  
  if (nrow(days_bin) == 0L) {
    grid[, overlap_days  := 0L]
    grid[, overlap_score := 0]
    return(grid[])
  }
  
  # Lista de asignaturas activas por día
  split_by_day <- split(days_bin$subject_code, days_bin$day)
  
  # Se mantiene sólo subject_code y day, una fila por asignatura-día
  #days_active <- unique(
  #  days_active[, .(
  #    subject_code = as.character(subject_code),
  #    day         = as.Date(day)
  #  )]
  #)
  
  # Se hace split por día: lista de asignaturas activas en cada día
  #split_by_day <- split(days_active$subject_code, days_active$day)
  
  # Se generan todas las combinaciones de pares (ASSIG_A, ASSIG_B) con actividad
  pair_list <- vector("list", length(split_by_day))
  idx <- 1L
  
  for (subs in split_by_day) {
    subs <- subs[!is.na(subs)]
    subs <- sort(unique(subs))
    
    # Si hay menos de 2 asignaturas con actividad, no hay pares de asignaturas
    if (length(subs) < 2L)
      next
    
    # Se obtiene la combinación de asignaturas con actividad
    cmb <- t(combn(subs, 2L))
    pair_list[[idx]] <- data.table(
      ASSIG_A = cmb[, 1L],
      ASSIG_B = cmb[, 2L]
    )
    idx <- idx + 1L
  }
  
  # Si no se generó ningún par de asignaturas, se devuelve todo 0
  if (idx == 1L) {
    grid[, overlap_days := 0L]
    grid[, overlap_score := 0]
    return(grid[])
  }
  
  # Elementos de la lista que realmente contienen pares
  pair_list <- pair_list[seq_len(idx - 1L)]
  
  # Tabla con parejas de asignaturas que tuvieron actividades activas el mismo día
  pairs_by_day <- rbindlist(pair_list, use.names = TRUE, fill = TRUE)
  
  # Se cuentan los días de solape por pareja
  co <- pairs_by_day[, .N, by = .(ASSIG_A, ASSIG_B)]
  setnames(co, "N", "overlap_days")
  setkey(co, ASSIG_A, ASSIG_B)
  
  # Se construye matriz completa
  grid <- co[grid]
  
  # Se simetriza: si no hubiera frecuencias en (B,A) y sí en (A,B), (B,A) toma los
  # valores de frecuencia de (A,B) y así se hace simétrica la matriz
  miss <- is.na(grid$overlap_days)
  if (any(miss)) {
    swap <- grid[miss, .(ASSIG_A = ASSIG_B, ASSIG_B = ASSIG_A)]
    setkey(swap, ASSIG_A, ASSIG_B)
    tmp <- co[swap]
    grid$overlap_days[miss] <- tmp$overlap_days
  }
  
  # Se define la diagonal y los NAs como 0
  grid[ASSIG_A == ASSIG_B, overlap_days := 0L]
  grid[is.na(overlap_days), overlap_days := 0L]
  
  # Se calcula el overlap_score y se añade como columna a la matriz
  D <- span$D
  if (is.null(D) || !is.finite(D) || D <= 0) {
    warning("Span$D no es válido, se pone overlap_score = 0")
    grid[, overlap_score := 0]
  } else {
    grid[, overlap_score := pmin(1, overlap_days / D)]
  }
  
  grid[]
}

# Se obtiene el semestre relativo haciendo %10 a Semestre absoluto. No importa
# el año, interesa si es semestre 1 o 2 para calcular las matrices Dso1 y Dso2
activitats[, semestre_rel := SEMESTRE_ABSOLUT %% 10L]

# Se elige un único CODI_ASSIGNATURA original por cada subject_code normalizado y semestre
activitats[
  ,
  assignatura_ref := min(CODI_ASSIGNATURA, na.rm = TRUE),
  by = .(subject_code, SEMESTRE_ABSOLUT)
]

activitats <- activitats[CODI_ASSIGNATURA == assignatura_ref]
activitats[, assignatura_ref := NULL]

# Actividades por semestre
act_sem1 <- activitats[semestre_rel == 1L]
act_sem2 <- activitats[semestre_rel == 2L]

# Span temporal por semestre
span1 <- get_D_and_span(act_sem1)
span2 <- get_D_and_span(act_sem2)

# Se indica por pantalla los datos de cada semestre
cat("Semestre 1: D =", span1$D, "días, desde", as.character(span1$start),
    "hasta", as.character(span1$end), "\n")
cat("Semestre 2: D =", span2$D, "días, desde", as.character(span2$start),
    "hasta", as.character(span2$end), "\n")

# Se expande a días y se calculan las matrices
days1 <- expand_to_days(act_sem1)
days2 <- expand_to_days(act_sem2)
Dso1_grid <- overlap_matrix(days1, span1, S)
Dso2_grid <- overlap_matrix(days2, span2, S)

# Se formatean las matrices al formato adecuado para exportarse
Dso1_out <- data.table(
  subject_code_1 = as.character(Dso1_grid$ASSIG_A),
  subject_code_2 = as.character(Dso1_grid$ASSIG_B),
  overlap_data   = round(as.numeric(Dso1_grid$overlap_score), ROUND_DIGITS)
)

Dso2_out <- data.table(
  subject_code_1 = as.character(Dso2_grid$ASSIG_A),
  subject_code_2 = as.character(Dso2_grid$ASSIG_B),
  overlap_data   = round(as.numeric(Dso2_grid$overlap_score), ROUND_DIGITS)
)

# Se exportan los resultados
fwrite(
  Dso1_out,
  file = file.path(base_dir, "Dso1_INFORMATICA.csv"),
  sep = ";",
  bom = TRUE
)

fwrite(
  Dso2_out,
  file = file.path(base_dir, "Dso2_INFORMATICA.csv"),
  sep = ";",
  bom = TRUE
)

# Resultados por pantalla
cat("Ficheros escritos:\n")
cat(" -", file.path(base_dir, "Dso1_INFORMATICA.csv"), "\n")
cat(" -", file.path(base_dir, "Dso2_INFORMATICA.csv"), "\n")


# Función que construye calendario día x asignatura con n_activities
build_calendar <- function(days_dt, span, subjects_universe) {
  if (nrow(days_dt) == 0L || is.na(span$start) || is.na(span$end)) {
    return(NULL)
  }
  
  # Se genera la secuencia completa de días del semestre
  all_days <- data.table(day = seq(span$start, span$end, by = "day"))
  
  # Se crea la matiz día x asignatura
  cal <- CJ(
    day = all_days$day,
    subject_code = subjects_universe
  )
  
  setkey(cal, subject_code, day)
  setkey(days_dt, subject_code, day)
  
  # Se añade el número de actividades (n_activities) a la matriz, los NA = 0 
  cal <- days_dt[cal]
  cal[is.na(n_activities), n_activities := 0L]
  
  # Se da formato ancho, de una columna por asignatura
  cal_wide <- dcast(
    cal,
    day ~ subject_code,
    value.var = "n_activities"
  )
  
  # Se ordena por día
  cal_wide[order(day)]
}

# Se construyen los calendarios
cal1 <- build_calendar(days1, span1, S)
cal2 <- build_calendar(days2, span2, S)

# Se exportan los calendarios de solapamiento
if (!is.null(cal1)) {
  fwrite(
    cal1,
    file = file.path(base_dir, "solap1_INFORMATICA.csv"),
    sep  = ";",
    bom  = TRUE
  )
}

if (!is.null(cal2)) {
  fwrite(
    cal2,
    file = file.path(base_dir, "solap2_INFORMATICA.csv"),
    sep  = ";",
    bom  = TRUE
  )
}

# Se calculan histogramas para visualización rápida
png(file.path(fig_dir, "overlap_sem1_hist.png"), width = 800, height = 500, res = 120)
ggplot(Dso1_out[subject_code_1 != subject_code_2],
    aes(x = overlap_data)) +
    geom_histogram(bins = 50) +
    labs(
      title = "Solapamiento de actividades (Semestre 1)",
      x = "Fracción de días con solape",
      y = "Número de parejas"
  )
dev.off()

png(file.path(fig_dir, "overlap_sem2_hist.png"), width = 800, height = 500, res = 120)
ggplot(Dso2_out[subject_code_1 != subject_code_2],
    aes(x = overlap_data)) +
    geom_histogram(bins = 50) +
    labs(
      title = "Solapamiento de actividades (Semestre 2)",
      x = "Fracción de días con solape",
      y = "Número de parejas"
  )
dev.off()

cat("Figuras de histogramas generadas en", fig_dir, "\n")
