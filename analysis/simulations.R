library(data.table)
library(MASS)

################################
## BLOQUE 1: LECTURA FICHEROS ##
################################

# Se definen las rutas base a utilizar, suponiendo que se parte de TFM/VisualEnrollment/R
data_dir   <- "inst/data_files"
results_dir <- "inst/reports"

if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
}

# Se definen rutas de los ficheros originales
expedient_file <- file.path(data_dir, "expedient.csv")
activitat_file <- file.path(data_dir, "activitat.csv")
subjects_file  <- file.path(data_dir, "subjects_INFORMATICA.csv")

# Se leen los ficheros de entrada, renombrando las columnas a nombres más homogéneos

## Fichero EXPEDIENT ##
expedient <- fread(expedient_file, sep = ";", encoding = "UTF-8")
setnames(
  expedient,
  old = c("ESTUDIANT",
          "SEMESTRE_ABSOLUT",
          "SEMESTRE_RELATIU",
          "CODI_ASSIGNATURA",
          "ASSIGNATURA",
          "NOTA",
          "QUALIFICACIO"),
  new = c("student_id",
          "semester_abs",
          "semester_rel",
          "subject_code",
          "subject_name",
          "final_grade",
          "final_grade_label")
)

## Fichero ACTIVITAT ##
activitat <- fread(activitat_file, sep = ";", encoding = "UTF-8")
setnames(
  activitat,
  old = c("SEMESTRE_ABSOLUT",
          "CODI_ASSIGNATURA",
          "CODI_ASSIGNATURA_DOCENCIA",
          "ASSIGNATURA_AULA",
          "ACTIVITAT",
          "AVALUABLE",
          "DATA_PUBLICACIO_INI_ACTIVITAT",
          "DATA_ENTREGA_FIN_ACTIVITAT"),
  new = c("semester_abs",
          "subject_code",
          "subject_code_teaching",
          "subject_name_aula",
          "activity_name",
          "assessable",
          "start_date",
          "end_date")
)

## Fichero SUBJECTS ##
subjects <- fread(subjects_file, sep = ";", encoding = "UTF-8")
setnames(
  subjects,
  old = c("absolute_semester", "semester_number"),
  new = c("curriculum_abs_sem", "curriculum_rel_sem")
)

##########################################################
## BLOQUE 2: DEFINICIÓN POBLACIÓN OBJETIVO Y MATRÍCULAS ##
##########################################################

# Se definen los semestres en los que se harán simulaciones
target_semesters <- c(20241, 20242, 20251)

# Se extraen los registros de estos semestres
cases <- unique(
  expedient[semester_abs %in% target_semesters,
            .(student_id, semester_abs, semester_rel)]
)

# se filtra por semestre relativo del 3 al 10
min_sem_rel <- 3
max_sem_rel <- 10
cases <- cases[semester_rel >= min_sem_rel & semester_rel <= max_sem_rel]

# Se marcan qué semestres tienen notas disponibles para posterior análisis
cases[, has_grades := semester_abs %in% c(20241, 20242)]

# Se imprime resumen para comprobar la distribución por semestres
cases_summary <- cases[, .N, by = .(semester_abs, semester_rel, has_grades)][
  order(semester_abs, semester_rel)
]
print(cases_summary)

# Función que detecta suspenso con distintos formatos (numéricos y textuales)
# Misma función que la definida en expedients_crossed.R
is_fail <- function(qual) {
  qual_c <- trimws(toupper(as.character(qual)))
  is_na_pending <- qual_c %in% c("NA", "")
  num <- suppressWarnings(as.numeric(gsub(",", ".", qual_c)))
  is_num_fail <- !is.na(num) & num < 5
  fail_codes <- c("SU", "NP", "N\\s*P", "PQ")
  is_txt_fail <- grepl(paste0(fail_codes, collapse = "|"), qual_c)
  (is_num_fail | is_txt_fail) & !is_na_pending
}

# Función para normalizar los códigos de las asignaturas a formato XX.YYY
normalize_code <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- NA_character_
  x <- sub("^75\\.", "05.", x)
  x[x == "22.500"] <- "22.400"
  
  # Se detectan pares iguales que aparecen separados (ej: “05.616” vs “5.616”).
  # Se normaliza: %02d.%03d (se admite variantes como "5.6", "05.006", "5.616"...)
  parts <- tstrsplit(x, "\\.")
  if (length(parts) < 2) {
    return(rep(NA_character_, length(x)))
  }
  left  <- suppressWarnings(as.integer(parts[[1]]))
  right <- suppressWarnings(as.integer(parts[[2]]))
  code  <- sprintf("%02d.%03d", left, right)
  
  code[code == "05.672"] <- "05.615"
  code[code %in% c("05.606", "05.610")] <- NA_character_
  code
}

# Se define dataset de matriculaciones de estudiantes más limpio, con menos columnas
enrollments <- unique(expedient[, .(student_id, semester_abs, subject_code, final_grade_label)])

# Se marcan las asignaturas convalidadas
enrollments[, convalidated := !is.na(final_grade_label) & trimws(toupper(final_grade_label)) == "C"]

# Se marcan las asignaturas que tienen nota, excluyendo las convalidadas para evaluación
enrollments[, has_grade := !is.na(final_grade_label) & trimws(final_grade_label) != "" & !convalidated]

# Se añade indicador de suspenso, determina si es suspenso sólo para notas evaluables
enrollments[, failed := has_grade & is_fail(final_grade_label)]

# Se añade indicador de aprobado, solo si hay nota evaluable y no es suspenso
enrollments[, passed := has_grade & !failed]

# Se añade indicador de completada (aprobada evaluable O convalidada)
enrollments[, completed := passed | convalidated]

# Se imprime resumen para ver cuántas notas están disponibles
print(enrollments[, .N, by = .(semester_abs, has_grade)][
  order(semester_abs)
])

# Aparecen casos de matrículas de 20251 con nota. Se observa que son casos de
# asignaturas convalidadas. No se tienen en cuenta estos casos
notas_20251 <- expedient[
  semester_abs == 20251 &
    !is.na(final_grade_label) &
    trimws(final_grade_label) != "",
  .N,
  by = final_grade_label
][order(-N)]

print(notas_20251)

#############################################
## BLOQUE 3: CONFIGURACIONES DE SIMULACIÓN ##
#############################################

# Tabla de configuraciones de las simulaciones (8 posibles casos)
# - Dpop_variant: "unweighted" (no ponderado) / "weighted" (ponderado)
# - Ddif_variant: "any" (suspende una) / "both" (suspenden ambas)
# - data_scope:   "all" (todos los datos) / "new" (desde 20201)
# - w_pop, w_dif, w_ovl, w_sem: pesos de las matrices en el recomendador
sim_cfg <- data.table(
  cfg_id       = 1:8,
  Dpop_variant = c("unweighted", "unweighted", "weighted", "weighted",
                   "unweighted", "unweighted", "weighted", "weighted"),
  Ddif_variant = c("any", "any", "any", "any",
                   "both", "both", "both", "both"),
  data_scope   = c("all", "new", "all", "new",
                   "all", "new", "all", "new"),
  w_pop = 1,
  w_dif = 1,
  w_ovl = 1,
  w_sem = 1
)

# Función que devuelve el semestre relativo a partir del semestre absoluto
get_semester_index <- function(semester_abs) {
  idx <- semester_abs %% 10L
  if (!idx %in% c(1L, 2L)) {
    warning("semester_abs con último dígito distinto de 1 o 2: ", semester_abs)
  }
  idx
}

# Función que devuelve la ruta de los ficheros de matrices Dpop, Ddif, Dso a partir
# de una fila de sim_cfg (cfg) y un semestre_abs concreto
get_matrix_paths <- function(cfg_row, semester_abs) {
  sem_idx <- get_semester_index(semester_abs)
  scope   <- cfg_row$data_scope
  
  list(
    Dpop_file = file.path(
      data_dir,
      sprintf("Dpop_INFORMATICA_%s_%s.csv", cfg_row$Dpop_variant, scope)
    ),
    Ddif_file = file.path(
      data_dir,
      sprintf("Ddif_INFORMATICA_%s_%s.csv", cfg_row$Ddif_variant, scope)
    ),
    Dso_file  = file.path(
      data_dir,
      sprintf("Dso%d_INFORMATICA.csv", sem_idx)
    )
  )
}

#################################################
## PRE-BLOQUE 4: DEFINIR CACHE: FICHEROS FIJOS ##
#################################################
# Se define función para hacer la lectura de ficheros una única vez, normalizando
# los códigos de asignatura en cada caso
build_static_cache <- function(data_dir) {
  
  # Se lee el catálogo de asignaturas
  subj <- fread(
    file.path(data_dir, "subjects_INFORMATICA.csv"),
    sep=";", encoding="UTF-8"
  )
  subj[, subject_code := trimws(subject_code)]
  setnames(subj, old="semester_number", new="curriculum_rel_sem", skip_absent=TRUE)
  subj[, subject_code := normalize_code(subject_code)]
  S <- sort(unique(subj$subject_code[!is.na(subj$subject_code)]))
  
  # Se obtiene también una matriz con el número de créditos por asignatura
  subj[, credits_num := suppressWarnings(as.numeric(credits))]
  credits_map <- subj[!is.na(subject_code), .(subject_code, credits_num)]
  
  # Se leen los prerrequisitos
  prereq <- fread(
    file.path(data_dir, "prerequisites_INFORMATICA.csv"),
    sep=";", encoding="UTF-8",
    colClasses = list(character = c("prerequisite","subject"))
  )
  setnames(prereq, c("prerequisite","subject"), c("prerequisite_code","subject_code"))
  prereq[, ':='(
    subject_code      = normalize_code(subject_code),
    prerequisite_code = normalize_code(prerequisite_code)
  )]
  
  # Se leen las restricciones
  restr <- fread(
    file.path(data_dir, "restrictions_INFORMATICA.csv"),
    sep=";", encoding="UTF-8",
    colClasses = list(character = c("restricted_subject_code","antirequisites"),
                      numeric   = "min_ects")
  )
  restr[, restricted_subject_code := normalize_code(restricted_subject_code)]
  
  # Se leen las distancias absolutas de las asignaturas en el plan de estudios
  dabs <- fread(file.path(data_dir, "Dabs_INFORMATICA.csv"), sep=";", encoding="UTF-8")
  setnames(dabs, c("ass1","ass2","abs"), c("subject_code_1","subject_code_2","semester_distance"))
  dabs[, subject_code_1 := normalize_code(trimws(subject_code_1))]
  dabs[, subject_code_2 := normalize_code(trimws(subject_code_2))]
  
  # Se guardan en una lista todos los ficheros leídos
  list(
    subjects     = subj,
    S            = S,
    prereq       = prereq,
    restr        = restr,
    dabs_long    = dabs,
    credits_map  = credits_map
  )
}

static_cache <- build_static_cache(data_dir)


#######################################
## BLOQUE 4: LLAMADA AL RECOMENDADOR ##
#######################################
# Función que lee las matrices en formato largo y las deja preparadas para trabajar
read_matrix_long <- function(path, value_col, value_alias = NULL) {
  dt <- fread(path, sep = ";", encoding = "UTF-8",
              colClasses = list(character = c("subject_code_1","subject_code_2")))
  dt[, subject_code_1 := trimws(subject_code_1)]
  dt[, subject_code_2 := trimws(subject_code_2)]
  
  # La lectura de Ddif es particular porque la columna difficulty_score viene con
  # diferentes nombres según el caso
  if (!(value_col %in% names(dt))) {
    if (!is.null(value_alias)) {
      cand <- intersect(value_alias, names(dt))
      if (length(cand) > 0) {
        setnames(dt, cand[1], value_col)
      }
    }
  }
  
  # Se hace una verificación final
  stopifnot(all(c("subject_code_1","subject_code_2", value_col) %in% names(dt)))
  dt
}

# Función que convierte una matriz en formato largo a matriz cuadrada SxS
long_to_matrix <- function(dt, subjects, value_col) {
  # Estructura dt: subject_code_1, subject_code_2, value_col
  # subjects: vector ordenado de códigos de asignatura (caracter)
  m <- matrix(NA_real_, nrow = length(subjects), ncol = length(subjects),
              dimnames = list(subjects, subjects))
  idx1 <- match(dt$subject_code_1, subjects)
  idx2 <- match(dt$subject_code_2, subjects)
  ok <- !is.na(idx1) & !is.na(idx2)
  m[cbind(idx1[ok], idx2[ok])] <- dt[[value_col]][ok]
  m[cbind(idx2[ok], idx1[ok])] <- dt[[value_col]][ok]
  diag(m) <- NA_real_
  
  m
}

# Función que normaliza un vector o matriz a formato [0,1]
minmax01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[2] <= rng[1]) return(x*0)
  (x - rng[1]) / (rng[2] - rng[1])
}

# Función que convierte matriz de distancia combinada en distancias 2D con Sammon
sammon_distance_matrix <- function(M, subjects, seed = 1L) {
  # Se hace validación de tamaño de la matriz M
  stopifnot(is.matrix(M), length(subjects) == nrow(M), nrow(M) == ncol(M))
  
  # Se verifica simetría y diagonal 0
  M <- (M + t(M)) / 2
  diag(M) <- 0
  
  # Se reemplazan valores NA/Inf por el máximo finito (sammon no admite NA)
  finite_vals <- M[is.finite(M)]
  max_finite  <- if (length(finite_vals) > 0) max(finite_vals) else 0
  M[!is.finite(M)] <- max_finite
  
  # Se normaliza globalmente a [0,1]
  min_dist <- min(M, na.rm = TRUE)
  max_dist <- max(M, na.rm = TRUE)
  if (is.finite(min_dist) && is.finite(max_dist) && max_dist > min_dist) {
    M <- (M - min_dist) / (max_dist - min_dist)
  } else {
    M[,] <- 0
  }
  
  # Se aplican reglas de ajuste para mejor visualización en sammon
  set.seed(seed)
  M <- apply(M, c(1,2), function(x) {
    if (!is.finite(x)) return(1)
    if (x == 0) return(runif(1, 0.5, 0.6))
    if (x < 0.5) return(0.5)
    x
  })
  diag(M) <- 0
  M <- M + 1
  
  # Se calculan las coordenadas 2D que devuelve Sammon
  coords <- MASS::sammon(as.dist(M), trace = FALSE)$points
  rownames(coords) <- subjects
  
  # Se calculan las distancias euclídeas en 2D, matriz final de distancias
  D2 <- as.matrix(dist(coords))
  diag(D2) <- 0
  D2
}


# Función que hace la simulación llamando al recomendador a partir de los
# parámetros de entrada
get_recommendations_blackbox <- function(student_id,
                                         semester_abs,
                                         Dpop_file,
                                         Ddif_file,
                                         Dso_file,
                                         k = 6,
                                         static_cache,
                                         w_pop,
                                         w_dif,
                                         w_ovl,
                                         w_sem) {
  
  sid <- student_id
  sem <- semester_abs
  data_dir <- "inst/data_files"
  
  # Para los casos en que no haya nada que recomendar, se define una salida de la simulación
  empty_rec <- list(rec_subjs = character(), n_candidates = 0L, k_eff = 0L)
  
  # Se define parámetro de reajuste para favorecer que se recursen asignaturas ya suspendidas
  failed_distance_adjustment <- 2
  
  # Se leen todos los ficheros a partir de la cache
  subjects <- static_cache$subjects
  S        <- static_cache$S
  prereq   <- static_cache$prereq
  restr    <- static_cache$restr
  dabs_long <- static_cache$dabs_long
  
  # Se construyen las matrices a partir de los ficheros pasados por parámetro
  # El fichero Dabs es fijo, no depende de ninguna parametrización
  pop_long <- read_matrix_long(Dpop_file, "popularity_score")
  ovl_long <- read_matrix_long(Dso_file,  "overlap_data")
  
  # La lectura de Ddif es particular porque la columna difficulty_score viene con
  # diferentes nombres según el caso
  dif_long <- read_matrix_long(
    Ddif_file,
    value_col   = "difficulty_score",
    value_alias = c("difficulty_score_any", "difficulty_score_both")
  )
  
  # Se normalizan también los códigos de asignatura
  pop_long[, ':='(
    subject_code_1 = normalize_code(subject_code_1),
    subject_code_2 = normalize_code(subject_code_2)
  )]
  dif_long[, ':='(
    subject_code_1 = normalize_code(subject_code_1),
    subject_code_2 = normalize_code(subject_code_2)
  )]
  ovl_long[, ':='(
    subject_code_1 = normalize_code(subject_code_1),
    subject_code_2 = normalize_code(subject_code_2)
  )]
  
  M_pop <- long_to_matrix(pop_long, S, "popularity_score")
  M_dif <- long_to_matrix(dif_long, S, "difficulty_score")
  M_ovl <- long_to_matrix(ovl_long, S, "overlap_data")
  M_sem <- long_to_matrix(dabs_long, S, "semester_distance")
  
  # Se normaliza cada componente a [0,1]
  M_pop <- minmax01(M_pop)
  M_dif <- minmax01(M_dif)
  M_ovl <- minmax01(M_ovl)
  M_sem <- minmax01(M_sem)
  
  # Se calcula la matriz combinada (distancia)
  w_sum <- (w_pop + w_dif + w_ovl + w_sem)
  M <- (w_pop*M_pop + w_dif*M_dif + w_ovl*M_ovl + w_sem*M_sem) / w_sum
  
  # Se calcula la distancia con sammon
  D2 <- sammon_distance_matrix(M, subjects = S, seed = 1L)
  
  # Se obtiene el estado del expediente del estudiante a partir de su expediente
  stu_enr <- enrollments[student_id == sid & semester_abs < sem]
  
  # Asignaturas completadas antes de este semestre
  completed_codes <- normalize_code(unique(stu_enr[completed == TRUE, subject_code]))
  completed_codes <- completed_codes[!is.na(completed_codes)]
  
  # Asignaturas suspendidas antes de este semestre
  failed_codes <- normalize_code(
    unique(stu_enr[failed == TRUE, subject_code])
  )
  failed_codes <- failed_codes[!is.na(failed_codes)]
  
  # Se define el pool de asignaturas matriculables este semestre
  offered_codes <- subjects$subject_code
  offered_codes <- offered_codes[!is.na(offered_codes)]
  
  # Se descartan las asignaturas ya superadas y que pertenecen al catálogo de Subjects
  candidates <- setdiff(offered_codes, completed_codes)
  candidates <- intersect(candidates, S)
  
  # Si, tras el filtrado, no quedan asignaturas, no se recomienda ninguna
  if (length(candidates) == 0) return(empty_rec)
  
  # De cara a posteriores posibles restricciones en función del número de créditos ECTS
  # completados, se calcula cuántos créditos ya tiene completados el estudiante
  completed_ects <- 0
  if (length(completed_codes) > 0) {
    completed_ects <- subjects[subject_code %in% completed_codes, sum(as.numeric(credits), na.rm=TRUE)]
    if (!is.finite(completed_ects)) completed_ects <- 0
  }
  
  # Función que filtra por restricciones de cara a recomendar una asignatura
  meets_restrictions <- function(code) {
    # Se obtienen las posibles restricciones de esta asignatura
    rules <- restr[restricted_subject_code == code]
    if (nrow(rules) == 0) return(TRUE)
    
    # Se filtra por mínimo de créditos ECTS superados. Si la asignatura exige un mínimo de
    # créditos que el estudiante no tiene, se rechaza
    if (!is.na(rules$min_ects[1]) && completed_ects < rules$min_ects[1]) return(FALSE)
    
    # Si una asignatura aparece como antirequisito (no cursarse en caso de haberse aprobado
    # otra), se rechaza dicha asignatura
    anti <- rules$antirequisites[1]
    if (!is.na(anti) && trimws(anti) != "") {
      blocks <- trimws(unlist(strsplit(anti, ",")))
      blocks <- blocks[blocks != ""]
      if (length(intersect(blocks, completed_codes)) > 0) return(FALSE)
    }
    TRUE
  }
  
  # Función que filtra por prerrequisitos de cara a recomendar una asignatura
  meets_prereqs <- function(code) {
    # Se obtienen los posible prerrequisitos de esta asignatura
    reqs <- prereq[subject_code == code, prerequisite_code]
    
    # Si no tiene prerrequisitos, se da por válida
    if (length(reqs) == 0) return(TRUE)
    
    # Si todas las asignaturas que están como prerrequisitos están también en la lista
    # de asignaturas aprobadas, se da por válida; si falta alguna por aprobar, se rechaza
    all(reqs %in% completed_codes)
  }
  
  # Se aplican las funciones de cumplimiento de prerrequisitos y restricciones. De esta
  # forma, se descartan todas las asignaturas que no cumplan los criterios
  candidates <- candidates[vapply(candidates, meets_prereqs, logical(1))]
  candidates <- candidates[vapply(candidates, meets_restrictions, logical(1))]
  
  # Si, tras el filtrado de prerrequisitos y restricciones, no quedan asignaturas, no
  # se recomienda ninguna
  if (length(candidates) == 0) return(empty_rec)
  
  # Se calcula el scoring por distancia media a completadas.
  # Si no hay completadas, invertimos criterio para no dejar todo igual
  completed <- completed_codes
  candidates_idx <- match(candidates, S)
  completed_idx <- match(completed, S)
  completed_idx <- completed_idx[!is.na(completed_idx)]
  
  if (length(completed_idx) > 0) {
    # Se calcula la distancia media a las ya completadas
    d_mean <- rowMeans(D2[candidates_idx, completed_idx, drop=FALSE], na.rm=TRUE)
    
    # Se favorecen las asignaturas ya suspendidas para volver a cursarlas
    # Se indica valor de reajuste = 2, reduce el valor a la mitad para favorecer
    is_failed <- candidates %in% failed_codes
    d_mean[is_failed] <- d_mean[is_failed] / failed_distance_adjustment
    
    # Al final del ranking las que no tengan información
    d_mean[!is.finite(d_mean)] <- Inf
    
    # Se ordenan para obtener las que están más cerca en primer lugar
    ord <- order(d_mean, candidates)
  } else {
    # Si no hay asignaturas superadas, se priorizan las "centrales", las que tienen
    # una distancia media al resto. Esto no debería ocurrir porque no se están valorando
    # los primeros semestres relativos, siempre debería tener asignaturas superadas
    d_mean <- rowMeans(D2[candidates_idx, , drop=FALSE], na.rm=TRUE)
    d_mean[!is.finite(d_mean)] <- Inf
    ord <- order(d_mean, candidates)
  }
  
  # Se devuelven las k asignaturas recomendadas junto con información auxiliar
  # necesaria para la evaluación: tamaño del conjunto de candidatas (N) y
  # número efectivo de recomendaciones generadas (k_eff)
  rec <- head(candidates[ord], k)
  return(list(
    rec_subjs    = rec,
    n_candidates = length(candidates),
    k_eff        = length(rec)
  ))
}


###################################
## BLOQUE 5: BUCLE DE SIMULACIÓN ##
###################################

# Se genera lista de resultados
results <- list()

n_cfg   <- nrow(sim_cfg)
n_cases <- nrow(cases)

# Pequeña prueba
#cases_run <- cases[1:3]

cat("Configuraciones:", n_cfg, "\n")
cat("Casos (estudiante-semestre):", n_cases, "\n")
cat("Simulaciones máximas teóricas:", n_cfg * n_cases, "\n")

# Se recorre la lista de configuraciones de las diferentes simulaciones
for (i_cfg in seq_len(nrow(sim_cfg))) {
  cfg <- sim_cfg[i_cfg]
  cat("Ejecutando configuración", cfg$cfg_id,
      "(Dpop =", cfg$Dpop_variant,
      ", Ddif =", cfg$Ddif_variant,
      ", datos =", cfg$data_scope, ")...\n")
  
  # Se recorre el expediente objeto de estudio definido, es decir, por cada
  # estudiante, semestre absoluto y semestre relativo
  for (i_case in seq_len(nrow(cases))) {
  #for (i_case in seq_len(nrow(cases_run))) {
    this_case <- cases[i_case]
    #this_case <- cases_run[i_case]
    sid       <- this_case$student_id
    sem_abs   <- this_case$semester_abs
    hasg      <- this_case$has_grades
    
    # Se obtienen las rutas de las matrices según configuración y semestre
    paths <- get_matrix_paths(cfg, sem_abs)
    
    # Se obtiene la recomendación desde la caja negra
    # k=6 número de asignaturas que recomienda el recomendador
    rec_obj <- tryCatch(
      get_recommendations_blackbox(
        student_id   = sid,
        semester_abs = sem_abs,
        Dpop_file    = paths$Dpop_file,
        Ddif_file    = paths$Ddif_file,
        Dso_file     = paths$Dso_file,
        k            = 6,
        static_cache = static_cache,
        w_pop        = cfg$w_pop,
        w_dif        = cfg$w_dif,
        w_ovl        = cfg$w_ovl,
        w_sem        = cfg$w_sem
      ),
      error = function(e) {
        # Si la caja negra aún no está implementada, se para la ejecución
        stop("Error al obtener recomendaciones en la configuración ",
             cfg$cfg_id, " para estudiante ", sid, " y semestre ", sem_abs,
             ":\n", conditionMessage(e))
      }
    )
    
    rec_subjs    <- rec_obj$rec_subjs
    n_candidates <- as.integer(rec_obj$n_candidates)
    k_eff        <- as.integer(rec_obj$k_eff)
    
    # Si no devuelve nada, se salta el caso
    if (k_eff == 0L) next

    # Se definen las asignaturas realmente matriculadas por el estudiante en ese semestre
    real_subjs <- enrollments[student_id == sid & semester_abs == sem_abs]
    real_subjs[, subject_code_norm := normalize_code(subject_code)]
    real_subjs <- real_subjs[!is.na(subject_code_norm)]
    
    # Se excluyen las asignaturas convalidadas de la evaluación del semestre
    real_subjs_eval <- real_subjs[convalidated == FALSE]
    
    # Si no hay matrícula, se salta
    if (nrow(real_subjs_eval) == 0L) next
    
    # Se marcan cuáles de las realmente matriculadas estaban recomendadas
    real_subjs_eval[, recommended := subject_code_norm %in% rec_subjs]
    
    # PRIMER CASO: semestres con notas (20241, 20242)
    if (hasg) {
      # Se recorren las asignaturas realmente matriculadas y se observa si fueron
      # recomendas y si se aprobaron o no
      n_rec_pass    <- real_subjs_eval[recommended == TRUE  & passed == TRUE,  .N]
      n_rec_fail    <- real_subjs_eval[recommended == TRUE  & passed == FALSE, .N]
      n_nrec_pass   <- real_subjs_eval[recommended == FALSE & passed == TRUE,  .N]
      n_nrec_fail   <- real_subjs_eval[recommended == FALSE & passed == FALSE, .N]
      n_real <- nrow(real_subjs_eval)
      n_rec_tot  <- n_rec_pass + n_rec_fail
      
      # Se guardan los resultados de cada caso
      results[[length(results) + 1L]] <- data.table(
        student_id    = sid,
        semester_abs  = sem_abs,
        cfg_id        = cfg$cfg_id,
        Dpop_variant  = cfg$Dpop_variant,
        Ddif_variant  = cfg$Ddif_variant,
        data_scope    = cfg$data_scope,
        has_grades    = TRUE,
        n_rec_pass    = n_rec_pass,
        n_rec_fail    = n_rec_fail,
        n_nrec_pass   = n_nrec_pass,
        n_nrec_fail   = n_nrec_fail,
        hit_rate      = ifelse(n_real > 0, n_rec_tot / n_real, NA_real_),
        pass_rate_rec = ifelse(n_rec_tot > 0, n_rec_pass / n_rec_tot, NA_real_),
        n_candidates  = n_candidates,
        k_eff         = k_eff
      )
      # SEGUNDO CASO: semestres sin notas (20251)
    } else {
      # Aquí NO se usa passed/failed. Sólo se mira la elección
      n_rec_chosen    <- real_subjs_eval[recommended == TRUE,  .N]
      n_notrec_chosen <- real_subjs_eval[recommended == FALSE, .N]
      
      # Se guardan los resultados de cada caso
      results[[length(results) + 1L]] <- data.table(
        student_id      = sid,
        semester_abs    = sem_abs,
        cfg_id          = cfg$cfg_id,
        Dpop_variant    = cfg$Dpop_variant,
        Ddif_variant    = cfg$Ddif_variant,
        data_scope      = cfg$data_scope,
        has_grades      = FALSE,
        n_rec_chosen    = n_rec_chosen,
        n_notrec_chosen = n_notrec_chosen,
        n_candidates  = n_candidates,
        k_eff         = k_eff
      )
    }
  }
}

# Se unen todo en una sola tabla
sim_results <- rbindlist(results, use.names = TRUE, fill = TRUE)

cat("Filas en results:", length(results), "\n")
print(head(sim_results, 10))
print(sim_results[, .N, by = .(cfg_id, semester_abs, has_grades)][order(cfg_id, semester_abs)])

# Se guardan los resultados en fichero
out_file <- file.path(results_dir, "simulation_results.csv")
fwrite(sim_results, out_file, sep = ";")

cat("\n============================================\n")
cat("Simulaciones finalizadas.\n")
cat("Resultados guardados en: ", out_file, "\n")
cat("Filas generadas: ", nrow(sim_results), "\n")
cat("============================================\n\n")