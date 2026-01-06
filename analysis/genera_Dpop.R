library(data.table)
library(ggplot2)


# Función para normalizar los códigos de las asignaturas a formato XX.YYY
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

# Se define parámetro de redondeo y mínimo de co-matriculaciones
# MIN_COOC = 10, umbral mínimo de co-matriculaciones
ROUND_DIGITS <- 6
MIN_COOC <- 10L

# Se definen las rutas a utilizar, suponiendo que se parte de TFM/VisualEnrollment/R
data_dir     <- "inst/data_files"
rep_dir      <- "inst/reports"
pairs_file   <- file.path(data_dir, "expedient_pairs.csv")
subjects_file <- file.path(data_dir, "subjects_INFORMATICA.csv")

# Se definen variables para las diferentes configuraciones a ejecutar de la matriz
min_sem_new <- 20201L
scopes   <- c("all", "new")
variants <- c("unweighted", "weighted")

# Se leen los registros de pares de asignaturas, traído del expediente completo
# de los estudiantes. Se hace verificación previa de existencia del fichero
if (!file.exists(pairs_file)) {
  stop("No se encuentra ", pairs_file, 
       ". Asegúrate de haber ejecutado antes expedients_crossed.R.")
}
pairs_all <- fread(pairs_file, sep=";", encoding="UTF-8",
               colClasses = list(character = c("ASSIG_A","ASSIG_B")))

# Leer catálogo si existe
subjects <- NULL
subjects_ord <- NULL

# Para generar posteriormente la matriz final de popularidad, en primer lugar se
# define el listado ordenado de todas las asignaturas. Si hay fichero de asignaturas,
# se usa el orden del catálogo (caso 1); sino el orden de los pares de asignaturas en
# el fichero intermedio "expedient_pairs.csv" (caso 2)

# CASO 1: existe fichero subjects_INFORMATICA.csv y todas las asignaturas están en
# fichero intermedio. Se lee el fichero de asignaturas. Se fuerza subjects a
# caracter para mantener formato del código de la asignatura
if (file.exists(subjects_file)) {
  subjects <- fread(
    subjects_file,
    sep = ";",
    encoding = "UTF-8",
    colClasses = list(character = "subject_code")
  )
  subjects[, subject_code := trimws(subject_code)]
  subjects_ord <- subjects$subject_code
  
  # Se normaliza el código de asignatura de subjects a formato XX.YYY
  if (!all(grepl("^\\d{2}\\.\\d{3}$", subjects_ord))) {
    subjects_ord <- normalize_code(subjects_ord)
    subjects[, subject_code := subjects_ord]
  }
  # CASO 2: se ordena según los pares de asignaturas en el fichero "expedient_pairs.csv"
} else{
  warning("No se encuentra fichero subjects_INFORMATICA.csv. ",
          "Se usará el listado de códigos presentes en expedient_pairs.csv.")
}

# Se recorre bucle doble para hacer el cálculo de la matriz x 4: tomando en cuenta
# las 2 variantes (unweighted, weighted) y los dos scopes (all, new)
for (scope in scopes) {
  for (variant in variants) {
    
    # Se asigna la variante en cuestión
    USE_WEIGHTS <- (variant == "weighted")
    
    # Se asigna nombre de fichero resultante según la configuración
    dpop_file <- file.path(
      data_dir,
      sprintf("Dpop_INFORMATICA_%s_%s.csv", variant, scope)
    )
    
    cat("\n=============================\n")
    cat("Generando Dpop | variant =", variant, "| scope =", scope, "\n")
    cat("=============================\n")
    
    # Se aplica el scope correspondiente
    pairs_scope <- copy(pairs_all)
    if (scope == "new") {
      pairs_scope <- pairs_scope[SEMESTRE_ABSOLUT >= min_sem_new]
    }
    
    # Se realiza el conteo real de co-matrículas
    cooc_count <- pairs_scope[, .(count = .N), by = .(ASSIG_A, ASSIG_B)]
    
    # Conteo/ponderación según variante
    if (USE_WEIGHTS && "weight_adj" %in% names(pairs_scope)) {
      cooc <- pairs_scope[, .(freq = sum(weight_adj, na.rm = TRUE)), by = .(ASSIG_A, ASSIG_B)]
    } else {
      cooc <- pairs_scope[, .(freq = .N), by = .(ASSIG_A, ASSIG_B)]
    }
    
    # Se integra el conteo real de co-matrículas para aplicar el umbral
    setkey(cooc, ASSIG_A, ASSIG_B)
    setkey(cooc_count, ASSIG_A, ASSIG_B)
    cooc <- cooc_count[cooc]
    
    # Se aplica el umbral. Si el conteo es < 10, se considera que no se han cursado juntas
    cooc[count < MIN_COOC | is.na(count), freq := 0]
    
    # Se determina el listado ordenado de asignaturas (en función de si existe o no el
    # fichero subjects_INFORMATICA.csv). Sino se toma el orden del expediente
    if (!is.null(subjects_ord)) {
      # Aviso si hay códigos en cooc que no aparecen en catálogo
      codes_pairs <- sort(unique(c(cooc$ASSIG_A, cooc$ASSIG_B)))
      if (!all(codes_pairs %in% subjects_ord)) {
        warning("Existen códigos en expedient_pairs/cooc que no están en subjects_INFORMATICA.csv")
      }
      subj_list <- subjects_ord
    } else {
      subj_list <- sort(unique(c(cooc$ASSIG_A, cooc$ASSIG_B)))
      if (!all(grepl("^\\d{2}\\.\\d{3}$", subj_list))) {
        subj_list <- normalize_code(subj_list)
      }
    }
    
    # Se crea la cuadrícula completa N×N y se propagan las frecuencias simétricas
    grid <- CJ(ASSIG_A = subj_list, ASSIG_B = subj_list)
    setkey(cooc, ASSIG_A, ASSIG_B)
    setkey(grid, ASSIG_A, ASSIG_B)
    # Se hace left join para añadir 'freq' donde exista
    grid <- cooc[grid]
    
    # Se simetriza: si no hubiera frecuencias en (B,A) y sí en (A,B), (B,A) toma los
    # valores de frecuencia de (A,B) y así se hace simétrica la matriz
    missing <- is.na(grid$freq)
    if (any(missing)) {
      swap <- grid[missing, .(ASSIG_A = ASSIG_B, ASSIG_B = ASSIG_A)]
      setkey(swap, ASSIG_A, ASSIG_B)
      tmp <- cooc[swap]
      grid$freq[missing] <- tmp$freq
    }
    
    # Se pone diagonal a 0 (no cuenta co-ocurrencia A=A). Posteriormente, para este
    # caso particular, la distancia será 0 también
    grid[ASSIG_A == ASSIG_B, freq := 0]
    
    # Se calcula la distancia como la inversa a la frecuencia de co-matriculación
    # Si freq = 0 o NA: distancia = NA (sin información) = 1
    # Si freq > 0: distancia = 1/freq
    dist_raw <- ifelse(grid$freq > 0, 1 / grid$freq, NA_real_)
    
    # Se normaliza dist_raw a [0,1] a partir del valor máximo
    maxv <- max(dist_raw, na.rm = TRUE)
    dist <- dist_raw / maxv
    
    # Si freq == 0: distancia = 1, máxima distancia
    dist[is.na(dist)] <- 1
    
    # Se estipula que la diagonal = 0 si A=B
    dist[grid$ASSIG_A == grid$ASSIG_B] <- 0
    
    # Se ordena la matriz, por consistencia
    setorderv(grid, c("ASSIG_A","ASSIG_B"))
    
    # Se realiza exportación
    out <- data.table(
      subject_code_1    = as.character(grid$ASSIG_A),
      subject_code_2    = as.character(grid$ASSIG_B),
      popularity_score  = round(as.numeric(dist), ROUND_DIGITS)
    )
    
    # Se verifica que hay exactamente S^2 filas
    expected_rows <- length(subj_list)^2
    if (nrow(out) != expected_rows) {
      warning("El número de filas de matriz Dpop (", nrow(out), 
              ") no coincide con S^2 (", expected_rows, ").")
    }
    
    # Se escribe en fichero
    fwrite(out, dpop_file, sep=";", bom = TRUE)
    cat("Escrito Dpop:", dpop_file,
        "| filas:", nrow(out),
        "| asignaturas:", length(subj_list),
        "| ponderado:", USE_WEIGHTS, "\n")
    
    #####################################
    ## Estadísticas matriz popularidad ##
    #####################################
    
    # Se construye una tabla de pares no ordenados (evita duplicar (A,B) y (B,A))
    pairs_sym <- out[, .(subject_code_1, subject_code_2, popularity_score)]
    pairs_sym[, code_min := pmin(subject_code_1, subject_code_2)]
    pairs_sym[, code_max := pmax(subject_code_1, subject_code_2)]
    
    # Se añade el número de co-matriculaciones reales por pareja
    pairs_count <- cooc_count[, .(
      code_min = pmin(ASSIG_A, ASSIG_B),
      code_max = pmax(ASSIG_A, ASSIG_B),
      count
    )]
    pairs_count <- pairs_count[, .(n = sum(count, na.rm = TRUE)), by = .(code_min, code_max)]
    
    # Se eliminan los pares duplicados, calculando la media de la popularidad (que se
    # supone que es la misma)
    pairs_sym <- pairs_sym[, .(
      popularity_score = mean(popularity_score, na.rm = TRUE)
    ), by = .(code_min, code_max)]
    
    # Se excluye también la diagonal A=A
    pairs_sym <- pairs_sym[code_min != code_max]
    
    # Se hace merge con pairs_count para incluir el número de co-matriculaciones
    pairs_sym <- merge(
      pairs_sym,
      pairs_count,
      by = c("code_min", "code_max"),
      all.x = TRUE
    )
    
    # Para opción No Ponderado (USE_WEIGHTS = FALSE)
    if (!USE_WEIGHTS) {
      # Se aplica el mismo umbral que en el cálculo de la matriz
      pairs_sym[n < MIN_COOC | is.na(n), n := 0L]
      
      # Se toma el mínimo n informativo (n > 0) para normalizar
      min_n <- suppressWarnings(
        min(pairs_sym$n[pairs_sym$n > 0], na.rm = TRUE)
      )
      if (!is.finite(min_n)) {
        stop("No hay ninguna pareja con n >= MIN_COOC; revisa el valor de MIN_COOC.")
      }
      
      # Se redefine la popularity_score solamente PARA LA VARIANTE NO PONDERADA:
      # - Si n > 0: Dpop = min_n / n  (más co-matrículas ⇒ menor distancia)
      # - Si n == 0: Dpop = 1 (sin información ⇒ distancia máxima)
      pairs_sym[n > 0, popularity_score := round(min_n / n, ROUND_DIGITS)]
      pairs_sym[n == 0, popularity_score := 1]
    } else {
      #Para opción Ponderado (USE_WEIGHTS = TRUE)
      # Se fuerza a que TODAS las parejas con n < MIN_COOC o NA tengan score = 1
      pairs_sym[n < MIN_COOC | is.na(n), popularity_score := 1]
    }
    
    # Se obtiene el vector de distancias a partir de la columna de popularidad
    v <- pairs_sym$popularity_score
    
    # Se calculan los estadísticos básicos
    min_val  <- min(v, na.rm = TRUE)
    max_val  <- max(v, na.rm = TRUE)
    median_v <- median(v, na.rm = TRUE)
    q        <- quantile(v, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    
    cat("\nEstadísticas Dpop (variant =", variant, ", scope =", scope, ")\n")
    cat("Min:", min_val,
        "| Median:", median_v,
        "| Max:", max_val, "\n")
    cat("Cuartiles 25/50/75:\n")
    print(q)
    
    # Se obtiene el número de parejas totales y con score = 1
    n_pairs_total <- nrow(pairs_sym)
    n_pairs_ones  <- pairs_sym[popularity_score == 1, .N]
    prop_ones     <- n_pairs_ones / n_pairs_total
    
    cat("Número total de parejas:", n_pairs_total, "\n")
    cat("Número de parejas con score de popularidad = 1:", n_pairs_ones, "\n")
    cat("Proporción de parejas con score de popularidad = 1:", prop_ones, "\n")
    
    # Top 20 parejas más populares (distancia más baja)
    top20 <- pairs_sym[order(popularity_score)][1:20]
    
    # Top 20 parejas menos populares entre las que tienen alguna co-matrícula (score < 1)
    pairs_obs     <- pairs_sym[popularity_score < 1]
    bottom20 <- pairs_obs[order(-popularity_score)][1:20]
    
    # Si se tiene catálogo de asignaturas, se añaden los nombres. Se hace un merge
    # por cada código de asignatura con la tabla de subjects
    if (!is.null(subjects)) {
      setkey(subjects, subject_code)
      
      top20_named <- merge(
        merge(
          top20,
          subjects, by.x = "code_min", by.y = "subject_code",
          all.x = TRUE, suffixes = c("", "_min")
        ),
        subjects, by.x = "code_max", by.y = "subject_code",
        all.x = TRUE, suffixes = c("_min", "_max")
      )
      
      bottom20_named <- merge(
        merge(
          bottom20,
          subjects, by.x = "code_min", by.y = "subject_code",
          all.x = TRUE, suffixes = c("", "_min")
        ),
        subjects, by.x = "code_max", by.y = "subject_code",
        all.x = TRUE, suffixes = c("_min", "_max")
      )
      
      # Solo interesan unos pocos campos tras el merge con subjects
      top20_clean <- top20_named[, .(
        code_min, name_es_min,
        code_max, name_es_max,
        n,
        popularity_score
      )]
      
      bottom20_clean <- bottom20_named[, .(
        code_min, name_es_min,
        code_max, name_es_max,
        n,
        popularity_score
      )]
      
      # Exportar auxiliares SIN PISARSE (incluye variant+scope)
      suf_out <- sprintf("_%s_%s", variant, scope)
      
      fwrite(
        pairs_sym,
        file = file.path(rep_dir, paste0("Dpop_pairs", suf_out, ".csv")),
        sep = ";", bom = TRUE
      )
      fwrite(
        top20_clean,
        file = file.path(rep_dir, paste0("Dpop_top20", suf_out, ".csv")),
        sep = ";", bom = TRUE
      )
      fwrite(
        bottom20_clean,
        file = file.path(rep_dir, paste0("Dpop_bottom20", suf_out, ".csv")),
        sep = ";", bom = TRUE
      )
    }
    
  }
}