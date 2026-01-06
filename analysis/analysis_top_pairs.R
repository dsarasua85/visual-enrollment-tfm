library(data.table)

# Se normalizan los códigos de asignaturas. Formato XX.YYY
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

# Se definen las rutas a utilizar, suponiendo que se parte de TFM/VisualEnrollment/R
base_dir <- "inst/data_files"
rep_dir  <- "inst/reports"
if (!dir.exists(rep_dir)) dir.create(rep_dir, recursive = TRUE, showWarnings = FALSE)
unwgt_file <- file.path(base_dir, "pairs_unweighted.csv")
wgt_file <- file.path(base_dir, "pairs_weighted.csv")
subjects_file <- file.path(base_dir, "subjects_INFORMATICA.csv")

# Se realiza la lectura de los ficheros CSV generados en expedients_crossed.R
unw <- fread(unwgt_file, sep = ";", encoding = "UTF-8",
             colClasses = list(character = c("ASSIG_A","ASSIG_B")))
wgt <- fread(wgt_file, sep = ";", encoding = "UTF-8",
             colClasses = list(character = c("ASSIG_A","ASSIG_B")))

# Aunque se han tratado los campos como caracter, se fuerza la normalización de los
# códigos de asignatura a formato XX.YYY
unw[, ':='(ASSIG_A = normalize_code(ASSIG_A),
           ASSIG_B = normalize_code(ASSIG_B))]
wgt[, ':='(ASSIG_A = normalize_code(ASSIG_A),
           ASSIG_B = normalize_code(ASSIG_B))]

# Se obtienen los rankings para cada caso
unw <- unw[order(-n, ASSIG_A, ASSIG_B)]
wgt <- wgt[order(-w, ASSIG_A, ASSIG_B)]

# Se filtran los primeros 20 casos de cada ranking y se añade posición dentro del ranking
topN <- 20L
top_unw <- unw[1:min(.N, topN)][, rank_unw := .I]
top_wgt <- wgt[1:min(.N, topN)][, rank_wgt := .I]

# Se obtiene la tabla resultante de comparar ambas tablas de ranking
# all=TRUE sirve para mantener todas las filas aunque no aparezca en una de las tablas
cmp <- merge(
  top_unw[, .(ASSIG_A, ASSIG_B, n, rank_unw)],
  top_wgt[, .(ASSIG_A, ASSIG_B, w, rank_wgt)],
  by = c("ASSIG_A","ASSIG_B"), all = TRUE
)

# Se ordena la tabla resultante de la comparación entre las dos tablas de ranking
# miss = columna auxiliar que indica si falta alguno de los dos rangos en cada fila
# rank_unw2, rank_wgt2 = columnas auxiliares para poder ordenar posteriormente,
# asignando un número muy alto cuando el valor sea NA. setorder necesita valor numérico
cmp[, miss := as.integer(is.na(rank_unw) | is.na(rank_wgt))]
cmp[, rank_unw2 := fifelse(is.na(rank_unw), 1e9L, rank_unw)]
cmp[, rank_wgt2 := fifelse(is.na(rank_wgt), 1e9L, rank_wgt)]

# Orden final: filas completas (ambas asignaturas en ambos rankings), filas incompletas
setorder(cmp, miss, rank_unw2, rank_wgt2, ASSIG_A, ASSIG_B)

# Se eliminan columnas auxiliares
cmp[, c("miss","rank_unw2","rank_wgt2") := NULL]

# Se crean las etiquetas de presencia y delta de rango en valor absoluto
# presence = indica en qué tablas de ranking (top20) está el par de asignaturas
# delta_rank = diferencia de posición entre una tabla y otra
cmp[, presence := fifelse(!is.na(rank_unw) & !is.na(rank_wgt), "Ambos",
                          fifelse(!is.na(rank_unw), "Sólo no ponderado", "Sólo ponderado"))]
cmp[, delta_rank := abs(rank_wgt - rank_unw)]


# Se calcula la métrica de solapamiento y el índice Jaccard
# intersect devuelve los elementos que están en ambos conjuntos
# overlap_count devuelve el número de elementos que están en ambos conjuntos
# jaccard devuelve el cálculo de la similitud entre dos conjuntos
overlap_pairs <- intersect(
  paste(top_unw$ASSIG_A, top_unw$ASSIG_B),
  paste(top_wgt$ASSIG_A, top_wgt$ASSIG_B)
)
overlap_count <- length(overlap_pairs)
jaccard <- overlap_count / (length(unique(paste(top_unw$ASSIG_A, top_unw$ASSIG_B))) +
                              length(unique(paste(top_wgt$ASSIG_A, top_wgt$ASSIG_B))) -
                              overlap_count)

message(sprintf("Solapamiento top-20: %d pares; Jaccard=%.3f", overlap_count, jaccard))

# Se añaden los nombres a las asignaturas, usando fichero subjects_INFORMATICA.csv
if (file.exists(subjects_file)) {
  subjects <- fread(subjects_file, sep=";", encoding="UTF-8",
                    colClasses = list(character = "subject_code"))
  subjects[, subject_code := normalize_code(subject_code)]
  setnames(subjects, c("subject_code","name_es"), c("ASSIG_A","name_A"))
  cmp <- subjects[, .(ASSIG_A, name_A)][cmp, on = "ASSIG_A"]
  setnames(subjects, c("ASSIG_A","name_A"), c("ASSIG_B","name_B"))
  cmp <- subjects[, .(ASSIG_B, name_B)][cmp, on = "ASSIG_B"]
}

# Se exportan a CSV el top20 de ambos rankings
fwrite(top_unw, file.path(rep_dir, "top20_pairs_unweighted.csv"), sep=";", bom=TRUE)
fwrite(top_wgt, file.path(rep_dir, "top20_pairs_weighted.csv"),  sep=";", bom=TRUE)

# Se exporta a CSV la tabla final con la tabla resultante de la comparación
# Si no se encuentra nombre de asignatura en fichero de subjects, se asigna valor NA
ROUND_DIGITS <- 3
tablacmp <- cmp[, .(ASSIG_A,
                  name_A = if ("name_A" %in% names(cmp)) name_A else NA_character_,
                  ASSIG_B,
                  name_B = if ("name_B" %in% names(cmp)) name_B else NA_character_,
                  n,
                  w = ifelse(is.na(w), NA_real_, round(w, ROUND_DIGITS)),
                  rank_unw, rank_wgt, delta_rank, presence)]
fwrite(tablacmp, file.path(rep_dir, "Tabla_top20_comparacion.csv"), sep=";", bom=TRUE)

# Se imprime por pantalla un resumen de lo obtenido anteriormente
print(head(tablacmp, 20))
cat("\nSolapamiento top-20:", overlap_count, "parejas",
    "| Índice Jaccard:", round(jaccard, 3), "\n")