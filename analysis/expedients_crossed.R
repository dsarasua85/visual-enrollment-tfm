library(data.table)
library(stringi)
library(ggplot2)

# Se definen las rutas a utilizar, suponiendo que se parte de TFM/VisualEnrollment/R
base_dir  <- file.path("inst", "data_files")
fig_dir   <- file.path("inst", "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

infile        <- file.path(base_dir, "expedient.csv")
outfile       <- file.path(base_dir, "expedient_pairs.csv")
rank_unw      <- file.path(base_dir, "pairs_unweighted.csv")
rank_w        <- file.path(base_dir, "pairs_weighted.csv")
hist_png_noP  <- file.path(fig_dir,  "pairs_frequency_noPond.png")
hist_png_P      <- file.path(fig_dir,  "pairs_frequency_Pond.png")

# Se normalizan los códigos de asignaturas de la siguiente manera:
# 75.xxx (castellano) -> 05.xxx (catalán)
# 22.500 -> 22.400
# 05.672 -> 05.615
# 05.606, 05.610 -> NA (asignaturas que ya no existen)
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

# Función que detecta suspenso con distintos formatos (numéricos y textuales)
is_fail <- function(qual) {
  # Se convierte la nota a caracter, si no viene en este formato, se pasa todo a
  # MAYUS y se suprimen espacios en blanco al principio o al final
  qual_c <- trimws(toupper(as.character(qual)))
  
  # La calificación NA no computa como suspenso, al ser nota aún no disponible
  is_na_pending <- qual_c %in% c("NA", "")
  
  # En caso de que la calificación venga en formato numérico, se considera
  # suspenso a toda nota por debajo de 5, se acepta separador con coma o punto
  num <- suppressWarnings(as.numeric(gsub(",", ".", qual_c)))
  is_num_fail <- !is.na(num) & num < 5
  
  # Se toman los siguientes códigos textuales que indican suspenso o no superación
  # SU: suspenso, NP: no presentado (con o sin espacios en medio), PQ: no presentado
  fail_codes <- c("SU", "NP", "N\\s*P", "PQ")
  is_txt_fail <- grepl(paste0(fail_codes, collapse = "|"), qual_c)
  
  # Se obtiene el resultado final: suspenso si número < 5 o coincide con nota
  # correspondiente a suspenso, excluyendo NA
  (is_num_fail | is_txt_fail) & !is_na_pending
}

# Se realiza la lectura del fichero CSV origen
dt <- fread(
  infile,
  sep = ";",
  header = TRUE,
  encoding = "UTF-8",
  showProgress = TRUE
)

# Normalización de columnas
setnames(dt, names(dt), stri_trans_general(names(dt), "Any-Latin; Latin-ASCII"))

# Se definen los tipos de datos esperados
dt[, ':='(
  ESTUDIANT        = as.character(ESTUDIANT),
  SEMESTRE_ABSOLUT = as.integer(SEMESTRE_ABSOLUT),
  SEMESTRE_RELATIU = as.integer(SEMESTRE_RELATIU),
  CODI_ASSIGNATURA = as.character(CODI_ASSIGNATURA),
  QUALIFICACIO     = trimws(as.character(QUALIFICACIO))
)]

# Se guarda el valor original del código asignatura para detectar casos atípicos
dt[, CODI_ASSIGNATURA_RAW := trimws(CODI_ASSIGNATURA)]

# Se eliminan los registros cuyos campos clave no vengan informados
dt <- dt[!is.na(CODI_ASSIGNATURA) & CODI_ASSIGNATURA != "" &
           !is.na(SEMESTRE_RELATIU) &
           !is.na(SEMESTRE_ABSOLUT)
]

# Se normalizan los códigos de asignaturas por idioma/equivalencias.
# Se hace comprobación de que todas las asignaturas tienen el formato correcto
# Normaliza a partir del valor original
dt[, CODI_ASSIGNATURA := normalize_code(CODI_ASSIGNATURA_RAW)]

# Se averigua qué cod_asignatura son atípicos para excluirlos
bad <- dt[is.na(CODI_ASSIGNATURA) | !grepl("^\\d{2}\\.\\d{3}$", CODI_ASSIGNATURA),
          .N, by = CODI_ASSIGNATURA_RAW][order(-N)]
print(head(bad, 30))

# Se genera un informe de códigos no curriculares (excluidos)
asig_excluidas <- dt[
  is.na(CODI_ASSIGNATURA) | !grepl("^\\d{2}\\.\\d{3}$", CODI_ASSIGNATURA),
  .N, by = CODI_ASSIGNATURA_RAW
][order(-N)]

cat("\n— Resumen de códigos NO curriculares (se excluyen):\n")
print(asig_excluidas)

# Se guardan en CSV estos casos excluidos para trazabilidad
rep_dir <- file.path("inst", "reports")
if (!dir.exists(rep_dir)) dir.create(rep_dir, recursive = TRUE, showWarnings = FALSE)
fwrite(asig_excluidas, file.path(rep_dir, "asignaturas_excluidas.csv"), sep = ";", bom = TRUE)

# Se sigue adelante ignorando los inválidos las asignaturas excluidas:
dt <- dt[!is.na(CODI_ASSIGNATURA) & grepl("^\\d{2}\\.\\d{3}$", CODI_ASSIGNATURA)]

# Se hace una validación estricta sobre el formato del código asignatura
stopifnot(all(grepl("^\\d{2}\\.\\d{3}$", dt$CODI_ASSIGNATURA)))

# Se crea un dataset mínimo, con los campos necesarios
base <- dt[, .(ESTUDIANT, SEMESTRE_ABSOLUT, SEMESTRE_RELATIU, CODI_ASSIGNATURA, QUALIFICACIO)]
setkey(base, ESTUDIANT, SEMESTRE_ABSOLUT, SEMESTRE_RELATIU)

# Se define el tamaño del semestre: cuántas asignaturas cursa cada estudiante
# en cada semestre relativo = K
k_tbl <- base[, .N, by = .(ESTUDIANT, SEMESTRE_ABSOLUT, SEMESTRE_RELATIU)]
setnames(k_tbl, "N", "K")

# Se hace un self join para formar las parejas
pairs <- base[
  base,
  on = .(ESTUDIANT, SEMESTRE_ABSOLUT, SEMESTRE_RELATIU),
  allow.cartesian = TRUE,
  nomatch = 0L
]

# Se renombran los campos resultantes del self join
setnames(
  pairs,
  old = c("CODI_ASSIGNATURA","QUALIFICACIO","i.CODI_ASSIGNATURA","i.QUALIFICACIO"),
  new = c("ASSIG_A","QUAL_A","ASSIG_B","QUAL_B")
)


# Se añade K (nº asignaturas que cursa un estudiante por semestre) a cada fila
pairs <- pairs[k_tbl, on = .(ESTUDIANT, SEMESTRE_ABSOLUT, SEMESTRE_RELATIU)]

# Se mantienen las siguientes parejas en el fichero resultante:
# A!=B : todos los casos
# A==B : solo si K==1 (una única asignatura en el semestre)
pairs <- pairs[(ASSIG_A != ASSIG_B) | (ASSIG_A == ASSIG_B & K == 1)]

# Se ordenan las parejas como A–B cuando son distintas, se dejan A=A tal cual (el
# caso A=A no entra en el swap)
swap <- pairs$ASSIG_A > pairs$ASSIG_B & pairs$ASSIG_A != pairs$ASSIG_B
pairs[swap, c("ASSIG_A","ASSIG_B","QUAL_A","QUAL_B") := .(ASSIG_B, ASSIG_A, QUAL_B, QUAL_A)]

# Se quitan las duplicidades exactas dentro de cada par estudiante/semestre
setkey(pairs, ESTUDIANT, SEMESTRE_ABSOLUT, SEMESTRE_RELATIU, ASSIG_A, ASSIG_B)
pairs <- unique(pairs, by = key(pairs))

# Se obtienen dos posibles resultados, para hacer comparación:
# - No ponderado: todos los casos tienen el mismo peso: 1.
# - Ponderado: los casos tienen un peso correspondiente al nº de posibles parejas
#   si K==1 -> 1 (caso A-A)
#   si K>=2 -> 1 / choose(K, 2)
#   Siendo choose(K, 2) = número de combinaciones posibles de 2 elementos dentro
#   de un conjunto de tamaño K
pairs[, weight_eq := 1]
pairs[, weight_adj := fifelse(K <= 1, 1, 1 / choose(K, 2))]

# Se calculan los indicadores de aprobado/suspenso por asignatura A y B
pairs[, fail_A := is_fail(QUAL_A)]
pairs[, fail_B := is_fail(QUAL_B)]

# Se seleccionan las columnas que se necesitan en última instancia
pairs_out <- pairs[, list(
  ESTUDIANT,
  SEMESTRE_ABSOLUT,
  SEMESTRE_RELATIU,
  ASSIG_A,
  ASSIG_B,
  QUAL_A,
  QUAL_B,
  K,
  weight_eq,
  weight_adj,
  fail_A,
  fail_B
)]

# Se exporta el resultado al fichero expedient_pairs.csv
fwrite(pairs_out, outfile, sep = ";", bom = TRUE)

# Se calculan los rankings de parejas para los dos casos:
# 1: Sin ponderar
# Se agrupa por parejas y se aplica orden descendente del nº de repeticiones de parejas
# Se crea tabla donde se indica el nº de veces que aparece cada pareja
rank_unweighted <- pairs_out[, .(n = .N), by = .(ASSIG_A, ASSIG_B)][order(-n, ASSIG_A, ASSIG_B)]
fwrite(rank_unweighted, rank_unw, sep = ";", bom = TRUE)

# 2: Ponderado
# Se agrupa por parejas y se aplica orden descendente del peso de las parejas
# Se crea tabla donde se indica el el peso de cada pareja
rank_weighted <- pairs_out[, .(w = round(sum(weight_adj), 6)), by = .(ASSIG_A, ASSIG_B)][order(-w, ASSIG_A, ASSIG_B)]
fwrite(rank_weighted, rank_w, sep = ";", bom = TRUE)

# Se representa gráficamente la distribución del caso 1: sin ponderar
pdat <- rank_unweighted[n > 0L]
g <- ggplot(pdat, aes(x = n)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribución de frecuencias de co-matrícula por pareja (no ponderado)",
    x = "Veces que una pareja aparece (n)",
    y = "Número de parejas"
  )
ggsave(filename = hist_png_noP, plot = g, width = 8, height = 5, dpi = 150)

# Se representa gráficamente la distribución del caso 2: ponderado
pdat_w <- rank_weighted[w > 0]
g2 <- ggplot(pdat_w, aes(x = w)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Distribución de pesos ajustados por pareja (ponderado)",
    x = "Peso acumulado (w)",
    y = "Número de parejas"
  )
ggsave(filename = hist_png_P, plot = g2, width = 8, height = 5, dpi = 150)

# Se imprimen resultados por pantalla
cat("\n============================================\n")
cat("Fichero de parejas generado correctamente\n")
cat("Fichero entrada: ", infile, "\n")
cat("Fichero salida: ", outfile, "\n")
cat("Filas: ", format(nrow(pairs), big.mark = ","), "\n")
cat("Ranking sin peso: ", rank_unw, "\n")
cat("Ranking con peso: ", rank_w, "\n")
cat("Histograma Frecuencias No Ponderado: ", hist_png_noP, "\n")
cat("Histograma Frecuencias Ponderado: ", hist_png_P, "\n")
cat("============================================\n\n")
