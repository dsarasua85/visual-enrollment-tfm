library(data.table)

# Se obtienen el fichero de resultados
in_file  <- file.path("inst", "reports", "simulation_results.csv")
out_dir  <- file.path("inst", "reports")
sim <- fread(in_file, sep = ";", encoding = "UTF-8")

# Se verifica que algunos campos tengan el formato requerido
sim[, cfg_id := as.integer(cfg_id)]
sim[, semester_abs := as.integer(semester_abs)]
sim[, has_grades := as.logical(has_grades)]

# Se obtiene una etiqueta de configuración, que será conveniente para interpretar
# los resultados
sim[, cfg_setup := sprintf("Dpop=%s, Ddif=%s, scope=%s", Dpop_variant, Ddif_variant, data_scope)]

# Se obtienen las variables auxiliares para comparar frente a un recomendador aleatorio:
# - n_candidates: tamaño del conjunto de asignaturas recomendables (N) para cada estudiante-semestre,
#   tras aplicar filtros (asignaturas completadas, prerrequisitos y restricciones)
# - k_eff: número efectivo de recomendaciones generadas (<= k), ya que en algunos casos pueden existir
#   menos candidatas disponibles
for (cc in c("n_candidates", "k_eff")) {
  if (!cc %in% names(sim)) sim[, (cc) := NA_integer_]
  sim[, (cc) := as.integer(get(cc))]
}

# Función para simplemente confirmar que la operación puede realizarse por el valor
# del denominador, si no se devuelve NA
safe_div <- function(num, den) {
  # División segura (NA si den == 0 o NA)
  fifelse(!is.na(den) & den > 0, num / den, NA_real_)
}

##################################################################
## Semestres con notas (20241–20242) agregado por configuración ##
##################################################################

# Se tiene en cuenta que tengan notas (20241-20242)
graded <- sim[has_grades == TRUE]

# Se asegura que las columnas numéricas vengan en dicho formato
for (cc in c("n_rec_pass", "n_rec_fail", "n_nrec_pass", "n_nrec_fail")) {
  if (!cc %in% names(graded)) graded[, (cc) := 0L]
  graded[, (cc) := as.integer(fifelse(is.na(get(cc)), 0L, get(cc)))]
}

# Se obtienen determinadas variables en función de los resultados
# n_real: número de asignaturas matriculadas en realidad por el estudiante
graded[, n_real := n_rec_pass + n_rec_fail + n_nrec_pass + n_nrec_fail]

# Indicador de idoneidad: se marca como 1 si el estudiante aprueba todas las asignaturas
# recomendadas que ha cursado (rec_tot > 0, n_rec_fail = 0). Se marca como NA si no
# hubo ninguna asignatura recomendada cursada (rec_tot == 0)
graded[, all_rec_passed := fifelse((n_rec_pass + n_rec_fail) > 0, n_rec_fail == 0L, NA)]

# Se obtiene un resumen por configuración, en primer lugar con la información básica
# Incluyendo estas nuevas métricas:
# rec_tot: número de asignaturas realmente matriculadas incluidas en el top-k recomendado
# nrec_tot: número de asignaturas cursadas pero no recomendadas por el recomendador
# cases_with_hit: porcentaje de casos en los que el recomendador acierta al menos una asignatura
# p_all_rec_passed: probabilidad de que todas las asignaturas recomendadas se aprueben
sum_cfg <- graded[, .(
  rows      = .N,
  k_eff_sum       = sum(k_eff, na.rm=TRUE),
  N_candidates_sum = sum(n_candidates, na.rm=TRUE),
  students  = uniqueN(student_id),
  semesters = uniqueN(semester_abs),
  n_real    = sum(n_real),
  rec_pass  = sum(n_rec_pass),
  rec_fail  = sum(n_rec_fail),
  nrec_pass = sum(n_nrec_pass),
  nrec_fail = sum(n_nrec_fail),
  rec_tot   = sum(n_rec_pass + n_rec_fail),
  nrec_tot  = sum(n_nrec_pass + n_nrec_fail),
  cases_with_hit = sum((n_rec_pass + n_rec_fail) > 0, na.rm = TRUE),
  p_all_rec_passed = mean(all_rec_passed, na.rm = TRUE)
), by = .(cfg_id, Dpop_variant, Ddif_variant, data_scope, cfg_setup)][order(cfg_id)]

# Se añaden más métricas a los resultados que pueden ser interesantes
# hit_rate: proporción de asignaturas matriculadas que aparecen entre las recomendadas
# pass_rate_rec: proporción de asignaturas recomendadas que fueron aprobadas
# pass_rate_nrec: proporción de asignaturas no recomendadas que fueron aprobadas
# delta_pass_rate: diferencia entre tasa de aprobado de asignaturas recomendadas y no recomendadas
# lift_pass_rate: cociente entre tasa de aprobado de asignaturas recomendadas y no recomendadas
# hit_rate_rand: hit_rate para recomendador aleatorio
sum_cfg[, hit_rate       := safe_div(rec_tot, n_real)]
sum_cfg[, pass_rate_rec  := safe_div(rec_pass, rec_tot)]
sum_cfg[, pass_rate_nrec := safe_div(nrec_pass, nrec_tot)]
sum_cfg[, delta_pass_rate := pass_rate_rec - pass_rate_nrec]
sum_cfg[, lift_pass_rate  := safe_div(pass_rate_rec, pass_rate_nrec)]

# Baseline recomendador aleatorio (hit rate esperado): E[hit_rate] = k/N
sum_cfg[, hit_rate_rand := safe_div(k_eff_sum, N_candidates_sum)]
sum_cfg[, delta_hit_rate_vs_rand := hit_rate - hit_rate_rand]

# Baseline esperado en nº de aciertos: E[X] = k * m / N, siendo m = n_real por caso
# Con agregado requiere sumar k*m/N por fila.
# Para hacerlo bien, lo se calcula por fila y se suma:
graded[, exp_hits_rand := fifelse(!is.na(n_candidates) & n_candidates > 0,
                                  k_eff * ( (n_rec_pass + n_rec_fail + n_nrec_pass + n_nrec_fail) / n_candidates ),
                                  NA_real_)]

exp_hits_cfg <- graded[, .(exp_hits_rand_sum = sum(exp_hits_rand, na.rm=TRUE)),
                       by=.(cfg_id, Dpop_variant, Ddif_variant, data_scope, cfg_setup)]

sum_cfg <- merge(sum_cfg, exp_hits_cfg,
                 by=c("cfg_id","Dpop_variant","Ddif_variant","data_scope","cfg_setup"),
                 all.x=TRUE)

sum_cfg[, lift_hits_vs_rand := safe_div(rec_tot, exp_hits_rand_sum)]


# Se exportan los resultados a fichero
out1 <- file.path(out_dir, "simulation_summary_graded_cfg.csv")
fwrite(sum_cfg, out1, sep = ";", dec=",")


#############################################################
## Semestres con notas (20241–20242) separado por semestre ##
#############################################################

# Se obtiene un resumen por configuración, en primer lugar con la información básica
sum_cfg_sem <- graded[, .(
  rows  = .N,
  k_eff_sum  = sum(k_eff, na.rm=TRUE),
  N_candidates_sum = sum(n_candidates, na.rm=TRUE),
  students = uniqueN(student_id),
  n_real   = sum(n_real),
  rec_pass  = sum(n_rec_pass),
  rec_fail  = sum(n_rec_fail),
  nrec_pass = sum(n_nrec_pass),
  nrec_fail = sum(n_nrec_fail),
  rec_tot   = sum(n_rec_pass + n_rec_fail),
  nrec_tot  = sum(n_nrec_pass + n_nrec_fail),
  cases_with_hit = sum((n_rec_pass + n_rec_fail) > 0, na.rm = TRUE),
  p_all_rec_passed = mean(all_rec_passed, na.rm = TRUE)
), by = .(cfg_id, Dpop_variant, Ddif_variant, data_scope, cfg_setup, semester_abs)][
  order(cfg_id, semester_abs)
]

# Se añaden más métricas a los resultados que pueden ser interesantes
sum_cfg_sem[, hit_rate       := safe_div(rec_tot, n_real)]
sum_cfg_sem[, pass_rate_rec  := safe_div(rec_pass, rec_tot)]
sum_cfg_sem[, pass_rate_nrec := safe_div(nrec_pass, nrec_tot)]
sum_cfg_sem[, delta_pass_rate := pass_rate_rec - pass_rate_nrec]
sum_cfg_sem[, lift_pass_rate  := safe_div(pass_rate_rec, pass_rate_nrec)]

# Baseline recomendador aleatorio (hit rate esperado): E[hit_rate] = k/N
sum_cfg_sem[, hit_rate_rand := safe_div(k_eff_sum, N_candidates_sum)]
sum_cfg_sem[, delta_hit_rate_vs_rand := hit_rate - hit_rate_rand]

exp_hits_cfg_sem <- graded[, .(
  exp_hits_rand_sum = sum(exp_hits_rand, na.rm = TRUE)
), by = .(cfg_id, Dpop_variant, Ddif_variant, data_scope, cfg_setup, semester_abs)]

sum_cfg_sem <- merge(
  sum_cfg_sem,
  exp_hits_cfg_sem,
  by = c("cfg_id", "Dpop_variant", "Ddif_variant", "data_scope", "cfg_setup", "semester_abs"),
  all.x = TRUE
)

sum_cfg_sem[, lift_hits_vs_rand := safe_div(rec_tot, exp_hits_rand_sum)]

# Se exportan los resultados a fichero
out2 <- file.path(out_dir, "simulation_summary_graded_cfg_semester.csv")
fwrite(sum_cfg_sem, out2, sep = ";", dec=",")

###########################################################
## Semestre sin notas (20251) agregado por configuración ##
###########################################################

# Se tiene en cuenta que no tengan notas (20251)
nongraded <- sim[has_grades == FALSE]

# Se asegura que las columnas numéricas vengan en dicho formato
for (cc in c("n_rec_chosen", "n_notrec_chosen")) {
  if (!cc %in% names(nongraded)) nongraded[, (cc) := 0L]
  nongraded[, (cc) := as.integer(fifelse(is.na(get(cc)), 0L, get(cc)))]
}

# Se obtienen determinadas variables en función de los resultados
# n_real: número de asignaturas matriculadas en realidad por el estudiante
nongraded[, n_real := n_rec_chosen + n_notrec_chosen]

# Baseline esperado en nº de aciertos para recomendador aleatorio por caso:
# E[X] = k_eff * n_real / n_candidates
nongraded[, exp_hits_rand := fifelse(
  !is.na(n_candidates) & n_candidates > 0,
  k_eff * (n_real / n_candidates),
  NA_real_
)]

# Se obtiene un resumen por configuración, en primer lugar con la información básica
# Variables específicas a evaluar en semestre sin notas:
# rec_chosen: cuántas asignaturas recomendadas han sido matriculadas realmente
# notrec_chosen: cuántas asignaturas no recomendadas han sido matriculadas realmente
sum_20251 <- nongraded[, .(
  rows      = .N,
  k_eff_sum        = sum(k_eff, na.rm = TRUE),
  N_candidates_sum = sum(n_candidates, na.rm = TRUE),
  students  = uniqueN(student_id),
  semesters = uniqueN(semester_abs),
  n_real    = sum(n_real),
  rec_chosen    = sum(n_rec_chosen),
  notrec_chosen = sum(n_notrec_chosen),
  exp_hits_rand_sum = sum(exp_hits_rand, na.rm = TRUE)
), by = .(cfg_id, Dpop_variant, Ddif_variant, data_scope, cfg_setup, semester_abs)][order(cfg_id)]

# Se añaden más métricas a los resultados que pueden ser interesantes
sum_20251[, hit_rate := safe_div(rec_chosen, n_real)]

# Baseline recomendador aleatorio (hit rate esperado): E[hit_rate] = k/N
sum_20251[, hit_rate_rand := safe_div(k_eff_sum, N_candidates_sum)]
sum_20251[, delta_hit_rate_vs_rand := hit_rate - hit_rate_rand]

# Lift en nº de aciertos frente al azar (observado vs esperado)
sum_20251[, lift_hits_vs_rand := safe_div(rec_chosen, exp_hits_rand_sum)]

# Se exportan los resultados a fichero
out3 <- file.path(out_dir, "simulation_summary_20251_cfg.csv")
fwrite(sum_20251, out3, sep = ";", dec=",")


########################################
## Checks para consola y trazabilidad ##
########################################

cat("\n=== RESUMEN GENERADO ===\n")
cat("Entrada: ", in_file, "\n")
cat("Salida 1 (con notas (20241-20242) por config):       ", out1, "\n")
cat("Salida 2 (con notas por config y por semestre):   ", out2, "\n")
cat("Salida 3 (sin notas (20251) por config):        ", out3, "\n")

cat("\nFilas input: ", nrow(sim), "\n")
cat("Filas con notas: ", nrow(graded), " | Filas sin notas: ", nrow(nongraded), "\n")

cat("\nSemestres con notas: ", paste(sort(unique(graded$semester_abs)), collapse=", "), "\n")
cat("Semestres sin notas: ", paste(sort(unique(nongraded$semester_abs)), collapse=", "), "\n")

cat("\nTop hit_rate 20241-20242 con notas (cfg):\n")
print(sum_cfg[order(-hit_rate)][1:3, .(cfg_id, cfg_setup, hit_rate, pass_rate_rec, pass_rate_nrec, lift_pass_rate)])

cat("\nTop hit_rate 20251 sin notas (cfg):\n")
print(sum_20251[order(-hit_rate)][1:3, .(cfg_id, cfg_setup, hit_rate)])

cat("\n========================\n")
