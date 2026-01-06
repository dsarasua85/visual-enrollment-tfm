library(data.table)

# Se lee el fichero de expedientes
exp_file <- "inst/data_files/expedient.csv"
exp <- fread(
  exp_file,
  sep = ";",
  encoding = "UTF-8",
  colClasses = list(
    character = c("ESTUDIANT"),
    integer = c("SEMESTRE_ABSOLUT", "SEMESTRE_RELATIU")
  )
)

# Se filtra por semestres de interés
semestres_objetivo <- c(20241L, 20242L, 20251L)
exp_sel <- exp[SEMESTRE_ABSOLUT %in% semestres_objetivo]

# Se obtiene tabla agrupando por semestre absoluto y relativo, y haciendo unique
# por estudiante
tabla <- exp_sel[
  ,
  .(num_estudiantes = uniqueN(ESTUDIANT)),
  by = .(SEMESTRE_ABSOLUT, SEMESTRE_RELATIU)
][order(SEMESTRE_ABSOLUT, SEMESTRE_RELATIU)]

# Se exporta tabla
out_file <- "inst/reports/tabla_semestres_relativos.csv"
fwrite(tabla, out_file, sep = ";", bom = TRUE)

cat("Tabla generada en:", out_file, "\n")
print(tabla)
