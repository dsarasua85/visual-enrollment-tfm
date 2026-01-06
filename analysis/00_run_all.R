# Se ejecuta todo desde la raíz del proyecto
library(data.table)
library(ggplot2)

# Se generan los pares de asignaturas
source("analysis/expedients_crossed.R")

# Se generan las matrices
source("analysis/genera_Dpop.R")
source("analysis/genera_Ddif.R")
source("analysis/genera_Dso.R")