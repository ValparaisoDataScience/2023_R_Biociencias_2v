# ----------------------------------------------------------
# Clase 02 - Importar datos a R
# Dr. José Gallardo y Dra. María Angélica Rueda Calderón
# 12 enero 2022
# Curso R para Biociencias.
# ----------------------------------------------------------

# Remover objetos de la sesión de trabajo
rm(list = ls())

# ¿Cómo instalar paquetes?
install.packages("readxl")

# Luego de instalar inhabilite la función de instalar

# ¿Cómo habilitar paquetes?
library(readxl)

# Instale paquetes readr, luego inhabilite


# Habilite paquetes readr

# ¿Cómo importar datos datos a R? (Formatos .txt, .csv, .xlsx)

# Importa base de datos en formato .txt
?read.delim
datos_txt <- read.delim("datos.txt")

# Importa base de datos en formato .csv
help(read_csv)
datos_csv <- read_csv("datos.csv")

# Importa base de datos en formato .xlsx
help(read_excel)
datos_xlsx <- read_excel("datos.xlsx")

# Tarea explore y compare los datos importados con los comandos summary(), str(), class().
# Transforme variables de clasificación a factor.
# Grafique algunas variables cuantitativas usando hist(), boxplot().

