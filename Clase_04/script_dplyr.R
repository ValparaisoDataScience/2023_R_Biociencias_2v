# ----------------------------------------------------------
# Clase 04 - Script Manipulación de datos con tidyr y dplyr
# Dra. María Angélica Rueda
# 13 de enero 2023
# Curso R para Biociencias.
# ----------------------------------------------------------

# Habilita librerías
library(readxl) # Para importar datos desde excel a R

library(tidyr) # Para manipular datos

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos


# LIBRERÍA DPLYR: EL OPERADOR PIPE (TUBERÍA).

# dplyr usa el operador pipe %>% como una tubería para enlazar un data.frame con una o más funciones.

x <- rnorm(5)
y <- rnorm(5)
dat <- data.frame(x,y)
dat
max(dat) 
dat %>% max
dat %>% arrange(y) # Ordena filas de un data.frame por el valor de alguna columna

# TRABAJANDO CON DATOS REALES

# https://lter.kbs.msu.edu/datatables/51

agronomic_data <- read_excel("agronomic_data.xlsx", sheet = 1)
head(agronomic_data)
agronomic_data$Sample_id <- as.factor(agronomic_data$Sample_id)
agronomic_data$Crop <- as.factor(agronomic_data$Crop)
agronomic_data$Year <- as.factor(agronomic_data$Year)
agronomic_data$yield_bu_A <- as.numeric(agronomic_data$yield_bu_A)
summary(agronomic_data)

# FUNCIÓN SELECT()
# Permite extraer o seleccionar variables/columnas específicas de un data.frame.
select(agronomic_data, Crop, Year)

# FUNCIÓN SELECT() CON PIPE
agronomic_data %>% select(Crop, Year)

# FUNCIÓN FILTER() CON PIPE
# **filter()**: Para filtrar desde una tabla de datos un subconjunto de filas.
# Ej. solo un nivel de de un factor, observaciones que cumplen algún criterio (ej. > 20).
agronomic_data  %>% filter(Crop == "Glycine max")
agronomic_data  %>% filter(Crop == "Zea mays")

# MÚLTIPLES FUNCIONES Y TUBERÍAS
agronomic_data %>% select(Crop, Year, yield_bu_A) %>% 
  filter(Crop == "Glycine max")

# FUNCIÓN SUMMARIZE()
agronomic_data %>% select(Crop, Year, yield_bu_A) %>% 
          summarize(n = n(), 
                    Promedio_yield = mean(yield_bu_A), 
                    Maximo_yield = max(yield_bu_A))

# FUNCIÓN SUMMARIZE() removiendo NA antes de calcular
agronomic_data %>% select(Crop, Year, yield_bu_A) %>% 
  summarize(n = n(), 
            Promedio_yield = mean(yield_bu_A, na.rm=T), 
            Maximo_yield = sd(yield_bu_A, na.rm=T))

# FUNCIÓN SUMMARIZE() + GROUP_BY()
# Permite agrupar filas con base a los niveles de alguna variable o factor.
agronomic_data %>% group_by(Crop) %>% 
  summarize(n = n(), 
            mean_yield = mean(yield_bu_A, na.rm=T), 
            sd_yield = sd(yield_bu_A, na.rm=T))

agro_tab <- agronomic_data %>% group_by(Crop) %>% 
  summarize(n = n(), 
            mean_yield = mean(yield_bu_A, na.rm=T), 
            sd_yield = sd(yield_bu_A, na.rm=T))

# FUNCIÓN MUTATE()
# Permite calcular nuevas variables "derivadas", ej. proporciones, tasas, log.
# Calcularemos el coeficiente de variación como  sd / mean * 100
agro_tab %>% mutate(CV_yield = sd_yield/mean_yield*100)

# EJERCICIO
# Importe la hoja 1 del set de datos Peces.
# Explore usando str() y summary(), transforme a factor las variables Pez, Especie	y Sexo.
# Filtre, seleccione, agrupe y calcule variables derivadas con los comandos aprendidos en clase.


