# ----------------------------------------------------------
# Clase 02 - Simular variables aleatorias continuas
# Dr. José Gallardo y Dra. María Angélica Rueda Calderón
# 12 enero 2022
# Curso R para Biociencias.
# ----------------------------------------------------------

# Remover objetos de la sesión de trabajo
rm(list = ls())


# ¿Qué tipos de distribuciones hay en paquete stats?

help(Distributions)
help(Normal)

help(seq)
help(sample)

# Simular base de datos con variable aleatoria continua con distribución normal paso a paso
set.seed(1) #semilla para fijar resultados cada vez que se corre la simulación
Animal <- seq(1:100)
Talla <- rnorm(100, 77, 5)
Peso <- rnorm(100, 6078, 1190)
Sexo <- sample(c("Hembra","Macho"), size = 100, replace = TRUE)
datos <- data.frame(Animal, Talla, Peso, Sexo)

# Explore y grafique el objeto "datos" con summary(), hist(), boxplot()
# plot()

# Simula una variable cuantitatica continua de su interes personal.
# Use set.set() y rnomr(). 
# Guarde su variables como un objeto.
# Simule un tratamiento asociado a su variable de interes.
# Use sample()
# Guarde su tratamiento como un objeto.
# Elabore un data.frame y explore con summary() y otras funciones aprendidas en clase hist(), boxplot().




























# Respuesta a ejercicios
summary(datos)
hist(datos$Peso)
boxplot(datos$Peso ~ datos$Sexo)
