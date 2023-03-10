# ----------------------------------------------------------
# Clase 03 - Script ggplot2
# Dr. José Gallardo Matus
# 13 de enero 2022
# Curso R para Biociencias.
# ----------------------------------------------------------

# habilitar paquetes ggplot2 y agridat
library(ggplot2) # Paquete para hacer lindos gráficos en R

# Explore el objeto CO2 con el comando help
help(CO2)

# Intento de gráfica con función ggplot.
# La gráfica queda vacía pues falta indicar el tipo de gráfica que deseamos
ggplot(CO2, aes(uptake))

# Histograma con ggplot. 
ggplot(CO2, aes(uptake))+
 geom_histogram()

# Agregamos titulo y nombre de los ejes
ggplot(CO2, aes(uptake))+
  geom_histogram()+
  labs(title="Histograma", x="Consumo de CO2", 
       y="Frecuencia")

# Modificamos tamaño de etiquetas
My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 18))

ggplot(CO2, aes(uptake))+
  geom_histogram()+
  labs(title="Histograma", x="Consumo de CO2", 
       y="Frecuencia") +
  My_Theme

# Gráfica con dos ejes y tratamiento pero incompleta
ggplot(CO2, aes(x=Treatment, y=uptake))

# Gráfica de boxplot
ggplot(CO2, aes(x=Treatment, y=uptake, fill=Treatment))+
geom_boxplot()

# Tarea. Explore el set de datos chickwts.
# Utilice str(), summary()
# Realice un histograma con la variable cuantitativa continua weight.
# Realice un boxplot de weight en funcion de las dietas (feed).
summary(chickwts)
str(chickwts)


