# ----------------------------------------------------------
# Clase 05 - Script inferencia estadística
# Dr. María Angélica Rueda
# 15 enero 2023
# Curso R para Biociencias
# ----------------------------------------------------------

# Paquetes
library(UsingR)
library(ggplot2)
library(dplyr)
library(knitr)

# ESTUDIO DE CASO: RELACIÓN ESTATURA PADRES - HIJOS

# Explora 
?father.son
summary(father.son)

# Formato texto de la gráfica
My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 18),
  axis.title.y = element_text(size = 18),
  axis.text.y = element_text(size = 18))

# boxplot básico con ggplot
ggplot(data = father.son) +
  geom_point(aes(x = fheight, y = sheight))+
  labs( x="Estatura padres (Pulgadas)", y="Estatura hijos (Pulgadas)") +
  My_Theme

# boxplot con estética de símbolos
ggplot(data = father.son) +
  geom_point(aes(x = fheight, y = sheight),
             col = 'darkblue',
             size = 2,
             alpha = 1/2)+
  labs( x="Estatura padres (Pulgadas)", y="Estatura hijos (Pulgadas)") + My_Theme

# boxplot con linea de regresión
ggplot(data = father.son) +
  geom_point(aes(x = fheight, y = sheight),
             col = 'darkblue',
             size = 2,
             alpha = 1/2) + 
  geom_smooth(aes(x = fheight, y = sheight), method = 'lm')+
  labs( x="Estatura padres (Pulgadas)", y="Estatura hijos (Pulgadas)") + My_Theme

#  **Pearson's product-moment correlation**
cor <- cor.test(father.son$fheight, father.son$sheight)
cor

# Imprime resultado prueba estadistica en formato tabla.
pander::pander(cor, caption = "prueba de correlación", digits=3)

# ESTUDIO DE CASO: COMPARACIÓN TAMAÑO ENTRE SEXOS
dat <- data.frame(Zona=rep(c("Urbano", "Bosque"), each=10), Cortisol=c(rnorm(10, 10, 2),rnorm(10, 8, 2)))

ggplot(dat, aes(x=Zona, y=Cortisol, fill=Zona))+
  geom_boxplot()+
  labs( x="Zona", y="Cortisol (nanogramos / decilitro)") + My_Theme+theme(legend.position='none')

# **Two Sample t-test**

test <- t.test(Cortisol ~ Zona, dat, alternative = c("two.sided"),
       var.equal=TRUE)
test

# Imprime resultado prueba estadistica en formato tabla.
pander::pander(test, caption = "prueba de t", digits=3)
