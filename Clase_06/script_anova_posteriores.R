# ---------------------------------------------------------------------------------
# Clase 06 - Script Anova y posteriores
# Dra. Dra. Angélica Rueda
# 15 enero 2022
# Curso Análisis de datos con R para Biociencias.
# ---------------------------------------------------------------------------------

# PAQUETES
library(ggplot2)
library(dplyr)
library(gridExtra) # Permite realizar graficas en paneles
library(knitr) # Permite imprimir tablas desde pruebas estadisticas
library(broom)  # Permite imprimir tablas desde pruebas estadisticas

# ESTUDIO DE CASO: CRECIMIENTO DE PLANTAS

rm(list = ls())

help(PlantGrowth)

my_data <- PlantGrowth
table(my_data$group)
colnames(my_data) <- c("Dried weight","Group")

p <-  ggplot(my_data, aes(x=`Dried weight`)) +
  geom_histogram(color="darkblue", fill="lightblue", bins = 10)

q <- my_data%>% 
  ggplot(aes(x=Group,y=`Dried weight`,fill=Group))+
  geom_boxplot()+
  theme(legend.position="none")+
  labs(x="Group",y="Dried weight")


grid.arrange(p, q, ncol=2, nrow =1)

# ajusta modelo lineal
res.aov <- lm(`Dried weight` ~ Group, data = my_data)

# Realiza inferencia con anova
anova(res.aov)
kable(anova(res.aov))

# Realiza prueba de comparaciones múltiples
fit_anova <- aov(res.aov)
tk <- TukeyHSD(fit_anova)
tk

tidy(tk) %>% kable(caption = "Prueba de Tukey.", digits=2,
                   col.names=c("Trat.","Contraste", "H0",
                               "Diferencia", "IC-bajo","IC-alto",
                               "p-ajustado"))


# ESTUDIO DE CASO: GUINEA PIGS

help(ToothGrowth)
my_data1 <- ToothGrowth
summary(my_data1)

my_data1$dose <- as.factor(my_data1$dose)
summary(my_data1)
table(my_data1$supp, my_data1$dose)

p <-  ggplot(my_data1, aes(x=len)) +
  geom_histogram(color="darkblue", fill="lightblue", bins = 10)

q<- my_data1%>% 
  ggplot(aes(x=dose,y=len,fill=supp))+
  geom_boxplot()+
  labs(x="Dose vitamin c",y="Length of odontoblasts")

grid.arrange(p, q, ncol=2, nrow =1)

# Anova de dos vías con interacción
res.aov2 <- aov(len ~ dose * supp,
                data = my_data1)

# Imprime resultado anova
anova(res.aov2)

# Imprime resultado en formato tabla.
anova(res.aov2)%>% kable(caption = "Anova de dos vías con interacción.",
                         digits=3)
