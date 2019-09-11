library(ggplot2); library(tidyverse); library(stringr); library(stargazer)


# CAMBIAR TODO ESTO A RELACIÓN ONVRE VS INDÍGENA!!!!!!

# desactivamos la notación científica, y redondeamos a dos
options(scipen = 999, digits = 2)

#importamos la data, notar que aunque es csv, excel los exporta como ";"
casen <- read.csv("casen_2017.csv", sep = ";")

#tomo solo porblación adulta, para considerar escolaridad e ingreso
#quito valores vacíos
casen_mayores <- casen %>% filter(edad >= 20, str_length(es_indigena)>0) 

#genero tabla resumen
tabla.resumen <- casen_mayores %>% 
  group_by(sexo, es_indigena) %>%
  summarize(escolaridad = mean(esc, na.rm = T), 
            ingresos = mean(ingreso, na.rm = T))


#exporto la tabla
write.table(tabla.resumen, "tabla resumen.csv", sep = ";", row.names = F)


# generando la cateogría mujer y hombre indígena y no indígena
casen_mayores$sexo2 <- as.factor(paste0( str_c(casen_mayores$sexo, 
                                               casen_mayores$es_indigena, 
                                               sep = " ")))

# reacomodo los niveles
casen_mayores$sexo2 <- factor(casen_mayores$sexo2, 
                              levels(casen_mayores$sexo2)[c(2,1,4,3)])

# convierto quintiles a números romanos, porque bonito
casen_mayores$romano <- as.factor( paste0( as.roman(casen_mayores$qaut)) )

# creo un df sin valores NA
casen_mayores2 <- na.omit(casen_mayores)

# creo un df para plotear frecuencia acumulada
quintil_df <- casen_mayores2 %>%
  group_by(sexo2, romano) %>%
  summarize(n = n()) %>%
  mutate(perc = n/sum(n) )

# ploteo los quintiles autónomos por categoría interseccional
ggplot(quintil_df, aes(x=sexo2, y = perc, fill = romano) ) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label= scales::percent(perc)), 
            position = position_stack(vjust = .5))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y= "Proporción", fill= "Quintil autónomo\nnacional")


# construyo una tabla para plotear la pobreza multidimensional
multidimensional <- casen_mayores2 %>%
  group_by(sexo2, multidimensional_5) %>%
  summarize(n= n()) %>%
  mutate(perc = n/sum(n))
  
# ploteo multidimensional
ggplot(multidimensional, aes(sexo2, y= perc, fill = as.factor(multidimensional_5) )) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label=scales::percent(perc)),
            position = position_stack(vjust = .5)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y= "Proporción", fill= "Pobreza\nmultidimensional")

# cambiar orden de es indigena

casen_mayores2$es_indigena <- relevel(casen_mayores2$es_indigena, "No indigena")

# Hay intereacción??
ggplot(casen_mayores2, aes(es_indigena, multidimensional_5 , color=sexo, group=sexo)) +
  geom_point(stat = "summary", fun.y= mean) +
  stat_summary(fun.y=mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom ="pointrange" ) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x="", y="Promedio pobreza multidimensional")



# paparando el set específico para los test
df_trabajo <- casen_mayores2[,c(2, 6, 7, 14)]

# dar vuelta el índice de sexo
df_trabajo$sexo_cod2 <- (df_trabajo$sexo_cod-1)^2


# ajustando modelo de onvre*indigena
logit.model1 <- glm(multidimensional_5 ~ sexo_cod2*es_indigena_cod, 
                    data = df_trabajo, family = binomial())

# mostrando modelo doble interacción
stargazer(logit.model1, type = "text")



# construyo un factor de discriminación por género, ya que ésta está en 0 y 1
casen_mayores2$es_discriminado <- factor(casen_mayores2$disc_identidad_genero, levels = c(0,1),
                                         labels = c("No discriminado", "Discriminado"))

# graficando interacicion onvre*discriminacion_genero
ggplot(casen_mayores2, aes(es_discriminado, multidimensional_5 , color=sexo, group=sexo)) +
  geom_point(stat = "summary", fun.y= mean) +
  stat_summary(fun.y=mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom ="pointrange" ) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x="", y="Promedio pobreza multidimensional",
       title = "Discriminación por identidad de género") 


# ajustando modelo de onvre*discriminacion_genero
logit.model2 <- glm(multidimensional_5 ~ sexo_cod2*disc_identidad_genero,
                    data = df_trabajo, family = binomial())

#mostrando resultado modelo triple interaccion
stargazer(logit.model1, logit.model2, type = "text")

# Comparación de modelos por Criterio de información de Aikake y Bayes
AIC(logit.model1, logit.model2)
BIC(logit.model1, logit.model2)


# Ajustando un modelo de solo dos predictores
logit.model <- glm(multidimensional_5 ~ sexo_cod2+es_indigena_cod, 
                   data = df_trabajo, family = binomial())

# Comparando si modelos de interacción son mejores que el sin interacción
AIC(logit.model,logit.model1, logit.model2)
BIC(logit.model,logit.model1, logit.model2)
