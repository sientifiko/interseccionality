library(ggplot2); library(tidyverse); library(stringr); library(stargazer)

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


# ploteo los quintiles autónomos por categoría interseccional
ggplot(na.omit(casen_mayores), aes(x=sexo2, fill=romano))+
  geom_bar(position = "fill") +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y= "Proporción", fill= "Quintil autónomo\nnacional")


# plot multi xd
ggplot(na.omit(casen_mayores), aes(sexo2, fill=as.factor(multidimensional_5)))+
  geom_bar(position = "fill")

ggplot(na.omit(casen_mayores), aes(sexo, fill=as.factor(multidimensional_5)))+
  geom_bar(position = "fill")

ggplot(na.omit(casen_mayores), aes(es_indigena, fill=as.factor(multidimensional_5)))+
  geom_bar(position = "fill")


# paparando el set específico para los test
df_trabajo <- casen_mayores[,c(2,6, 7, 14, 16)]


# ajustando modelos

logit.model1 <- glm(multidimensional_5 ~ sexo_cod, data = df_trabajo, family = binomial())

logit.model2 <- glm(multidimensional_5 ~ sexo_cod*es_indigena_cod,
                   data = df_trabajo, family = binomial())

logit.model3 <- glm(multidimensional_5 ~ sexo_cod*es_indigena_cod*disc_identidad_genero,
                    data = df_trabajo, family = binomial())


stargazer(logit.model3, type = "text")

# mostrando regresion

df_trabajo %>% na.omit() %>%
  mutate(pred_logit = predict(logit.model3)) -> df

ggplot(df, aes(as.factor(sexo_cod), pred_logit, 
               color=as.factor(es_indigena_cod) )) +
  geom_jitter()


ggplot(df, aes(pred_logit, fill=as.factor(sexo_cod))) + 
  geom_density()


