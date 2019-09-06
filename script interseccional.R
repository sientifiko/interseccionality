library(ggplot2); library(tidyverse); library(stringr)


#importamos la data, notar que aunque es csv, excel los exporta como ";"
casen <- read.csv("casen_2017.csv", sep = ";")

#tomo solo porblaci�n adulta, para considerar escolaridad e ingreso
#quito valores vac�os
casen_mayores <- casen %>% filter(edad >= 20, str_length(es_indigena)>0) 

#genero tabla resumen
tabla.resumen <- casen_mayores %>% 
  group_by(sexo, es_indigena) %>%
  summarize(escolaridad = mean(esc, na.rm = T), 
            ingresos = mean(ingreso, na.rm = T))


#exporto la tabla
write.table(tabla.resumen, "tabla resumen.csv", sep = ";", row.names = F)




# generando la cateogr�a mujer y hombre ind�gena y no ind�gena
casen_mayores$sexo2 <- as.factor(paste0( str_c(casen_mayores$sexo, casen_mayores$es_indigena, sep = " ")))

# reacomodo los niveles
casen_mayores$sexo2 <- factor(casen_mayores$sexo2, 
                              levels(casen_mayores$sexo2)[c(2,1,4,3)])



# convierto quintiles a n�meros romanos, porque bonito
casen_mayores$romano <- as.factor( paste0( as.roman(casen_mayores$qaut)) )


# ploteo los quintiles aut�nomos por categor�a interseccional
ggplot(na.omit(casen_mayores)) +
  geom_bar(aes(sexo2, fill=romano ), position = "fill" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", y= "Proporci�n", fill= "Quintil aut�nomo\nnacional")

