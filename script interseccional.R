library(ggplot2); library(tidyverse); library(stringr)


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
casen_mayores$sexo2 <- as.factor(paste0( str_c(casen_mayores$sexo, casen_mayores$es_indigena, sep = " ")))

# reacomodo los niveles
levels(casen_mayores$sexo2)[c(2,1,4,3)]


# convierto quintiles a números romanos, porque bonito
casen_mayores$romano <- as.factor( paste0( as.roman(casen_mayores$qaut)) )


# ploteo los quintiles autónomos por categoría interseccional
ggplot(na.omit(casen_mayores)) +
  geom_bar(aes(sexo2, fill=romano ), position = "fill" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x="", y= "Proporción", fill= "Quintil autónomo\n nacional")


