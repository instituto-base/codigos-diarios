#Codigos de uso diario en la gestion de datos
#Catalina Marín - BASE
#2024-10-29


#SETTINGS-----

setwd("C:/R/coding-club-2024S2") #Definir la carpeta de trabajo

#Library
library(dplyr)
library(tidyr)
library(stringr)
library(rgbif)
library(ggplot2)
#DATOS----

aves_lago <- read.csv("aves_lagoestacion.csv", fileEncoding = "UTF-8",
                      sep=";")
new_aves <- read.csv("aves_lagoestacion2.csv", fileEncoding = "UTF-8", sep = ",")
#Cambiar nombres de columnas----


#usando R base

colnames(aves_lago)[colnames(aves_lago) == 'Nombre.cienfífico'] <- 'nombre_cientifico' #según nombre

colnames(aves_lago)[2] <- 'nombre_comun' #según número

#Usando dplyr

aves_lago <- aves_lago %>% rename_at('P', ~'primavera')

aves_lago <- aves_lago %>% rename_at(4, ~'verano')

#Renombrar más de una columna

aves_lago <- aves_lago %>% rename(otoño = O, invierno = I)

#Trabajar con NA------

#Eliminar todas las filas que contengan NA en cualquier fila

sin_na1 <- na.omit(aves_lago) 

sin_na2 <- aves_lago %>% drop_na()

#reemplazar datos na
aves_lago[is.na(aves_lago)] <- 0 #en este caso reemplazamos por 0 debido a que son conteos

#Resumen simple----

resumen1 <- summary(aves_lago) #total de especies por estación

#contar total de avistamientos por especie

resumen_conteo_especie <- aves_lago %>% 
            group_by(nombre_cientifico) %>% 
            summarise( total = sum(primavera + verano + otoño + invierno) )
#Trabajando con texto-----

#Extraer el género
aves_genero <- aves_lago

aves_genero$genero <- word(aves_genero$nombre_cientifico, 1)

#transformar palabras en frases

aves_genero$nombre_comun <- str_to_sentence(aves_genero$nombre_comun)

aves_genero$nombre_comun <- str_to_upper(aves_genero$nombre_comun)


#Unir con otro muestreo ----

#Unificar nombre de columnas

new_aves <- new_aves %>% rename(nombre_cientifico = Nombre.cienfífico,
                                nombre_comun = Nombre.vulgar,
                                otoño = O, verano = V, primavera = P)
uni_aves <- bind_rows(aves_lago, new_aves)

#Completar NA con 0
uni_aves[is.na(uni_aves)] <- 0

#Crear nuevo dataframe con la suma según la ave

aves_sum <- uni_aves %>% 
            group_by(nombre_cientifico) %>% 
            summarise(total_primavera = sum(primavera, na.rm = TRUE),
                      total_verano = sum(verano, na.rm = TRUE),
                      total_otoño = sum(otoño, na.rm = TRUE),
                      total_invierno = sum(invierno, na.rm = TRUE),
                      .groups = "drop")
#agregar nombres comunes

aves_sum$nombre_comun <- aves_genero$nombre_comun[match(aves_sum$nombre_cientifico,
                                           aves_genero$nombre_cientifico)]

#Obtener taxonomía superior con GBIF----

tax_completa <- name_backbone_checklist(uni_aves$nombre_cientifico)

#hacer resumen por familia
fam_aves <- uni_aves #otro dataframe para trabajar

fam_aves$familia <- tax_completa$family #agregar columna familia

fam_sum <- fam_aves %>% 
  group_by(familia) %>% 
  summarise(total_primavera = sum(primavera, na.rm = TRUE),
            total_verano = sum(verano, na.rm = TRUE),
            total_otoño = sum(otoño, na.rm = TRUE),
            total_invierno = sum(invierno, na.rm = TRUE),
            .groups = "drop")

#Hacer gráfico con el número de avistamientos por familia -----

#Formato largo
fam_sum_long <- fam_sum %>%
  pivot_longer(cols = starts_with("total_"),
               names_to = "estacion",
               values_to = "avistamientos")

# Crear el gráfico de barras
g_fam1 <- ggplot(fam_sum_long, 
                aes(x = familia, y = avistamientos, fill = estacion)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(x = "Familia de Ave", y = "Cantidad de Avistamientos", fill = "Estación") +
         theme_minimal() +
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(g_fam1)

