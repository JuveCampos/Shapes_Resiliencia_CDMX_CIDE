# coNTRUCCION DE MAPA DE VIVIENDAS CON DISPONIBILIDAD DE AGUA. 
# A NIVEL AGEB.
# INFORMACION DEL 
# CENSO 2010

# LIBRERÍAS
library(readr)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)

# Cargamos data de la ZMVM
zmvm <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/ZMVM_shell.geojson")

# Cargamos data de los AGEBS (datos recogidos durante el censo)
data <- as_tibble(read.csv("04 Censo de población por Agebs 2010.csv")) %>%
  mutate(I_05 = ((vph_aguafv/(vph_aguafv + vph_aguadv))) * 100) %>%
  mutate(I_03 = ((vivpar_hab - vph_excsa)/(vivpar_hab))  * 100) %>%
  mutate(I_07 = ((vph_nodren)/(vph_drenaj + vph_nodren)) * 100)

# Cargamos los poligonos de las agebs
ageb <- st_read("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/Ageb.geojson") 
  
# Mismas clases?
class(ageb$CVE_AGEB) == class(data$ClaveAgeb)

# Sacamos el indice para filtrar la base de los AGEBs (el objeto `data`)
c <- c()
for (i in 1:length(ageb$CVEGEO)) {
c[i] <-   which(ageb$CVEGEO[i] == data$ClaveAgeb)
}

#Filtramos las variables que necesitamos
variables <- c("ClaveAgeb", "I_05","I_03","I_07")
d <- c()
for(i in 1:4){
  d[i] <- which(names(data) == variables[i])
}

# Filtramos la base de los AGEBs (el objeto `data`)
data <- data[c,d]

# Mergeamos con el shape
map <- merge(ageb, data, by.x = "CVEGEO", by.y = "ClaveAgeb")
head(map)
class(map)

# Pequeño mapa
pal <- colorNumeric(palette = "YlOrRd", domain = map$I_05)

leaflet(map) %>%
  addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
  addPolygons(data= zmvm, color = "blue", weight = 1, fillOpacity = 0.1) %>%
  addPolygons(color = "black", fillColor = ~pal(I_05), weight = 0.1, fillOpacity = 1) %>%
  addScaleBar(position = "bottomleft") %>%
  addLegend(position = "bottomright", pal = pal, values = map$I_05, title = "% de viviendas sin disponibilidad\n<br> de agua entubada")
