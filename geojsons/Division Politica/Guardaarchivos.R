# Extractor de geojsons para esta pagina 
pacman::p_load("sf", "tidyverse")

urls <- c("https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/EdosZM.geojson", 
          "https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/EstadosZMVM.geojson", 
          "https://github.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/raw/master/Zona%20Metropolitana/ZMVM_shell.geojson")

#st_read(urls[1]) %>% plot


Nombres <- c("MunicipiosZMVM.geojson", "EstadosZMVM.geojson", "PerimetroZMVM.geojson")

for (i in 1:3) {
  st_read(urls[i]) %>% st_write(Nombres[i])
}

