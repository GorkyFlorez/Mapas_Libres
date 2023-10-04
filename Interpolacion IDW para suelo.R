rm(list = ls())

library(pacman)
p_load(sf, raster, rgdal, rgeos, wesanderson, sp, mapview, gstat, tidyverse, readxl, ggspatial)

ruta = "DATOS_Fil.xlsx"
lts = lapply(excel_sheets(ruta), read_excel, path= ruta)

fnc = shapefile("SHP/Zona.shp")
mapview(fnc)
ext = extent(fnc)

grd = expand.grid(x = seq(from = ext[1], to = ext[2], by = 10),
                  y = seq(from = ext[3], to = ext[4], by = 10))

plot(grd)
coordinates(grd) = ~ x + y 
gridded(grd) = TRUE
crs(grd) = crs(fnc)

pts = lts[[1]]
coordinates(pts) = ~ x + y 
crs(pts) = crs(fnc)
plot(pts)

calc_idw = function(x) {
  print(x)
  
  pts = lts[[x]]
  a= unique(pts$Profund)
  coordinates(pts) = ~ x + y 
  crs(pts) = crs(fnc)
  idw.p = gstat::idw(pH ~ 1, pts, grd)
  idw.p = raster(idw.p)
  idw.p = raster::mask(idw.p, fnc)
  dir.create("pH")
  writeRaster(idw.p, paste0("pH/idw_pH_", a, ".tif"), overwrite = T)
}

ord = c(1:length(lts))
rsl = map(.x = ord, .f = calc_idw)


fle = list.files("./pH/", full.names = T , pattern = ".tif$")
lyr = stack(fle)

plot(lyr)

tbl = rasterToPoints(lyr, xy = T) %>% as_tibble() %>% 
  gather(var, value, -x, -y)

unique(tbl$var)


tbl$var = gsub("idw_pH_10.20", "Profundidad 20 cm", tbl$var)

tbl$var = gsub("idw_pH_30.40", "Profundidad 40 cm", tbl$var)

pal = wes_palette("Cavalcanti1", 5, "continuous")
pa1 = c("#440154", "#39518F", "#21918F", "#FFF809", "#FF0001")


gmap = ggplot()+
  geom_raster(data = tbl, mapping = aes(x=x, y=y, fill=value))+
  facet_wrap(~var) +
  scale_fill_gradientn(colours = pa1, na.value = "white", limits = c(3, 6),
                       breaks= seq(3, 6, by = 0.5))+
  geom_sf(data = st_as_sf(fnc), fill = NA)+
  geom_sf(data = st_as_sf(pts), color= "gray50")+
  theme_bw()+
  
  ggtitle("Interpolación de datos de pH (1:1) en R")+
  labs(subtitle="Ing. Gorky Florez Castillo", x="Longitud",y="Latitud",
       fill= "pH", 
       caption="Fuente Data: Datos de Sanchez (2018), Estudio en suelos de diferentes uso en el Distrito de Las Piedras")+
  
  theme(legend.position = "bottom",
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        strip.text=element_text(family='Anton', size=9, hjust=0.5, color='black'),
        strip.background=element_rect(fill='grey60'),
        plot.title = element_text(size = 16, hjust = 0.5, color = "black", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "black", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "black", family="serif", face = "italic"))+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(0.7, "cm"),
    width = unit(1.1, "cm"),
    pad_x = unit(0.1, "cm"), pad_y = unit(0.8, "cm"),
    style=north_arrow_fancy_orienteering())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")+
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(5, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  ))+
  ggplot2::annotate(geom = "text", x = 485857.607, y = 8653870.549, hjust = 0, vjust = 1, 
                    label = "Sistema agroforestal",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 486399.818, y = 8657630.509, hjust = 0, vjust = 1, 
                    label = "Pastizal",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 486400.428, y = 8656303.556, hjust = 0, vjust = 1, 
                    label = "Monocultivo",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 485313.558, y = 8654091.443, hjust = 0, vjust = 1, 
                    label = "Bosque protegido",size = 3, family="serif", color = 
                      "black",  fontface="italic", angle=90)

gmap
dir.create('./png')
ggsave(plot = gmap, filename = './png/01_mapa_pH.png', units = 'cm', width = 20, height = 20, dpi = 1200)










rm(list = ls())

library(pacman)
p_load(sf, raster, rgdal, rgeos, wesanderson, sp, mapview, gstat, tidyverse, readxl, ggspatial)

ruta = "DATOS_Fil.xlsx"
lts = lapply(excel_sheets(ruta), read_excel, path= ruta)

fnc = shapefile("SHP/Zona.shp")
mapview(fnc)
ext = extent(fnc)

grd = expand.grid(x = seq(from = ext[1], to = ext[2], by = 10),
                  y = seq(from = ext[3], to = ext[4], by = 10))

plot(grd)
coordinates(grd) = ~ x + y 
gridded(grd) = TRUE
crs(grd) = crs(fnc)

pts = lts[[1]]
coordinates(pts) = ~ x + y 
crs(pts) = crs(fnc)
plot(pts)

calc_idw = function(x) {
  print(x)
  
  pts = lts[[x]]
  a= unique(pts$Profund)
  coordinates(pts) = ~ x + y 
  crs(pts) = crs(fnc)
  idw.p = gstat::idw(MO ~ 1, pts, grd)
  idw.p = raster(idw.p)
  idw.p = raster::mask(idw.p, fnc)
  dir.create("MO")
  writeRaster(idw.p, paste0("MO/idw_MO_", a, ".tif"), overwrite = T)
}

ord = c(1:length(lts))
rsl = map(.x = ord, .f = calc_idw)


fle = list.files("./MO/", full.names = T , pattern = ".tif$")
lyr = stack(fle)

plot(lyr)

tbl = rasterToPoints(lyr, xy = T) %>% as_tibble() %>% 
  gather(var, value, -x, -y)

unique(tbl$var)


tbl$var = gsub("idw_MO_10.20", "Profundidad 20 cm", tbl$var)

tbl$var = gsub("idw_MO_30.40", "Profundidad 40 cm", tbl$var)

pal = wes_palette("Cavalcanti1", 5, "continuous")
pa1 = c("#AE1124", "#E34931", "#FA834A", "#FCC072", "#FCEC9E", "#E7F49A", "#B5E06D", "#6FC264", "#219A4F", "#006836")


gmap = ggplot()+
  geom_raster(data = tbl, mapping = aes(x=x, y=y, fill=value))+
  facet_wrap(~var) +
  scale_fill_gradientn(colours = pa1, na.value = "white", limits = c(0.3, 2),
                       breaks= seq(0.3, 2, by = 0.2))+
  geom_sf(data = st_as_sf(fnc), fill = NA)+
  geom_sf(data = st_as_sf(pts), color= "gray50")+
  theme_bw()+
  
  ggtitle("Interpolación de datos de Materia Organica en R")+
  labs(subtitle="Ing. Gorky Florez Castillo", x="Longitud",y="Latitud",
       fill= "Materia Organica (%)", 
       caption="Fuente Data: Datos de Sanchez (2018), Estudio en suelos de diferentes uso en el Distrito de Las Piedras")+
  
  theme(legend.position = "bottom",
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        strip.text=element_text(family='Anton', size=9, hjust=0.5, color='black'),
        strip.background=element_rect(fill='grey60'),
        plot.title = element_text(size = 16, hjust = 0.5, color = "black", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "black", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "black", family="serif", face = "italic"))+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(0.7, "cm"),
    width = unit(1.1, "cm"),
    pad_x = unit(0.1, "cm"), pad_y = unit(0.8, "cm"),
    style=north_arrow_fancy_orienteering())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")+
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(5, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  ))+
  ggplot2::annotate(geom = "text", x = 485857.607, y = 8653870.549, hjust = 0, vjust = 1, 
                    label = "Sistema agroforestal",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 486399.818, y = 8657630.509, hjust = 0, vjust = 1, 
                    label = "Pastizal",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 486400.428, y = 8656303.556, hjust = 0, vjust = 1, 
                    label = "Monocultivo",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 485313.558, y = 8654091.443, hjust = 0, vjust = 1, 
                    label = "Bosque protegido",size = 3, family="serif", color = 
                      "black",  fontface="italic", angle=90)

gmap
dir.create('./png')
ggsave(plot = gmap, filename = './png/02_mapa_MO.png', units = 'cm', width = 20, height = 20, dpi = 1200)











rm(list = ls())

library(pacman)
p_load(sf, raster, rgdal, rgeos, wesanderson, sp, mapview, gstat, tidyverse, readxl, ggspatial)

ruta = "DATOS_Fil.xlsx"
lts = lapply(excel_sheets(ruta), read_excel, path= ruta)

fnc = shapefile("SHP/Zona.shp")
mapview(fnc)
ext = extent(fnc)

grd = expand.grid(x = seq(from = ext[1], to = ext[2], by = 10),
                  y = seq(from = ext[3], to = ext[4], by = 10))

plot(grd)
coordinates(grd) = ~ x + y 
gridded(grd) = TRUE
crs(grd) = crs(fnc)

pts = lts[[1]]
coordinates(pts) = ~ x + y 
crs(pts) = crs(fnc)
plot(pts)

calc_idw = function(x) {
  print(x)
  
  pts = lts[[x]]
  a= unique(pts$Profund)
  coordinates(pts) = ~ x + y 
  crs(pts) = crs(fnc)
  idw.p = gstat::idw(K ~ 1, pts, grd)
  idw.p = raster(idw.p)
  idw.p = raster::mask(idw.p, fnc)
  dir.create("K")
  writeRaster(idw.p, paste0("K/idw_K_", a, ".tif"), overwrite = T)
}

ord = c(1:length(lts))
rsl = map(.x = ord, .f = calc_idw)


fle = list.files("./K/", full.names = T , pattern = ".tif$")
lyr = stack(fle)

plot(lyr)

tbl = rasterToPoints(lyr, xy = T) %>% as_tibble() %>% 
  gather(var, value, -x, -y)

unique(tbl$var)


tbl$var = gsub("idw_K_10.20", "Profundidad 20 cm", tbl$var)

tbl$var = gsub("idw_K_30.40", "Profundidad 40 cm", tbl$var)

pal = wes_palette("Zissou1", 5, "continuous")
pa1 = c( "#D0D717", "#8FC349", "#73BB4D", "#70CB93", "#0096C0", "#0A4D9C", "#262C80")


gmap = ggplot()+
  geom_raster(data = tbl, mapping = aes(x=x, y=y, fill=value))+
  facet_wrap(~var) +
  scale_fill_gradientn(colours = pal, na.value = "white", limits = c(15, 95),
                       breaks= seq(15, 95, by = 10))+
  geom_sf(data = st_as_sf(fnc), fill = NA)+
  geom_sf(data = st_as_sf(pts), color= "gray50")+
  theme_bw()+
  
  ggtitle("Interpolación de datos de Potasio K en R")+
  labs(subtitle="Ing. Gorky Florez Castillo", x="Longitud",y="Latitud",
       fill= "K (ppm)", 
       caption="Fuente Data: Datos de Sanchez (2018), Estudio en suelos de diferentes uso en el Distrito de Las Piedras")+
  
  theme(legend.position = "bottom",
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        strip.text=element_text(family='Anton', size=9, hjust=0.5, color='black'),
        strip.background=element_rect(fill='grey60'),
        plot.title = element_text(size = 16, hjust = 0.5, color = "black", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "black", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "black", family="serif", face = "italic"))+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(0.7, "cm"),
    width = unit(1.1, "cm"),
    pad_x = unit(0.1, "cm"), pad_y = unit(0.8, "cm"),
    style=north_arrow_fancy_orienteering())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")+
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(5, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  ))+
  ggplot2::annotate(geom = "text", x = 485857.607, y = 8653870.549, hjust = 0, vjust = 1, 
                    label = "Sistema agroforestal",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 486399.818, y = 8657630.509, hjust = 0, vjust = 1, 
                    label = "Pastizal",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 486400.428, y = 8656303.556, hjust = 0, vjust = 1, 
                    label = "Monocultivo",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 485313.558, y = 8654091.443, hjust = 0, vjust = 1, 
                    label = "Bosque protegido",size = 3, family="serif", color = 
                      "black",  fontface="italic", angle=90)

gmap
dir.create('./png')
ggsave(plot = gmap, filename = './png/03_mapa_K.png', units = 'cm', width = 20, height = 20, dpi = 1200)








rm(list = ls())

library(pacman)
p_load(sf, raster, rgdal, rgeos, wesanderson, sp, mapview, gstat, tidyverse, readxl, ggspatial)

ruta = "DATOS_Fil.xlsx"
lts = lapply(excel_sheets(ruta), read_excel, path= ruta)

fnc = shapefile("SHP/Zona.shp")
mapview(fnc)
ext = extent(fnc)

grd = expand.grid(x = seq(from = ext[1], to = ext[2], by = 10),
                  y = seq(from = ext[3], to = ext[4], by = 10))

plot(grd)
coordinates(grd) = ~ x + y 
gridded(grd) = TRUE
crs(grd) = crs(fnc)

pts = lts[[1]]
coordinates(pts) = ~ x + y 
crs(pts) = crs(fnc)
plot(pts)

calc_idw = function(x) {
  print(x)
  
  pts = lts[[x]]
  a= unique(pts$Profund)
  coordinates(pts) = ~ x + y 
  crs(pts) = crs(fnc)
  idw.p = gstat::idw(P ~ 1, pts, grd)
  idw.p = raster(idw.p)
  idw.p = raster::mask(idw.p, fnc)
  dir.create("P")
  writeRaster(idw.p, paste0("P/idw_P_", a, ".tif"), overwrite = T)
}

ord = c(1:length(lts))
rsl = map(.x = ord, .f = calc_idw)


fle = list.files("./P/", full.names = T , pattern = ".tif$")
lyr = stack(fle)

plot(lyr)

tbl = rasterToPoints(lyr, xy = T) %>% as_tibble() %>% 
  gather(var, value, -x, -y)

unique(tbl$var)


tbl$var = gsub("idw_P_10.20", "Profundidad 20 cm", tbl$var)

tbl$var = gsub("idw_P_30.40", "Profundidad 40 cm", tbl$var)

pal = wes_palette("Darjeeling1", 5, "continuous")
pa1 = c( "#D0D717", "#8FC349", "#73BB4D", "#70CB93", "#0096C0", "#0A4D9C", "#262C80")


gmap = ggplot()+
  geom_raster(data = tbl, mapping = aes(x=x, y=y, fill=value))+
  facet_wrap(~var) +
  scale_fill_gradientn(colours = pal, na.value = "white", limits = c(1, 7),
                       breaks= seq(1, 7, by = 1.5))+
  geom_sf(data = st_as_sf(fnc), fill = NA)+
  geom_sf(data = st_as_sf(pts), color= "gray50")+
  theme_bw()+
  
  ggtitle("Interpolación de datos de Fósforo P en R")+
  labs(subtitle="Ing. Gorky Florez Castillo", x="Longitud",y="Latitud",
       fill= "P (ppm)", 
       caption="Fuente Data: Datos de Sanchez (2018), Estudio en suelos de diferentes uso en el Distrito de Las Piedras")+
  
  theme(legend.position = "bottom",
        axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
        axis.text.x  = element_text(face="bold", color="black", size=8),
        strip.text=element_text(family='Anton', size=9, hjust=0.5, color='black'),
        strip.background=element_rect(fill='grey60'),
        plot.title = element_text(size = 16, hjust = 0.5, color = "black", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "black", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "black", family="serif", face = "italic"))+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    height = unit(0.7, "cm"),
    width = unit(1.1, "cm"),
    pad_x = unit(0.1, "cm"), pad_y = unit(0.8, "cm"),
    style=north_arrow_fancy_orienteering())+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")+
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(5, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  ))+
  ggplot2::annotate(geom = "text", x = 485857.607, y = 8653870.549, hjust = 0, vjust = 1, 
                    label = "Sistema agroforestal",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 486399.818, y = 8657630.509, hjust = 0, vjust = 1, 
                    label = "Pastizal",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 486400.428, y = 8656303.556, hjust = 0, vjust = 1, 
                    label = "Monocultivo",size = 3, family="serif", color = 
                      "black",  fontface="italic")+
  ggplot2::annotate(geom = "text", x = 485313.558, y = 8654091.443, hjust = 0, vjust = 1, 
                    label = "Bosque protegido",size = 3, family="serif", color = 
                      "black",  fontface="italic", angle=90)

gmap

dir.create('./png')
ggsave(plot = gmap, filename = './png/04_mapa_P.png', units = 'cm', width = 20, height = 20, dpi = 1200)






