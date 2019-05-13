# Resource: https://www.r-graph-gallery.com/
install.packages(c("devtools", "tidyverse", "lubridate","janitor", "gganimate","gifski", "png", "ggthemes"))
library(devtools)
library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(gganimate)
library(gifski)
library(png)
library(ggthemes)

#-------------------------------------------------------------------------------
# Ideas de análisis con el dataset de #DatosDeMiercoles

# A- Voy a utilizar las burbujas para representar cada categoria con sus subcategorias o componentes. Paquete: data.tree o circlepackeR.
# B- Voy a utilizar el diagrama chord diagram para visibilizar las relaciones recíprocas. Paquete: chorddiag
# C- Voy a utilizar leaflet para armar un mapa básico mostrando por área cantidad de importaciones/exportaciones, seleccionando regiones. Paquete:leaflet
# D - Voy a utilizar Dendrogram, para mostrar jerarquia
#-------------------------------------------------------------------------------
#esto sería una buena práctica
data_url <- "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv"
datoscomercio <- readr::read_csv(data_url, col_types = cols())
View(datoscomercio)
#lectura rápida
#comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
#View(comercio_hispanoamerica_mundo)

#sólo los renombro para facilitar trabajar con los mismos
colnames(datoscomercio) <-c("anio","codOrigen","codigoDestino","paisOrigen", "paisDestino", "codigoProducto", "nombreProducto", "colorProducto", "valorExportado", "valorImportado", "origenHispanoamerica","destinoHisponoamerica")
View(datoscomercio)

#----------------------------------------------------------------------------
#cuáles son los productos importados/exportados
productos <- datoscomercio%>%select(codigoProducto, nombreProducto, colorProducto)%>%arrange (codigoProducto)%>%distinct()
View(productos)

productosC <- janitor::clean_names(productos)
write_csv(productos, 'productos.csv')
View(productosC)
#------------------------------------------------------------------------------
# IMPORTACION MUNDIAL
# total de importaciones realizadas por Argentina por año, paisdestino y producto, OK - importaM=importa mundial
totalImportaMundial<-datoscomercio%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto, colorProducto)%>% summarize(importaM=sum(valorImportado))%>% 
  filter(importaM!=0 & (codOrigen!=codigoDestino)& codOrigen=="arg" & nombreProducto!='Sin Especificar')%>%arrange(desc(importaM))
View(totalImportaMundial)
dim(totalImportaMundial)
tail(totalImportaMundial)
(codOrigen!=codigoDestino)
#-----------------------------------------------------------------------------------------------------
# Ranking para graficos de IMPORTACION MUNDIAL
#------------------------------------------------------------------------------------------------------
#Ranking de productos más importados por Argentina en el mundo DURANTE EL PERÍODO 2013-2017
rankingMundial <- totalImportaMundial%>% group_by(nombreProducto, colorProducto, anio)%>% summarize(importaTotal=sum(importaM))%>% arrange(desc(importaTotal))
View(rankingMundial)
dim(rankingMundial)
#Ranking de productos más importados por Argentina en el mundo en 2017
ranking2017 <- totalImportaMundial%>%filter(anio =="2017")%>% group_by(nombreProducto, colorProducto)%>% summarize(importaT2017=sum(importaM))%>% arrange(desc(importaT2017))
View(ranking2017)

#top 10 de productos con mayor importación de Argentina en el mundo - AÑO:2017
top10Mundial <-ranking2017 %>% arrange(desc(importaT2017))%>% filter(importaT2017 >1219308521)
View(top10Mundial)
color <- ranking$colorProducto
View(color)
#--------------------------------------------------
#graficos importacion a nivel mundial en el año 2017
#------------------------------------------------------
#paleta más llamativa  
ggplot(top10Mundial, aes(reorder(nombreProducto, importaT2017), importaT2017, size =(importaT2017))) + 
  geom_col(aes(fill=nombreProducto)) +
  coord_flip()+ 
  theme_economist()+  
  labs(title = "Top 10 de productos importados por Argentina \n a nivel Mundial",
       subtitle = "Período:2017",
       x = "",
       y = "Valor de importación expresado en doláres",
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(vjust = 1.5, hjust=1.4, size=10, color="black"),
        axis.text.y =element_text(color="black"),
        plot.title = element_text(family="Courier",
                                  size=rel(1.3),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2), legend.position= " ", legend.text= element_blank(),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(color = "darkblue", face = "bold", size = 9))+
  #theme(aspect.ratio=1) +
  transition_time(importaT2017) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)

#---------------------------------------------------------------------------------------------------------
#filtro por países de hispanoamerica
#---------------------------------------------------------------------------------------------------------
comercioHispano <-datoscomercio %>%filter(origenHispanoamerica=="1" & destinoHisponoamerica=="1"& (codOrigen!=codigoDestino)) %>% select(c(-origenHispanoamerica, -destinoHisponoamerica))%>% distinct()
View(comercioHispano)
#---------------------------------------------------------------------------------------------------------
#filtro con origen Argentina y destino Brasil 
comercioArgBra <-datoscomercio %>%filter((codOrigen=='arg' & codigoDestino== 'bra')) %>% select(c(-origenHispanoamerica, -destinoHisponoamerica))%>% distinct()
View(comercioArgBra)
#---------------------------------------------------------------------------------------------------------
# IMPORTACIÓN EN HISPANOAMERICA
# total de importacion por año, paisorigen, paisdestino y producto OK
totalImportaH<-comercioHispano%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto, colorProducto)%>% summarize(importa=sum(valorImportado))%>% arrange(desc(importa))
View(totalImportaH)
#filtro los valores de importacion= 0
totalImportadoHispano <-totalImportaH%>% filter(importa!=0)%>% arrange(desc(importa))
View(totalImportadoHispano)
tail(totalImportadoHispano)
#-----------------------------------------------------------------------------------------------------
# Ranking para graficos de IMPORTACION EN HISPANOAMERICA
#------------------------------------------------------------------------------------------------------
#Ranking de productos más importados por Argentina en hispanoamerica DURANTE EL PERÍODO 2013-2017
rankingTodos <- totalImportadoHispano%>%filter(codOrigen =="arg")%>% group_by(nombreProducto, colorProducto, anio)%>% summarize(importaXP=sum(importa))%>% arrange(desc(importaXP))
View(rankingTodos)
#Ranking de productos más importados por Argentina en hispanoamerica en 2017
ranking <- totalImportadoHispano%>%filter(codOrigen =="arg" & anio =="2017")%>% group_by(nombreProducto, colorProducto)%>% summarize(importaXP=sum(importa))%>% arrange(desc(importaXP))
View(ranking)

#top 10 de productos con mayor importación de Argentina en hispoanoamerica - AÑO:2017
top10H <-ranking %>% arrange(desc(importaXP))%>% filter(importaXP >112914435)#112914435   105645400
View(top10H)
color <- ranking$colorProducto
View(color)

#------------------------------------------------------------------------------
# graficos importacion a nivel hispanoamérica en el año 2017
#------------------------------------------------------------------------------
#primer gráfico GGANIMATE con paleta de la tabla (colorProducto)
ggplot(top10H, aes(reorder(nombreProducto, importaXP), importaXP, size =(importaXP))) + 
  #geom_point(show.legend = F) + 
  geom_col(aes(fill=nombreProducto)) +
  scale_fill_manual(values =  ranking$colorProducto) +
  coord_flip()+ 
  theme_wsj()+   #theme_wsj()theme_classic() +theme_economist()
  labs(title = "Top 10 de productos importados por Argentina  \n a nivel Hispanoam\u00e9rica",
       subtitle = "Período:2017",
       x = "",
       y = "Valor de importación expresado en doláres",
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(color="black", size=9),  #
        axis.text.y =element_text(color="black", size=9),
        plot.caption = element_text(color = "brown", face ="bold", size = 10, vjust=1),  ##562457
        plot.title = element_text(size=12,
                                  #size=rel(0.4),
                                  vjust=2,
                                  hjust=0.5,
                                  #position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2), legend.position= " ", legend.text= element_blank(),
                                  plot.subtitle = element_text(hjust = 0.5, size = 11))+
                                  
  #theme(aspect.ratio=1)
  transition_time(importaXP) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)
#------------------------------------------------------------------------------   
#primer gráfico GGANIMATE con una paleta más llamativa  
ggplot(top10, aes(reorder(nombreProducto, importaXP), importaXP, size =(importaXP))) + 
geom_col(aes(fill=nombreProducto)) +
  coord_flip()+ 
  theme_economist()+  #theme_stata() +
  labs(title = "Top 10 de productos importados por Argentina \n a nivel de Hispanoam\u00e9rica",
       subtitle = "Período:2017",
       x = "",
       y = "Valor de importación expresado en doláres",
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(vjust = 1.5, hjust=1.4, size=10, color="black"),
        axis.text.y =element_text(color="black"),
        plot.title = element_text(family="Courier",
                                  size=rel(1.3),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2), legend.position= " ", legend.text= element_blank(),
  plot.subtitle = element_text(hjust = 0.5),
  plot.caption = element_text(color = "darkblue", face = "bold", size = 9))+
  #theme(aspect.ratio=1) +
  transition_time(importaXP) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)
#-------------------------------------------------------------------------------
# EXPORTACION MUNDIAL
# total de exportaciones realizadas por Argentina por año, paisdestino y producto, OK
totalExportaMundial<-datoscomercio%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto, colorProducto)%>% summarize(exportaM=sum(valorExportado))%>% 
  filter(exportaM!=0 & (codOrigen!=codigoDestino)& codOrigen=='arg'& nombreProducto!='Sin Especificar')%>%arrange(desc(exportaM))
View(totalExportaMundial)
dim(totalExportaMundial)
tail(totalExportaMundial)

