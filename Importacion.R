# Resource: https://www.r-graph-gallery.com/
install.packages(c("devtools", "tidyverse", "lubridate","janitor", "data.tree","circlepackeR"))
install.packages("here")
install.packages("gganimate")
install.packages("gifski")
install.packages("png")
#Paleta LaCroixColoR (beta)
devtools::install_github("johannesbjork/LaCroixColoR")
install.packages("extrafont")
#library(readxl)

#library(ggplot2)
library(devtools)
library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
library(data.tree)
#library(circlepackeR)
library(here)
library(gganimate)
library(gifski)
library(png)
library(LaCroixColoR)
#library(dplyr)
#fuentes
library(extrafont)
# se debe instalar la fuente en wintendo
# font_import(paths = "R/2019/2019-04-17/")
loadfonts(device = "win")
#-------------------------------------------------------------------------------
# Ideas de análisis con el dataset de #DatosDeMiercoles

# A- Voy a utilizar las burbujas para representar cada categoria con sus subcategorias o componentes. Paquete: data.tree o circlepackeR.
# B- Voy a utilizar el diagrama chord diagram para visibilizar las relaciones recíprocas. Paquete: chorddiag
# C- Voy a utilizar leaflet para armar un mapa básico mostrando por área cantidad de importaciones/exportaciones, seleccionando regiones. Paquete:leaflet
# D - Voy a utilizar Dendrogram, para mostrar jerarquia
#-------------------------------------------------------------------------------
#para pasar los nombres de columnas a minúsculas
#rename_all(str_to_lower) 

#esto sería una buena práctica
data_url <- "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv"
datoscomercio <- readr::read_csv(data_url, col_types = cols())
View(datoscomercio)
#lectura rápida
#comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
#View(comercio_hispanoamerica_mundo)
#renombro columnas para acortar nombres

colnames(datoscomercio) <-c("anio","codOrigen","codigoDestino","paisOrigen", "paisDestino", "codigoProducto", "nombreProducto", "colorProducto", "valorExportado", "valorImportado", "origenHispanoamerica","destinoHisponoamerica")
View(datoscomercio)

#cuáles son los productos importados/exportados
productos <- datoscomercio%>%select(codigoProducto, nombreProducto, colorProducto)%>%arrange (codigoProducto)%>%distinct()
View(productos)

productosC <- janitor::clean_names(productos)
write_csv(productos, 'productos.csv')
View(productosC)
#-------------------------------------------------------------------------
#07/05/2019
#--------------------------------------------------------------------
#total de productos importados por año, por destino, por producto
# total de importacion realizadas por Argentina por año, paisorigen, paisdestino y producto, OK
totalImportaMundial<-datoscomercio%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto, colorProducto)%>% summarize(importa=sum(valorImportado))%>% 
  filter(importa!=0 & (codOrigen!=codigoDestino)& codOrigen=='arg')%>%arrange(desc(importa))
View(totalImportaMundial)
#IMPORTADOS MUNDIAL
#ranking productos importados por Argentina a nivel mundial en el año 2017 
rankingMundial <- totalImportaMundial%>%filter(codOrigen =="arg")%>% group_by(nombreProducto, colorProducto, anio)%>% summarize(importaMU=sum(importa))%>% arrange(desc(importaMU))
View(rankingMundial)

rankingMundial10 <- rankingMundial%>% filter (nombreProducto %in% c("Maquinaria","Transporte","Productos Químicos",                                                                   "Productos Minerales", "Metales", "Plásticos y Gomas", "Instrumentos", "Textiles", "Productos Vegetales","Miscelánea"))
View(rankingMundial10)


#IMPORTADOS REGIONAL
#ranking productos importados por Argentina en el año 2017 
rankingM2017 <- totalImportahispano%>%filter(anio =="2017")%>% group_by(nombreProducto, colorProducto)%>% summarize(importaXP=sum(importa))%>% arrange(desc(importaXP))
View(rankingM)

#-----------------------------------------------------------------------
#PALETAS DE COLORES de LaCroixColoR (beta) de johannesbjork
#-----------------------------------------------------------------------
p<-lacroix_palette("PassionFruit", n = 10, type = "continuous")
p1<-lacroix_palette("Pamplemousse", n = 10, type = "continuous")
p2<-lacroix_palette("PeachPear", n = 10, type = "continuous")
p3<-lacroix_palette(type = "paired")
#-------------------------------------------------------------------------------
#FACET GRID ESTATICO - accidental__aRt.Son demasiados productos, por que sale desfasado
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = ranking$colorProducto) +
  scale_color_viridis_d()+
  scale_size(range = c(2, 12)) +
  theme_set(theme_minimal())+
  #theme_set(theme_gray())+
  #scale_x_log10() +
  facet_grid(nombreProducto ~.) +
  labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el período 2013-2017 ", #Año: {round(frame_time,0)}
       x = 'Año', 
       y = 'Valor de importación en $',
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4, size = 8),
        axis.text.y =element_text(vjust = 2, hjust=1.4, size = 8),
        plot.title = element_text(family="Verdana",
                                  size=rel(1),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2),
        plot.caption = element_text(color = "maroon", face = "bold"))+
  # Here comes the gganimate specific bits
  transition_time(anio) +
  #shadow_mark(alpha = 0.7, size = 0.7)+           #DEJA LAS MARCAS
  shadow_wake(wake_length = 0.1)

#-------------------------------------------------------------------------------
# FACET WRAP ESTÁTICO con Paleta PassionFruit de @johannesbjork  -  PUBLICADO
#-------------------------------------------------------------------------------
  #Paleta PassionFruit de @johannesbjork 
  p<-lacroix_palette("PassionFruit", n = 10, type = "continuous")
  
  ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto))+
  geom_point(show.legend = F)+
  scale_colour_manual(values = p)+
  scale_size(range = c(2, 12))+
  theme_set(theme_minimal())+
  labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el período 2013-2017 ", 
       x = 'Año', 
       y = 'Valor de importación en $',
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4, size = 8),
        axis.text.y =element_text(vjust = 2, hjust=1.4, size = 8),
        plot.title = element_text(family="Verdana", size=rel(1), vjust=2,hjust=0.5,position_identity(center),face="bold",       
                                  color="black",     
                                  lineheight=1.2),
        plot.caption = element_text(color = "maroon", face = "bold"),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        rect = element_rect(fill = "black", color = "black"),
        text = element_text(family = "Verdana", colour = "white", size = 12)) +
  facet_wrap(~nombreProducto)
#-------------------------------------------------------------------------------
#FACET WRAP ANIMACIÓN - Paleta: PassionFruit PUBLICAR
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = ranking$colorProducto) +
  scale_colour_manual(values = p) +
  #scale_color_viridis_d()+
  scale_size(range = c(2, 12)) +
  theme_set(theme_minimal())+
  #scale_x_log10() +
  facet_wrap(~nombreProducto) +
  labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el período 2013-2017 ", #Año: {round(frame_time,0)}
       x = 'Año', 
       y = 'Valor de importación en $',
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4, size = 8),
        axis.text.y =element_text(vjust = 2, hjust=1.4, size = 8),
        plot.title = element_text(family="Verdana",
                                  size=rel(1),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2),
        plot.caption = element_text(color = "maroon", face = "bold"))+
  # Here comes the gganimate specific bits
  transition_time(anio) +
  shadow_mark(alpha = 1, size = 1.2)+           #DEJA LAS MARCAS
  #shadow_wake(wake_length = 0.3)
  #-------------------------------------------------------------------------------
#FACET WRAP ANIMACIÓN - Paleta: PeachPear PUBLICAR
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = ranking$colorProducto) +
  scale_colour_manual(values = p2) +
  #scale_color_viridis_d()+
  scale_size(range = c(2, 12)) +
  theme_set(theme_minimal())+
  #theme_set(theme_gray())+
  #scale_x_log10() +
  facet_wrap(~nombreProducto) +
  labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el período 2013-2017 ", #Año: {round(frame_time,0)}
       x = 'Año', 
       y = 'Valor de importación en $',
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4, size = 8),
        axis.text.y =element_text(vjust = 2, hjust=1.4, size = 8),
        plot.title = element_text(family="Verdana",
                                  size=rel(1),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2),
        plot.caption = element_text(color = "orange", face = "bold"))+
  # Here comes the gganimate specific bits
  transition_time(anio) +
  #shadow_mark(alpha = 1, size = 1.2)+           #DEJA LAS MARCAS
  shadow_wake(wake_length = 0.4)
  
#-------------------------------------------------------------------------------
#FACET WRAP CON ANIMACIÓN Paleta: viridis con theme black - se podría publicar
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = ranking$colorProducto) +
  scale_color_viridis_d()+
  scale_size(range = c(2, 12)) +
  theme_set(theme_minimal())+
  #theme_set(theme_gray())+
  #scale_x_log10() +
  facet_wrap(~nombreProducto) +
  labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el Año: {round(frame_time,0)} ", 
       x = 'Año', 
       y = 'Valor de importación en $',
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4, size = 8),
        axis.text.y =element_text(vjust = 2, hjust=1.4, size = 8),
        plot.title = element_text(family="Verdana",
                                  size=rel(1),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="white",     
                                  lineheight=1.2),
        plot.caption = element_text(color = "maroon", face = "bold"),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        rect = element_rect(fill = "black", color = "black"),
        text = element_text(family = "Roboto Condensed", colour = "white", size = 12))+
        #Here comes the gganimate specific bits
  transition_time(anio) +
  shadow_mark(alpha = 0.7, size = 0.7)          #DEJA LAS MARCAS
  #shadow_wake(wake_length = 0.3)

  
  
  
  
#-------------------------------------------------------------------------------
#FACET GRID
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = ranking$colorProducto) +
  scale_color_viridis_d()+
  scale_size(range = c(2, 12)) +
  theme_set(theme_minimal())+
  #theme_set(theme_gray())+
  #scale_x_log10() +
  facet_grid(~nombreProducto) +
  labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el Año: {round(frame_time,0)}", 
       x = 'Año', 
       y = 'Valor de importación en $',
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(angle= 60, vjust = 1.5, hjust=1.4), 
        plot.title = element_text(family="Verdana",
                                  size=rel(1),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2),
        plot.caption = element_text(color = "maroon", face = "bold"))+
  # Here comes the gganimate specific bits
  transition_time(anio) +
  #shadow_mark(alpha = 0.7, size = 0.7)+           #DEJA LAS MARCAS
  shadow_wake(wake_length = 0.1)
  
   
#-------------------------------------------------------------------------------
#CUADRO COMPLETO - se puede publicar - black
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = T) +
  #scale_colour_manual(values = ranking$colorProducto) +
  #scale_color_viridis_d()+
  scale_colour_manual(values = p2)+
  scale_size(range = c(2, 12)) +
  theme_set(theme_bw())+
  #theme_set(theme_gray()) +
  labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el Año: {round(frame_time,0)}", 
       x = 'Año', 
       y = 'Valor expresado en doláres $',
       colour= "",
       size= "Importación en millones",
       colour= "",
       size= "Importación en millones",
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(axis.text.x =element_text(angle=45,vjust = 1.5, hjust=1.4,size=9,  color="white"),
        axis.text.y =element_text(vjust = 1.5, hjust=1.4,size=9,  color="white"),
                                  plot.title = element_text(family="Verdana",
                                  size=rel(1),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="white",     
                                  lineheight=1.2),
        plot.caption = element_text(color = "maroon", face = "bold"),
        legend.position = "none",
       # panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        rect = element_rect(fill = "lightblue"),
        text = element_text(family = "Roboto Condensed", colour = "white", size = 12))
  +
  
  
  # Here comes the gganimate specific bits
  transition_time(anio) +
  shadow_mark(alpha = 1, size = 1.5)+     
  shadow_wake(wake_length = 0.1)
  
  
  #-------------------------------------------------------------------------------
  #CUADRO COMPLETO - se puede publicar 
  #-------------------------------------------------------------------------------
  ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
    geom_point(alpha = 0.7, show.legend = T) +
    #scale_colour_manual(values = ranking$colorProducto) +
    #scale_color_viridis_d()+
    scale_colour_manual(values = p2)+
    scale_size(range = c(2, 12)) +
    theme_set(theme_bw())+
    #theme_set(theme_gray()) +
    labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el Año: {round(frame_time,0)}", 
         x = 'Año', 
         y = 'Valor expresado en doláres $',
         colour= "",
         size= "Importación en millones",
         caption="#DatosDeMiercoles por Patricia Loto") +
    theme(axis.text.x =element_text(vjust = 1.5, hjust=1.4,size=9,  color="white"),
          axis.text.y =element_text(vjust = 1.5, hjust=1.4,size=9,  color="white"),
          plot.title = element_text(family="Verdana",
                                    size=rel(1),        
                                    vjust=2,
                                    hjust=0.5,
                                    position_identity(center),   
                                    face="bold",       
                                    color="white",     
                                    lineheight=1.2),
          plot.caption = element_text(color = "maroon", face = "bold"))+
    # Here comes the gganimate specific bits
    transition_time(anio) +
    shadow_mark(alpha = 1, size = 1.5)+     
    shadow_wake(wake_length = 0.1)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#CUADRO COMPLETO 
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = F) +
  #scale_colour_manual(values = ranking$colorProducto) +
  scale_color_viridis_d()+
  scale_size(range = c(2, 12)) +
  theme_set(theme_classic())+
  #theme_set(theme_gray()) +
  labs(title = "Evolución de los productos importados por Argentina \n a nivel Mundial en el Año: {round(frame_time,0)}", 
       x = 'Año', 
       y = 'Valor de importación en $',
       caption="#DatosDeMiercoles por Patricia Loto") +
  theme(plot.title = element_text(family="Verdana",
                                  size=rel(1),        
                                  vjust=2,
                                  hjust=0.5,
                                  position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2),
        plot.caption = element_text(color = "purple", face = "bold"))+
  # Here comes the gganimate specific bits
  transition_time(anio) +
  #shadow_mark(alpha = 1, size = 1.2)+     
  shadow_wake(wake_length = 0.1)+
  #view_follow(fixed_x = TRUE)

  #ease_aes('linear')+
  #view_follow(fixed_y = TRUE)

#---------------------------------------------------------------------------------------------------------
#filtro por países de hispanoamerica, queda excluido brasil
#---------------------------------------------------------------------------------------------------------
comercioHispano <-datoscomercio %>%filter(origenHispanoamerica=="1" & destinoHisponoamerica=="1"& (codOrigen!=codigoDestino)) %>% select(c(-origenHispanoamerica, -destinoHisponoamerica))%>% distinct()
View(comercioHispano)
#---------------------------------------------------------------------------------------------------------
#filtro con origen Argentina y destino Brasil 
comercioArgBra <-datoscomercio %>%filter((codOrigen=='arg' & codigoDestino== 'bra')) %>% select(c(-origenHispanoamerica, -destinoHisponoamerica))%>% distinct()
View(comercioArgBra)
#---------------------------------------------------------------------------------------------------------
# IMPORTACIÓN
# total de importacion por año, paisorigen, paisdestino y producto OK
totalImportaH<-comercioHispano%>% filter(codOrigen =='arg')%>% group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto, colorProducto)%>% summarize(importa=sum(valorImportado))%>% arrange(desc(importa))
View(totalImportaH)
#filtro los valores en cero de importacion
totalImportadoHispano <-totalImportaH%>% filter(importa!=0)%>% arrange(desc(importa))
View(totalImportadoHispano)
tail(totalImportadoHispano)

#Ranking de productos más importados por Argentina en hispanoamerica en el año 2017 
rankingTodos <- totalImportadoHispano%>%filter(codOrigen =="arg")%>% group_by(nombreProducto, colorProducto, anio)%>% summarize(importaXP=sum(importa))%>% arrange(desc(importaXP))
View(rankingTodos)

ranking <- totalImportadoHispano%>%filter(codOrigen =="arg" & anio =="2017")%>% group_by(nombreProducto, colorProducto)%>% summarize(importaXP=sum(importa))%>% arrange(desc(importaXP))
View(ranking)

#productos con mayor importación
top10 <-ranking %>% arrange(desc(importaXP))%>% filter(importaXP > 105645400)
View(top10)
color <- ranking$colorProducto
View(color)
ranking %>%arrange(desc(importaXP))%>% select(0:10) 


#---------------------------------------------------------------------------------------------------------

# EXPORTACIÓN
# total de Exportacion por año, paisorigen, paisdestino y producto - filtro lo valores en cero OK
totalExportaH<-comercioHispano%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto)%>% summarize(exporta=sum(valorExportado))%>% filter(exporta!=0)%>% arrange(desc(exporta))
View(totalExportaH)

rankingproductos <-ranking %>% select(- anio)%>% group_by(nombreProducto)%>% distinct() %>% arrange(desc(importa))
View(rankingproductos) 

totalExportaH<-comercioHispano%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino)%>% filter(codOrigen!=codigoDestino)%>% summarize(exporta=sum(valorExportado))%>% arrange(anio, importa)
View(totalExportaH)


paisesImporta <-datoscomercio %>% select(anio,codOrigen,codigoDestino,paisOrigen, paisDestino, codigoProducto, nombreProducto, colorProducto, valorExportado, valorImportado)%>% 
  group_by(anio, codOrigen)%>% summarize(importa=sum(valorImportado))%>% arrange(anio, importa)

View(paisesImporta) 

paisesExporta <-datoscomercio %>% group_by(anio, codOrigen)%>% summarize(exporta=sum(valorExportado))%>% arrange(anio,exporta)
View(paisesExporta)

paisesHispanoImporta <-datoscomercio %>% select(datoscomercio, -origenHispanoamerica, -destinoHisponoamerica)%>% group_by(anio, codOrigen)%>% summarize(importa=sum(valorImportado))%>% arrange(importa)
View(paisesHispanoImporta)  
paisesHispanoExporta <-datoscomercio %>% group_by(anio, codOrigen)%>% summarize(exporta=sum(valorExportado))%>% arrange(exporta)
View(paisesHispanoExporta)




#-------------------------------------------------------------------------------
# D - Dendrogram, similar to the circleparcke
#-------------------------------------------------------------------------------
library(ggraph)
library(igraph)
library(tidyverse)
library(RColorBrewer) 
# create a data frame giving the hierarchical structure of your individuals
d1=data.frame(from="origin", to=paste("group", seq(1,10), sep=""))
d2=data.frame(from=rep(d1$to, each=10), to=paste("subgroup", seq(1,100), sep="_"))
edges=rbind(d1, d2)

# create a vertices data.frame. One line per object of our hierarchy
vertices = data.frame(
  name = unique(c(as.character(edges$from), as.character(edges$to))) , 
  value = runif(111)
) 
# Let's add a column with the group of each name. It will be useful later to color points
vertices$group = edges$from[ match( vertices$name, edges$to ) ]


#Let's add information concerning the label we are going to add: angle, horizontal adjustement and potential flip
#calculate the ANGLE of the labels
vertices$id=NA
myleaves=which(is.na( match(vertices$name, edges$from) ))
nleaves=length(myleaves)
vertices$id[ myleaves ] = seq(1:nleaves)
vertices$angle= 90 - 360 * vertices$id / nleaves

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
vertices$hjust<-ifelse( vertices$angle < -90, 1, 0)

# flip angle BY to make them readable
vertices$angle<-ifelse(vertices$angle < -90, vertices$angle+180, vertices$angle)

# Create a graph object
mygraph <- graph_from_data_frame( edges, vertices=vertices )

# Make the plot
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, angle = angle, hjust=hjust, colour=group), size=2.7, alpha=1) +
  geom_node_point(aes(filter = leaf, x = x*1.07, y=y*1.07, colour=group, size=value, alpha=0.2)) +
  scale_colour_manual(values= rep( brewer.pal(9,"Paired") , 30)) +
  scale_size_continuous( range = c(0.1,10) ) +
  theme_void() +
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"),
  ) +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
