# Resource: https://www.r-graph-gallery.com/
install.packages(c("devtools", "tidyverse", "lubridate","janitor", "data.tree","circlepackeR"))
install.packages("here")
install.packages("gganimate")
install.packages("gifski")
install.packages("png")
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
#library(dplyr)

#-------------------------------------------------------------------------------
# Ideas de análisis con el dataset de #DatosDeMiercoles

# A- Voy a utilizar las burbujas para representar cada categoria con sus subcategorias o componentes. Paquete: data.tree o circlepackeR.
# B- Voy a utilizar el diagrama chord diagram para visibilizar las relaciones recíprocas. Paquete: chorddiag
# C- Voy a utilizar leaflet para armar un mapa básico mostrando por área cantidad de importaciones/exportaciones, seleccionando regiones. Paquete:leaflet
# D - Voy a utilizar Dendrogram, para mostrar jerarquia
#-------------------------------------------------------------------------------
#para pasar los nombres de columnas a minúsculas
#rename_all(str_to_lower) 

datos_path <- here("D:/Patri/RPROJECTS/Comercio")

#esto sería una buena práctica
data_url <- "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv"
datoscomercio <- readr::read_csv(data_url, col_types = cols())
View(datoscomercio)
#lectura rápida
#comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
#View(comercio_hispanoamerica_mundo)
#renombro columnas para acortar nombres

#names(datocomercio)[1] = "anio"

colnames(datoscomercio) <-c("anio","codOrigen","codigoDestino","paisOrigen", "paisDestino", "codigoProducto", "nombreProducto", "colorProducto", "valorExportado", "valorImportado", "origenHispanoamerica","destinoHisponoamerica")
View(datoscomercio)


#cuáles son los productos importados/exportados
productos <- datoscomercio%>%select(codigoProducto, nombreProducto, colorProducto)%>%arrange (codigoProducto)%>%distinct()
View(productos)

productosC <- janitor::clean_names(productos)
write_csv(productos, 'productos.csv')
View(productosC)

#total de productos importados por año, por destino, por producto
# total de importacion realizadas por Argentina por año, paisorigen, paisdestino y producto, OK
totalImportaMundial<-datoscomercio%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto, colorProducto)%>% summarize(importa=sum(valorImportado))%>% 
  filter(importa!=0 & (codOrigen!=codigoDestino)& codOrigen=='arg')%>%arrange(desc(importa))
View(totalImportaMundial)



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
totalImportaH<-comercioHispano%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto, colorProducto)%>% summarize(importa=sum(valorImportado))%>% arrange(desc(importa))
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

  #primer gráfico GGANIMATE con paleta de la tabla 
  ggplot(top10, aes(reorder(nombreProducto, importaXP), importaXP, size =(importaXP))) + 
  #geom_point(show.legend = F) + 
  geom_col(aes(fill=nombreProducto)) +
  scale_fill_manual(values =  ranking$colorProducto) +
  #scale_fill_manual(values = c("#bcd8af2", "#406662","#a1aafb5", "#d1a1bc","#a8c380", "#8abdb6", "#ede788","#a17cb0", "#d05555", "#bf3251", "#5c57d9"))+
  coord_flip()+ 
  theme_bw() +
  labs(title = "Productos con mayor importación por Argentina a nivel de Hispanoam\u00e9rica",
       subtitle = "Año:2017",
       x = "",
       y = "", color=" ") +
  theme(plot.title = element_text(family="Courier",
                                    size=rel(1),        
                                    vjust=2,
                                    hjust=0.7,
                                    position_identity(center),   
                                    face="bold",       
                                    color="black",     
                                    lineheight=1.2), legend.position= " ", legend.text= element_blank())+
    transition_time(importaXP) +
    ease_aes('linear')+
    shadow_mark(alpha = 0.4, size = 2)
  #------------------------------------------------------------------------------   
  #primer gráfico GGANIMATE con una paleta más llamativa  ggplot(top10, aes(reorder(nombreProducto, importaXP), importaXP, size =(importaXP))) + 
    #geom_point(show.legend = F) + 
    geom_col(aes(fill=nombreProducto)) +
    #scale_fill_manual(values =  ranking$colorProducto) +
    #scale_fill_manual(values = c("#bcd8af2", "#406662","#a1aafb5", "#d1a1bc","#a8c380", "#8abdb6", "#ede788","#a17cb0", "#d05555", "#bf3251", "#5c57d9"))+
    coord_flip()+ 
    theme_bw() +
    labs(title = "Productos con mayor importación por Argentina a nivel de Hispanoam\u00e9rica",
         subtitle = "Argentina - Año:2017",
         x = "",
         y = "", color=" ",
         caption="#DatosDeMiercoles por Patricia Loto" ) +
    theme(plot.title = element_text(family="Courier",
                                    size=rel(1),        
                                    vjust=2,
                                    hjust=0.7,
                                    position_identity(center),   
                                    face="bold",       
                                    color="black",     
                                    lineheight=1.2), legend.position= " ", legend.text= element_blank())+
    transition_time(importaXP) +
    ease_aes('linear')+
    shadow_mark(alpha = 0.4, size = 2)
  
  
  
  
  
 #------------------------------------------------------------------------------   
  ggplot(rankingTodos, aes(nombreProducto, im, size = pop, colour = country)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    facet_wrap(~continent) +
    # Here comes the gganimate specific bits
    labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
    transition_time(year) +
    ease_aes('linear')
#bcd8af
2
#406662
3
#a1aafb
4

5
#a8c380
6
#8abdb6
7
#ede788
8
#a17cb0
9
#d05555
10
#bf3251
11
#5c57d9
12
#74c0e2
13
#4d6fd0
14
#549e95
15
#993f7b
16
#dc8e7a
17
#d6c650
18
#635b56
19
#872a41
20
#7454a6
21
#7485aa
22
#1c26b3

#productos más importados


anime_scores %>% 
  arrange(rank) %>% 
  slice(1:20) %>% 
  ggplot(aes(reorder(title, score), score, colour = title, size = log(scored_by))) + 
  geom_point(show.legend = F) + 
  coord_flip() + 
  labs(title = "Top 20 Anime By Score on MyAnimeList.net",
       subtitle = "Size of dots indicate the credibility of an anime score",
       x = "",
       y = "Score (1 To 10)")

#rankingproductos <-ranking %>% group_by(nombreProducto)%>% summarize(totalXPro = sum(importa)) %>% arrange(desc(importa))
#View(rankingproductos) 

#---------------------------------------------------------------------------------------------------------

# EXPORTACIÓN
# total de Exportacion por año, paisorigen, paisdestino y producto - filtro lo valores en cero OK
totalExportaH<-comercioHispano%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto)%>% summarize(exporta=sum(valorExportado))%>% filter(exporta!=0)%>% arrange(desc(exporta))
View(totalExportaH)




rankingproductos <-ranking %>% select(- anio)%>% group_by(nombreProducto)%>% distinct() %>% arrange(desc(importa))
View(rankingproductos) 


anime_scores %>% 
  arrange(rank) %>% 
  slice(1:20) %>% 
  ggplot(aes(reorder(title, score), score, colour = title, size = log(scored_by))) + 
  geom_point(show.legend = F) + 
  coord_flip() + 
  labs(title = "Top 20 Anime By Score on MyAnimeList.net",
       subtitle = "Size of dots indicate the credibility of an anime score",
       x = "",
       y = "Score (1 To 10)")


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