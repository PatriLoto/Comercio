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
#-------------------------------------------------------------------------------------
#esto sería una buena práctica
data_url <- "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv"
datoscomercio <- readr::read_csv(data_url, col_types = cols())
View(datoscomercio)
#lectura rápida
#comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
#View(comercio_hispanoamerica_mundo)

#sólo los renombro para seguir con el formato que siempre utilizo (primera palabra comienza con minúscula y segunda palabra comienza con mayúscula) 
colnames(datoscomercio) <-c("anio","codOrigen","codigoDestino","paisOrigen", "paisDestino", "codigoProducto", "nombreProducto", "colorProducto", "valorExportado", "valorImportado", "origenHispanoamerica","destinoHisponoamerica")
View(datoscomercio)

#----------------------------------------------------------------------------
#cuáles son los productos importados/exportados
productos <- datoscomercio%>%select(codigoProducto, nombreProducto, colorProducto)%>%arrange (codigoProducto)%>%distinct()
View(productos)

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
#---------------------------------------------------------------------------------------------------------
# EXPORTACION MUNDIAL
# total de exportaciones realizadas por Argentina por año, paisdestino y producto, OK
totalExportaMundial<-datoscomercio%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto, colorProducto)%>% summarize(exportaM=sum(valorExportado))%>% 
  filter(exportaM!=0 & (codOrigen!=codigoDestino)& codOrigen=='arg'& nombreProducto!='Sin Especificar')%>%arrange(desc(exportaM))
View(totalExportaMundial)
dim(totalExportaMundial)
tail(totalExportaMundial)

# EXPORTACIÓN
# total de Exportacion por año, paisorigen, paisdestino y producto - filtro lo valores en cero OK
totalExportaH<-comercioHispano%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto)%>% summarize(exporta=sum(valorExportado))%>% filter(exporta!=0)%>% arrange(desc(exporta))
View(totalExportaH)

#filtro los valores en cero de exportacion
totalExportadoHispano <-totalExportaH%>% filter(exporta!=0)%>% arrange(desc(exporta))
View(totalExportadoHispano)
tail(totalExportadoHispano)

#Ranking de productos más importados por Argentina en hispanoamerica en el año 2017 
rankingTodos <- totalExportaMundial%>%filter(codOrigen =="arg")%>% group_by(nombreProducto, colorProducto, anio)%>% summarize(exportaM=sum(exporta))%>% arrange(desc(exportaM))
View(rankingTodos)

rankingH <- totalExportadoHispano%>%filter(codOrigen =="arg" & anio =="2017")%>% group_by(nombreProducto, colorProducto)%>% summarize(exportaH=sum(exporta))%>% arrange(desc(exportaH))
View(ranking)

#productos con mayor importación nivel mundial
top10 <-ranking %>% arrange(desc(exportaM))%>% filter(exportaM > 1200000) #reemplazar valor exportación)
View(top10)
color <- ranking$colorProducto
View(color)
ranking %>%arrange(desc(exportaM))%>% select(0:10) 

paisesExporta <-datoscomercio %>% group_by(anio, codOrigen)%>% summarize(exporta=sum(valorExportado))%>% arrange(anio,exporta)
View(paisesExporta)
  
paisesHispanoExporta <-datoscomercio %>% group_by(anio, codOrigen)%>% summarize(exporta=sum(valorExportado))%>% arrange(exporta)
View(paisesHispanoExporta)
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
#cambiar lo valores del gráfico por los de exportación

#primer gráfico GGANIMATE con una paleta más llamativa  
ggplot(top10, aes(reorder(nombreProducto, importaXP), importaXP, size =(importaXP))) + 
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


