# Resource: https://www.r-graph-gallery.com/
install.packages(c("devtools", "tidyverse", "lubridate","janitor", "gifski","gganimate", "png"))

#Paleta LaCroixColoR (beta)
devtools::install_github("johannesbjork/LaCroixColoR")
install.packages("extrafont")

library(devtools)
library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
library(gganimate)
library(gifski)
library(png)
library(LaCroixColoR)
#fuentes
library(extrafont)


# se debe instalar la fuente en wintendo
# font_import(paths = "R/2019/2019-04-17/")
loadfonts(device = "win")

#-------------------------------------------------------------------------------
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

rankingMundial10 <- rankingMundial%>% filter (nombreProducto %in% c("Maquinaria","Transporte","Productos Químicos","Productos Minerales", "Metales", "Plásticos y Gomas", "Instrumentos", "Textiles", "Productos Vegetales","Miscelánea"))
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
#FACET WRAP ESTATICO - Paleta: viridis - se puede PUBLICAR
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = F) +
  scale_colour_manual(values = ranking$colorProducto) +
  #cale_colour_manual(values = p) +
  scale_color_viridis_d()+
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
        plot.caption = element_text(color = "maroon", face = "bold")) 
         # + scale_x_continuous(labels=dollar_format(prefix="$")) 
         # theme_elegante())
  # Here comes the gganimate specific bits
  transition_time(anio) +
  #shadow_mark(alpha = 0.7, size = 0.7)+           #DEJA LAS MARCAS
  shadow_wake(wake_length = 0.3)

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
  facet_wrap(~nombreProducto) +
  labs(title = "Evolución de los productos importados por Argentina a nivel Mundial en el período 2013-2017",                      #Año: {round(frame_time,0)}
       #subtitle= "Año:",
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
#FACET WRAP CON ANIMACIÓN Paleta, con theme black - se podría publicar
#-------------------------------------------------------------------------------
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = F) +
  scale_colour_manual(values = p) +
  #scale_color_viridis_d()+
  scale_size(range = c(2, 12)) +
  theme_set(theme_minimal())+
  #theme_set(theme_gray())+
  #scale_x_log10() +
  facet_wrap(~nombreProducto) +
  labs(title = "Evolución del top 10 de productos importados por Argentina a nivel Mundial en el período 2013-2017 ", #Año: {round(frame_time,0)}
       subtitle="Año: {round(frame_time,0)}",
       x = 'Año', 
       y = 'Valor de importación en doláres',
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
        text = element_text(family = "Roboto Condensed", colour = "white", size = 12))+ #+
  transition_time(anio) +
  shadow_mark(alpha = 1, size = 1.2)          
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
ggplot(rankingMundial10, aes(anio, importaMU, size = importaMU, colour = nombreProducto, label= nombreProducto)) +
  geom_point(alpha = 0.7, show.legend = T) +
  #scale_color_viridis_d()+
  scale_colour_manual(values = p)+
  scale_size(range = c(2, 12)) +
  theme_set(theme_bw())+
  #theme_set(theme_gray()) +
    geom_text(
      aes(label = nombreProducto),
      #data = lealtades,
      color = "white",
      size=3,
      alpha = 0.80) + 
  labs(title = "Evolución del top 10 de productos importados por Argentina a nivel Mundial", 
       subtitle="Año: {round(frame_time,0)}",
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
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
       rect = element_rect(fill = "black", color = "black"),
        text = element_text(family = "Roboto Condensed", colour = "white", size = 12))
  #+
  
  
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

