install.packages(c("devtools", "tidyverse", "lubridate","janitor","data.tree", "leaflet"))
#install.packages("devtools")
#install.packages("circlepackeR")
devtools::install_github("jeromefroe/circlepackeR")
install.packages("data.tree")
install.packages("leaflet")
install.packages("chorddiag")
#library(readxl)
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(devtools)

library(data.tree)
library(leaflet)
#library(dplyr)

#-------------------------------------------------------------------------------
# Ideas de análisis con el dataset de #DatosDeMiercoles

# A- Voy a utilizar las burbujas para representar cada categoria con sus subcategorias o componentes. Paquete: data.tree o circlepackeR.
# B- Voy a utilizar el diagrama chord diagram para visibilizar las relaciones recíprocas. Paquete: chorddiag
# C- Voy a utilizar leaflet para armar un mapa básico mostrando por área cantidad de importaciones/exportaciones, seleccionando regiones. Paquete:leaflet
# D - Dendrogram, similar to the circleparcke,también muestra jerarquia
#-------------------------------------------------------------------------------
# C- Voy a utilizar leaflet para armar un mapa básico mostrando por área cantidad de importaciones/exportaciones, seleccionando regiones. Paquete:leaflet
#-------------------------------------------------------------------------------

# D - Dendrogram, similar to the circleparcke,también muestra jerarquia

comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
View(comercio_hispanoamerica_mundo)

#-------------------------------------------------------------------------------
# A - circlepackeR
#-------------------------------------------------------------------------------
# create a nested data frame giving the info of a nested dataset:
data=data.frame(
  root=rep("root", 15),
  group=c(rep("group A",5), rep("group B",5), rep("group C",5)), 
  subgroup= rep(letters[1:5], each=3),
  subsubgroup=rep(letters[1:3], 5),
  value=sample(seq(1:15), 15)
)

# Change the format. This use the data.tree library. This library needs a column that looks like root/group/subgroup/..., so I build it
library(data.tree)
data$pathString <- paste("world", data$group, data$subgroup, data$subsubgroup, sep = "/")
population <- as.Node(data)

# Make the plot
circlepackeR(population, size = "value")

# You can custom the minimum and maximum value of the color range.
circlepackeR(population, size = "value", color_min = "hsl(56,80%,80%)", color_max = "hsl(341,30%,40%)")

#-------------------------------------------------------------------------------
# C -Leaflet
#-------------------------------------------------------------------------------

#ejemplo
data(quakes)
quakes =  head(quakes, 100)

# Create a color palette with handmade bins.
mybins=seq(4, 6.5, by=0.5)
mypalette = colorBin( palette="YlOrBr", domain=quakes$mag, na.color="transparent", bins=mybins)

# Prepar the text for the tooltip:
mytext=paste("Depth: ", quakes$depth, "<br/>", "Stations: ", quakes$stations, "<br/>", "Magnitude: ", quakes$mag, sep="") %>%
  lapply(htmltools::HTML)

# Final Map
leaflet(quakes) %>% 
  addTiles()  %>% 
  setView( lat=-27, lng=170 , zoom=4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~long, ~lat, 
                   fillColor = ~mypalette(mag), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                   label = mytext,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addLegend( pal=mypalette, values=~mag, opacity=0.9, title = "Magnitude", position = "bottomright" )


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