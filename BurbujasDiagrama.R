install.packages(c("devtools", "tidyverse","janitor", "lubridate"))
install.packages(c("data.tree", "circlepackeR"))
install.packages("here")
# WARNING: el paquete circlepackeR no está disponible par la versión 3.6.0 de rstudio, por lo que es necesario
#instalar la versión actualizada: devtools::install_github("jeromefroe/circlepackeR")
devtools::install_github("jeromefroe/circlepackeR")
library(readr)
library(tidyverse)
library(janitor)
library(lubridate)
library(data.tree)
library(circlepackeR)
library(here)

#----------------------------------------------------------------------------------------------------------
# A- voy a utilizar las burbujas para representar cada categoria con sus subcategorias o componentes.y quizás,
#porcentaje de importacion/ exportación de algún país en particular. Paquete: data.tree o circlepackeR.
#-------------------------------------------------------------------------------

datos_path <- here("/PROYECTOS_CON_R/ComercioInternacional")
#esto sería una buena práctica
data_url <- "https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv"
datoscomercio <- readr::read_csv(data_url, col_types = cols())
View(datoscomercio)
#lectura rápida
#comercio_hispanoamerica_mundo <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-01/comercio_hispanoamerica_mundo_agregado.csv")
#View(comercio_hispanoamerica_mundo)
#renombro columnas para acortar nombres

names(datocomercio)[1] = "anio"
colnames(datoscomercio) <-c("anio","codOrigen","codigoDestino","paisOrigen", "paisDestino", "codigoProducto", "nombreProducto", "colorProducto", "valorExportado", "valorImportado", "origenHispanoamerica","destinoHisponoamerica")
View(datoscomercio)

productos <- datoscomercio%>%select(codigoProducto, nombreProducto, colorProducto)%>%arrange (codigoProducto)%>%distinct()
View(productos)

productosC <- janitor::clean_names(productos)
write_csv(productos, '/PROYECTOS_CON_R/ComercioInternacional/productos.csv')
View(productosC)

comercioHispano <-datoscomercio %>%filter(origenHispanoamerica=="1" & destinoHisponoamerica=="1")%>% select(c(-origenHispanoamerica, -destinoHisponoamerica))%>% distinct()
View(comercioHispano)

totalImportaH<-comercioHispano%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino)%>% filter(codOrigen!=codigoDestino)%>% summarize(importa=sum(valorImportado))%>% arrange(anio, importa)
View(totalImportaH)

totalImportaHProducto<-comercioHispano%>%group_by(anio, codOrigen, paisOrigen, codigoDestino, paisDestino, nombreProducto)%>% filter(codOrigen!=codigoDestino)%>% 
  summarize(importa=sum(valorImportado))%>% arrange(importa)
View(totalImportaHProducto)

ranking <- totalImportaHProducto%>%filter(codOrigen =="arg" & anio =="2017")%>% arrange(desc(importa))
View(ranking)

rankingproductos <-ranking %>% group_by(nombreProducto)%>% summarize(totalXPro = sum(importa)) %>% arrange(desc(importa))
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

select(codorigen='arg' )

# A - circlepackeR
#-------------------------------------------------------------------------------
# El Ejemplo funciona correctamente.
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
View(population)

# Make the plot
circlepackeR(population, size = "value")

# You can custom the minimum and maximum value of the color range.
circlepackeR(population, size = "value", color_min = "hsl(56,80%,80%)", color_max = "hsl(341,30%,40%)")



crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2008.csv", header=TRUE, sep="\t")
p <- ggplot(crime, aes(murder,burglary,size=population, label=state))
p <- p+geom_point(colour="red") +scale_area(to=c(1,20))+geom_text(size=3)
p + xlab("Murders per 1,000 population") + ylab("Burglaries per 1,000")