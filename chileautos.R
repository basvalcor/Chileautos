##########################################################
###################### WWW.CHILEAUTOS.CL #################
##########################################################

install.packages("ggplot2")
install.packages("rvest")

# Usando la librer�a rvest
library('rvest')

# Usando la libreria ggplot2
library('ggplot2')

# Abrir CSV
if(file.exists("fileTextoYFreqchileAutos.txt")){
  print("Abre CSV")
  fileTextoYFreqchileAutos <- read.table(file = "fileTextoYchileAutos.txt", header = TRUE, sep = " ")
}


#========================================================================#

# Se analizar� como prueba con la primera p�gina

# Asignaci�n de una variable a la URL de la p�gina
chileAutos <- 'https://www.chileautos.cl/autos/busqueda?q=Regi%C3%B3n.Metropolitana%20de%20Santiago.'

# Lectura de HTML como variable chileAutos
webpagechileAutos <- read_html(chileAutos)

# Extracci�n del texto contenido en la clase categor�a de la web
contenidochileAutos <- html_nodes(webpagechileAutos,'.cs-inav__filter-item-record')

# Viendo el contenido de la variable contenidochileAutos
print(contenidochileAutos)

# Extrayendo el texto de contenidoYapo
textochileAutos <- html_text(contenidochileAutos)

# Viendo el contenido de la variable textoYapo
print(textochileAutos)

# Contando palabras
unlistTextochileAutos <- unlist(textochileAutos)
tablaTextochileAutos <- table(unlistTextochileAutos)

# Transformando a data framtabla
contachileAutos <- as.data.frame(tablaTextochileAutos)

# Se trabajar� para las primeras 100 p�ginas de la web

# Se crea una lista
todosLasCategorias <- list()

# Recorriendo las p�ginas y obteniendo el dato de la clase
for(i in 1:100){
  print(paste("https://www.chileautos.cl/autos/busqueda?q=Regi%C3%B3n.Metropolitana%20de%20Santiago.",i,sep = ""))
  
  paginaDescargada <- read_html(paste("https://www.yapo.cl/region_metropolitana?ca=15_s&o=",i,sep = ""))
  contenidochileAutos <- html_nodes(webpagechileAutos,'.category')
  texto <- html_text(contenidochileAutos)
  todosLasCategorias <- c(todosLasCategorias,texto)
}

# Contando y pasando a dataframe
tablaTextochileAutos <- table(unlist(todosLasCategorias))
dfTextoYFreqchileAutos <- as.data.frame(tablaTextochileAutos)

# Uniendo dos dataframes por Var1 sumando frecuencias
if(exists("fileTextoYFreqchileAutos")){
  print("Uniendo los DataFrames")
  dfTextoYFreqchileAutos <- aggregate(cbind(Freq) ~ Var1, rbind(dfTextoYFreqchileAutos,dfTextoYFreqchileAutos), sum)
}

#Grafico de Barra de la informaci�n
dfTextoYFreqchileAutos %>%
  ggplot() +
  aes(x = Var1 , y = Freq) +
  geom_bar(stat="identity")

# Guardando informaci�n en txt
write.table(dfTextoYFreqchileAutos, file="fileTextoYFreqchileAutos.txt")
