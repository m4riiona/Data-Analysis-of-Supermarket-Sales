####Introduction to Sampling in R######

install.packages("readxl")
library("readxl")
dd <- read_excel("C:/Users/Anna/Downloads/Base_de_dades_supermercat_V5.xlsx")
install.packages("samplingbook")
library("samplingbook")


sd <- sd(dd$Total) # Se considera como la desviación típica poblacional
N <- nrow(dd) # Tamaño de la población
e <- 0.3 # Margen de error prefijado
samp<-sample.size.mean(e, sd, N, level = 0.95)
samp
set.seed(196) # Fija la semilla de aleatorización para poder reproducir los resultados
muestra <- sample(1:N, 800, replace = TRUE) # Si se quisiera un muestreo sin reemplazo, se utilizaría la sentencia replace=FALSE
datos_muestra <- dd[muestra, ] # Se seleccionan los datos que conforman la muestra
head(datos_muestra)

Smean(datos_muestra$Total, N, level = 0.95)

mean(dd$Total) # Valor de la media poblacional


################MUESTREO ESTRATIFICADO########
library("plyr")
estratos <- ddply(dd, .(City), summarize,
                  media.sl = mean(Total),
                  desv.sl = sd(Total)
)
estratos

stratasize(e, Nh = c(50, 50, 50), Sh = estratos[, 3], level = 0.95)

stratasamp(n = 11, Nh = c(50, 50, 50), Sh = estratos[, 3], type = "prop")
stratasamp(n = 11, Nh = c(50, 50, 50), Sh = estratos[, 3], type = "opt")

set.seed(195) # Fija la semilla de aleatorización
muestra1 <- sample(1:333, 266, replace = TRUE)
muestra2 <- sample(334:666, 267, replace = TRUE)
muestra3 <- sample(667:1000, 267, replace = TRUE) # m.a.s. en cada estrato
muestra_estr <- c(muestra1, muestra2, muestra3)
datos_muestra_estr <- dd[muestra_estr, ] # Selección de los datos que conforman la muestra
datos_muestra_estr

Smean(datos_muestra_estr$Total, N, level = 0.95)

table(cluster_sample$tour)

# Instalar e importar la biblioteca openxlsx
install.packages("openxlsx")
library(openxlsx)

# Exportar el dataframe a Excel
write.xlsx(datos_muestra_estr, "C:/Users/Anna/Downloads/cluster_sample.xlsx")

################ MUESTREO POR CONGLOMERADOS##########
#Hacer este ejemplo reproducible
set.seed(1)
#Esta línea asegura la reproducibilidad de los números aleatorios. Al establecer la semilla en 1, 
#cualquier proceso aleatorio en el código producirá los mismos resultados cada vez que se ejecute.
#crear marco de datos

mean(dd$Total)
sd(dd$Total)

df <- data.frame(tour = rep(1:10, each=80),experience = rnorm(800, mean=7, sd=1))

#Esto crea un marco de datos llamado df con dos columnas: #Esto cremean()a un marco de datos llamado df con dos columnas: 
#"tour" y " experience". La columna "tour" repite los números
#del 1 al 10, cada uno 20 veces, y la columna "experience" se rellena con
#800 números aleatorios generados a partir de una distribución normal con 
#media 7 y desviación típica 1

#Elige al azar 4 grupos turísticos de los 10
clusters <- sample(unique(df$tour), size=80, replace=F)







