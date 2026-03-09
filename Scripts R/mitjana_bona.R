
install.packages("samplingbook") #install the library
library("samplingbook") #load the library 


install.packages("readxl")
library("readxl")
dd <- read_excel("C:/Users/Anna/Downloads/cluster_sample.xlsx")

hist(dd$Total)

sd <- sd(dd$Total)
N <- nrow(dd) # size of dataframe (nºrows)
e <- 0.3 # prefixed margin of error
samp<-sample.size.mean(e, sd, N, level = 0.95) # obtain sample size with desired margin of error and desired lvl of confidence 
samp


dd<- dd[dd$Total>0,]
dd$`Moment del dia`
hist(dd$Total)


sd <- sd(dd$Total)
N <- nrow(dd) # size of dataframe (nºrows)
e <- 0.3 # prefixed margin of error
samp<-sample.size.mean(e, sd, N, level = 0.95) # obtain sample size with desired margin of error and desired lvl of confidence 
samp

set.seed(196) # Fija la semilla de aleatorización para poder reproducir los resultados

muestra <- sample(1:N, 264, replace = TRUE) # Generates a random sample of 25
muestra

ldatos_muestra <- dd[muestra, ] # we get the sample data 
head(ldatos_muestra$Total)

Smean(ldatos_muestra$Total, N, level = 0.95)
mean(dd$Total)
mean(ldatos_muestra$Total)
dd$`Moment del dia`

library("plyr")
estratos <- ddply(dd, .(`Moment del dia`), summarize,
                  media.sl = mean(Total),
                  desv.sl = sd(Total)
)

estratos


dd$`Moment del dia`



conteo_filas <- function(dd, valor) {
  return(sum(dd$`Moment del dia` == valor))
}




numMatí <- conteo_filas(dd, "Matí")
numMigdia <- conteo_filas(dd, "Migdia")
numTarda <- conteo_filas(dd, "Tarda")
numVespre <- conteo_filas(dd, "Vespre")

stratasize(e, Nh = c(numMatí, numMigdia, numTarda, numVespre), Sh = estratos[,2], level = 0.95) # obtain sample size 
stratasamp(n = 800, Nh = c(numMatí, numMigdia, numTarda, numVespre), Sh = estratos[,2], type = "prop") # get sample using proportional method
stratasamp(n = 800, Nh = c(numMatí, numMigdia, numTarda, numVespre), Sh = estratos[,2], type = "opt") # get sample using optimal method

indxMatí <- which(dd$`Moment del dia` == "Matí")
indxMigdia <- which(dd$`Moment del dia` == "Migdia")
indxTarda <- which(dd$`Moment del dia` == "Tarda")
indxVespre <- which(dd$`Moment del dia` == "Vespre")

set.seed(195) # Fija la semilla de aleatorización
muestra1 <- sample(indxMatí, numMatí, replace = TRUE)
muestra2 <- sample(indxMigdia, numMigdia, replace = TRUE)
muestra3 <- sample(indxTarda, numTarda, replace = TRUE) # m.a.s. en cada estrato
muestra4 <- sample(indxVespre, numVespre, replace = TRUE)


muestra_estr <- c(muestra1, muestra2, muestra3, muestra4)
datos_muestra_estr <- dd[muestra_estr, ] # Selección de los datos que conforman la muestra
datos_muestra_estr

Smean(datos_muestra_estr$Total, N, level = 0.95)






hist(dd$Total)
mean(dd$Total)
sd(dd$Total)


K<-10
k1<-1
mitjana<-NULL
desviacio<-NULL

for(k in k1:K){
  
  l1<-1+( (k-1) *40 )
  l2<- 40*k
  
  print(l1)
  print(l2)
  
  muestra <- sample(l1:l2, 26, replace = TRUE) # Generates a random sample of 30
  print(muestra)
  
  dd_muestra <- dd[muestra, ]
  m<-mean(dd_muestra$Total)
  s<-sd(dd_muestra$Total)
  mitjana<-c(mitjana,m)
  desviacio<-c(desviacio,s)
  print(paste("iteracio", k, "mitjana",m,"; desviacio",s))
}

realizationNr<-c(1:K)
dades<-data.frame(realizationNr,mitjana, desviacio)

n <- nrow(dd)
sigma <- sd(dd$Total)

limInfICz<-mitjana-1.95*sigma/sqrt(n) #limit inferior
limSupICz<-mitjana+1.95*sigma/sqrt(n) #limit superior

dades[,4]<-limInfICz
dades[,5]<-limSupICz
names(dades)[4]<-"limInfICz"
names(dades)[5]<-"limSupICz"

mu<-mean(dd$Total)
mus<-rep(mu,10)
dades[,6]<-mus
names(dades)[6]<-"Mu"

plot(limInfICz, realizationNr, pch="[", col="green")
points(limSupICz, realizationNr,pch="]", col="red")
points(mus, realizationNr, pch=15, col="blue")

#might be required when occlusions
points(limInfICz, realizationNr, pch="[", col="green")
points(limSupICz, realizationNr,pch="]", col="red")

NrFailingICs<-length(which(limInfICz>mu))+length(which(limSupICz<mu))
NrFailingICs

