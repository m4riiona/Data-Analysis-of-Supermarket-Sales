
install.packages("read.xl")
library(readxl)
dd <- read_excel("C:/Users/Anna/Downloads/cluster_sample.xlsx")

#mas grandaria 1000 d'una normal 60, 10
n<-800
mu<-mean(dd$Total)
sigma<-sd(dd$Total) #standard deviation 

#farem K realitzacions d'aquesta mostra
K<-100

#inicialitzem estructures
k1<-1
mitjana<-numeric(K)
desviacio<-numeric(K)

for (k in k1:K){
  
  l1<-1+( (k-1) *40)
  l2<-40*k
 
  sample_indices <- sample(1:nrow(dd), n, replace = TRUE)
  sample_data <- dd$Total[sample_indices]
  
  mitjana[k] <- mean(sample_data)
  desviacio[k] <- sd(sample_data)
  
  print(paste("iteracio", k, "mitjana", mitjana[k], "; desviacio", desviacio[k]))
}


#recollim en un data frame tots els resultats
realizationNr<-c(1:K)
dades<-data.frame(realizationNr,mitjana, desviacio)
dades

#calculem extrems de l'interval de confianÃ§a per mu a nivell 0.95 coneguent la sigma
limInfICz<-mitjana-1.95*sigma/sqrt(n) #limit inferior

limSupICz<-mitjana+1.95*sigma/sqrt(n) #limit superior

#els posem al dataframe
dades[,4]<-limInfICz
dades[,5]<-limSupICz
names(dades)[4]<-"limInfICz"
names(dades)[5]<-"limSupICz"
dades
#ara calculem els limits de l'interval de confianÃ§a per mu a nivell 0.95 SI NO CONEGUESSIM sigma
degreesOfFreedom<-n-1
confianza<-0.95
alfa<-1-confianza
nivell<-1-(alfa/2)
tcritic<-qt(nivell,degreesOfFreedom)

limInfICt<-mitjana-tcritic*desviacio/sqrt(n)

limSupICt<-mitjana+tcritic*desviacio/sqrt(n)

#  qchisq(nivell,degreesOfFreedom)

#i se posen a la base de dades
dades[,6]<-limInfICt
dades[,7]<-limSupICt

names(dades)[6]<-"limInfICt"
names(dades)[7]<-"limSupICt"

plot(limInfICz, realizationNr, pch="[", col="green", xlim=c(min(limInfICz, limSupICz), max(limInfICz, limSupICz)))
points(limSupICz, realizationNr, pch="]", col="red")
points(rep(mu, K), realizationNr, pch=15, col="blue")

# Count the number of failing intervals for known sigma
NrFailingICsZ <- length(which(limInfICz > mu)) + length(which(limSupICz < mu))
NrFailingICsZ

# Plotting the t-distribution confidence intervals
plot(limInfICt, realizationNr, pch="[", col="green", xlim=c(min(limInfICt, limSupICt), max(limInfICt, limSupICt)))
points(limSupICt, realizationNr, pch="]", col="red")
points(rep(mu, K), realizationNr, pch=15, col="blue")

# Count the number of failing intervals for unknown sigma
NrFailingICsT <- length(which(limInfICt > mu)) + length(which(limSupICt < mu))
NrFailingICsT

