install.packages("GPArotation")
install.packages("nFactor")
install.packages("psych")
install.packages("lattice")
library(GPArotation)
library(lattice)
library(nFactors)
library(psych)

data <- read.delim(file.choose(),header = TRUE)
data.pca <- prcomp(data)
summary(data.pca)

parallel<-fa.parallel(data, fm='minres', fa='fa')

threefactor <- fa(data,nfactors = 3,rotate = "oblimin",fm="minres")
print(threefactor)

print(threefactor$loadings,cutoff = 0.3, sort=TRUE)