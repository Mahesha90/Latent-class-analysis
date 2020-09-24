install.packages("GPArotation")
install.packages("nFactor")
install.packages("psych")
install.packages("lattice")
library(GPArotation)
library(lattice)
library(nFactors)
library(psych)

#--------------------Factor Analysis-----------------------
data <- read.delim(file.choose(),header = TRUE)
View(data)
#PCA to identify number of factors
data.pca <- prcomp(data)
summary(data.pca)
plot(data.pca)
#to find out the number of factors that we’ll be selecting for factor analysis.
parallel<-fa.parallel(data, fm='minres', fa='fa')
#Parallel analysis suggests that the number of factors =  4
threefactor <- fa(data,nfactors = 3,rotate = "oblimin",fm="minres")
print(threefactor)
print(threefactor$loadings,cutoff = 0.3, sort=TRUE)

fourfactor <- fa(data,nfactors = 4,rotate = "oblimin",fm="minres")
print(fourfactor)
print(fourfactor$loadings,cutoff = 0.3, sort=TRUE)

#-------------Latent Class Analysis------------------
library(scatterplot3d)
library(MASS)
library(poLCA)
library(xlsx)
library("reshape2")
library("plyr")
library("dplyr")
library("poLCA")
library("ggplot2")
library("ggparallel")
library("igraph")
library("tidyr")
library("knitr")
data1 <- read.delim(file.choose(),header = TRUE)
#
poLCA(cbind(SI,SC,PP,NHL, NMI,Finishdegree,GP,LowCompletion,LPjob,WI,TPSE,N_PopularS,NInterest,OTransfer,WSpeciality,EDropouts,E_Workload,FamilyI,FinancialI,HealthP,PandS,DifficulutCourse,LearningD,Middle,Employment,Thesis,Perfectionism,Final,Checkwithteachers,Counseling,ICourses,Inform,Tracking,SupervisorC,Talk,AStablished,SandCourses,SelfM,CD,GD,Suggested,CantfigureSP,DifficultConvince,DifficultCont,LecturersLI)~1, data=data1)
#this suggest 2 latent classes

f<-with(data1, cbind(SI,SC,PP,NHL, NMI,Finishdegree,GP,LowCompletion,LPjob,WI,TPSE,N_PopularS,NInterest,OTransfer,WSpeciality,EDropouts,E_Workload,FamilyI,FinancialI,HealthP,PandS,DifficulutCourse,LearningD,Middle,Employment,Thesis,Perfectionism,Final,Checkwithteachers,Counseling,ICourses,Inform,Tracking,SupervisorC,Talk,AStablished,SandCourses,SelfM,CD,GD,Suggested,CantfigureSP,DifficultConvince,DifficultCont,LecturersLI)~1) #

max_II <- -100000
min_bic <- 100000
for(i in 2:10){
  lc <- poLCA(f, data1, nclass=i, maxiter=3000, 
              tol=1e-5, na.rm=FALSE,  
              nrep=10, verbose=TRUE, calc.se=TRUE)
  if(lc$bic < min_bic){
    min_bic <- lc$bic
    LCA_best_model<-lc
  }
}    	
LCA_best_model

lca.lcca<-lcca::lca(
  cbind(SI,SC,PP,NHL, NMI,Finishdegree,GP,LowCompletion,LPjob,WI,TPSE,N_PopularS,NInterest,OTransfer,WSpeciality,EDropouts,E_Workload,FamilyI,FinancialI,HealthP,PandS,DifficulutCourse,LearningD,Middle,Employment,Thesis,Perfectionism,Final,Checkwithteachers,Counseling,ICourses,Inform,Tracking,SupervisorC,Talk,AStablished,SandCourses,SelfM,CD,GD,Suggested,CantfigureSP,DifficultConvince,DifficultCont,LecturersLI)~1,
  nclass=2,
  data=data1,
  flatten.rhos=1
)
summary.lca(lca.lcca, show.all=T)

#-------------------Cluster Analysis-----------------

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

d <- dist(data, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
