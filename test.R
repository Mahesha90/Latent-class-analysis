install.packages("GPArotation")
install.packages("nFactor")
install.packages("psych")
install.packages("lattice")
library(GPArotation)
library(lattice)
library(nFactors)
library(psych)
library(dplyr)

#--------------------Factor Analysis for All codes-----------------------
#This has done for both "All codes" and "DropoutReasons"
data <- read.delim(file.choose(),header = TRUE)
View(data)
#CorrelationMatrix
raqMatrix <- cor(data[2:49])

head(round(raqMatrix, 2))
#remove negativeIM and liketo see student data
#M1 <- select(data,SocietalIssue,	StudentChoice,	PoliticalProblem,	NotaHugeLoss,	NotMuchImpact,	Finishdegree,	GetPromotedinCareer,	LowCompletion,	LowProbabilityinJob,	WasteofInvestments,	StudenttookSomeonesplace,	N_PopularS,	NInterest,	OTransfer,	WSpeciality,	E_Workload,	FamilyI,	FinancialI,	HealthP,	Psyco_Social,	DifficulutCourse,	LearningD,	Employment,	Thesis,	Perfectionism,	Checkwithteachers,	Counseling,	ICourses,	Inform,	Tracking,	SupervisorC,	Talk,	SandCourses,	ImroveStudy_SelfM,	CurriculumDevelopmnet,	GroupDiscussions,	CantfigureSP,	DifficultConvince,	DifficultCont,	LecturersLI,	NPtakingActions,	TooLate,	WorkHours,	TMIssues,	OwnData,	DoNotuseSIS,	UseSIS,	LiketoseeStudentD,	PositiveImp,	NagativeImp)
data <- select(data,SocietalIssue,	StudentChoice,	PoliticalProblem,	NotaHugeLoss,	NotMuchImpact,	Finishdegree,	GetPromotedinCareer,	LowCompletion,	LowProbabilityinJob,	WasteofInvestments,	StudenttookSomeonesplace,	N_PopularS,	NInterest,	OTransfer,	WSpeciality,	E_Workload,	FamilyI,	FinancialI,	HealthP,	Psyco_Social,	DifficulutCourse,	LearningD,	Employment,	Thesis,	Perfectionism,	Checkwithteachers,	Counseling,	ICourses,	Inform,	Tracking,	SupervisorC,	Talk,	SandCourses,	ImroveStudy_SelfM,	CurriculumDevelopmnet,	GroupDiscussions,	CantfigureSP,	DifficultConvince,	DifficultCont,	LecturersLI,	NPtakingActions,	TooLate,	WorkHours,	TMIssues,	OwnData,	UseSIS,	PositiveImp)
data
raqMatrix <- cor(data)

head(round(raqMatrix, 2))
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

#--------------------Factor Analysis for Dropout Reasons and Patterns-----------------------
#This has done for both "All codes" and "DropoutReasons"
data <- read.delim(file.choose(),header = TRUE)
View(data)
#CorrelationMatrix
raqMatrix <- cor(data[2:26])

head(round(raqMatrix, 2))

#PCA to identify number of factors
data.pca <- prcomp(data)

#Screeplot to find number of factors
ev <- eigen(cor(data[2:26])) # get eigenvalues
ap <- parallel(subject=nrow(data[2:26]),var=ncol(data[2:26]),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

summary(data.pca)
plot(data.pca)
#to find out the number of factors that we’ll be selecting for factor analysis.
parallel<-fa.parallel(data[2:26], fm='minres', fa='fa')
#Parallel analysis suggests that the number of factors =  1, but based on PCA data 3 factors were executed

threefactor <- fa(data[2:26],nfactors = 3,rotate = "oblimin",fm="minres")
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
dataAC <- read.delim(file.choose(),header = TRUE)
#latent class analysis for all the codes
poLCA(cbind(SocietalIssue,	StudentChoice,	PoliticalProblem,	NotaHugeLoss,	NotMuchImpact,	Finishdegree,	GetPromotedinCareer,	LowCompletion,	LowProbabilityinJob,	WasteofInvestments,	StudenttookSomeonesplace,	N_PopularS,	NInterest,	OTransfer,	WSpeciality,	E_Workload,	FamilyI,	FinancialI,	HealthP,	Psyco_Social,	DifficulutCourse,	LearningD,	Employment,	Thesis,	Perfectionism,	Checkwithteachers,	Counseling,	ICourses,	Inform,	Tracking,	SupervisorC,	Talk,	SandCourses,	ImroveStudy_SelfM,	CurriculumDevelopmnet,	GroupDiscussions,	CantfigureSP,	DifficultConvince,	DifficultCont,	LecturersLI,	NPtakingActions,	TooLate,	WorkHours,	TMIssues,	OwnData,	UseSIS,	PositiveImp)~1, data=dataAC, graphs = TRUE, na.rm = TRUE)
#this suggest best fit model for 2 latent classes


#f<-with(data1, cbind(SocietalIssue,	StudentChoice,	PoliticalProblem,	NotaHugeLoss,	NotMuchImpact,	Finishdegree,	GetPromotedinCareer,	LowCompletion,	LowProbabilityinJob,	WasteofInvestments,	StudenttookSomeonesplace,	N_PopularS,	NInterest,	OTransfer,	WSpeciality,	E_Workload,	FamilyI,	FinancialI,	HealthP,	Psyco_Social,	DifficulutCourse,	LearningD,	Employment,	Thesis,	Perfectionism,	Checkwithteachers,	Counseling,	ICourses,	Inform,	Tracking,	SupervisorC,	Talk,	SandCourses,	ImroveStudy_SelfM,	CurriculumDevelopmnet,	GroupDiscussions,	CantfigureSP,	DifficultConvince,	DifficultCont,	LecturersLI,	NPtakingActions,	TooLate,	WorkHours,	TMIssues,	OwnData,	DoNotuseSIS,	UseSIS,	LiketoseeStudentD,	PositiveImp,	NagativeImp)~1) #

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

dataClus <- read.delim(file.choose(),header = TRUE)
#Dissimilarity matrix
HC <- dist(dataClus[2:14], method = "euclidean")
#Hierarchical clustering using wards method
hc1 <- hclust(HC, method = "ward.D2" )
#hc1 <- hclust(HC, method = "average" )
#Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
##Plot the obtained dendrogram with Labels
plot(hc1, cex = 0.6, labels = dataClus$Participant, hang = -1)
#make subgroups
plot(hc1, labels = dataClus$Participant, cex = 0.6)
rect.hclust(hc1, k = 5, border = 2:5)