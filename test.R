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
ds <- poLCA(cbind(SocietalIssue,	StudentChoice,	PoliticalProblem,	NotaHugeLoss,	NotMuchImpact,	Finishdegree,	GetPromotedinCareer,	LowCompletion,	LowProbabilityinJob,	WasteofInvestments,	StudenttookSomeonesplace,	N_PopularS,	NInterest,	OTransfer,	WSpeciality,	E_Workload,	FamilyI,	FinancialI,	HealthP,	Psyco_Social,	DifficulutCourse,	LearningD,	Employment,	Thesis,	Perfectionism,	Checkwithteachers,	Counseling,	ICourses,	Inform,	Tracking,	SupervisorC,	Talk,	SandCourses,	ImroveStudy_SelfM,	CurriculumDevelopmnet,	GroupDiscussions,	CantfigureSP,	DifficultConvince,	DifficultCont,	LecturersLI,	NPtakingActions,	TooLate,	WorkHours,	TMIssues,	OwnData,	UseSIS,	PositiveImp)~1, data=dataAC, graphs = TRUE, na.rm = TRUE)
#this suggest best fit model for 2 latent classes

mydata <- dataAC %>% dplyr::select(SocietalIssue,	StudentChoice,	PoliticalProblem,	NotaHugeLoss,	NotMuchImpact,	Finishdegree,	GetPromotedinCareer,	LowCompletion,	LowProbabilityinJob,	WasteofInvestments,	StudenttookSomeonesplace,	N_PopularS,	NInterest,	OTransfer,	WSpeciality,	E_Workload,	FamilyI,	FinancialI,	HealthP,	Psyco_Social,	DifficulutCourse,	LearningD,	Employment,	Thesis,	Perfectionism,	Checkwithteachers,	Counseling,	ICourses,	Inform,	Tracking,	SupervisorC,	Talk,	SandCourses,	ImroveStudy_SelfM,	CurriculumDevelopmnet,	GroupDiscussions,	CantfigureSP,	DifficultConvince,	DifficultCont,	LecturersLI,	NPtakingActions,	TooLate,	WorkHours,	TMIssues,	OwnData,	UseSIS,	PositiveImp)
f<-with(mydata, cbind(SocietalIssue,	StudentChoice,	PoliticalProblem,	NotaHugeLoss,	NotMuchImpact,	Finishdegree,	GetPromotedinCareer,	LowCompletion,	LowProbabilityinJob,	WasteofInvestments,	StudenttookSomeonesplace,	N_PopularS,	NInterest,	OTransfer,	WSpeciality,	E_Workload,	FamilyI,	FinancialI,	HealthP,	Psyco_Social,	DifficulutCourse,	LearningD,	Employment,	Thesis,	Perfectionism,	Checkwithteachers,	Counseling,	ICourses,	Inform,	Tracking,	SupervisorC,	Talk,	SandCourses,	ImroveStudy_SelfM,	CurriculumDevelopmnet,	GroupDiscussions,	CantfigureSP,	DifficultConvince,	DifficultCont,	LecturersLI,	NPtakingActions,	TooLate,	WorkHours,	TMIssues,	OwnData,	UseSIS,	PositiveImp)~1)

set.seed(01012)
lc1<-poLCA(f, data=mydata, nclass=1, na.rm = FALSE, nrep=30, maxiter=3000) #Loglinear independence model.
lc2<-poLCA(f, data=mydata, nclass=2, na.rm = FALSE, nrep=30, maxiter=3000)
lc3<-poLCA(f, data=mydata, nclass=3, na.rm = FALSE, nrep=30, maxiter=3000)
lc4<-poLCA(f, data=mydata, nclass=4, na.rm = FALSE, nrep=30, maxiter=3000) 
lc5<-poLCA(f, data=mydata, nclass=5, na.rm = FALSE, nrep=30, maxiter=3000)
lc6<-poLCA(f, data=mydata, nclass=6, na.rm = FALSE, nrep=30, maxiter=3000)

#-- generate dataframe with fit-values
results <- data.frame(Modell=c("Modell 1"),
                      log_likelihood=lc1$llik,
                      df = lc1$resid.df,
                      BIC=lc1$bic,
                      ABIC=  (-2*lc1$llik) + ((log((lc1$N + 2)/24)) * lc1$npar),
                      CAIC = (-2*lc1$llik) + lc1$npar * (1 + log(lc1$N)), 
                      likelihood_ratio=lc1$Gsq)
results$Modell<-as.integer(results$Modell)
results[1,1]<-c("Modell 1")
results[2,1]<-c("Modell 2")
results[3,1]<-c("Modell 3")
results[4,1]<-c("Modell 4")
results[5,1]<-c("Modell 5")
results[6,1]<-c("Modell 6")

results[2,2]<-lc2$llik
results[3,2]<-lc3$llik
results[4,2]<-lc4$llik
results[5,2]<-lc5$llik
results[6,2]<-lc6$llik

results[2,3]<-lc2$resid.df
results[3,3]<-lc3$resid.df
results[4,3]<-lc4$resid.df
results[5,3]<-lc5$resid.df
results[6,3]<-lc6$resid.df

results[2,4]<-lc2$bic
results[3,4]<-lc3$bic
results[4,4]<-lc4$bic
results[5,4]<-lc5$bic
results[6,4]<-lc6$bic

results[2,5]<-(-2*lc2$llik) + ((log((lc2$N + 2)/24)) * lc2$npar) #abic
results[3,5]<-(-2*lc3$llik) + ((log((lc3$N + 2)/24)) * lc3$npar)
results[4,5]<-(-2*lc4$llik) + ((log((lc4$N + 2)/24)) * lc4$npar)
results[5,5]<-(-2*lc5$llik) + ((log((lc5$N + 2)/24)) * lc5$npar)
results[6,5]<-(-2*lc6$llik) + ((log((lc6$N + 2)/24)) * lc6$npar)

results[2,6]<- (-2*lc2$llik) + lc2$npar * (1 + log(lc2$N)) #caic
results[3,6]<- (-2*lc3$llik) + lc3$npar * (1 + log(lc3$N))
results[4,6]<- (-2*lc4$llik) + lc4$npar * (1 + log(lc4$N))
results[5,6]<- (-2*lc5$llik) + lc5$npar * (1 + log(lc5$N))
results[6,6]<- (-2*lc6$llik) + lc6$npar * (1 + log(lc6$N))

results[2,7]<-lc2$Gsq
results[3,7]<-lc3$Gsq
results[4,7]<-lc4$Gsq
results[5,7]<-lc5$Gsq
results[6,7]<-lc6$Gsq
#print results
results
#calculate enthrophy
poLCA.entropy(lc1)

#Enthropy

entropy<-function (p) sum(-p*log(p))

results$R2_entropy
results[1,8]<-c("-")

error_prior<-entropy(lc2$P) # class proportions model 2
error_post<-mean(apply(lc2$posterior,1, entropy),na.rm = TRUE)
results[2,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc3$P) # class proportions model 3
error_post<-mean(apply(lc3$posterior,1, entropy),na.rm = TRUE)
results[3,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc4$P) # class proportions model 4
error_post<-mean(apply(lc4$posterior,1, entropy),na.rm = TRUE)
results[4,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc5$P) # class proportions model 5
error_post<-mean(apply(lc5$posterior,1, entropy),na.rm = TRUE)
results[5,8]<-round(((error_prior-error_post) / error_prior),3)

error_prior<-entropy(lc6$P) # class proportions model 6
error_post<-mean(apply(lc6$posterior,1, entropy),na.rm = TRUE)
results[6,8]<-round(((error_prior-error_post) / error_prior),3)

# combining results to a dataframe
colnames(results)<-c("Model","log-likelihood","resid. df","BIC","aBIC","cAIC","likelihood-ratio","Entropy")
lca_results<-results

library(ztable)
ztable::ztable(lca_results)
#f<-with(data1, cbind(SocietalIssue,	StudentChoice,	PoliticalProblem,	NotaHugeLoss,	NotMuchImpact,	Finishdegree,	GetPromotedinCareer,	LowCompletion,	LowProbabilityinJob,	WasteofInvestments,	StudenttookSomeonesplace,	N_PopularS,	NInterest,	OTransfer,	WSpeciality,	E_Workload,	FamilyI,	FinancialI,	HealthP,	Psyco_Social,	DifficulutCourse,	LearningD,	Employment,	Thesis,	Perfectionism,	Checkwithteachers,	Counseling,	ICourses,	Inform,	Tracking,	SupervisorC,	Talk,	SandCourses,	ImroveStudy_SelfM,	CurriculumDevelopmnet,	GroupDiscussions,	CantfigureSP,	DifficultConvince,	DifficultCont,	LecturersLI,	NPtakingActions,	TooLate,	WorkHours,	TMIssues,	OwnData,	DoNotuseSIS,	UseSIS,	LiketoseeStudentD,	PositiveImp,	NagativeImp)~1) #


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