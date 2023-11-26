#Copyright 2023 Tuobang Li

#These codes and manuscripts are under review in PNAS, please do not share them.

#If you are interested, please do not hesitate to contact me. Cooperation is also welcomed!

#require foreach and doparallel for parallel processing of bootstrap (not available for some types of computers)
if (!require("foreach")) install.packages("foreach")
library(foreach)
if (!require("doParallel")) install.packages("doParallel")
library(doParallel)
#require randtoolbox for random number generations
if (!require("randtoolbox")) install.packages("randtoolbox")
library(randtoolbox)
if (!require("Rcpp")) install.packages("Rcpp")
library(Rcpp)
if (!require("Rfast")) install.packages("Rfast")
library(Rfast)
if (!require("REDSReview")) install.packages("REDSReview_1.0.zip", repos = NULL)
library(REDSReview)
if (!require("matrixStats")) install.packages("matrixStats")
library(matrixStats)

numCores <- detectCores()-4 # Detect the number of available cores
cl <- makeCluster(numCores) # Create a cluster with the number of cores
registerDoParallel(cl) # Register the parallel backend

#bootsize for bootstrap approximation of the distributions of the kernal of U-statistics.
n <- 331776*3*8
(n%%10)==0
# maximum order of moments
morder <- 4
#large sample size (approximating asymptotic)
largesize<-331776*8

#generate quasirandom numbers based on the Sobol sequence
quasiunisobol<-sobol(n=n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                     mixed = FALSE, method = "C", start = 1)

quasiuni<-quasiunisobol

quasiuni_sorted2 <- na.omit(rowSort(quasiuni[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted3 <- na.omit(rowSort(quasiuni[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted4 <- na.omit(rowSort(quasiuni, descend = FALSE, stable = FALSE, parallel = TRUE))
# Forever...

quasiuni<-Sort(sobol(n=largesize, dim = 1, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                     mixed = FALSE, method = "C", start = 1))

#the function, createorderlist, can transform the Sobol sequence into non-repeated integer sequences.

#the largesize is the maximum value of this integer sequence
orderlist1_AB2<-createorderlist(quni1=quasiuni_sorted2,size=largesize,interval=8,dimension=2)
orderlist1_AB2<-orderlist1_AB2[1:largesize,]
orderlist1_AB3<-createorderlist(quni1=quasiuni_sorted3,size=largesize,interval=8,dimension=3)
orderlist1_AB3<-orderlist1_AB3[1:largesize,]
orderlist1_AB4<-createorderlist(quni1=quasiuni_sorted4,size=largesize,interval=8,dimension=4)
orderlist1_AB4<-orderlist1_AB4[1:largesize,]
quasiuni_sorted2<-c()
quasiuni_sorted3<-c()
quasiuni_sorted4<-c()

asymptotic_n <- 331776*3*8
(asymptotic_n%%10)==0
# maximum order of moments
morder <- 6
#large sample size (asymptotic bias)
largesize<-331776*8

#generate quasirandom numbers based on the Sobol sequence
quasiunisobol_asymptotic<-sobol(n=asymptotic_n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                                mixed = FALSE, method = "C", start = 1)

quasiuni_M<-rbind(quasiunisobol_asymptotic)

quasiunisobol_asymptotic<-c()

orderlist1_hllarge<-createorderlist(quni1=quasiuni_M[,1:6],size=largesize,interval=8,dimension=6)
orderlist1_hllarge<-orderlist1_hllarge[1:largesize,]


kurtWeibull<- read.csv(("kurtWeibull_28260.csv"))
allkurtWeibull<-unlist(kurtWeibull)

simulatedbatchWeibull_bias<-foreach(batchnumber = (1:length(allkurtWeibull)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  set.seed(1)
  a=allkurtWeibull[batchnumber]
  x<-c(dsWeibull(uni=quasiuni, shape=a/1, scale = 1))

  targetm<-gamma(1+1/(a/1))
  targetvar<-(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2)
  targettm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^3)*(gamma(1+3/(a/1))-3*(gamma(1+1/(a/1)))*((gamma(1+2/(a/1))))+2*((gamma(1+1/(a/1)))^3))/((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(3))
  targetfm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^4)*(gamma(1+4/(a/1))-4*(gamma(1+3/(a/1)))*((gamma(1+1/(a/1))))+6*(gamma(1+2/(a/1)))*((gamma(1+1/(a/1)))^2)-3*((gamma(1+1/(a/1)))^4))/(((gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(2))
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))

  sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
  x<-c()
  dall<-compalldmoments(x=sortedx,targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm,orderlist1_sorted20=orderlist1_AB2,orderlist1_sorted30=orderlist1_AB3,orderlist1_sorted40=orderlist1_AB4,orderlist1_hlsmall=orderlist1_hllarge,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",boot=TRUE)
  sortedx<-c()
  all1<-(c(kurtx=kurtx,skewx=skewx,dall))
}

write.csv(simulatedbatchWeibull_bias,paste("asymptotic_Weibull_dcalibration_raw",largesize,".csv", sep = ","), row.names = FALSE)

asymptotic_d_Weibull<-simulatedbatchWeibull_bias[,c(1,2,seq(from=3, to=386, by=2))]

write.csv(asymptotic_d_Weibull,paste("asymptotic_d_Weibull.csv", sep = ","), row.names = FALSE)

stopCluster(cl)
registerDoSEQ()

