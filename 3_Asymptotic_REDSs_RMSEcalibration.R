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
if (!require("REDSReview")) install.packages("REDSReview_1.0.tar.gz", repos = NULL)
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

#load asymptotic d for two parameter distributions

#set the stop criterion
criterionset=1e-10

samplesize=331776*8
batchsizebase=50


orderlist1_AB2<-createorderlist(quni1=quasiuni_sorted2,size=largesize,interval=8,dimension=2)
orderlist1_AB2<-orderlist1_AB2[1:largesize,]
orderlist1_AB3<-createorderlist(quni1=quasiuni_sorted3,size=largesize,interval=8,dimension=3)
orderlist1_AB3<-orderlist1_AB3[1:largesize,]
orderlist1_AB4<-createorderlist(quni1=quasiuni_sorted4,size=largesize,interval=8,dimension=4)
orderlist1_AB4<-orderlist1_AB4[1:largesize,]
batchsize=batchsizebase

n <- samplesize
setSeed(1)
unibatchran<-matrix(SFMT(samplesize*batchsize),ncol=batchsize)

unibatch<-colSort(unibatchran, descend = FALSE, stable = FALSE, parallel = TRUE)

#input the d value table previously generated
d_values<- read.csv(("d_values.csv"))

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

#Then, start the Monte Simulation

kurtlognorm<- read.csv(("kurtlognorm_31180.csv"))
allkurtlognorm<-unlist(kurtlognorm)

simulatedbatch_bias_Monte<-foreach(batchnumber =c((1:length(allkurtlognorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  set.seed(1)
  a=allkurtlognorm[batchnumber]

  targetm<-exp((a^2)/2)
  targetvar<-(exp((a/1)^2)*(-1+exp((a/1)^2)))
  targettm<-sqrt(exp((a/1)^2)-1)*((2+exp((a/1)^2)))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^3)
  targetfm<-((-3+exp(4*((a/1)^2))+2*exp(3*((a/1)^2))+3*exp(2*((a/1)^2))))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))

  RMSEbataches<-c()
  for (batch1 in c(1:batchsize)){
    x<-c(dslnorm(uni=unibatch[,batch1], meanlog =0, sdlog  = a/1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    rqmoments1<-rqmoments(x=sortedx,start_kurt=kurtx,start_skew=skewx,dtype1=1,releaseall=TRUE,standist_d=d_values,orderlist1_sorted20=orderlist1_AB2,orderlist1_sorted30=orderlist1_AB3,orderlist1_sorted40=orderlist1_AB4,orderlist1_hlsmall=orderlist1_hllarge,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=criterionset)

    standardizedmomentsx<-standardizedmoments(x=sortedx)

    sortedx<-c()
    
    all1<-unlist(c(rqmoments1,targetall,standardizedmomentsx))

    RMSEbataches<-rbind(RMSEbataches,all1)
  }
  
  write.csv(RMSEbataches,paste("asymptotic_lognorm_Icalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  RMSEbataches <- apply(RMSEbataches[1:batchsize,], 2, as.numeric)
  RMSEbatachesmean <-apply(RMSEbataches, 2, calculate_column_mean)
  
  rqkurt<-sqrt(colMeans((RMSEbataches[1:batchsize,c(1:728,1961:2688)]-kurtx)^2))
  
  rqskew<-sqrt(colMeans((RMSEbataches[1:batchsize,c(729:1768,2689:3728)]-skewx)^2))
  
  rankkurtall1<-rank(rqkurt)
  rankskewall1<-rank(rqskew)
  
  allresultsRMSE<-c(samplesize=samplesize,type=4,kurtx=kurtx,skewx=skewx,rankkurtall1,rankskewall1,RMSEbatachesmean,RMSErqkurt=rqkurt,RMSErqskew=rqskew)
}


write.csv(simulatedbatch_bias_Monte,paste("asymptotic_lognorm_Icalibration_raw",largesize,".csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte<- read.csv(paste("asymptotic_lognorm_Icalibration_raw",largesize,".csv", sep = ","))

Optimum_RMSE<-simulatedbatch_bias_Monte[,1:3540]

write.csv(Optimum_RMSE,paste("asymptotic_I_lognorm.csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte_SE<-foreach(batchnumber =c((1:length(allkurtlognorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)


  a=allkurtlognorm[batchnumber]
  
  targetm<-exp((a^2)/2)
  targetvar<-(exp((a/1)^2)*(-1+exp((a/1)^2)))
  targettm<-sqrt(exp((a/1)^2)-1)*((2+exp((a/1)^2)))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^3)
  targetfm<-((-3+exp(4*((a/1)^2))+2*exp(3*((a/1)^2))+3*exp(2*((a/1)^2))))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  

  SEbataches<- read.csv(paste("asymptotic_lognorm_Icalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","))

  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  
  rqkurt_se<-apply(((SEbataches[1:batchsize,c(1:728,1961:2688)])), 2, se_sd)
  
  rqskew_se<-apply((SEbataches[1:batchsize,c(729:1768,2689:3728)]), 2, se_sd)
  
  allresultsSE<-c(samplesize=samplesize,type=4,kurtx,skewx,se_mean_all1,rqkurt_se,rqskew_se)
  
  allresultsSE
}

write.csv(simulatedbatch_bias_Monte_SE,paste("asymptotic_lognorm_Icalibration_raw_error",largesize,".csv", sep = ","), row.names = FALSE)

asymptotic_I_lognorm<- read.csv(("asymptotic_I_lognorm.csv"))

kurtgnorm<- read.csv(("kurtgnorm_21180.csv"))
allkurtgnorm<-unlist(kurtgnorm)

simulatedbatch_bias_Monte<-foreach(batchnumber =c((1:length(allkurtgnorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  set.seed(1)
  a=allkurtgnorm[batchnumber]
  
  targetm<-0
  targetvar<-gamma(3/a)/((gamma(1/a)))
  targettm<-0
  targetfm<-((gamma(3/a)/((gamma(1/a))))^2)*gamma(5/a)*gamma(1/a)/((gamma(3/a))^2)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  RMSEbataches<-c()
  for (batch1 in c(1:batchsize)){
    x<-c(dsgnorm(uni=unibatch[,batch1], shape=a/1, scale = 1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    rqmoments1<-rqmoments(x=sortedx,start_kurt=kurtx,start_skew=skewx,dtype1=1,releaseall=TRUE,standist_d=d_values,orderlist1_sorted20=orderlist1_AB2,orderlist1_sorted30=orderlist1_AB3,orderlist1_sorted40=orderlist1_AB4,orderlist1_hlsmall=orderlist1_hllarge,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=criterionset)
    
    standardizedmomentsx<-standardizedmoments(x=sortedx)
    
    sortedx<-c()
    
    all1<-unlist(c(rqmoments1,targetall,standardizedmomentsx))
    
    RMSEbataches<-rbind(RMSEbataches,all1)
  }
  
  write.csv(RMSEbataches,paste("asymptotic_gnorm_Icalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  RMSEbataches <- apply(RMSEbataches[1:batchsize,], 2, as.numeric)
  RMSEbatachesmean <-apply(RMSEbataches, 2, calculate_column_mean)
  
  rqkurt<-sqrt(colMeans((RMSEbataches[1:batchsize,c(1:728,1961:2688)]-kurtx)^2))
  
  rqskew<-sqrt(colMeans((RMSEbataches[1:batchsize,c(729:1768,2689:3728)]-skewx)^2))
  
  rankkurtall1<-rank(rqkurt)
  rankskewall1<-rank(rqskew)
  
  allresultsRMSE<-c(samplesize=samplesize,type=5,kurtx=kurtx,skewx=skewx,rankkurtall1,rankskewall1,RMSEbatachesmean,RMSErqkurt=rqkurt,RMSErqskew=rqskew)
}


write.csv(simulatedbatch_bias_Monte,paste("asymptotic_gnorm_Icalibration_raw",largesize,".csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte<- read.csv(paste("asymptotic_gnorm_Icalibration_raw",largesize,".csv", sep = ","))

Optimum_RMSE<-simulatedbatch_bias_Monte[,1:3540]

write.csv(Optimum_RMSE,paste("asymptotic_I_gnorm.csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte_SE<-foreach(batchnumber =c((1:length(allkurtgnorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  
  
  a=allkurtgnorm[batchnumber]
  
  ttargetm<-0
  targetvar<-gamma(3/a)/((gamma(1/a)))
  targettm<-0
  targetfm<-((gamma(3/a)/((gamma(1/a))))^2)*gamma(5/a)*gamma(1/a)/((gamma(3/a))^2)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  
  SEbataches<- read.csv(paste("asymptotic_gnorm_Icalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  
  rqkurt_se<-apply(((SEbataches[1:batchsize,c(1:728,1961:2688)])), 2, se_sd)
  
  rqskew_se<-apply((SEbataches[1:batchsize,c(729:1768,2689:3728)]), 2, se_sd)
  
  allresultsSE<-c(samplesize=samplesize,type=5,kurtx,skewx,se_mean_all1,rqkurt_se,rqskew_se)
  
  allresultsSE
}

write.csv(simulatedbatch_bias_Monte_SE,paste("asymptotic_gnorm_Icalibration_raw_error",largesize,".csv", sep = ","), row.names = FALSE)

asymptotic_I_gnorm<- read.csv(("asymptotic_I_gnorm.csv"))
asymptotic_I_lognorm<- read.csv(("asymptotic_I_lognorm.csv"))
names(asymptotic_I_lognorm)<-1:ncol(asymptotic_I_lognorm)
names(asymptotic_I_gnorm)<-1:ncol(asymptotic_I_lognorm)
asymptotic_I_lognorm<-rbind(asymptotic_I_lognorm,asymptotic_I_gnorm)
asymptotic_I_gnorm<- read.csv(("asymptotic_I_lognorm.csv"))
names(asymptotic_I_lognorm)<-names(asymptotic_I_gnorm)
write.csv(asymptotic_I_lognorm,paste("asymptotic_I.csv", sep = ","), row.names = FALSE)

stopCluster(cl)
registerDoSEQ()

