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

kurtlognorm<- read.csv(("kurtlognorm_31180.csv"))
allkurtlognorm<-unlist(kurtlognorm)

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

#Then, start the Monte Simulation

#input the d value table previously generated
d_values<- read.csv(("d_values.csv"))
I_values<-read.csv(("I_values.csv"))
#Then, start the Monte Simulation

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
  
  RMSEbataches<- read.csv(paste("asymptotic_lognorm_Icalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  RMSEbataches2<-c()
  for (batch1 in c(1:batchsize)){
    iall11<-RMSEbataches[batch1,]
    
    x<-c(dslnorm(uni=unibatch[,batch1], meanlog =0, sdlog  = a/1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    rqmomentselect1<-rqmoments2(x=sortedx,iall1=iall11,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 4,releaseall=TRUE,standist_d=d_values,standist_I=I_values,orderlist1_sorted20=orderlist1_AB2,orderlist1_sorted30=orderlist1_AB3,orderlist1_sorted40=orderlist1_AB4,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    
    standardizedmomentsx<-standardizedmoments(x=sortedx)
    
    sortedx<-c()
    
    all1<-(c(rqmomentselect1,targetall,standardizedmomentsx))
    
    RMSEbataches2<-rbind(RMSEbataches2,all1)
  }
  
  write.csv(RMSEbataches2,paste("asymptotic_lognorm_Ismomentscalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  RMSEbatachesmean <-apply(RMSEbataches2, 2, calculate_column_mean)
  
  ikurt2<-sqrt(colMeans((RMSEbataches2[1:batchsize,c(1,3)]-kurtx)^2))
  
  iskew2<-sqrt(colMeans((RMSEbataches2[1:batchsize,c(2,4)]-skewx)^2))
  
  rankikurt2<-rank(ikurt2)
  rankiskew2<-rank(iskew2)
  
  allresultsSE<-c(samplesize=samplesize,type=4,kurtx,skewx,rankikurt2,rankiskew2,RMSEbatachesmean,RMSEikurt2=ikurt2,RMSEiskew2=iskew2)
}

write.csv(simulatedbatch_bias_Monte,paste("asymptotic_lognorm_Ismomentscalibration_raw",largesize,".csv", sep = ","), row.names = FALSE)

Optimum_RMSE<-simulatedbatch_bias_Monte[,1:8]

write.csv(Optimum_RMSE,paste("asymptotic_Ismoments_lognorm.csv", sep = ","), row.names = FALSE)

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
  

  SEbataches<- read.csv(paste("asymptotic_lognorm_Ismomentscalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","))

  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  
  ikurt2_se<-apply(((SEbataches[1:batchsize,c(1,3)])), 2, se_sd)
  
  iskew2_se<-apply((SEbataches[1:batchsize,c(2,4)]), 2, se_sd)
  
  allresultsSE<-c(samplesize=samplesize,type=4,kurtx,skewx,se_mean_all1,ikurt2_se,iskew2_se)

  allresultsSE
}

write.csv(simulatedbatch_bias_Monte_SE,paste("asymptotic_lognorm_Ismomentscalibration_raw_error",largesize,".csv", sep = ","), row.names = FALSE)


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
  
  RMSEbataches<- read.csv(paste("asymptotic_gnorm_Icalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  RMSEbataches2<-c()
  for (batch1 in c(1:batchsize)){
    iall11<-RMSEbataches[batch1,]
    
    x<-c(dsgnorm(uni=unibatch[,batch1], shape=a/1, scale = 1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    rqmomentselect1<-rqmoments2(x=sortedx,iall1=iall11,dtype1=1,Iskewtype1 = 5,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,orderlist1_sorted20=orderlist1_AB2,orderlist1_sorted30=orderlist1_AB3,orderlist1_sorted40=orderlist1_AB4,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    
    standardizedmomentsx<-standardizedmoments(x=sortedx)
    
    sortedx<-c()
    
    all1<-(c(rqmomentselect1,targetall,standardizedmomentsx))
    
    RMSEbataches2<-rbind(RMSEbataches2,all1)
  }
  
  write.csv(RMSEbataches2,paste("asymptotic_gnorm_Ismomentscalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  RMSEbatachesmean <-apply(RMSEbataches2, 2, calculate_column_mean)
  
  ikurt2<-sqrt(colMeans((RMSEbataches2[1:batchsize,c(1,3)]-kurtx)^2))
  
  iskew2<-sqrt(colMeans((RMSEbataches2[1:batchsize,c(2,4)]-skewx)^2))
  
  rankikurt2<-rank(ikurt2)
  rankiskew2<-rank(iskew2)
  
  allresultsSE<-c(samplesize=samplesize,type=5,kurtx,skewx,rankikurt2,rankiskew2,RMSEbatachesmean,RMSEikurt2=ikurt2,RMSEiskew2=iskew2)
}

write.csv(simulatedbatch_bias_Monte,paste("asymptotic_gnorm_Ismomentscalibration_raw",largesize,".csv", sep = ","), row.names = FALSE)

Optimum_RMSE<-simulatedbatch_bias_Monte[,1:8]

write.csv(Optimum_RMSE,paste("asymptotic_Ismoments_gnorm.csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte_SE<-foreach(batchnumber =c((1:length(allkurtgnorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  
  
  a=allkurtgnorm[batchnumber]
  
  targetm<-0
  targetvar<-gamma(3/a)/((gamma(1/a)))
  targettm<-0
  targetfm<-((gamma(3/a)/((gamma(1/a))))^2)*gamma(5/a)*gamma(1/a)/((gamma(3/a))^2)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  
  SEbataches<- read.csv(paste("asymptotic_gnorm_Ismomentscalibration_raw",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  
  ikurt2_se<-apply(((SEbataches[1:batchsize,c(1,3)])), 2, se_sd)
  
  iskew2_se<-apply((SEbataches[1:batchsize,c(2,4)]), 2, se_sd)
  
  allresultsSE<-c(samplesize=samplesize,type=5,kurtx,skewx,se_mean_all1,ikurt2_se,iskew2_se)
  
  allresultsSE
}

write.csv(simulatedbatch_bias_Monte_SE,paste("asymptotic_gnorm_Ismomentscalibration_raw_error",largesize,".csv", sep = ","), row.names = FALSE)


asymptotic_I_gnorm<- read.csv(("asymptotic_Ismoments_gnorm.csv"))
asymptotic_I_lognorm<- read.csv(("asymptotic_Ismoments_lognorm.csv"))
names(asymptotic_I_lognorm)<-1:ncol(asymptotic_I_lognorm)
names(asymptotic_I_gnorm)<-1:ncol(asymptotic_I_lognorm)
asymptotic_I_lognorm<-rbind(asymptotic_I_lognorm,asymptotic_I_gnorm)
asymptotic_I_gnorm<- read.csv(("asymptotic_Ismoments_lognorm.csv"))
names(asymptotic_I_lognorm)<-names(asymptotic_I_gnorm)
write.csv(asymptotic_I_lognorm,paste("asymptotic_Ismoments.csv", sep = ","), row.names = FALSE)



stopCluster(cl)
registerDoSEQ()

