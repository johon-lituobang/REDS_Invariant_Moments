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
if (!require("rootSolve")) install.packages("rootSolve")
library(rootSolve)
numCores <- detectCores()-4 # Detect the number of available cores
cl <- makeCluster(numCores) # Create a cluster with the number of cores
registerDoParallel(cl) # Register the parallel backend



asymptotic_n <- 331776*3*8
(asymptotic_n%%10)==0
# maximum order of moments
morder <- 6
#large sample size (asymptotic bias)
largesize<-331776*8

#generate quasirandom numbers based on the Sobol sequence
quasiunisobol_asymptotic<-sobol(n=asymptotic_n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                                mixed = FALSE, method = "C", start = 1)

quasiuni_asymptotic<-rbind(quasiunisobol_asymptotic)

quasiunisobol_asymptotic<-c()

quasiuni_sorted2_asymptotic <- na.omit(rowSort(quasiuni_asymptotic[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted3_asymptotic <- na.omit(rowSort(quasiuni_asymptotic[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted4_asymptotic <- na.omit(rowSort(quasiuni_asymptotic[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted5_asymptotic <- na.omit(rowSort(quasiuni_asymptotic[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted6_asymptotic <- na.omit(rowSort(quasiuni_asymptotic[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))

quasiuni_asymptotic<-(quasiuni_asymptotic[,1])
quasiuni_asymptotic<-quasiuni_asymptotic[1:largesize]
# Forever...
#quasiuni_asymptotic<-Sort(sobol(n=largesize, dim = 1, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
#                                mixed = FALSE, method = "C", start = 1))

orderlist1_AB2_asymptotic<-createorderlist(quni1=quasiuni_sorted2_asymptotic,size=largesize,interval=16,dimension=2)
orderlist1_AB2_asymptotic<-orderlist1_AB2_asymptotic[1:largesize,]
orderlist1_AB3_asymptotic<-createorderlist(quni1=quasiuni_sorted3_asymptotic,size=largesize,interval=16,dimension=3)
orderlist1_AB3_asymptotic<-orderlist1_AB3_asymptotic[1:largesize,]
orderlist1_AB4_asymptotic<-createorderlist(quni1=quasiuni_sorted4_asymptotic,size=largesize,interval=16,dimension=4)
orderlist1_AB4_asymptotic<-orderlist1_AB4_asymptotic[1:largesize,]
orderlist1_AB5_asymptotic<-createorderlist(quni1=quasiuni_sorted5_asymptotic,size=largesize,interval=16,dimension=5)
orderlist1_AB5_asymptotic<-orderlist1_AB5_asymptotic[1:largesize,]
orderlist1_AB6_asymptotic<-createorderlist(quni1=quasiuni_sorted6_asymptotic,size=largesize,interval=16,dimension=6)
orderlist1_AB6_asymptotic<-orderlist1_AB6_asymptotic[1:largesize,]
quasiuni_sorted2_asymptotic<-c()
quasiuni_sorted3_asymptotic<-c()
quasiuni_sorted4_asymptotic<-c()
quasiuni_sorted5_asymptotic<-c()
quasiuni_sorted6_asymptotic<-c()


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

orderlist1_hllarge_asymptotic<-createorderlist(quni1=quasiuni_M[,1:6],size=largesize,interval=8,dimension=6)
orderlist1_hllarge_asymptotic<-orderlist1_hllarge_asymptotic[1:largesize,]

morder=6

quasiunisobol_asymptotic_rand<-matrix(randtoolbox::SFMT(largesize*3*morder),ncol=morder)

quasiuni_asymptotic_rand<-rbind(quasiunisobol_asymptotic_rand)

quasiunisobol_asymptotic_rand<-c()

quasiuni_sorted2_asymptotic_rand <- na.omit(rowSort(quasiuni_asymptotic_rand[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted3_asymptotic_rand <- na.omit(rowSort(quasiuni_asymptotic_rand[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted4_asymptotic_rand <- na.omit(rowSort(quasiuni_asymptotic_rand[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted5_asymptotic_rand <- na.omit(rowSort(quasiuni_asymptotic_rand[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted6_asymptotic_rand <- na.omit(rowSort(quasiuni_asymptotic_rand[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))

orderlist1_AB2_asymptotic_rand <-createorderlist(quni1=quasiuni_sorted2_asymptotic_rand ,size=largesize,interval=16,dimension=2)
orderlist1_AB2_asymptotic_rand <-orderlist1_AB2_asymptotic_rand [1:largesize,]
orderlist1_AB3_asymptotic_rand <-createorderlist(quni1=quasiuni_sorted3_asymptotic_rand ,size=largesize,interval=16,dimension=3)
orderlist1_AB3_asymptotic_rand <-orderlist1_AB3_asymptotic_rand [1:largesize,]
orderlist1_AB4_asymptotic_rand <-createorderlist(quni1=quasiuni_sorted4_asymptotic_rand ,size=largesize,interval=16,dimension=4)
orderlist1_AB4_asymptotic_rand <-orderlist1_AB4_asymptotic_rand [1:largesize,]
orderlist1_AB5_asymptotic_rand <-createorderlist(quni1=quasiuni_sorted5_asymptotic_rand ,size=largesize,interval=16,dimension=5)
orderlist1_AB5_asymptotic_rand <-orderlist1_AB5_asymptotic_rand [1:largesize,]
orderlist1_AB6_asymptotic_rand <-createorderlist(quni1=quasiuni_sorted6_asymptotic_rand ,size=largesize,interval=16,dimension=6)
orderlist1_AB6_asymptotic_rand <-orderlist1_AB6_asymptotic_rand [1:largesize,]
quasiuni_sorted2_asymptotic_rand <-c()
quasiuni_sorted3_asymptotic_rand <-c()
quasiuni_sorted4_asymptotic_rand <-c()
quasiuni_sorted5_asymptotic_rand <-c()
quasiuni_sorted6_asymptotic_rand <-c()


#load asymptotic d for two parameter distributions
d_values<- read.csv(("d_values.csv"))
I_values<- read.csv(("I_values.csv"))
Ismoments_values<- read.csv(("Ismoments_values.csv"))
Imoments_values<- read.csv(("Imoments_values.csv"))

#set the stop criterion
criterionset=1e-10

kurtWeibull<- read.csv(("kurtWeibull_31100.csv"))
allkurtWeibull<-unlist(kurtWeibull)

simulatedbatch_asymptoticbias<-foreach(batchnumber = (1:length(allkurtWeibull)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  a=allkurtWeibull[batchnumber]
  x<-c(dsWeibull(uni=quasiuni_asymptotic, shape=a/1, scale = 1))
  targetm<-gamma(1+1/(a/1))
  targetvar<-(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2)
  targettm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^3)*(gamma(1+3/(a/1))-3*(gamma(1+1/(a/1)))*((gamma(1+2/(a/1))))+2*((gamma(1+1/(a/1)))^3))/((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(3))
  targetfm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^4)*(gamma(1+4/(a/1))-4*(gamma(1+3/(a/1)))*((gamma(1+1/(a/1))))+6*(gamma(1+2/(a/1)))*((gamma(1+1/(a/1)))^2)-3*((gamma(1+1/(a/1)))^4))/(((gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(2))
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
  
  targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
  x<-c()
  
  Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
  SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
  
  midhinge1<-midhinge(x=sortedx,sorted = TRUE)
  SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
  SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic,orderlist1_sorted3=orderlist1_AB3_asymptotic,orderlist1_sorted4=orderlist1_AB4_asymptotic,orderlist1_sorted5=orderlist1_AB5_asymptotic,orderlist1_sorted6=orderlist1_AB6_asymptotic,batch="auto")
  
  SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic_rand,orderlist1_sorted3=orderlist1_AB3_asymptotic_rand ,orderlist1_sorted4=orderlist1_AB4_asymptotic_rand ,orderlist1_sorted5=orderlist1_AB5_asymptotic_rand ,orderlist1_sorted6=orderlist1_AB6_asymptotic_rand ,batch="auto")
  
  rqm1<-rqm(x=sortedx,sorted = TRUE)
  
  MoM2<-median_of_means(sortedx,korder=2)
  MoM3<-median_of_means(sortedx,korder=3)
  MoM4<-median_of_means(sortedx,korder=4)
  MoM5<-median_of_means(sortedx,korder=5)
  MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
  
  imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB2_asymptotic,orderlist1_sorted30=orderlist1_AB3_asymptotic,orderlist1_sorted40=orderlist1_AB4_asymptotic,orderlist1_hlsmall=orderlist1_hllarge_asymptotic,orderlist1_hllarge=orderlist1_hllarge_asymptotic,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
  imoments1<-unlist(imoments1)
  momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
  
  #Generalized Catoni M-estimator
  #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
  # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
  
  Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
  
  #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
  QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
  
  alpha1<-QE1[1]-0.3
  alpha2<-QE1[1]+0.3
  
  #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
  RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
  
  #all parameter setting are from
  #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
  
  moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
  
  moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
  
  sortedx<-c()
  momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
  
  allrawmoBias<-c(
    firstbias=abs(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm)/sqrt(targetvar),
    secondbias=abs(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar)/momentssd[2],
    thirdbias=abs(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm)/momentssd[3],
    fourbias=abs(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm)/momentssd[4],
    skewbias=abs(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
    kurtbias=abs(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
  )
  
  allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
               second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
               third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
               fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
               skew=(c(imoments1[5],momentsx[7],imoments1[979])),
               kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
  )
  
  medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
  standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
  all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
}

write.csv(simulatedbatch_asymptoticbias,paste("asymptotic_Weibull_raw_Process",largesize,".csv", sep = ","), row.names = FALSE)

#bootsize for bootstrap approximation of the distributions of the kernel of U-statistics.
n <- 13824*2*3
(n%%10)==0
# maximum order of moments
morder <- 6
#large sample size (approximating asymptotic)
largesize1<-13824*2

#generate quasirandom numbers based on the Sobol sequence
quasiunisobol<-sobol(n=n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                     mixed = FALSE, method = "C", start = 1)

quasiuni<-quasiunisobol

quasiuni_sorted2 <- na.omit(rowSort(quasiuni[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted3 <- na.omit(rowSort(quasiuni[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted4 <- na.omit(rowSort(quasiuni[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted5 <- na.omit(rowSort(quasiuni[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted6 <- na.omit(rowSort(quasiuni[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))


# Forever...

largesize1<-13824*2
samplesize=576*9
batchsizebase=1000
orderlist1_AB20<-createorderlist(quni1=quasiuni_sorted2,size=samplesize,interval=8,dimension=2)
orderlist1_AB20<-orderlist1_AB20[1:largesize1,]
orderlist1_AB30<-createorderlist(quni1=quasiuni_sorted3,size=samplesize,interval=8,dimension=3)
orderlist1_AB30<-orderlist1_AB30[1:largesize1,]
orderlist1_AB40<-createorderlist(quni1=quasiuni_sorted4,size=samplesize,interval=8,dimension=4)
orderlist1_AB40<-orderlist1_AB40[1:largesize1,]
orderlist1_AB50<-createorderlist(quni1=quasiuni_sorted5,size=samplesize,interval=8,dimension=5)
orderlist1_AB50<-orderlist1_AB50[1:largesize1,]
orderlist1_AB60<-createorderlist(quni1=quasiuni_sorted6,size=samplesize,interval=8,dimension=6)
orderlist1_AB60<-orderlist1_AB60[1:largesize1,]

morder=6

quasiunisobol0<-matrix(randtoolbox::SFMT(largesize*3*morder),ncol=morder)

quasiuni0<-rbind(quasiunisobol0)

quasiunisobol0<-c()

quasiuni_sorted20 <- na.omit(rowSort(quasiuni0[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted30 <- na.omit(rowSort(quasiuni0[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted40 <- na.omit(rowSort(quasiuni0[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted50 <- na.omit(rowSort(quasiuni0[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
quasiuni_sorted60 <- na.omit(rowSort(quasiuni0[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))

orderlist1_AB20_rand <-createorderlist(quni1=quasiuni_sorted20 ,size=samplesize,interval=16,dimension=2)
orderlist1_AB20_rand <-orderlist1_AB20_rand [1:largesize1,]
orderlist1_AB3_rand <-createorderlist(quni1=quasiuni_sorted30 ,size=samplesize,interval=16,dimension=3)
orderlist1_AB3_rand <-orderlist1_AB3_rand [1:largesize1,]
orderlist1_AB4_rand <-createorderlist(quni1=quasiuni_sorted40 ,size=samplesize,interval=16,dimension=4)
orderlist1_AB4_rand <-orderlist1_AB4_rand [1:largesize1,]
orderlist1_AB5_rand <-createorderlist(quni1=quasiuni_sorted50 ,size=samplesize,interval=16,dimension=5)
orderlist1_AB5_rand <-orderlist1_AB5_rand [1:largesize1,]
orderlist1_AB6_rand <-createorderlist(quni1=quasiuni_sorted60 ,size=samplesize,interval=16,dimension=6)
orderlist1_AB6_rand <-orderlist1_AB6_rand [1:largesize1,]
quasiuni_sorted20 <-c()
quasiuni_sorted30 <-c()
quasiuni_sorted40 <-c()
quasiuni_sorted50 <-c()
quasiuni_sorted60 <-c()



setSeed(1)
morder=6
quasiuni_M<-sobol(n=(largesize1*3*morder), dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                  mixed = FALSE, method = "C", start = 1)
largesize1<-13824*2
samplesize=576*9
orderlist1_hlsmall<-createorderlist(quni1=quasiuni_M[,1:6],size=samplesize,interval=8,dimension=6)
orderlist1_hlsmall<-orderlist1_hlsmall[1:largesize1,]
orderlist1_hllarge<-createorderlist(quni1=quasiuni_M[,1:6],size=largesize1,interval=8,dimension=6)
orderlist1_hllarge<-orderlist1_hllarge[1:largesize1,]

morder=6
# unibatchran_M<-matrix(randtoolbox::SFMT(largesize1*3*morder),ncol=morder)
# 
# orderlist1_hlsmall_rand<-createorderlist(quni1=unibatchran_M[,1:6],size=samplesize,interval=8,dimension=6)
# orderlist1_hlsmall_rand<-orderlist1_hlsmall_rand[1:largesize1,]

setSeed(1)

orderlist1_AB6_randomall<-c()
for(i in (1:batchsizebase)){
  unibatchran_M<-matrix(randtoolbox::SFMT(largesize1*3*morder),ncol=morder)
  
  orderlist1_AB6_random<-createorderlist(quni1=unibatchran_M[,1:6],size=samplesize,interval=8,dimension=6)
  orderlist1_AB6_random<-orderlist1_AB6_random[1:largesize1,]
  
  unibatchran_M<-c()
  
  orderlist1_AB6_randomall<-cbind(orderlist1_AB6_randomall,orderlist1_AB6_random)
}


batchsize=batchsizebase

n <- samplesize
setSeed(1)
unibatchran<-matrix(SFMT(samplesize*batchsize),ncol=batchsize)

unibatch<-colSort(unibatchran, descend = FALSE, stable = FALSE, parallel = TRUE)

simulatedbatch_ABSE<-foreach(batchnumber =c((1:length(allkurtWeibull))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  a=allkurtWeibull[batchnumber]
  
  targetm<-gamma(1+1/(a/1))
  targetvar<-(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2)
  targettm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^3)*(gamma(1+3/(a/1))-3*(gamma(1+1/(a/1)))*((gamma(1+2/(a/1))))+2*((gamma(1+1/(a/1)))^3))/((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(3))
  targetfm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^4)*(gamma(1+4/(a/1))-4*(gamma(1+3/(a/1)))*((gamma(1+1/(a/1))))+6*(gamma(1+2/(a/1)))*((gamma(1+1/(a/1)))^2)-3*((gamma(1+1/(a/1)))^4))/(((gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(2))
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    
    x<-c(dsWeibull(uni=unibatch[,batch1], shape=a/1, scale = 1))
    
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))

    SEbataches<-rbind(SEbataches,all1)
  }
  
  write.csv(SEbataches,paste("Weibull_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}

write.csv(simulatedbatch_ABSE,paste("Weibull_ABSSE.csv", sep = ","), row.names = FALSE)



simulatedbatch_ABSE_SE<-foreach(batchnumber =c((1:length(allkurtWeibull))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  a=allkurtWeibull[batchnumber]
  
  targetm<-gamma(1+1/(a/1))
  targetvar<-(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2)
  targettm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^3)*(gamma(1+3/(a/1))-3*(gamma(1+1/(a/1)))*((gamma(1+2/(a/1))))+2*((gamma(1+1/(a/1)))^3))/((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(3))
  targetfm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^4)*(gamma(1+4/(a/1))-4*(gamma(1+3/(a/1)))*((gamma(1+1/(a/1))))+6*(gamma(1+2/(a/1)))*((gamma(1+1/(a/1)))^2)-3*((gamma(1+1/(a/1)))^4))/(((gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(2))
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("Weibull_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}

write.csv(simulatedbatch_ABSE_SE,paste("Weibull_ABSSE_error.csv", sep = ","), row.names = FALSE)


simulatedbatch_bias_Monte<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  library(randtoolbox)
  setSeed(1)
  set.seed(1)
  a=allkurtWeibull[60]
  
  targetm<-gamma(1+1/(a/1))
  targetvar<-(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2)
  targettm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^3)*(gamma(1+3/(a/1))-3*(gamma(1+1/(a/1)))*((gamma(1+2/(a/1))))+2*((gamma(1+1/(a/1)))^3))/((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(3))
  targetfm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^4)*(gamma(1+4/(a/1))-4*(gamma(1+3/(a/1)))*((gamma(1+1/(a/1))))+6*(gamma(1+2/(a/1)))*((gamma(1+1/(a/1)))^2)-3*((gamma(1+1/(a/1)))^4))/(((gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(2))
  kurtx<-c(kurtx=targetfm/(targetvar^(4/2)))
  skewx<-c(skewx=targettm/(targetvar^(3/2)))
  
  #bootsize for bootstrap approximation of the distributions of the kernal of U-statistics.
  n <- round(batchnumber*13824*2*3/100)
  (n%%10)==0
  # maximum order of moments
  morder <- 6
  #large sample size (approximating asymptotic)
  largesize<-round(batchnumber*13824*2/100)
  
  #generate quasirandom numbers based on the Sobol sequence
  quasiunisobol<-sobol(n=n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                       mixed = FALSE, method = "C", start = 1)
  
  quasiuni<-quasiunisobol
  
  quasiuni_sorted2 <- na.omit(rowSort(quasiuni[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted3 <- na.omit(rowSort(quasiuni[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted4 <- na.omit(rowSort(quasiuni[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted5 <- na.omit(rowSort(quasiuni[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted6 <- na.omit(rowSort(quasiuni[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  samplesize=576*9
  
  orderlist1_AB20<-createorderlist(quni1=quasiuni_sorted2,size=samplesize,interval=8,dimension=2)
  orderlist1_AB20<-orderlist1_AB20[1:largesize,]
  orderlist1_AB30<-createorderlist(quni1=quasiuni_sorted3,size=samplesize,interval=8,dimension=3)
  orderlist1_AB30<-orderlist1_AB30[1:largesize,]
  orderlist1_AB40<-createorderlist(quni1=quasiuni_sorted4,size=samplesize,interval=8,dimension=4)
  orderlist1_AB40<-orderlist1_AB40[1:largesize,]
  orderlist1_AB50<-createorderlist(quni1=quasiuni_sorted5,size=samplesize,interval=8,dimension=5)
  orderlist1_AB50<-orderlist1_AB50[1:largesize,]
  orderlist1_AB60<-createorderlist(quni1=quasiuni_sorted6,size=samplesize,interval=8,dimension=6)
  orderlist1_AB60<-orderlist1_AB60[1:largesize,]
  
  morder=6
  quasiuni_M<-sobol(n=(largesize*3*morder), dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                    mixed = FALSE, method = "C", start = 1)
  
  samplesize=576*9
  orderlist1_hlsmall<-createorderlist(quni1=quasiuni_M[,1:6],size=samplesize,interval=8,dimension=6)
  orderlist1_hlsmall<-orderlist1_hlsmall[1:largesize,]
  orderlist1_hllarge<-createorderlist(quni1=quasiuni_M[,1:6],size=largesize,interval=8,dimension=6)
  orderlist1_hllarge<-orderlist1_hllarge[1:largesize,]
  
  morder=6
  
  quasiunisobol0<-matrix(randtoolbox::SFMT(largesize*3*morder),ncol=morder)
  
  quasiuni0<-rbind(quasiunisobol0)
  
  quasiunisobol0<-c()
  
  quasiuni_sorted20 <- na.omit(rowSort(quasiuni0[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted30 <- na.omit(rowSort(quasiuni0[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted40 <- na.omit(rowSort(quasiuni0[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted50 <- na.omit(rowSort(quasiuni0[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted60 <- na.omit(rowSort(quasiuni0[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  orderlist1_AB20_rand <-createorderlist(quni1=quasiuni_sorted20 ,size=samplesize,interval=16,dimension=2)
  orderlist1_AB20_rand <-orderlist1_AB20_rand [1:largesize,]
  orderlist1_AB3_rand <-createorderlist(quni1=quasiuni_sorted30 ,size=samplesize,interval=16,dimension=3)
  orderlist1_AB3_rand <-orderlist1_AB3_rand [1:largesize,]
  orderlist1_AB4_rand <-createorderlist(quni1=quasiuni_sorted40 ,size=samplesize,interval=16,dimension=4)
  orderlist1_AB4_rand <-orderlist1_AB4_rand [1:largesize,]
  orderlist1_AB5_rand <-createorderlist(quni1=quasiuni_sorted50 ,size=samplesize,interval=16,dimension=5)
  orderlist1_AB5_rand <-orderlist1_AB5_rand [1:largesize,]
  orderlist1_AB6_rand <-createorderlist(quni1=quasiuni_sorted60 ,size=samplesize,interval=16,dimension=6)
  orderlist1_AB6_rand <-orderlist1_AB6_rand [1:largesize,]
  quasiuni_sorted20 <-c()
  quasiuni_sorted30 <-c()
  quasiuni_sorted40 <-c()
  quasiuni_sorted50 <-c()
  quasiuni_sorted60 <-c()
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    
    x<-c(dsWeibull(uni=unibatch[,batch1], shape=a/1, scale = 1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  write.csv(SEbataches,paste("finite_Weibull_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[60,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[60,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[60,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[60,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[60,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[60,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[60,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[60,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[60,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[60,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[60,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[60,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}


write.csv(simulatedbatch_bias_Monte,paste("finite_Weibull_bootstrapsize_raw",samplesize,".csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte_SE<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  largesize<-round(batchnumber*13824*2/100)
  a=allkurtWeibull[60]
  
  targetm<-gamma(1+1/(a/1))
  targetvar<-(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2)
  targettm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^3)*(gamma(1+3/(a/1))-3*(gamma(1+1/(a/1)))*((gamma(1+2/(a/1))))+2*((gamma(1+1/(a/1)))^3))/((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(3))
  targetfm<-((sqrt(gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^4)*(gamma(1+4/(a/1))-4*(gamma(1+3/(a/1)))*((gamma(1+1/(a/1))))+6*(gamma(1+2/(a/1)))*((gamma(1+1/(a/1)))^2)-3*((gamma(1+1/(a/1)))^4))/(((gamma(1+2/(a/1))-(gamma(((1+1/(a/1)))))^2))^(2))
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("finite_Weibull_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[60,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[60,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[60,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[60,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[60,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[60,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1

  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}

write.csv(simulatedbatch_bias_Monte_SE,paste("finite_Weibull_bootstrapsize_raw_error",samplesize,".csv", sep = ","), row.names = FALSE)



kurtgamma<- read.csv(("kurtgamma_31100.csv"))
allkurtgamma<-unlist(kurtgamma)

simulatedbatch_asymptoticbias<-foreach(batchnumber = (1:length(allkurtgamma)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  a=allkurtgamma[batchnumber]
  x<-c(dsgamma(uni=quasiuni_asymptotic, shape=a/1, rate  = 1))
  targetm<-a
  targetvar<-(a)
  targettm<-((sqrt(a))^3)*2/sqrt(a)
  targetfm<-((sqrt(a))^4)*((6/(a))+3)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
  
  targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
  x<-c()
  
  Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
  SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
  
  midhinge1<-midhinge(x=sortedx,sorted = TRUE)
  SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
  SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic,orderlist1_sorted3=orderlist1_AB3_asymptotic,orderlist1_sorted4=orderlist1_AB4_asymptotic,orderlist1_sorted5=orderlist1_AB5_asymptotic,orderlist1_sorted6=orderlist1_AB6_asymptotic,batch="auto")
  
  SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic_rand,orderlist1_sorted3=orderlist1_AB3_asymptotic_rand ,orderlist1_sorted4=orderlist1_AB4_asymptotic_rand ,orderlist1_sorted5=orderlist1_AB5_asymptotic_rand ,orderlist1_sorted6=orderlist1_AB6_asymptotic_rand ,batch="auto")
  
  rqm1<-rqm(x=sortedx,sorted = TRUE)
  
  MoM2<-median_of_means(sortedx,korder=2)
  MoM3<-median_of_means(sortedx,korder=3)
  MoM4<-median_of_means(sortedx,korder=4)
  MoM5<-median_of_means(sortedx,korder=5)
  MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
  
  imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB2_asymptotic,orderlist1_sorted30=orderlist1_AB3_asymptotic,orderlist1_sorted40=orderlist1_AB4_asymptotic,orderlist1_hlsmall=orderlist1_hllarge_asymptotic,orderlist1_hllarge=orderlist1_hllarge_asymptotic,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
  imoments1<-unlist(imoments1)
  momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
  
  #Generalized Catoni M-estimator
  #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
  # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
  
  Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
  
  #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
  QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
  
  alpha1<-QE1[1]-0.3
  alpha2<-QE1[1]+0.3
  
  #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
  RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
  
  #all parameter setting are from
  #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
  
  moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
  
  moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
  
  sortedx<-c()
  momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
  
  allrawmoBias<-c(
    firstbias=abs(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm)/sqrt(targetvar),
    secondbias=abs(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar)/momentssd[2],
    thirdbias=abs(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm)/momentssd[3],
    fourbias=abs(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm)/momentssd[4],
    skewbias=abs(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
    kurtbias=abs(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
  )
  
  allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
               second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
               third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
               fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
               skew=(c(imoments1[5],momentsx[7],imoments1[979])),
               kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
  )
  
  medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
  standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
  all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
}

write.csv(simulatedbatch_asymptoticbias,paste("asymptotic_gamma_raw_Process",largesize,".csv", sep = ","), row.names = FALSE)


simulatedbatch_ABSE<-foreach(batchnumber =c((1:length(allkurtgamma))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  
  a=allkurtgamma[batchnumber]
  
  targetm<-a
  targetvar<-(a)
  targettm<-((sqrt(a))^3)*2/sqrt(a)
  targetfm<-((sqrt(a))^4)*((6/(a))+3)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    
    x<-c(dsgamma(uni=unibatch[,batch1], shape=a/1, rate  = 1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  
  write.csv(SEbataches,paste("gamma_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}



write.csv(simulatedbatch_ABSE,paste("gamma_ABSSE.csv", sep = ","), row.names = FALSE)



simulatedbatch_ABSE_SE<-foreach(batchnumber =c((1:length(allkurtgamma))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  a=allkurtgamma[batchnumber]
  
  targetm<-a
  targetvar<-(a)
  targettm<-((sqrt(a))^3)*2/sqrt(a)
  targetfm<-((sqrt(a))^4)*((6/(a))+3)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("gamma_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}

write.csv(simulatedbatch_ABSE_SE,paste("gamma_ABSSE_error.csv", sep = ","), row.names = FALSE)



simulatedbatch_bias_Monte<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  library(randtoolbox)
  setSeed(1)
  set.seed(1)
  a=allkurtgamma[15]
  
  targetm<-a
  targetvar<-(a)
  targettm<-((sqrt(a))^3)*2/sqrt(a)
  targetfm<-((sqrt(a))^4)*((6/(a))+3)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  #bootsize for bootstrap approximation of the distributions of the kernal of U-statistics.
  n <- round(batchnumber*13824*2*3/100)
  (n%%10)==0
  # maximum order of moments
  morder <- 6
  #large sample size (approximating asymptotic)
  largesize<-round(batchnumber*13824*2/100)
  
  #generate quasirandom numbers based on the Sobol sequence
  quasiunisobol<-sobol(n=n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                       mixed = FALSE, method = "C", start = 1)
  
  quasiuni<-quasiunisobol
  
  quasiuni_sorted2 <- na.omit(rowSort(quasiuni[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted3 <- na.omit(rowSort(quasiuni[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted4 <- na.omit(rowSort(quasiuni[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted5 <- na.omit(rowSort(quasiuni[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted6 <- na.omit(rowSort(quasiuni[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  samplesize=576*9
  
  orderlist1_AB20<-createorderlist(quni1=quasiuni_sorted2,size=samplesize,interval=8,dimension=2)
  orderlist1_AB20<-orderlist1_AB20[1:largesize,]
  orderlist1_AB30<-createorderlist(quni1=quasiuni_sorted3,size=samplesize,interval=8,dimension=3)
  orderlist1_AB30<-orderlist1_AB30[1:largesize,]
  orderlist1_AB40<-createorderlist(quni1=quasiuni_sorted4,size=samplesize,interval=8,dimension=4)
  orderlist1_AB40<-orderlist1_AB40[1:largesize,]
  orderlist1_AB50<-createorderlist(quni1=quasiuni_sorted5,size=samplesize,interval=8,dimension=5)
  orderlist1_AB50<-orderlist1_AB50[1:largesize,]
  orderlist1_AB60<-createorderlist(quni1=quasiuni_sorted6,size=samplesize,interval=8,dimension=6)
  orderlist1_AB60<-orderlist1_AB60[1:largesize,]
  
  morder=6
  quasiuni_M<-sobol(n=(largesize*3*morder), dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                    mixed = FALSE, method = "C", start = 1)
  
  samplesize=576*9
  orderlist1_hlsmall<-createorderlist(quni1=quasiuni_M[,1:6],size=samplesize,interval=8,dimension=6)
  orderlist1_hlsmall<-orderlist1_hlsmall[1:largesize,]
  orderlist1_hllarge<-createorderlist(quni1=quasiuni_M[,1:6],size=largesize,interval=8,dimension=6)
  orderlist1_hllarge<-orderlist1_hllarge[1:largesize,]
  
  morder=6
  
  quasiunisobol0<-matrix(randtoolbox::SFMT(largesize*3*morder),ncol=morder)
  
  quasiuni0<-rbind(quasiunisobol0)
  
  quasiunisobol0<-c()
  
  quasiuni_sorted20 <- na.omit(rowSort(quasiuni0[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted30 <- na.omit(rowSort(quasiuni0[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted40 <- na.omit(rowSort(quasiuni0[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted50 <- na.omit(rowSort(quasiuni0[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted60 <- na.omit(rowSort(quasiuni0[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  orderlist1_AB20_rand <-createorderlist(quni1=quasiuni_sorted20 ,size=samplesize,interval=16,dimension=2)
  orderlist1_AB20_rand <-orderlist1_AB20_rand [1:largesize,]
  orderlist1_AB3_rand <-createorderlist(quni1=quasiuni_sorted30 ,size=samplesize,interval=16,dimension=3)
  orderlist1_AB3_rand <-orderlist1_AB3_rand [1:largesize,]
  orderlist1_AB4_rand <-createorderlist(quni1=quasiuni_sorted40 ,size=samplesize,interval=16,dimension=4)
  orderlist1_AB4_rand <-orderlist1_AB4_rand [1:largesize,]
  orderlist1_AB5_rand <-createorderlist(quni1=quasiuni_sorted50 ,size=samplesize,interval=16,dimension=5)
  orderlist1_AB5_rand <-orderlist1_AB5_rand [1:largesize,]
  orderlist1_AB6_rand <-createorderlist(quni1=quasiuni_sorted60 ,size=samplesize,interval=16,dimension=6)
  orderlist1_AB6_rand <-orderlist1_AB6_rand [1:largesize,]
  quasiuni_sorted20 <-c()
  quasiuni_sorted30 <-c()
  quasiuni_sorted40 <-c()
  quasiuni_sorted50 <-c()
  quasiuni_sorted60 <-c()
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    
    x<-c(dsgamma(uni=unibatch[,batch1], shape=a/1, rate  = 1))
    
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  write.csv(SEbataches,paste("finite_gamma_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[15,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[15,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[15,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[15,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[15,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[15,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[15,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[15,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[15,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[15,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[15,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[15,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}


write.csv(simulatedbatch_bias_Monte,paste("finite_gamma_bootstrapsize_raw",samplesize,".csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte_SE<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  largesize<-round(batchnumber*13824*2/100)
  a=allkurtgamma[15]
  
  targetm<-a
  targetvar<-(a)
  targettm<-((sqrt(a))^3)*2/sqrt(a)
  targetfm<-((sqrt(a))^4)*((6/(a))+3)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("finite_gamma_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","))
  
  SEbatachesmean <- colMeans(SEbataches)

  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[15,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[15,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[15,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[15,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[15,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[15,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}



write.csv(simulatedbatch_bias_Monte_SE,paste("finite_gamma_bootstrapsize_raw_error",samplesize,".csv", sep = ","), row.names = FALSE)




kurtPareto<- read.csv(("kurtPareto_91160.csv"))
allkurtPareto<-unlist(kurtPareto)

simulatedbatch_asymptoticbias<-foreach(batchnumber = (1:length(allkurtPareto)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  a=allkurtPareto[batchnumber]
  x<-c(dsPareto(uni=quasiuni_asymptotic, shape=a/1, scale = 1))
  
  targetm<-a/(a-1)
  targetvar<-(((a))*(1)/((-2+(a))*((-1+(a))^2)))
  targettm<-((((a)+1)*(2)*(sqrt(a-2)))/((-3+(a))*(((a))^(1/2))))*(((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^3))
  targetfm<-(3+(6*((a)^3+(a)^2-6*(a)-2)/(((a))*((-3+(a)))*((-4+(a))))))*((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
  
  targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
  x<-c()
  
  
  Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
  SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
  
  midhinge1<-midhinge(x=sortedx,sorted = TRUE)
  SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
  SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic,orderlist1_sorted3=orderlist1_AB3_asymptotic,orderlist1_sorted4=orderlist1_AB4_asymptotic,orderlist1_sorted5=orderlist1_AB5_asymptotic,orderlist1_sorted6=orderlist1_AB6_asymptotic,batch="auto")
  
  SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic_rand,orderlist1_sorted3=orderlist1_AB3_asymptotic_rand ,orderlist1_sorted4=orderlist1_AB4_asymptotic_rand ,orderlist1_sorted5=orderlist1_AB5_asymptotic_rand ,orderlist1_sorted6=orderlist1_AB6_asymptotic_rand ,batch="auto")
  
  rqm1<-rqm(x=sortedx,sorted = TRUE)
  
  MoM2<-median_of_means(sortedx,korder=2)
  MoM3<-median_of_means(sortedx,korder=3)
  MoM4<-median_of_means(sortedx,korder=4)
  MoM5<-median_of_means(sortedx,korder=5)
  MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
  
  imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB2_asymptotic,orderlist1_sorted30=orderlist1_AB3_asymptotic,orderlist1_sorted40=orderlist1_AB4_asymptotic,orderlist1_hlsmall=orderlist1_hllarge_asymptotic,orderlist1_hllarge=orderlist1_hllarge_asymptotic,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
  imoments1<-unlist(imoments1)
  momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
  
  #Generalized Catoni M-estimator
  #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
  # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
  
  Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
  
  #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
  QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
  
  alpha1<-QE1[1]-0.3
  alpha2<-QE1[1]+0.3
  
  #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
  RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
  
  #all parameter setting are from
  #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
  
  moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
  
  moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
  
  sortedx<-c()
  momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
  
  allrawmoBias<-c(
    firstbias=abs(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm)/sqrt(targetvar),
    secondbias=abs(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar)/momentssd[2],
    thirdbias=abs(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm)/momentssd[3],
    fourbias=abs(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm)/momentssd[4],
    skewbias=abs(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
    kurtbias=abs(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
  )
  
  allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
               second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
               third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
               fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
               skew=(c(imoments1[5],momentsx[7],imoments1[979])),
               kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
  )
  
  medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
  standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
  all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
}

write.csv(simulatedbatch_asymptoticbias,paste("asymptotic_Pareto_raw_Process",largesize,".csv", sep = ","), row.names = FALSE)


simulatedbatch_ABSE<-foreach(batchnumber =c((1:length(allkurtPareto))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  
  a=allkurtPareto[batchnumber]
  
  targetm<-a/(a-1)
  targetvar<-(((a))*(1)/((-2+(a))*((-1+(a))^2)))
  targettm<-((((a)+1)*(2)*(sqrt(a-2)))/((-3+(a))*(((a))^(1/2))))*(((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^3))
  targetfm<-(3+(6*((a)^3+(a)^2-6*(a)-2)/(((a))*((-3+(a)))*((-4+(a))))))*((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    
    x<-c(dsPareto(uni=unibatch[,batch1], shape=a/1, scale = 1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  
  write.csv(SEbataches,paste("Pareto_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}



write.csv(simulatedbatch_ABSE,paste("Pareto_ABSSE.csv", sep = ","), row.names = FALSE)



simulatedbatch_ABSE_SE<-foreach(batchnumber =c((1:length(allkurtPareto))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  
  a=allkurtPareto[batchnumber]
  
  targetm<-a/(a-1)
  targetvar<-(((a))*(1)/((-2+(a))*((-1+(a))^2)))
  targettm<-((((a)+1)*(2)*(sqrt(a-2)))/((-3+(a))*(((a))^(1/2))))*(((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^3))
  targetfm<-(3+(6*((a)^3+(a)^2-6*(a)-2)/(((a))*((-3+(a)))*((-4+(a))))))*((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("Pareto_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}
  


write.csv(simulatedbatch_ABSE_SE,paste("Pareto_ABSSE_error.csv", sep = ","), row.names = FALSE)



simulatedbatch_bias_Monte<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  library(randtoolbox)
  setSeed(1)
  set.seed(1)
  
  a=allkurtPareto[65]
  targetm<-a/(a-1)
  targetvar<-(((a))*(1)/((-2+(a))*((-1+(a))^2)))
  targettm<-((((a)+1)*(2)*(sqrt(a-2)))/((-3+(a))*(((a))^(1/2))))*(((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^3))
  targetfm<-(3+(6*((a)^3+(a)^2-6*(a)-2)/(((a))*((-3+(a)))*((-4+(a))))))*((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  
  #bootsize for bootstrap approximation of the distributions of the kernal of U-statistics.
  n <- round(batchnumber*13824*2*3/100)
  (n%%10)==0
  # maximum order of moments
  morder <- 6
  #large sample size (approximating asymptotic)
  largesize<-round(batchnumber*13824*2/100)
  
  #generate quasirandom numbers based on the Sobol sequence
  quasiunisobol<-sobol(n=n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                       mixed = FALSE, method = "C", start = 1)
  
  quasiuni<-quasiunisobol
  
  quasiuni_sorted2 <- na.omit(rowSort(quasiuni[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted3 <- na.omit(rowSort(quasiuni[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted4 <- na.omit(rowSort(quasiuni[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted5 <- na.omit(rowSort(quasiuni[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted6 <- na.omit(rowSort(quasiuni[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  samplesize=576*9
  
  orderlist1_AB20<-createorderlist(quni1=quasiuni_sorted2,size=samplesize,interval=8,dimension=2)
  orderlist1_AB20<-orderlist1_AB20[1:largesize,]
  orderlist1_AB30<-createorderlist(quni1=quasiuni_sorted3,size=samplesize,interval=8,dimension=3)
  orderlist1_AB30<-orderlist1_AB30[1:largesize,]
  orderlist1_AB40<-createorderlist(quni1=quasiuni_sorted4,size=samplesize,interval=8,dimension=4)
  orderlist1_AB40<-orderlist1_AB40[1:largesize,]
  orderlist1_AB50<-createorderlist(quni1=quasiuni_sorted5,size=samplesize,interval=8,dimension=5)
  orderlist1_AB50<-orderlist1_AB50[1:largesize,]
  orderlist1_AB60<-createorderlist(quni1=quasiuni_sorted6,size=samplesize,interval=8,dimension=6)
  orderlist1_AB60<-orderlist1_AB60[1:largesize,]
  
  morder=6
  quasiuni_M<-sobol(n=(largesize*3*morder), dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                    mixed = FALSE, method = "C", start = 1)
  
  samplesize=576*9
  orderlist1_hlsmall<-createorderlist(quni1=quasiuni_M[,1:6],size=samplesize,interval=8,dimension=6)
  orderlist1_hlsmall<-orderlist1_hlsmall[1:largesize,]
  orderlist1_hllarge<-createorderlist(quni1=quasiuni_M[,1:6],size=largesize,interval=8,dimension=6)
  orderlist1_hllarge<-orderlist1_hllarge[1:largesize,]
  
  morder=6
  
  quasiunisobol0<-matrix(randtoolbox::SFMT(largesize*3*morder),ncol=morder)
  
  quasiuni0<-rbind(quasiunisobol0)
  
  quasiunisobol0<-c()
  
  quasiuni_sorted20 <- na.omit(rowSort(quasiuni0[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted30 <- na.omit(rowSort(quasiuni0[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted40 <- na.omit(rowSort(quasiuni0[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted50 <- na.omit(rowSort(quasiuni0[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted60 <- na.omit(rowSort(quasiuni0[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  orderlist1_AB20_rand <-createorderlist(quni1=quasiuni_sorted20 ,size=samplesize,interval=16,dimension=2)
  orderlist1_AB20_rand <-orderlist1_AB20_rand [1:largesize,]
  orderlist1_AB3_rand <-createorderlist(quni1=quasiuni_sorted30 ,size=samplesize,interval=16,dimension=3)
  orderlist1_AB3_rand <-orderlist1_AB3_rand [1:largesize,]
  orderlist1_AB4_rand <-createorderlist(quni1=quasiuni_sorted40 ,size=samplesize,interval=16,dimension=4)
  orderlist1_AB4_rand <-orderlist1_AB4_rand [1:largesize,]
  orderlist1_AB5_rand <-createorderlist(quni1=quasiuni_sorted50 ,size=samplesize,interval=16,dimension=5)
  orderlist1_AB5_rand <-orderlist1_AB5_rand [1:largesize,]
  orderlist1_AB6_rand <-createorderlist(quni1=quasiuni_sorted60 ,size=samplesize,interval=16,dimension=6)
  orderlist1_AB6_rand <-orderlist1_AB6_rand [1:largesize,]
  quasiuni_sorted20 <-c()
  quasiuni_sorted30 <-c()
  quasiuni_sorted40 <-c()
  quasiuni_sorted50 <-c()
  quasiuni_sorted60 <-c()
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    x<-c(dsPareto(uni=unibatch[,batch1], shape=a/1, scale = 1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  write.csv(SEbataches,paste("finite_Pareto_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[65,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[65,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[65,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[65,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[65,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[65,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[65,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[65,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[65,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[65,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[65,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[65,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}



write.csv(simulatedbatch_bias_Monte,paste("finite_Pareto_bootstrapsize_raw",samplesize,".csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte_SE<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  largesize<-round(batchnumber*13824*2/100)
  a=allkurtPareto[65]
  
  targetm<-a/(a-1)
  targetvar<-(((a))*(1)/((-2+(a))*((-1+(a))^2)))
  targettm<-((((a)+1)*(2)*(sqrt(a-2)))/((-3+(a))*(((a))^(1/2))))*(((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^3))
  targetfm<-(3+(6*((a)^3+(a)^2-6*(a)-2)/(((a))*((-3+(a)))*((-4+(a))))))*((sqrt(((a))*(1)/((-2+(a))*((-1+(a))^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("finite_Pareto_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[65,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[65,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[65,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[65,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[65,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[65,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}



write.csv(simulatedbatch_bias_Monte_SE,paste("finite_Pareto_bootstrapsize_raw_error",samplesize,".csv", sep = ","), row.names = FALSE)




kurtlognorm<- read.csv(("kurtlognorm_31100.csv"))
allkurtlognorm<-unlist(kurtlognorm)


simulatedbatch_asymptoticbias<-foreach(batchnumber = (1:length(allkurtlognorm)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  a=allkurtlognorm[batchnumber]
  x<-c(dslnorm(uni=quasiuni_asymptotic, meanlog =0, sdlog  = a/1))
  targetm<-exp((a^2)/2)
  targetvar<-(exp((a/1)^2)*(-1+exp((a/1)^2)))
  targettm<-sqrt(exp((a/1)^2)-1)*((2+exp((a/1)^2)))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^3)
  targetfm<-((-3+exp(4*((a/1)^2))+2*exp(3*((a/1)^2))+3*exp(2*((a/1)^2))))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
  
  targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
  x<-c()
  
  
  Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
  SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
  
  midhinge1<-midhinge(x=sortedx,sorted = TRUE)
  SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
  SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic,orderlist1_sorted3=orderlist1_AB3_asymptotic,orderlist1_sorted4=orderlist1_AB4_asymptotic,orderlist1_sorted5=orderlist1_AB5_asymptotic,orderlist1_sorted6=orderlist1_AB6_asymptotic,batch="auto")
  
  SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic_rand,orderlist1_sorted3=orderlist1_AB3_asymptotic_rand ,orderlist1_sorted4=orderlist1_AB4_asymptotic_rand ,orderlist1_sorted5=orderlist1_AB5_asymptotic_rand ,orderlist1_sorted6=orderlist1_AB6_asymptotic_rand ,batch="auto")
  
  rqm1<-rqm(x=sortedx,sorted = TRUE)
  
  MoM2<-median_of_means(sortedx,korder=2)
  MoM3<-median_of_means(sortedx,korder=3)
  MoM4<-median_of_means(sortedx,korder=4)
  MoM5<-median_of_means(sortedx,korder=5)
  MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
  
  imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB2_asymptotic,orderlist1_sorted30=orderlist1_AB3_asymptotic,orderlist1_sorted40=orderlist1_AB4_asymptotic,orderlist1_hlsmall=orderlist1_hllarge_asymptotic,orderlist1_hllarge=orderlist1_hllarge_asymptotic,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
  imoments1<-unlist(imoments1)
  momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
  
  #Generalized Catoni M-estimator
  #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
  # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
  
  Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
  
  #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
  QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
  
  alpha1<-QE1[1]-0.3
  alpha2<-QE1[1]+0.3
  
  #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
  RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
  
  #all parameter setting are from
  #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
  
  moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
  
  moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
  
  sortedx<-c()
  momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
  
  allrawmoBias<-c(
    firstbias=abs(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm)/sqrt(targetvar),
    secondbias=abs(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar)/momentssd[2],
    thirdbias=abs(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm)/momentssd[3],
    fourbias=abs(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm)/momentssd[4],
    skewbias=abs(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
    kurtbias=abs(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
  )
  
  allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
               second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
               third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
               fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
               skew=(c(imoments1[5],momentsx[7],imoments1[979])),
               kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
  )
  
  medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
  standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
  all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
}

write.csv(simulatedbatch_asymptoticbias,paste("asymptotic_lognorm_raw_Process",largesize,".csv", sep = ","), row.names = FALSE)


simulatedbatch_ABSE<-foreach(batchnumber =c((1:length(allkurtlognorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  a=allkurtlognorm[batchnumber]
  
  targetm<-exp((a^2)/2)
  targetvar<-(exp((a/1)^2)*(-1+exp((a/1)^2)))
  targettm<-sqrt(exp((a/1)^2)-1)*((2+exp((a/1)^2)))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^3)
  targetfm<-((-3+exp(4*((a/1)^2))+2*exp(3*((a/1)^2))+3*exp(2*((a/1)^2))))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    
    x<-c(dslnorm(uni=unibatch[,batch1],  meanlog =0, sdlog = a/1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  
  write.csv(SEbataches,paste("lognorm_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}


write.csv(simulatedbatch_ABSE,paste("lognorm_ABSSE.csv", sep = ","), row.names = FALSE)



simulatedbatch_ABSE_SE<-foreach(batchnumber =c((1:length(allkurtlognorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  
  a=allkurtlognorm[batchnumber]
  
  targetm<-exp((a^2)/2)
  targetvar<-(exp((a/1)^2)*(-1+exp((a/1)^2)))
  targettm<-sqrt(exp((a/1)^2)-1)*((2+exp((a/1)^2)))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^3)
  targetfm<-((-3+exp(4*((a/1)^2))+2*exp(3*((a/1)^2))+3*exp(2*((a/1)^2))))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("lognorm_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}





write.csv(simulatedbatch_ABSE_SE,paste("lognorm_ABSSE_error.csv", sep = ","), row.names = FALSE)



simulatedbatch_bias_Monte<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  library(randtoolbox)
  setSeed(1)
  set.seed(1)
  
  a=allkurtlognorm[59]
  targetm<-exp((a^2)/2)
  targetvar<-(exp((a/1)^2)*(-1+exp((a/1)^2)))
  targettm<-sqrt(exp((a/1)^2)-1)*((2+exp((a/1)^2)))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^3)
  targetfm<-((-3+exp(4*((a/1)^2))+2*exp(3*((a/1)^2))+3*exp(2*((a/1)^2))))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  
  #bootsize for bootstrap approximation of the distributions of the kernal of U-statistics.
  n <- round(batchnumber*13824*2*3/100)
  (n%%10)==0
  # maximum order of moments
  morder <- 6
  #large sample size (approximating asymptotic)
  largesize<-round(batchnumber*13824*2/100)
  
  #generate quasirandom numbers based on the Sobol sequence
  quasiunisobol<-sobol(n=n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                       mixed = FALSE, method = "C", start = 1)
  
  quasiuni<-quasiunisobol
  
  quasiuni_sorted2 <- na.omit(rowSort(quasiuni[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted3 <- na.omit(rowSort(quasiuni[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted4 <- na.omit(rowSort(quasiuni[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted5 <- na.omit(rowSort(quasiuni[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted6 <- na.omit(rowSort(quasiuni[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  samplesize=576*9
  
  orderlist1_AB20<-createorderlist(quni1=quasiuni_sorted2,size=samplesize,interval=8,dimension=2)
  orderlist1_AB20<-orderlist1_AB20[1:largesize,]
  orderlist1_AB30<-createorderlist(quni1=quasiuni_sorted3,size=samplesize,interval=8,dimension=3)
  orderlist1_AB30<-orderlist1_AB30[1:largesize,]
  orderlist1_AB40<-createorderlist(quni1=quasiuni_sorted4,size=samplesize,interval=8,dimension=4)
  orderlist1_AB40<-orderlist1_AB40[1:largesize,]
  orderlist1_AB50<-createorderlist(quni1=quasiuni_sorted5,size=samplesize,interval=8,dimension=5)
  orderlist1_AB50<-orderlist1_AB50[1:largesize,]
  orderlist1_AB60<-createorderlist(quni1=quasiuni_sorted6,size=samplesize,interval=8,dimension=6)
  orderlist1_AB60<-orderlist1_AB60[1:largesize,]
  
  morder=6
  quasiuni_M<-sobol(n=(largesize*3*morder), dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                    mixed = FALSE, method = "C", start = 1)
  
  samplesize=576*9
  orderlist1_hlsmall<-createorderlist(quni1=quasiuni_M[,1:6],size=samplesize,interval=8,dimension=6)
  orderlist1_hlsmall<-orderlist1_hlsmall[1:largesize,]
  orderlist1_hllarge<-createorderlist(quni1=quasiuni_M[,1:6],size=largesize,interval=8,dimension=6)
  orderlist1_hllarge<-orderlist1_hllarge[1:largesize,]
  
  morder=6
  
  quasiunisobol0<-matrix(randtoolbox::SFMT(largesize*3*morder),ncol=morder)
  
  quasiuni0<-rbind(quasiunisobol0)
  
  quasiunisobol0<-c()
  
  quasiuni_sorted20 <- na.omit(rowSort(quasiuni0[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted30 <- na.omit(rowSort(quasiuni0[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted40 <- na.omit(rowSort(quasiuni0[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted50 <- na.omit(rowSort(quasiuni0[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted60 <- na.omit(rowSort(quasiuni0[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  orderlist1_AB20_rand <-createorderlist(quni1=quasiuni_sorted20 ,size=samplesize,interval=16,dimension=2)
  orderlist1_AB20_rand <-orderlist1_AB20_rand [1:largesize,]
  orderlist1_AB3_rand <-createorderlist(quni1=quasiuni_sorted30 ,size=samplesize,interval=16,dimension=3)
  orderlist1_AB3_rand <-orderlist1_AB3_rand [1:largesize,]
  orderlist1_AB4_rand <-createorderlist(quni1=quasiuni_sorted40 ,size=samplesize,interval=16,dimension=4)
  orderlist1_AB4_rand <-orderlist1_AB4_rand [1:largesize,]
  orderlist1_AB5_rand <-createorderlist(quni1=quasiuni_sorted50 ,size=samplesize,interval=16,dimension=5)
  orderlist1_AB5_rand <-orderlist1_AB5_rand [1:largesize,]
  orderlist1_AB6_rand <-createorderlist(quni1=quasiuni_sorted60 ,size=samplesize,interval=16,dimension=6)
  orderlist1_AB6_rand <-orderlist1_AB6_rand [1:largesize,]
  quasiuni_sorted20 <-c()
  quasiuni_sorted30 <-c()
  quasiuni_sorted40 <-c()
  quasiuni_sorted50 <-c()
  quasiuni_sorted60 <-c()
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    x<-c(dslnorm(uni=unibatch[,batch1],  meanlog =0, sdlog = a/1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  write.csv(SEbataches,paste("finite_lognormal_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[59,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[59,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[59,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[59,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[59,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[59,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[59,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[59,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[59,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[59,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[59,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[59,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}


write.csv(simulatedbatch_bias_Monte,paste("finite_lognormal_bootstrapsize_raw",samplesize,".csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte_SE<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  largesize<-round(batchnumber*13824*2/100)
  
  a=allkurtlognorm[59]
  targetm<-exp((a^2)/2)
  targetvar<-(exp((a/1)^2)*(-1+exp((a/1)^2)))
  targettm<-sqrt(exp((a/1)^2)-1)*((2+exp((a/1)^2)))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^3)
  targetfm<-((-3+exp(4*((a/1)^2))+2*exp(3*((a/1)^2))+3*exp(2*((a/1)^2))))*((sqrt(exp((a/1)^2)*(-1+exp((a/1)^2))))^4)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("finite_lognormal_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[59,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[59,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[59,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[59,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[59,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[59,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}



write.csv(simulatedbatch_bias_Monte_SE,paste("finite_lognormal_bootstrapsize_raw_error",samplesize,".csv", sep = ","), row.names = FALSE)



kurtgnorm<- read.csv(("kurtgnorm_31100.csv"))
allkurtgnorm<-unlist(kurtgnorm)


simulatedbatch_asymptoticbias<-foreach(batchnumber = (1:length(allkurtgnorm)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  a=allkurtgnorm[batchnumber]
  x<-c(dsgnorm(uni=quasiuni_asymptotic, shape=a/1, scale = 1))
  
  targetm<-0
  targetvar<-gamma(3/a)/((gamma(1/a)))
  targettm<-0
  targetfm<-((gamma(3/a)/((gamma(1/a))))^2)*gamma(5/a)*gamma(1/a)/((gamma(3/a))^2)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
  
  targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
  x<-c()
  
  
  Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
  SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
  
  midhinge1<-midhinge(x=sortedx,sorted = TRUE)
  SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
  SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic,orderlist1_sorted3=orderlist1_AB3_asymptotic,orderlist1_sorted4=orderlist1_AB4_asymptotic,orderlist1_sorted5=orderlist1_AB5_asymptotic,orderlist1_sorted6=orderlist1_AB6_asymptotic,batch="auto")
  
  SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB2_asymptotic_rand,orderlist1_sorted3=orderlist1_AB3_asymptotic_rand ,orderlist1_sorted4=orderlist1_AB4_asymptotic_rand ,orderlist1_sorted5=orderlist1_AB5_asymptotic_rand ,orderlist1_sorted6=orderlist1_AB6_asymptotic_rand ,batch="auto")
  
  rqm1<-rqm(x=sortedx,sorted = TRUE)
  
  MoM2<-median_of_means(sortedx,korder=2)
  MoM3<-median_of_means(sortedx,korder=3)
  MoM4<-median_of_means(sortedx,korder=4)
  MoM5<-median_of_means(sortedx,korder=5)
  MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
  
  imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB2_asymptotic,orderlist1_sorted30=orderlist1_AB3_asymptotic,orderlist1_sorted40=orderlist1_AB4_asymptotic,orderlist1_hlsmall=orderlist1_hllarge_asymptotic,orderlist1_hllarge=orderlist1_hllarge_asymptotic,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
  imoments1<-unlist(imoments1)
  momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
  
  #Generalized Catoni M-estimator
  #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
  # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
  
  Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
  
  #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
  QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
  
  alpha1<-QE1[1]-0.3
  alpha2<-QE1[1]+0.3
  
  #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
  RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
  
  #all parameter setting are from
  #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
  
  moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
  
  moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
  
  sortedx<-c()
  momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
  
  allrawmoBias<-c(
    firstbias=abs(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm)/sqrt(targetvar),
    secondbias=abs(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar)/momentssd[2],
    thirdbias=abs(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm)/momentssd[3],
    fourbias=abs(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm)/momentssd[4],
    skewbias=abs(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
    kurtbias=abs(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
  )
  
  allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
               second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
               third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
               fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
               skew=(c(imoments1[5],momentsx[7],imoments1[979])),
               kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
  )
  
  medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
  standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
  all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
}
write.csv(simulatedbatch_asymptoticbias,paste("asymptotic_gnorm_raw_Process",largesize,".csv", sep = ","), row.names = FALSE)


simulatedbatch_ABSE<-foreach(batchnumber =c((1:length(allkurtgnorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  
  a=allkurtgnorm[batchnumber]
  
  targetm<-0
  targetvar<-gamma(3/a)/((gamma(1/a)))
  targettm<-0
  targetfm<-((gamma(3/a)/((gamma(1/a))))^2)*gamma(5/a)*gamma(1/a)/((gamma(3/a))^2)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    
    x<-c(dsgnorm(uni=unibatch[,batch1], shape=a/1, scale = 1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  
  write.csv(SEbataches,paste("gnorm_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}


write.csv(simulatedbatch_ABSE,paste("gnorm_ABSSE.csv", sep = ","), row.names = FALSE)


simulatedbatch_ABSE_SE<-foreach(batchnumber =c((1:length(allkurtgnorm))), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(randtoolbox)
  library(REDSReview)
  setSeed(1)
  set.seed(1)
  
  
  a=allkurtgnorm[batchnumber]
  
  targetm<-0
  targetvar<-gamma(3/a)/((gamma(1/a)))
  targettm<-0
  targetfm<-((gamma(3/a)/((gamma(1/a))))^2)*gamma(5/a)*gamma(1/a)/((gamma(3/a))^2)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("gnorm_raw_ABSSE_finite",samplesize,round(kurtx,digits = 1),".csv", sep = ","))
  
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[batchnumber,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[batchnumber,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[batchnumber,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}



write.csv(simulatedbatch_ABSE_SE,paste("gnorm_ABSSE_error.csv", sep = ","), row.names = FALSE)



simulatedbatch_bias_Monte<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  library(randtoolbox)
  setSeed(1)
  set.seed(1)
  
  a=allkurtgnorm[1]
  targetm<-0
  targetvar<-gamma(3/a)/((gamma(1/a)))
  targettm<-0
  targetfm<-((gamma(3/a)/((gamma(1/a))))^2)*gamma(5/a)*gamma(1/a)/((gamma(3/a))^2)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  
  #bootsize for bootstrap approximation of the distributions of the kernal of U-statistics.
  n <- round(batchnumber*13824*2*3/100)
  (n%%10)==0
  # maximum order of moments
  morder <- 6
  #large sample size (approximating asymptotic)
  largesize<-round(batchnumber*13824*2/100)
  
  #generate quasirandom numbers based on the Sobol sequence
  quasiunisobol<-sobol(n=n, dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                       mixed = FALSE, method = "C", start = 1)
  
  quasiuni<-quasiunisobol
  
  quasiuni_sorted2 <- na.omit(rowSort(quasiuni[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted3 <- na.omit(rowSort(quasiuni[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted4 <- na.omit(rowSort(quasiuni[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted5 <- na.omit(rowSort(quasiuni[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted6 <- na.omit(rowSort(quasiuni[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  samplesize=576*9
  
  orderlist1_AB20<-createorderlist(quni1=quasiuni_sorted2,size=samplesize,interval=8,dimension=2)
  orderlist1_AB20<-orderlist1_AB20[1:largesize,]
  orderlist1_AB30<-createorderlist(quni1=quasiuni_sorted3,size=samplesize,interval=8,dimension=3)
  orderlist1_AB30<-orderlist1_AB30[1:largesize,]
  orderlist1_AB40<-createorderlist(quni1=quasiuni_sorted4,size=samplesize,interval=8,dimension=4)
  orderlist1_AB40<-orderlist1_AB40[1:largesize,]
  orderlist1_AB50<-createorderlist(quni1=quasiuni_sorted5,size=samplesize,interval=8,dimension=5)
  orderlist1_AB50<-orderlist1_AB50[1:largesize,]
  orderlist1_AB60<-createorderlist(quni1=quasiuni_sorted6,size=samplesize,interval=8,dimension=6)
  orderlist1_AB60<-orderlist1_AB60[1:largesize,]
  
  morder=6
  quasiuni_M<-sobol(n=(largesize*3*morder), dim = morder, init = TRUE, scrambling = 0, seed = NULL, normal = FALSE,
                    mixed = FALSE, method = "C", start = 1)
  
  samplesize=576*9
  orderlist1_hlsmall<-createorderlist(quni1=quasiuni_M[,1:6],size=samplesize,interval=8,dimension=6)
  orderlist1_hlsmall<-orderlist1_hlsmall[1:largesize,]
  orderlist1_hllarge<-createorderlist(quni1=quasiuni_M[,1:6],size=largesize,interval=8,dimension=6)
  orderlist1_hllarge<-orderlist1_hllarge[1:largesize,]
  
  morder=6
  
  quasiunisobol0<-matrix(randtoolbox::SFMT(largesize*3*morder),ncol=morder)
  
  quasiuni0<-rbind(quasiunisobol0)
  
  quasiunisobol0<-c()
  
  quasiuni_sorted20 <- na.omit(rowSort(quasiuni0[,1:2], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted30 <- na.omit(rowSort(quasiuni0[,1:3], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted40 <- na.omit(rowSort(quasiuni0[,1:4], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted50 <- na.omit(rowSort(quasiuni0[,1:5], descend = FALSE, stable = FALSE, parallel = TRUE))
  quasiuni_sorted60 <- na.omit(rowSort(quasiuni0[,1:6], descend = FALSE, stable = FALSE, parallel = TRUE))
  
  orderlist1_AB20_rand <-createorderlist(quni1=quasiuni_sorted20 ,size=samplesize,interval=16,dimension=2)
  orderlist1_AB20_rand <-orderlist1_AB20_rand [1:largesize,]
  orderlist1_AB3_rand <-createorderlist(quni1=quasiuni_sorted30 ,size=samplesize,interval=16,dimension=3)
  orderlist1_AB3_rand <-orderlist1_AB3_rand [1:largesize,]
  orderlist1_AB4_rand <-createorderlist(quni1=quasiuni_sorted40 ,size=samplesize,interval=16,dimension=4)
  orderlist1_AB4_rand <-orderlist1_AB4_rand [1:largesize,]
  orderlist1_AB5_rand <-createorderlist(quni1=quasiuni_sorted50 ,size=samplesize,interval=16,dimension=5)
  orderlist1_AB5_rand <-orderlist1_AB5_rand [1:largesize,]
  orderlist1_AB6_rand <-createorderlist(quni1=quasiuni_sorted60 ,size=samplesize,interval=16,dimension=6)
  orderlist1_AB6_rand <-orderlist1_AB6_rand [1:largesize,]
  quasiuni_sorted20 <-c()
  quasiuni_sorted30 <-c()
  quasiuni_sorted40 <-c()
  quasiuni_sorted50 <-c()
  quasiuni_sorted60 <-c()
  
  SEbataches<-c()
  for (batch1 in c(1:batchsize)){
    x<-c(dsgnorm(uni=unibatch[,batch1], shape=a/1, scale = 1))
    sortedx<-Sort(x,descending=FALSE,partial=NULL,stable=FALSE,na.last=NULL)
    targetall<-c(targetm=targetm,targetvar=targetvar,targettm=targettm,targetfm=targetfm)
    x<-c()
    
    Huberx<-Huber_estimator(x=sortedx, tol = 1e-10)
    SMWM9<-SWA9(x=sortedx,interval=9,batch="auto",sorted=TRUE)
    
    midhinge1<-midhinge(x=sortedx,sorted = TRUE)
    SWA81<-SWA8(x=sortedx,interval=8,batch="auto",sorted = TRUE)
    SWAHlmean1<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20,orderlist1_sorted3=orderlist1_AB30,orderlist1_sorted4=orderlist1_AB40,orderlist1_sorted5=orderlist1_AB50,orderlist1_sorted6=orderlist1_AB60,batch="auto")
    
    SWAHlmean1_rand<-SWAHLmean(x=sortedx,orderlist1_sorted2=orderlist1_AB20_rand,orderlist1_sorted3=orderlist1_AB3_rand,orderlist1_sorted4=orderlist1_AB4_rand ,orderlist1_sorted5=orderlist1_AB5_rand ,orderlist1_sorted6=orderlist1_AB6_rand ,batch="auto")
    
    rqm1<-rqm(x=sortedx,sorted = TRUE)
    
    MoM2<-median_of_means(sortedx,korder=2)
    MoM3<-median_of_means(sortedx,korder=3)
    MoM4<-median_of_means(sortedx,korder=4)
    MoM5<-median_of_means(sortedx,korder=5)
    MoM519<-median_of_means(sortedx,korder=log(0.5,7/8))
    
    imoments1<-imoments(x=sortedx,dtype1=1,Iskewtype1 = 4,Ikurttype1 = 5,releaseall=TRUE,standist_d=d_values,standist_I=I_values,standist_Ismoments=Ismoments_values,standist_Imoments=Imoments_values,orderlist1_sorted20=orderlist1_AB20,orderlist1_sorted30=orderlist1_AB30,orderlist1_sorted40=orderlist1_AB40,orderlist1_hlsmall=orderlist1_hlsmall,orderlist1_hllarge=orderlist1_hllarge,percentage=1/24,batch="auto",stepsize=50,criterion=1e-10,boot=TRUE)
    imoments1<-unlist(imoments1)
    
    momentsx<-standardizedmoments(x=sortedx,releaseall = TRUE)
    
    #Generalized Catoni M-estimator
    #P Chen, X Jin, X Li, L Xu, A generalized Catoni’s M-estimator under finite α-th moment
    # assumption with α ∈ (1, 2). Electron. J. Stat. 15, 5523 – 5544 (2021).
    
    Generalized_Catoni<-Catoni_estimator(x=sortedx,beta=0.0001,alpha=1.5,bend=1.5)
    
    #NB Marks, Estimation of weibull parameters from common percentiles. J. applied Stat. 32, 17–24 (2005). 
    QE1<-Weibull_quantile_estimator(x=sortedx,sorted=TRUE)
    
    alpha1<-QE1[1]-0.3
    alpha2<-QE1[1]+0.3
    
    #X He, WK Fung, Method of medians for lifetime data with weibull models. Stat. medicine 18, 1993–2009 (1999)
    RMLE1<-Weibull_RMLE(sortedx,alpha1=alpha1,alpha2=alpha2)
    
    #all parameter setting are from
    #K Boudt, D Caliskan, C Croux, Robust explicit estimators of weibull parameters. Metrika 73, 187–209 (2011).
    
    moments_QE1<-Weibull_moments(alpha=QE1[1],lambda=QE1[2])
    
    moments_RMLE1<-Weibull_moments(alpha=RMLE1[1],lambda=RMLE1[2])
    
    sortedx<-c()
    momentssd<-c(sd=sqrt(momentsx[2]),imoments1[975],imoments1[976],imoments1[977])
    
    allrawmoBias<-c(
      firstbias=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])-targetm),
      secondbias=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])-targetvar),
      thirdbias=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])-targettm),
      fourbias=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])-targetfm),
      skewbias=(c(imoments1[5],momentsx[7],imoments1[979])-skewx),
      kurtbias=(c(imoments1[6],momentsx[8],imoments1[978])-kurtx)
    )
    
    allrawmo1<-c(first=(c(Huberx,Generalized_Catoni,SMWM9,midhinge1,SWA81=SWA81,SWAHlmean1=SWAHlmean1,SWAHlmean1_rand=SWAHlmean1_rand,rqm1,MoM2=MoM2,MoM3=MoM3,MoM4=MoM4,MoM5=MoM5,MoM519=MoM519,imoments1[1],momentsx[1],imoments1[c((7):(78),199:270,391:462,583:654,775:812)],mean_QE1=moments_QE1[1],mean_RMLE1=moments_RMLE1[1])),
                 second=(c(imoments1[2],momentsx[2],imoments1[c((79):(130),271:322,463:514,655:706,849:876)],var_QE1=moments_QE1[2],var_RMLE1=moments_RMLE1[2])),
                 third=(c(imoments1[3],momentsx[3],imoments1[c((131):(170),323:362,515:554,707:746,903:924)],tm_QE1=moments_QE1[3],tm_RMLE1=moments_RMLE1[3])),
                 fourth=(c(imoments1[4],momentsx[4],imoments1[c((171):(198),363:390,555:582,747:774,945:960)],fm_QE1=moments_QE1[4],fm_RMLE1=moments_RMLE1[4])),
                 skew=(c(imoments1[5],momentsx[7],imoments1[979])),
                 kurt=(c(imoments1[6],momentsx[8],imoments1[978]))
    )
    
    medianmoments<-c(imoments1[782],imoments1[856],imoments1[910],imoments1[951])
    standardizedm<-c(imoments1[782]/sqrt(targetvar),imoments1[856]/momentssd[2],imoments1[910]/momentssd[3],imoments1[951]/momentssd[4])
    all1<-(c(kurtx=kurtx,skewx=skewx,momentsx,allrawmoBias,momentssd,medianmoments,standardizedm=standardizedm,allrawmo1,Huberx,SMWM9,imoments1,targetall))
    
    SEbataches<-rbind(SEbataches,all1)
  }
  
  write.csv(SEbataches,paste("finite_gnorm_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","), row.names = FALSE)
  
  RMSE1_mean<-sqrt(colMeans((SEbataches[,11:416])^2))/sqrt(targetvar)
  
  RMSE1_var<-sqrt(colMeans((SEbataches[,417:656])^2))/simulatedbatch_asymptoticbias[1,2940]
  
  RMSE1_tm<-sqrt(colMeans((SEbataches[,657:842])^2))/simulatedbatch_asymptoticbias[1,2941]
  
  RMSE1_fm<-sqrt(colMeans((SEbataches[,843:974])^2))/simulatedbatch_asymptoticbias[1,2942]
  
  RMSE1_skew<-sqrt(colMeans((SEbataches[,975:977])^2))
  
  RMSE1_kurt<-sqrt(colMeans((SEbataches[,978:980])^2))
  
  AB1_mean<-abs(colMeans((SEbataches[,11:416])))/sqrt(targetvar)
  
  AB1_var<-abs(colMeans((SEbataches[,417:656])))/simulatedbatch_asymptoticbias[1,2940]
  
  AB1_tm<-abs(colMeans((SEbataches[,657:842])))/simulatedbatch_asymptoticbias[1,2941]
  
  AB1_fm<-abs(colMeans((SEbataches[,843:974])))/simulatedbatch_asymptoticbias[1,2942]
  
  AB1_skew<-abs(colMeans((SEbataches[,975:977])))
  
  AB1_kurt<-abs(colMeans((SEbataches[,978:980])))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, unbiasedsd)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, unbiasedsd)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, unbiasedsd)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[1,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, unbiasedsd)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[1,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, unbiasedsd)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[1,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, unbiasedsd)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[1,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[1939]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, unbiasedsd)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[1,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, unbiasedsd)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[1,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, unbiasedsd)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, unbiasedsd)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, unbiasedsd)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, unbiasedsd)
  kurt_SSE1<-kurtsd1
  
  
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],RMSE1_mean=RMSE1_mean,RMSE1_var=RMSE1_var,RMSE1_tm=RMSE1_tm,RMSE1_fm=RMSE1_fm,RMSE1_skew=RMSE1_skew,RMSE1_kurt=RMSE1_kurt,AB1_mean=AB1_mean,AB1_var=AB1_var,AB1_tm=AB1_tm,AB1_fm=AB1_fm,AB1_skew=AB1_skew,AB1_kurt=AB1_kurt,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  
  allErrors
}


write.csv(simulatedbatch_bias_Monte,paste("finite_gnorm_bootstrapsize_raw",samplesize,".csv", sep = ","), row.names = FALSE)

simulatedbatch_bias_Monte_SE<-foreach(batchnumber =c((1:100)), .combine = 'rbind') %dopar% {
  library(Rfast)
  library(matrixStats)
  library(REDSReview)
  largesize<-round(batchnumber*13824*2/100)
  a=allkurtgnorm[1]
  targetm<-0
  targetvar<-gamma(3/a)/((gamma(1/a)))
  targettm<-0
  targetfm<-((gamma(3/a)/((gamma(1/a))))^2)*gamma(5/a)*gamma(1/a)/((gamma(3/a))^2)
  kurtx<-targetfm/(targetvar^(4/2))
  skewx<-targettm/(targetvar^(3/2))
  
  SEbataches<- read.csv(paste("finite_gnorm_bootstrapsize_raw",batchnumber,round(kurtx,digits = 1),".csv", sep = ","))
  
  SEbatachesmean <- colMeans(SEbataches)
  
  ratiomean1<-c(SEbatachesmean[993:1398])/SEbatachesmean[1359]
  
  meansd_unscaled1<-apply((SEbataches[1:batchsize,993:1398]), 2, se_mean)
  
  mean_SE1<-meansd_unscaled1/sqrt(targetvar)
  mean_SEbatachesmeanprocess<-t(t(SEbataches[1:batchsize,993:1398])/ratiomean1)
  
  meansd1<-apply((mean_SEbatachesmeanprocess), 2, se_mean)
  mean_SSE1<-meansd1/sqrt(targetvar)
  
  ratiovar1<-SEbatachesmean[1399:1638]/SEbatachesmean[1609]
  
  varsd_unscaled1<-apply((SEbataches[1:batchsize,1399:1638]), 2, se_mean)
  
  var_SE1<-varsd_unscaled1/simulatedbatch_asymptoticbias[1,2940]
  
  var_SEbatachesvarprocess<-(t(t(SEbataches[1:batchsize,1399:1638])/ratiovar1))
  
  varsd1<-apply(var_SEbatachesvarprocess, 2, se_mean)
  
  var_SSE1<-varsd1/simulatedbatch_asymptoticbias[1,2940]
  
  ratiotm1<-SEbatachesmean[1639:1824]/SEbatachesmean[1801]
  
  tmsd_unscaled1<-apply((SEbataches[1:batchsize,1639:1824]), 2, se_mean)
  
  tm_SE1<-tmsd_unscaled1/simulatedbatch_asymptoticbias[1,2941]
  
  tm_SEbatachestmprocess<-(t(t(SEbataches[1:batchsize,1639:1824])/ratiotm1))
  tmsd1<-apply(tm_SEbatachestmprocess, 2, se_mean)
  tm_SSE1<-tmsd1/simulatedbatch_asymptoticbias[1,2941]
  
  ratiofm1<-SEbatachesmean[1825:1956]/SEbatachesmean[2942]
  
  fmsd_unscaled1<-apply((SEbataches[1:batchsize,1825:1956]), 2, se_mean)
  
  fm_SE1<-fmsd_unscaled1/simulatedbatch_asymptoticbias[1,2942]
  
  fm_SEbatachesfmprocess<-(t(t(SEbataches[1:batchsize,1825:1956])/ratiofm1))
  fmsd1<-apply(fm_SEbatachesfmprocess, 2, se_mean)
  fm_SSE1<-fmsd1/simulatedbatch_asymptoticbias[1,2942]
  
  ratio_skew1<-SEbatachesmean[1957:1959]/SEbatachesmean[1959]
  
  skewsd_unscaled1<-apply((SEbataches[1:batchsize,1957:1959]), 2, se_mean)
  
  skew_SE1<-skewsd_unscaled1
  
  skew_SEbatachesskewprocess<-(t(t(SEbataches[1:batchsize,1957:1959])/ratio_skew1))
  skewsd1<-apply(skew_SEbatachesskewprocess, 2, se_mean)
  skew_SSE1<-skewsd1
  
  ratio_kurt1<-SEbatachesmean[1960:1962]/SEbatachesmean[1962]
  
  kurtsd_unscaled1<-apply((SEbataches[1:batchsize,1960:1962]), 2, se_mean)
  
  kurt_SE1<-kurtsd_unscaled1
  
  kurt_SEbatacheskurtprocess<-(t(t(SEbataches[1:batchsize,1960:1962])/ratio_kurt1))
  kurtsd1<-apply(kurt_SEbatacheskurtprocess, 2, se_mean)
  kurt_SSE1<-kurtsd1
  
  allSE<-c(mean_SE1=mean_SE1,SEbatachesmean[1],var_SE1=var_SE1,SEbatachesmean[1],tm_SE1=tm_SE1,SEbatachesmean[1],fm_SE1=fm_SE1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSE_unstan<-c(SEbatachesmean[1],meansd_unscaled1=meansd_unscaled1,SEbatachesmean[1],varsd_unscaled1=varsd_unscaled1,SEbatachesmean[1],
                  
                  tmsd_unscaled1=tmsd_unscaled1,SEbatachesmean[1],fmsd_unscaled1=fmsd_unscaled1,SEbatachesmean[1],skew_SE1=skew_SE1,SEbatachesmean[1],kurt_SE1=kurt_SE1
  )
  allSSE<-c(SEbatachesmean[1],mean_SSE1=mean_SSE1,SEbatachesmean[1],var_SSE1=var_SSE1,SEbatachesmean[1],
            tm_SSE1=tm_SSE1,SEbatachesmean[1],fm_SSE1=fm_SSE1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  allSSE_unstand<-c(SEbatachesmean[1],meansd1=meansd1,SEbatachesmean[1],varsd1=varsd1,SEbatachesmean[1],
                    tmsd1=tmsd1,SEbatachesmean[1],fmsd1=fmsd1,SEbatachesmean[1],skew_SSE1=skew_SSE1,SEbatachesmean[1],kurt_SSE1=kurt_SSE1
  )
  
  
  se_mean_all1<-apply((SEbataches[1:batchsize,]), 2, se_mean)
  allErrors<-c(samplesize=samplesize,kurt=SEbatachesmean[1],skew=SEbatachesmean[2],se_mean=se_mean_all1,allSE=allSE,allSSE=allSSE,allSE_unstan=allSE_unstan,allSSE_unstand=allSSE_unstand,SEbatachesmean=SEbatachesmean)
  
  allErrors
}



write.csv(simulatedbatch_bias_Monte_SE,paste("finite_gnorm_bootstrapsize_raw_error",samplesize,".csv", sep = ","), row.names = FALSE)



stopCluster(cl)
registerDoSEQ()

