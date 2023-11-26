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

#this is a demo file explaining this package, NRS.

#This file is totally not relevant to the reviewing of NRS. I will gradually update once I have time..

#you can simulate a distribution with any sample size。
x<-rexp(5184)

#SWA function will return all six SWAs with breakdown point 1/8 plus mean and median.

#since 5207 is not a multiple of 8, there are two solutions
5207%%8

#one is randomly sampling a smaller sample that is a multiple of 8
#set the rand=TRUE, the blocknumber=1/percentage.
SWA(x,percentage=1/32,blocknumber=32,batch="auto",sorted=FALSE,rand=TRUE)

#another solution is forming an additional block, the middle block, that take the reminder into account.
SWA(x,percentage=1/8,blocknumber=9,batch="auto",sorted=FALSE,rand=FALSE)

#the first approach is used here, since it is accurate.

#median Hodges-Lehmann mean using the quasi-bootstrap
mHLM(x,dimension=4,boot=TRUE,quasi=TRUE,largesize=1.8*10^4)

#using bootstrap is equavalent to median of randomized means
mHLM(x,dimension=4,boot=TRUE,quasi=FALSE,largesize=1.8*10^4)

#compared to median of means 
median_of_means(x,korder=4)


#compared to recombined mean and quantile mean
rqm(x)

#the principle can be extended to central moments
SWAmoments(x)

#invariant moments
imoments(x)

#compared to standardized moments
standardizedmoments(x,releaseall=TRUE)

#you should ensure that the sample you provided have kurtosis smaller than 26, this is the current limit, but will improved soon.

#finally, median standardized moments are also provided, that is very helpful for studying distributions with infinite moments.
medianstandardizedmoments(x)

#for example
x<-rPareto(5184,shape=1, scale=1)


imoments(x)
#warning message will return, since the kurtosis is larger than 26.
#this is in fact based on the d values of Weibull distribution with kurtosis 26 and skewness 3.68, so the values will be 
#always close to this combination.

#the number in front of the warning message is the number of that message repeated.

#so median standardized moments are better choices.
medianstandardizedmoments(x)
#sample moments are invalid.
standardizedmoments(x)
#if the sample size is smaller than 4096, warning message will also return
#the finite sample bias hasn't not been integrated into this package yet. Will update later.
x<-rexp(5184)
imoments(x)

#if the distribution is not Weibull, small biases might exist, but much smaller than others.
x<-rnorm(2654208)

SWAmoments(x)

medianstandardizedmoments(x)

imoments(x)
#warning message also return, since the minimum skewness used to calibrate is 0.293.

standardizedmoments(x)

#even for the kurtosis, the bias is relatively small

#in fact, this can be further improved, but due to the length limit, this is all at this stage.

#related statistical tests will update later.


