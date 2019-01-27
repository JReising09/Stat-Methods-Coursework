# Data:  Time (in minutes) that a drug takes to relieve an irritated eye
# (irritation measured by color/redness) 

# Source:  Rao (1998), p. 178.

time <- c(0.4,4.6,2.2,1.2,4.5,5.7,8.0,2.1,4.8,3.0,8.8,11.4,1.3,1.4,2.1,1.3,12.5,2.4,4.6,2.8)
n<-length(time)
# Histogram:

boxplot(time)

#NEED TO CHECK FOR NORMALITY BC OF n<30 (MORE LIKE n<50)
# Normal Q-Q plot and Test:

qqnorm(time)
qqline(time)

shapiro.test(time)

#Try log transformation
shapiro.test(log(time))

######Insert t.test for log transform #####
t.test(log(time),mu=log(5))
###########################################

# R does not have a built-in sign test function, but we can write our own:
# (The code for this function is from Prof. Brian Habing at USC):

# Copy and paste the following function code into R:

#### Beginning of quantile.test function ####
####
quantile.test<-function(x,eta=0,quantile=.5,alternative="two.sided"){
  n<-length(x)
  p<-quantile
  T1<-sum(x<=eta)
  T2<-sum(x< eta)
  if (alternative=="less") {
    p.value<-1-pbinom(T2-1,n,p)}
  if (alternative=="greater"){
    p.value<-pbinom(T1,n,p)}
  if (alternative=="two.sided"){
    p.value<-2*min(1-pbinom(T2-1,n,p),pbinom(T1,n,p))}
  list(eta=eta,alternative=alternative,T1=T1,T2=T2,p.value=p.value)}
####
#### End of quantile.test function ####


# We want to test whether the true median time-to-relief equals 5: 
# As the eta argument, we enter this value 5.
# For the sign test about the MEDIAN, the quantile argument is always 0.5

quantile.test(time,eta=5,quantile=0.5,alternative="two.sided")

#########################################################################################

# A confidence interval for a quantile (such as the median) can be gotten using the following function:

#### Beginning of quantile.interval function ####

quantile.interval<-function(x,quantile=.5,conf.level=.95){
  n<-length(x)
  p<-quantile
  alpha<-1-conf.level
  rmin1<-qbinom(alpha/2,n,p)-1
  r<-rmin1+1
  alpha1<-pbinom(r-1,n,p)
  smin1<-qbinom(1-alpha/2,n,p)
  s<-smin1+1
  alpha2<-1-pbinom(s-1,n,p)
  clo<-sort(x)[r]
  chi<-sort(x)[s]
  conf.level<-1-alpha1-alpha2
  list(quantile=quantile,conf.level=conf.level,r=r,s=s,interval=c(clo,chi))}

#### End of quantile.interval function ####


# To run the function:

quantile.interval(time,quantile=0.5,conf.level=.95)

# This actually gives a CI for the median whose coverage probability is AT LEAST 0.95.

binom.test(5,20,alternative="two.sided") # alternative for p-value

install.packages("signmedian.test",repos="http://ftp.ussg.iu.edu/CRAN/") # Only need to do once
library(signmedian.test) 

signmedian.test(time,mu=5)