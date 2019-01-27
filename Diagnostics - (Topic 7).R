# R code to analyze the brain weight data
# using multiple linear regression

my.datafile <- tempfile()
cat(file=my.datafile, "
    Viscosity Pressure Temp
    0.099 42.49 369.9
    0.168 37.97 425.2
    0.245 33.69 469.7
    0.296 30.12 507.4
    0.39 27.36 540.3
    0.511 24.66 568.8
    0.672 23.06 595.7
    0.863 21.23 618.4
    1.1 19.66 638.8
    1.39 18.24 658.2
    1.72 17.23 675.8
    2.11 16.21 692.4
    2.56 15.2 706.8
    3.06 14.19 720.6
    3.61 13.17 733.4
    ", sep=" ")

options(scipen=999) # suppressing scientific notation

hydrocarbon<- read.table(my.datafile, header=T)

attach(hydrocarbon)
names(hydrocarbon)

# fitting the regression model:

reg.fit <- lm(Viscosity ~Pressure+ Temp)

# getting the summary regression output:

summary(reg.fit)

# plotting the residuals against the predictor "Pressure":

plot(Pressure, resid(reg.fit),pch=19,cex=1.5)

# plotting the residuals against the predictor "Temp":

plot(Temp, resid(reg.fit),pch=19,cex=1.5)

# Checking VIF values
library(car)
vif(reg.fit)

#  Added-variable plots (partial regression plots)
#  for each of those two predictors:

# For "Pressure":

plot(resid(lm(Pressure ~ Temp)), resid(lm(Viscosity~ Temp)), main="Partial regression plot for Pressure",pch=19,cex=1.5)

# For "Temp":

plot(resid(lm(Temp ~ Pressure)), resid(lm(Viscosity ~ Pressure)), main="Partial regression plot for Temp",pch=19,cex=1.5)

cPressure<-Pressure-mean(Pressure)
cTemp<-Temp-mean(Temp)
cPressure2<-cPressure^2
cTemp2<-cTemp^2

reg.fit2<- lm(Viscosity ~cPressure+ cTemp+cPressure2+ cTemp2)
summary(reg.fit2)

vif(reg.fit2)

reg.fit3<- lm(Viscosity ~ cTemp+ cTemp2)

summary(reg.fit3)

vif(reg.fit3)
##################################################
# 
#  OUTLIER AND INFLUENCE DIAGNOSTICS
#
##################################################

# rstandard gives the INTERNALLY studentized residuals 
# (what SAS calls "Student Residual")

rstandard(reg.fit3)

cbind(rstandard(reg.fit3),abs(rstandard(reg.fit3))>2.5)

# rstudent gives the EXTERNALLY studentized residuals 
# (what SAS calls "RStudent")

ti<-rstudent(reg.fit3)
count<-1:length(ti)

n<-15
p<-3
g<-15
alpha<-0.05

qt(1-alpha/2,n-p-1)
qt(1-alpha/(2*g),n-p-1) # Using Bonferroni's adjustment

pvalue<-rep(0,n)

for(i in 1:n){
  pvalue[i]<-2*pt(-abs(ti[i]),n-p-1)
  
}

x<-data.frame(ti,pvalue)

ord<-order(pvalue)

x<-x[ord,]


adj.pvalue<-p.adjust(x[,2],method="BH")

x<-data.frame(x,adj.pvalue,adj.pvalue<alpha)
x

plot(fitted(reg.fit3), y = ti, xlab = "Estimated mean response", ylab = "Studentized 
     Deleted Residuals", main = expression(paste(t[i], " vs. estimated mean response")), pch=19, panel.first = grid(col = "gray", lty = "dotted"), ylim = c(min(qt(p = 0.05/(2*g), n-p-1, min(ti))), max(qt(p = 1-0.05/(2*g), n-p-1, max(ti)))))

abline(h = c(qt(p = 0.05/(2*g), n-p-1),qt(p = 1-0.05/(2*g), n-p-1)), col = "deeppink", lwd = 2)

# getting the measures of influence:
# Gives DFFITS, Cook's Distance, Hat diagonal elements, and some others.

influence.measures(reg.fit3)

hii.rule<-(2*p)/n
hii.rule

hii<-hatvalues(model = reg.fit3)
hii

plot(x = 1:n, y = hii, pch=19, ylab = expression(h[ii]), xlab = "Observation Number", main = expression(paste("Index plot of ", h[ii])))
abline(h = 2*p/n, col = "orchid", lty = 2,lwd=2)
abline(h = c(0.2,0.5), col = "deeppink", lty = 3,lwd=2)


cbind(hii,hii.rule,hii>hii.rule)

######################################################################################

dffits.rule<-2*sqrt(p/n)

dffits.rule

dffits.i<-dffits(model = reg.fit3)


plot(x = 1:n, y = dffits.i, xlab = "Observation 
     Number", ylab = "DFFITS", main = "DFFITS vs. 
     Observation Number", panel.first = grid(col = 
                                               "grey", lty = "dotted"), ylim = c(min(-1, -
                                                                                       2*sqrt(p/n), min(dffits.i)), max(1, 2*sqrt(p/n), 
                                                                                                                        max(dffits.i))),pch=19)
abline(h = 0, col = "aquamarine")
abline(h = c(-2*sqrt(p/n), 2*sqrt(p/n)), col = "deeppink", lwd = 2)
abline(h = c(-1,1), col = "orchid", lwd = 3)

cbind(dffits.i,dffits.rule,abs(dffits.i)>dffits.rule)

######################################################################################

cook.i<-cooks.distance(model = reg.fit3)

# CooksD.rule<-qf(.3,p,n-p)

CooksD.rule<-qf(.5,p,n-p)
CooksD.rule

plot(x = 1:n, y = cook.i, xlab = "Observation Number", ylab = "Cook's D", main = "Cook's vs. Observation Number", panel.first = grid(col = "gray", lty = "dotted"), ylim = c(0, max(cook.i)),pch=19)
abline(h = 0, col = "aquamarine")
abline(h = qf(.5,p,n-p), col = "deeppink", lwd = 2)

cbind(cook.i,CooksD.rule,cook.i>CooksD.rule)