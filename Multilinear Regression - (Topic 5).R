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

# Getting the estimates using matrices

Y<-matrix(Viscosity, nrow=length(Viscosity),ncol=1)

X<-cbind(1,Pressure,Temp)

b<-solve(t(X)%*%X)%*%t(X)%*%Y

# t() computes the transpose of matrix
# solve() computes the inverse of a square matrix
# %*% does matrix multiplication in R

b

# getting the ANOVA table:

anova(reg.fit)

# Note that to get the "regression df" and SSR to reflect the SAS 
# ANOVA table, we must add the first two lines together.

# getting the fitted values:

fitted(reg.fit)

# getting the fitted values using matrices:

yhat<-X%*%b
yhat

# Correlation between the Y and Y_hat

fit<-fitted(reg.fit)
cor(fit, Viscosity)^2


# getting the residuals:

resid(reg.fit)

# getting the residuals using matrices:

H<-X%*%solve(t(X)%*%X)%*%t(X)
e<-Y-H%*%Y
e

# Bonferroni and Holm adjustments to the t-test P-values if doing simultaneous tests:

raw.ps <- c(0.0000355, 0.0000108)
p.adjust(raw.ps, method="bonferroni")
p.adjust(raw.ps, method="holm")
p.adjust(raw.ps, method="BH")
p.adjust(raw.ps, method="BY")

# getting the 95% confidence interval for the mean at X1=42.49  and X2=369.9:

xh.values <- data.frame(cbind(Pressure = 42.49, Temp = 369.9))

predict(reg.fit, xh.values, interval="confidence", level=0.95)

# getting the 95% prediction interval for a new response at X1=42.49 and X2=369.9: 

predict(reg.fit, xh.values, interval="prediction", level=0.95)

#### Residual plots and other plots ####

# producing the scatter plot matrix

pairs(hydrocarbon)

# residual plot (against fitted values)

plot(fitted(reg.fit), resid(reg.fit), ylab="Residuals", xlab="Fitted Values", pch=19, cex=1.25); abline(h=0)

# Q-Q plot of residuals

qqnorm(resid(reg.fit))

## Formal tests for the error assumptions:

# Breusch-Pagan test for nonconstant error variance

install.packages("lmtest",repos="http://ftp.ussg.iu.edu/CRAN/") # Only need to do once

library(lmtest); 

bptest(Viscosity~Pressure + Temp)

# The Shapiro-Wilk test for normality is produced by:

shapiro.test(resid(reg.fit))
