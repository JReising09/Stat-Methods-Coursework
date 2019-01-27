# R code to analyze the body fat data
# using ridge regression

my.datafile <- tempfile()
cat(file=my.datafile, "
    19.5 43.1 29.1 11.9
    24.7 49.8 28.2 22.8
    30.7 51.9 37.0 18.7
    29.8 54.3 31.1 20.1
    19.1 42.2 30.9 12.9
    25.6 53.9 23.7 21.7
    31.4 58.5 27.6 27.1
    27.9 52.1 30.6 25.4
    22.1 49.9 23.2 21.3
    25.5 53.5 24.8 19.3
    31.1 56.6 30.0 25.4
    30.4 56.7 28.3 27.2
    18.7 46.5 23.0 11.7
    19.7 44.2 28.6 17.8
    14.6 42.7 21.3 12.8
    29.5 54.4 30.1 23.9
    27.7 55.3 25.7 22.6
    30.2 58.6 24.6 25.4
    22.7 48.2 27.1 14.8
    25.2 51.0 27.5 21.1
    ", sep=" ")
options(scipen=999) # suppressing scientific notation


bodyfat.data <- read.table(file = my.datafile, 
                           header=FALSE, col.names = c('triceps', 'thigh', 'midarm', 'bodyfat'))

# attaching the data frame:

attach(bodyfat.data)

# The original least-squares fit:
bodyfat.reg <- lm(bodyfat ~ triceps + thigh + midarm)

# Obtaining the VIF values for the OLS

library(car)
vif(bodyfat.reg)

# must load the MASS package first:

install.packages("MASS",repos="http://ftp.ussg.iu.edu/CRAN/") # Only need to do once

library(MASS)

# Using R's automatic selection methods to select the biasing constant:
# R calls this constant "lambda"

select(lm.ridge(bodyfat ~ triceps + thigh + midarm, lambda = seq(0,1,0.001)))

# The generalized cross-validation (GCV) criterion says
# the optimal biasing constant is .019 (choice of c)

# must load the MASS package first:

install.packages("lmridge",repos="http://ftp.ussg.iu.edu/CRAN/") # Only need to do once

library(lmridge)

bodyfat.ridge<-lmridge(bodyfat~.,bodyfat.data,K=0.019)

summary(bodyfat.ridge)

#Estimate column is our biased beta estimates

summary(bodyfat.reg)
anova(bodyfat.reg)

# Obtaining the VIF values for the Ridge Regression model

vif.lmridge(bodyfat.ridge)

# Comparing SSEs

# Transforming the Variables Using Correlation Transformation

bodyfat2<-(1/(sqrt(20-1)))*((bodyfat-mean(bodyfat))/sd(bodyfat))
triceps2<-(1/(sqrt(20-1)))*((triceps-mean(triceps))/sd(triceps))
thigh2<-(1/(sqrt(20-1)))*((thigh-mean(thigh))/sd(thigh))
midarm2<-(1/(sqrt(20-1)))*((midarm-mean(midarm))/sd(midarm))

# The original least-squares fit using transformed variables:

bodyfat.reg.trans<- lm(bodyfat2 ~ triceps2 + thigh2 + midarm2)

summary(bodyfat.reg.trans)
anova(bodyfat.reg.trans)

sse.ols<-0.19864

sse.ols

# Computing SSE for Ridge Regression
# s_y and s_x for each variable
sd.org.y<-sd(bodyfat)
sd.org.triceps<-sd(triceps)
sd.org.thigh<-sd(thigh)
sd.org.midarm<-sd(midarm)

#b_i = (s_x_i/s_y)*b_i* (b_i* is the est biased estimator)
b1.trans.triceps<-0.5625*(sd.org.triceps/sd(bodyfat))
b2.trans.thigh<-0.3625*(sd.org.thigh/sd(bodyfat))
b3.trans.midarm<--0.1956*(sd.org.midarm/sd(bodyfat))

#Fitted values with transformed variables back to original units to compare SSE's
yhat.trans<-b1.trans.triceps*triceps2+b2.trans.thigh*thigh2+ b3.trans.midarm*midarm2

sse.ridge<-sum((bodyfat2-yhat.trans)^2)

sse.ridge
sse.ols

#Close so we haven't lost alot of variance and we also did not lose a lot in R^{2}
# So Ridge regression reduced VIF considerably and retained information( so it worked)


#Prediction

# Estimated mean body fat when X1 = 29.8, X2 = 54.3, X3 = 31.1

xh.values <- data.frame(cbind(triceps = 29.8, thigh=54.3, midarm=31.1))

#If we transform these ^ first then we can use the transformed reg equation.

predict(bodyfat.reg, xh.values)

predict(bodyfat.ridge, xh.values)

