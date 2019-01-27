# R code to analyze the data
# using multiple linear regression

my.datafile <- tempfile()
cat(file=my.datafile, "
    Pt BP Age Weight BSA Dur Pulse Stress
    1 105 47 85.4 1.75 5.1 63 33
    2 115 49 94.2 2.10 3.8 70 14
    3 116 49 95.3 1.98 8.2 72 10
    4 117 50 94.7 2.01 5.8 73 99
    5 112 51 89.4 1.89 7.0 72 95
    6 121 48 99.5 2.25 9.3 71 10
    7 121 49 99.8 2.25 2.5 69 42
    8 110 47 90.9 1.90 6.2 66 8
    9 110 49 89.2 1.83 7.1 69 62
    10 114 48 92.7 2.07 5.6 64 35
    11 114 47 94.4 2.07 5.3 74 90
    12 115 49 94.1 1.98 5.6 71 21
    13 114 50 91.6 2.05 10.2 68 47
    14 106 45 87.1 1.92 5.6 67 80
    15 125 52 101.3 2.19 10.0 76 98
    16 114 46 94.5 1.98 7.4 69 95
    17 106 46 87.0 1.87 3.6 62 18
    18 113 46 94.5 1.90 4.3 70 12
    19 110 48 90.5 1.88 9.0 71 99
    20 122 56 95.7 2.09 7.0 75 99
    ", sep=" ")
options(scipen=999) # suppressing scientific notation


bp.data <- read.table(file = my.datafile, header=T)

# attaching the data frame:

attach(bp.data)

# fitting the regression model:

bp.reg <- lm(BP ~ Age + Weight + BSA + Dur + Pulse + Stress)

# getting the summary regression output:

summary(bp.reg)

# getting the ANOVA table:

anova(bp.reg)

# To test whether beta_4=beta_6=0, given that all other predictors
# are in the model
# must use SS's for Duration and Stress 
# (note these must have been listed LAST in the lm statement)

# According to the formula:

SSEf<-15.213
MSEf<-1.17

bp.reg.red <- lm(BP ~ Age + Weight + BSA + Pulse)

anova(bp.reg.red)

SSEr<-15.327

F.stat<-((SSEr-SSEf)/2)/MSEf

F.stat

F.critical<-qf(1-0.05,2,13)

F.critical

# Getting the P-value (with the appropriate d.f. = (2,13))
pf(F.stat, 2, 13, lower=F)

####################################################################################
my.datafile <- tempfile()
cat(file=my.datafile, "
    ACL SDMT Vocab Abstract
    4.5 23 24 24
    5.9 50 18 14
    4.8 27 14 8
    4.5 26 15 10
    5.9 42 30 32
    4.7 35 26 26
    5.6 41 19 16
    4.8 13 14 10
    4.5 46 21 20
    4.8 52 26 28
    5.6 55 26 26
    4.8 36 16 10
    5.8 47 32 36
    4.8 50 26 30
    5 31 30 32
    3.9 61 30 30
    4.3 19 12 6
    3.4 2 13 8
    4.5 36 32 30
    4.8 37 33 36
    4.5 28 13 10
    6.6 63 26 30
    6.6 47 30 36
    ", sep=" ")
options(scipen=999) # suppressing scientific notation

acl.data <- read.table(file = my.datafile, header=T)

# attaching the data frame:

attach(acl.data)

# fitting the regression model:

acl.reg <- lm(ACL ~ SDMT + Vocab + Abstract)


# getting the summary regression output:

summary(acl.reg)

# getting the ANOVA table:

anova(acl.reg)

################## Investigating multicollinearity #########

# Noting correlation among predictors:

cor(cbind(SDMT,Vocab,Abstract))

# To get VIF's, first need to install the R package:

####################################

install.packages("car",repos="http://ftp.ussg.iu.edu/CRAN/") # Only need to do once
library(car) # Must use anytime you want to use the levene.test function

#####################################
# Then use it as follows:

vif(acl.reg)

#####################################
# Removing Abstract from the model:

acl.reg <- lm(ACL ~ SDMT  + Vocab)
summary(acl.reg)

#####################################
# Removing Abstract and Vocab from the model:

acl.reg <- lm(ACL ~ SDMT)
summary(acl.reg)


############## Interaction model? #####################

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

# fitting the regression model:

bodyfat.reg <- lm(bodyfat ~ triceps + thigh + midarm)

# getting the summary regression output:

summary(bodyfat.reg)

# getting the ANOVA table:

anova(bodyfat.reg)

################## Investigating multicollinearity #########

# Noting correlation among predictors:

cor(cbind(triceps, thigh, midarm))

library(car) 

vif(bodyfat.reg)

############## Interaction model? #####################

# Centering the predictor variables:

ctriceps = triceps - mean(triceps)
cthigh = thigh - mean(thigh)
cmidarm = midarm - mean(midarm)

# Now we fit the model with all interactions 

bodyfat.reg.int <- lm(bodyfat ~ ctriceps + cthigh + cmidarm + ctriceps:cthigh + ctriceps:cmidarm + cthigh:cmidarm+ ctriceps:cmidarm:cthigh)

# getting the summary regression output:

summary(bodyfat.reg.int)

# getting the ANOVA table:

anova(bodyfat.reg.int)


# Now we fit the model with no interactions 

bodyfat.reg.reduce <- lm(bodyfat ~ ctriceps + cthigh + cmidarm)

# getting the summary regression output:

summary(bodyfat.reg.reduce)

# getting the ANOVA table:

anova(bodyfat.reg.reduce)


# To test whether beta_4=beta_5=beta_6=beta_7=0, given that X1,X2,X3 are in the model
# must use SS's for the interaction terms 
# (note these interactions must have been listed LAST in the lm statement)

# According to the formula:

F.stat <- ((98.4-85.57)/4)/7.13
F.stat

# Getting the P-value (with the appropriate d.f. = (4,12))
pf(F.stat, 4, 12, lower=F)

# Getting the critical value (with the appropriate d.f. = (4,12))
qf(1-.05, 4, 12)

# Plot of Residuals (from reduced model, i.e. no interactions) versus
# ctriceps:cthigh Interaction term
# Note this plot can be adjusted for the other interaction terms
# Random scatter indicates the term is probably not needed in the model

x1x2<-ctriceps*cthigh

plot(x1x2, resid(bodyfat.reg.reduce), pch=19,cex=1.5)
