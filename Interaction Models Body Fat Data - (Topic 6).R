# R code to analyze the body fat data
# using multiple linear regression

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

# fitting the regression model without interaction terms:

bodyfat.reg <- lm(bodyfat ~ triceps + thigh + midarm)

# getting the summary regression output:

summary(bodyfat.reg)

# getting the ANOVA table:

anova(bodyfat.reg)

# To test whether beta_2=beta_3=0, given that X1 is in the model
# must use SS's for thigh and midarm 
# (note these must have been listed LAST in the lm statement)

# According to the formula:

F.stat <- ((33.17 + 11.55)/2)/6.15
F.stat

# Getting the P-value (with the appropriate d.f. = (2,16))
pf(F.stat, 2, 16, lower=F)


################## Investigating multicollinearity #########

# Noting correlation among predictors:

cor(cbind(triceps, thigh, midarm))

# To get VIF's, first copy this function (by Bill Venables) into R:

####################################
##
#
vif <- function(object, ...)
  UseMethod("vif")

vif.default <- function(object, ...)
  stop("No default method for vif. Sorry.")

vif.lm <- function(object, ...) {
  V <- summary(object)$cov.unscaled
  Vi <- crossprod(model.matrix(object))
  nam <- names(coef(object))
  if(k <- match("(Intercept)", nam, nomatch = F)) {
    v1 <- diag(V)[-k]
    v2 <- (diag(Vi)[-k] - Vi[k, -k]^2/Vi[k,k])
    nam <- nam[-k]
  } else {
    v1 <- diag(V)
    v2 <- diag(Vi)
    warning("No intercept term detected. Results may surprise.")
  }
  structure(v1*v2, names = nam)
} 
#
##
#####################################

# Then use it as follows:

vif(bodyfat.reg)

############## Interaction model? #####################

# Centering the predictor variables:

ctriceps = triceps - mean(triceps)
cthigh = thigh - mean(thigh)
cmidarm = midarm - mean(midarm)

# Now we fit the model with all pairwise interactions #
  
bodyfat.reg.int <- lm(bodyfat ~ ctriceps + cthigh + cmidarm + ctriceps:cthigh
                      + ctriceps:cmidarm + cthigh:cmidarm 
                      + cthigh:cmidarm:ctriceps)

# getting the summary regression output:

summary(bodyfat.reg.int)

# getting the ANOVA tables:

anova(bodyfat.reg.int)
anova(bodyfat.reg)

# To test whether beta_4=beta_5=beta_6=0, given that X1,X2,X3 are in the model
# must use SS's for the interaction terms 
# (note these interactions must have been listed LAST in the lm statement)

# According to the formula:

F.stat <- ((98.4-85.57)/4)/7.13
F.stat

# Getting the P-value (with the appropriate d.f. = (3,13))
pf(F.stat, 2, 12, lower=F)



## Plot against residuals
x1x2<-triceps*thigh
plot(x1x2, resid(bodyfat.reg))
# Random scatter implies interaction term is not needed
