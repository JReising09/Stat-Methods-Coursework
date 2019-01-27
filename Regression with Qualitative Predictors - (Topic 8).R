# R example: Regression with qualitative (categorical) predictors 

# type = 0 indicates a mutual company 
# type = 1 indicates a stock company  

my.datafile <- tempfile()
cat(file=my.datafile, "
    17 151 0
    26  92 0
    21 175 0
    30  31 0
    22 104 0
    0 277 0
    12 210 0
    19 120 0
    4 290 0
    16 238 0
    28 164 1
    15 272 1
    11 295 1
    38  68 1
    31  85 1
    21 224 1
    20 166 1
    13 305 1
    30 124 1
    14 246 1
    ", sep=" ")
options(scipen=999) # suppressing scientific notation

insur.inn.data <- read.table(my.datafile, 
                             header=FALSE, col.names = c('time', 'size', 'type'))

# attaching the data frame:

attach(insur.inn.data)

# Fitting the regression model through ordinary least squares: 

insur.inn.reg <- lm(time ~ size + type) 

summary(insur.inn.reg)

anova(insur.inn.reg)

# getting a 95% confidence interval for the true slope beta_1:

confint(insur.inn.reg,"size",level=.95)

# getting a 95% confidence interval for the true slope beta_2:

confint(insur.inn.reg,"type",level=.95)


# getting a 95% confidence interval for the true beta_1 (an indirect way):

alpha <- 0.05
b1 <- summary(insur.inn.reg)$coef[2,1]
s.b1 <- summary(insur.inn.reg)$coef[2,2]
error.df <- summary(insur.inn.reg)$df[2]
lower <- b1 - qt(1-alpha/2, df=error.df)*s.b1
upper <- b1 + qt(1-alpha/2, df=error.df)*s.b1
print(paste(100*(1-alpha), "percent CI for beta_1:", lower, upper))

# getting a 95% confidence interval for the true beta_2 (an indirect way):

alpha <- 0.05
b2 <- summary(insur.inn.reg)$coef[3,1]
s.b2 <- summary(insur.inn.reg)$coef[3,2]
error.df <- summary(insur.inn.reg)$df[2]
lower <- b2 - qt(1-alpha/2, df=error.df)*s.b2
upper <- b2 + qt(1-alpha/2, df=error.df)*s.b2
print(paste(100*(1-alpha), "percent CI for beta_2:", lower, upper))

############################################################# 

# Example of predictor with 4 categories 

# shirt size example 

my.datafile <- tempfile()
cat(file=my.datafile, "
    24.65 small
    16.53 large
    27.43 xlarge
    31.56 small
    16.03 medium
    65.23 small
    24.96 large
    14.78 xlarge
    27.34 large
    29.54 medium
    31.05 medium
    29.45 large
    41.89 small
    16.22 large
    21.87 xlarge
    43.67 small
    34.67 xlarge
    33.77 medium
    ", sep=" ")
options(scipen=999) # suppressing scientific notation

shirt.data <- read.table(my.datafile, 
                         header=FALSE, col.names = c('amount', 'size'))

# attaching the data frame:

attach(shirt.data)

# Creating Indicator Variables
x1 <- ifelse(size=="medium", 1, 0)
x2 <- ifelse(size=="large", 1, 0)
x3 <- ifelse(size=="xlarge", 1, 0)

# Looking at the data set: 

print(cbind(shirt.data, x1,x2,x3))

# Fitting the regression model through ordinary least squares: 

shirt.reg <- lm(amount ~ x1 + x2 + x3)

summary(shirt.reg)

anova(shirt.reg)


# Another way to fit the data

size<-factor(size)

shirt.reg2 <- lm(amount ~ size)

summary(shirt.reg2)

anova(shirt.reg2)

contrasts(size)

# By default, R will put the levels of a qualitative variable in alphabetical order and set the first level to 0 for all indicator variables. 

size<-factor(size,levels=c("small","medium","large","xlarge"))


# Interpreting estimated regression coefficients:            

# We estimate that shoppers with "medium" shirt size spend $13.80 LESS 
# on AVERAGE than shoppers with "small" shirt size.           

# We estimate that shoppers with "large" shirt size spend $18.50 LESS  
# on AVERAGE than shoppers with "small" shirt size.       