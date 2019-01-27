# R code to analyze the math proficiency data
# using robust regression

# The data set consists of the response "mathprof"
# for 40 states/provinces, along with several
# possible predictor variables

my.datafile <- tempfile()
cat(file=my.datafile, "
    Alabama 252 75 78 34 18 18
    Arizona 259 75 73 41 12 26
    Arkansas 256 77 77 28 20 23
    California 256 78 68 42 11 28
    Colorado 267 78 85 38 9 25
    Connecticut 270 79 86 43 12 22
    Delaware 261 75 83 32 18 28
    D.C. 231 47 76 24 33 37
    Florida 255 75 73 31 19 27
    Georgia 258 73 80 36 17 22
    Guam 231 81 64 32 20 28
    Hawaii 251 78 69 36 23 26
    Idaho 272 84 84 48 7 21
    Illinois 260 78 82 43 14 21
    Indiana 267 81 84 37 11 23
    Iowa 278 83 88 43 8 20
    Kentucky 256 79 78 36 14 23
    Louisiana 246 73 76 36 19 27
    Maryland 260 75 83 34 19 27
    Michigan 264 77 84 31 14 25
    Minnesota 276 83 88 36 7 20
    Montana 280 83 88 44 6 21
    Nebraska 276 85 88 42 9 19
    New_Hampshire 273 83 88 40 7 22
    New_Jersey 269 79 84 41 13 23
    New_Mexico 256 77 72 40 11 27
    New_York 261 76 79 35 17 29
    North_Carolina 250 74 78 37 21 25
    North_Dakota 281 85 90 41 6 14
    Ohio 264 79 84 36 11 22
    Oklahoma 263 78 78 37 14 22
    Oregon 271 81 82 41 9 31
    Pennsylvania 266 80 86 34 10 24
    Rhode_Island 260 78 80 38 12 28
    Texas 258 77 70 34 15 18
    Virgin_Islands 218 63 76 23 27 22
    Virginia 264 78 82 33 16 24
    West_Virginia 256 82 80 36 16 25
    Wisconsin 274 81 86 38 8 21
    Wyoming 272 85 86 43 7 23
    ", sep=" ")
options(scipen=999) # suppressing scientific notation

mathprof.data <- 
  read.table(my.datafile, header=FALSE, col.names = c('state', 'mathprof', 'parents', 'homelib', 'reading', 'tvwatch', 'absences'))


# attaching the data frame:

attach(mathprof.data)

# The ordinary least-squares fit:
mathprof.reg <- lm(mathprof ~ parents + homelib + reading + tvwatch + absences)

summary(mathprof.reg)

# Influence measures for this regression:

influence.measures(mathprof.reg)

# Normal Q-Q plot of residuals shows a couple of possible outliers:
qqnorm(resid(mathprof.reg),pch=19)
qqline(resid(mathprof.reg))

### Robust regression alternatives:

# must install and load the MASS and quantreg packages first:

install.packages("MASS")

library(MASS)

install.packages("quantreg")

library(quantreg)


# Least Absolute Residuals (LAR) regression:

mathprof.lar <- rq(mathprof ~ parents + homelib + reading + tvwatch + absences)

summary(mathprof.lar)

#Note parents, reading, absences CI's contain 0, may not be needed.

# Huber's method:

mathprof.huber <- rlm(mathprof ~ parents + homelib + reading + tvwatch + absences)

summary(mathprof.huber)
anova(mathprof.huber)

# Bisqare method:

mathprof.bisquare <- rlm(mathprof ~ parents + homelib + reading + tvwatch + absences,c = 4.685, psi=psi.bisquare)

summary(mathprof.bisquare)
anova(mathprof.bisquare)