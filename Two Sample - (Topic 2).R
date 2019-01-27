# Data on pollution around a chemical plant, from Rao (p. 137).
# Two independent samples of river water were taken, 
# one upstream and one downstream.
# Pollution level was measured in ppm.

# Entering in data:

upstream <- c(24.5,29.7,20.4,28.5,25.3,21.8,20.2,21.0,21.9,22.2)
downstream <- c(32.8,30.4,32.3,26.4,27.8,26.9,29.0,31.5,
                31.2,26.7,25.6,25.1,32.8,34.3,35.4)

##### Normality Test #####

qqnorm(upstream)
qqline(upstream)

qqnorm(downstream)
qqline(downstream)

shapiro.test(upstream)
shapiro.test(downstream)


##### Levene Test #####


install.packages("lawstat",repos="http://ftp.ussg.iu.edu/CRAN/") # Only need to do once
library(lawstat) # Must use anytime you want to use the levene.test function


treat<-c(rep(0,10),rep(1,15))
com<-c(upstream,downstream)

# If you get an error when running the levene.test when using the library(lawstat)
# Copy and Paste the Levene Test in R code in R and the run the below code again

levene.test(com, treat,location="mean") #Would attch for data in a dataframe/matrix

levene.test(com, treat,location="median")


boxplot(upstream,downstream,names=c("upstream","downstream"))

var(upstream)
var(downstream)

# Do the spreads seem equal across groups?

# Assuming equal variances:
t.test(upstream, downstream, var.equal=TRUE)

# Not assuming equal variances:
t.test(upstream, downstream)

# A 99% CI for the difference in means:
t.test(upstream, downstream, var.equal=TRUE, conf.level=0.99)$conf.int

# How should we interpret this interval?