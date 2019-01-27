# Data:  Eight daily temperatures (degrees C) at a South Florida weather station
# and at an experimental pasture site.  Of interest is whether the median difference is zero.


weather <- c(34.5,34,35.9,33.4,36.9,35.8,36.0,35.6,33,37.5)
pasture <- c(33.2,32.9,33.4,32.3,34.1,33.0,33.3,34.6,32,36.2)


# Let differences = weather - pasture:

diffs <- weather - pasture

boxplot(diffs)


qqnorm(diffs,pch=19,cex=1.5)
qqline(diffs)

shapiro.test(diffs)

shapiro.test(log(diffs))
shapiro.test(sqrt(diffs))
#Transforms did not help normality

#Note that the data is pretty symmetric
stem(diffs)

wilcox.test(diffs, alternative="two.sided", mu=0)

# wilcox.test(weather,pasture,paired=T)


######################################################################

# What if we had been testing whether the true median difference 
# was GREATER than 0 (i.e., weather station temperatures higher)?

wilcox.test(diffs, alternative="greater", mu=0)


######################################################################

diffs <- c(weather - pasture) #calculating the vector containing the differences

diffs <- diffs[ diffs!=0 ] #delete all differences equal to zero

n<-length(diffs)

diffs.rank <- rank(abs(diffs)) #check the ranks of the differences, taken in absolute

W<-sum(sign(diffs)*diffs.rank)

s<-sqrt(((n*(n+1)*(2*n+1))/6))

z<-(W-.5)/s

2*pnorm(-abs(z)) #two-sided test

pnorm(z) #less than

1-pnorm(z) #greater than
