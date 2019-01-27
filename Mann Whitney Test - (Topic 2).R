# Data:  Dental measurements (in mm) for two groups:  
# a group of 16-year-old females and another group of 16-year-old males
# Of interest is whether the median of the male population is greater than the 
# median of the female population.

# Source:  Rao (1998), p. 147.

girls <- c(23,25.5,26,26.5,23.5,22.5,25,24,21.5,19.5,28)
boys <- c(31,25.5,27.5,25,26,28.5,26.5,25.5,26,31.5,25,28,29.5,26,30,25)



hist(boys)

hist(girls)
# Histograms

qqnorm(boys)
qqline(boys)

shapiro.test(boys)

qqnorm(girls)
qqline(girls)

shapiro.test(girls)

boxplot(boys, girls)
# Do spreads for the two groups look roughly equal?

library(lawstat)

levene.test(c(boys,girls),c(rep(1,length(boys)),rep(2,length(girls))),location="mean")

levene.test(c(boys,girls),c(rep(1,length(boys)),rep(2,length(girls))),location="median")
######################################################################

# We are testing whether the true median of boys is greater than
# that of girls, so we list boys first, and then the "greater" alternative

wilcox.test(boys, girls, alternative="greater", mu=0)

####################################################################
x<-c(girls,boys)
y<-cbind(rank(x),c(rep(1,11),rep(2,16)))

n1<-11
n2<-16
n<-n1+n2

R1<-sum(y[1:n1,1])
R2<-sum(y[(n1+1):n,1])

u1<-R1-(n1*(n1+1))/2
u2<-R2-(n2*(n2+1))/2

u<-min(u1,u2)

mu<-(n1*n2)/2

s<-sqrt((n1*n2*(n+1))/12) #use if there is no ties

h<-table(y[,1])
h<-as.vector(h)
h<-h[h>1]

tie<-sum((h^3-h)/12)

s.ties<-sqrt((n1*n2/(n*(n-1)))*((n^3-n)/12-tie))

con.cor<-ifelse(u1>u2,-.5,.5)

z<-((u-mu)+con.cor)/s.ties #if there is no ties use s instead of s.ties

pnorm(z) #greater than if u1 < u2 and less than if u1 > u2

1- pnorm(z) #greater than if u1 > u2 and less than if u1 < u2

2*pnorm(-abs(z)) #two-sided test