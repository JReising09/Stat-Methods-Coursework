##### Polynomial regression on the Rabbit Jawbone Data (Table 8.8)


##########
##
# Reading the data into R:

my.datafile <- tempfile()
cat(file=my.datafile, "
    0.01 15.5
    0.2 26.1
    0.2 26.3
    0.21 26.7
    0.23 27.5
    0.24 27
    0.24 27
    0.25 26
    0.26 28.6
    0.34 29.8
    0.41 29.7
    0.83 37.7
    1.09 41.5
    1.17 41.9
    1.39 48.9
    1.53 45.4
    1.74 48.3
    2.01 50.7
    2.12 50.6
    2.29 49.2
    2.52 49
    2.61 45.9
    2.64 49.8
    2.87 49.4
    3.39 51.4
    3.41 49.7
    3.52 49.8
    3.65 49.9
    ", sep=" ")

options(scipen=999) # suppressing scientific notation

rabbit <- read.table(my.datafile, header=FALSE, col.names=c("age","length")) 

attach(rabbit)

#########

## Initial scatterplot of data:

plot(age, length)

# Not a linear trend?


# Note the correlation 
cor(rabbit)

# From correlation, we see we may have a strong linear relationship.
# However the plot shows a nonlinear trend.

# Defining transformed variables for the polynomial terms:
# center the age data
age<-age - mean(age)

age2 <- age^2 #quadratic term
age3 <- age^3 #cubic term
age4 <- age^4 #quartic term

quartic.reg <- lm(length ~ age + age2 + age3 + age4)

summary(quartic.reg)

## Looks like the fourth-degree term is not needed.

cubic.reg <- lm(length ~ age + age2 + age3)
summary(cubic.reg)

# Plotting the fitted curve (this plotting approach only works well with a moderate to large number of observations)

plot(age, length)
lines(age, fitted(cubic.reg),col = "red")  

#Check the quad model
quad.reg <- lm(length ~ age + age2)
summary(quad.reg)

plot(age, length)
lines(age, fitted(quad.reg),col = "red")  