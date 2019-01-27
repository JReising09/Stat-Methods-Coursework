# Data from Rao (p. 4.10).  
# Eight pairs of mice (biologically similar within each pair)
# were injected with cancer.  For each pair, one mouse was given
# a treatment and the other mouse was not.

# Entering the data:
control.data <- c(1.321,1.423,2.682,0.934,1.230,1.670,3.201)
treatment.data <- c(0.841,0.932,2.011,0.762,0.991,1.120,2.312)

# Calculating the differences:
diffs <- control.data - treatment.data

# Testing whether the true mean difference in tumor weight is more than zero:
t.test(diffs, alternative = "greater", mu = 0)


# An equivalent way, in which R will do the paired t-test automatically:

t.test(control.data, treatment.data, alternative = "greater", mu = 0, paired = TRUE)

####################################################################################
# Try a transformation to normalize

treat.dat<- treatment.data + sqrt(sd(treatment.data))
diffs_2<- control.data - treat.dat
diffs_2
t.test(diffs_2, alternative = "greater", mu = 0)
t.test(control.data, treat.dat, alternative = "greater", mu = 0, paired = TRUE)

#####################################################################################

# 95% CI for the true mean difference:
t.test(diffs, conf.level=0.95)$conf.int