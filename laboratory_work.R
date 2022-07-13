# installing packages needed for analysis
install.packages("BSDA")
install.packages("EnvStats")
# initialization
T <- data
T2 <- data2
n <- 50  # sample volume 
var_ <- 100  # given variation of X1

########################### Task 1. ###########################
# interval series
table(cut(T$X1, breaks=6)) 
# histogram
hist(T$X1, plot=1, breaks=6) 
# empirical distribution function
plot(ecdf(T$X1))  

# sample characteristics
summary(T$X1) # median, mean, quartiles, minimum, maximum
var(T$X1) # Bessel corrected sample variance
sd(T$X1)  # standard deviation
var(T$X1)/n*(n-1)  # sample variance
mean(T$X1)  # mean
median(T$X1) # median
sqrt(var_/n)  # standard mean error
kurtosis(T$X1)  # эксцесс
skewness(T$X1) # асимметричность

# confidence interval for unknown mean with known variance
z.test(T$X1, sigma.x = 10, conf.level = 0.95, alternative = "two.sided")

# confidence interval for unknown mean with unknown variance
t.test(T$X1, conf.level = 0.95, alternative = "two.sided")

# confidence interval for unknown variance with unknown mean
varTest(T$X1, conf.level = 0.95, alternative = "two.sided")


########################### Task 2 #########################

# equality of means of two samples X1 and Y1 with known variance 
z.test(T$X1,T$Y1,mu=0, sigma.x = 10, sigma.y = 3, conf.level = 0.95, alternative = 't')
# p-value=0.05219 > 0.05 => no reason to reject the equality of means hypothesis

########################### Task 3 #########################

# equality of means of two samples X1 and Y1 with unknown variance 
t.test(T$X2,T$Y2, mu=0, var.equal = TRUE, paired = FALSE, conf.level = 0.95, alternative = 't')
# p-value=0.8098 > 0.05 => no reason to reject the equality of means hypothesis
# equality of variances
var.test(T$X2,T$Y2, ratio=1, conf.level = 0.95, alternative = 't')
# p-value=0.8288 > 0.05 => no reason to reject the equality of variances hypothesis

########################### Task 4 #########################

# equality of means for multiple samples
oneway.test(T2$Z~T2$F, data = T2, var.equal = TRUE)
# p-value = 0.9296 > 0.05 => no reason to reject the equality of means hypothesis


