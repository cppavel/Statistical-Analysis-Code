#that function creates a Q - Q plot 
normality_1 <- function(x0){
#observed value against a standard normal distribution with 
#the same number of points 
  qqnorm(x0)
#plots the line of best fit for the points on Q - Q plot
  qqline(x0)
}
#that function creates a histogram of the values
normality_2<-function(x0,str){
#plots a probability densities histogram (total area == 1)
  hist(x0, freq = FALSE, main = str, xlab = str)
#generating a sequence of numbers from min to max of length 40
  xfit <- seq(min(x0), max(x0), length = 40) 
#getting heights of probability distribution with given mean and sd
  yfit <- dnorm(xfit, mean = mean(x0), sd = sd(x0))
#plotting the resulting distribution
  lines(xfit, yfit)
}
#create a vector normal_10000 of length 10000,
#drawn from a normal distribution with mean 4, sd 5
normal_10000 <- rnorm(10000,4,5)
#Q-Q plot for normal_10000
normality_1(x0 = normal_10000)
#histogram of normal_10000
normality_2(x0 = normal_10000,str = "Normal 10000")
#creating a vector of 10000 values drawn from the exponential
#distribution with lambda == 1
exp_10000 <- rexp(10000,1)
#Q-Q plot for exp_10000
normality_1(x0 = exp_10000)
#histogram of exp_10000
normality_2(x0 = exp_10000, str = "Exponential 10000")
#setting sample_size to 10
sample_size<-10
#sampling normal_10000 with 10 samples
sample_n <- sample(normal_10000,sample_size)
#Q-Q plot for sample_n
normality_1(x0 = sample_n)
#histogram of sample_n
normality_2(x0 = sample_n, str = paste("Sample Normal ",sample_size))
#sampling exp_10000 with 10 samples
sample_exp <- sample(exp_10000,sample_size)
#Q-Q plot for sample_exp
normality_1(x0 = sample_exp)
#histogram of sample_exp
normality_2(x0 = sample_exp, str = paste("Sample Exp ",sample_size))
#setting sample_size to 50
sample_size<-50
#sampling normal_10000 with 50 samples
sample_n <- sample(normal_10000,sample_size)
#Q-Q plot for sample_n
normality_1(x0 = sample_n)
#histogram of sample_n
normality_2(x0 = sample_n, str = paste("Sample Normal ",sample_size))
#sampling exp_10000 with 50 samples
sample_exp <- sample(exp_10000,sample_size)
#Q-Q plot for sample_exp
normality_1(x0 = sample_exp)
#histogram of sample_exp
normality_2(x0 = sample_exp, str = paste("Sample Exp ",sample_size))
#setting sample_size to 500
sample_size<-500
#sampling normal_10000 with 500 samples
sample_n <- sample(normal_10000,sample_size)
#Q-Q plot for sample_n
normality_1(x0 = sample_n)
#histogram of sample_n
normality_2(x0 = sample_n, str = paste("Sample Normal ",sample_size))
#sampling exp_10000 with 500 samples
sample_exp <- sample(exp_10000,sample_size)
#Q-Q plot for sample_exp
normality_1(x0 = sample_exp)
#histogram of sample_exp
normality_2(x0 = sample_exp, str = paste("Sample Exp ",sample_size))
#setting sample_size to 50
sample_size <- 50
#sampling normal_10000 with 50 samples
sample_n <-sample(normal_10000,sample_size)
#getting the length of sample (in this case 50)
n <- length(sample_n)
#since we want the middle 95% of the distribution, the boundary is
#97.5% (0.975)
#here we get the Z score
Z_score <- qnorm(0.975)
#here we get the t score, n-1 is the degrees of freedom
t_score <- qt(0.975,n-1)
#getting the mean of sample
mean_n <-mean(sample_n)
#finding known population sd
sd_kn <- sd(normal_10000)
#finding the sd of a sample
sd_unkn <- sd(sample_n)
#we use the ordinary formula for standard error since 50<<10000
#finding the known standard error
se_kn <-sd_kn/sqrt(n)
#finding the unknown standard error
se_unkw <-sd_unkn/sqrt(n)
#left boundary of a CI, normal distribution (Z score, known sd)
left_z95_kn_n <- mean_n-Z_score*se_kn
#right boundary of a CI,normal distribution  (Z score, known sd)
right_z95_kn_n <- mean_n+Z_score*se_kn
#printing the results
paste("Z-distribution w/known pop sd:", left_z95_kn_n, right_z95_kn_n)
#left boundary of a CI, normal distribution (t score, sample sd)
left_t95_unkn_n <- mean_n-t_score*se_unkw
#right boundary of a CI, normal distribution  (t score, sample sd)
right_t95_unkn_n <- mean_n+t_score*se_unkw
#printing the results
paste("t-distribution w/unknown pop sd:", left_t95_unkn_n, right_t95_unkn_n)
#sampling exp_10000 with 50 samples
sample_exp <-sample(exp_10000,sample_size)
#getting the length of sample (in this case 50)
n <- length(sample_exp)
#since we want the middle 95% of the distribution, the boundary is
#97.5% (0.975)
#here we get the Z score
Z_score <- qnorm(0.975)
#here we get the t score, n-1 is the degrees of freedom
t_score <- qt(0.975,n-1)
#getting the mean of sample
mean_exp <-mean(sample_exp)
#finding known population sd
sd_kn <- sd(exp_10000)
#finding the sd of a sample
sd_unkn <- sd(sample_exp)
#we use the ordinary formula for standard error since 50<<10000
#finding the known standard error
se_kn <-sd_kn/sqrt(n)
#finding the unknown standard error
se_unkw <-sd_unkn/sqrt(n)
#left boundary of a CI, exp distribution (Z score, known sd)
left_z95_kn_exp <- mean_exp-Z_score*se_kn
#right boundary of a CI,exp distribution  (Z score, known sd)
right_z95_kn_exp <- mean_exp+Z_score*se_kn
#printing the results
paste("Z-distribution w/known pop sd:", left_z95_kn_exp, right_z95_kn_exp)
#left boundary of a CI, exp distribution (t score, sample sd)
left_t95_unkn_exp <- mean_exp-t_score*se_unkw
#right boundary of a CI, exp distribution  (t score, sample sd)
right_t95_unkn_exp <- mean_exp+t_score*se_unkw
#printing the resuts
paste("t-distribution w/unknown pop sd:", left_t95_unkn_exp, right_t95_unkn_exp)






