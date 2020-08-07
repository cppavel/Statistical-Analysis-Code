#function calculating the estimate of the variance under the assumption that variance_1 == variance_2
#s1, s2 - sample sd's, n1, n2 - sample sizes
EqualVarianceSD <- function(s1,s2,n1,n2)
{
  return (sqrt(((n1-1)*s1*s1+(n2-1)*s2*s2)/(n1+n2-2)))
}
#function calculating the standard error under the assumption that variance_1 == variance_2
#s -estimate of variance, n1, n2 - sample sizes
EqualVarianceSE <-function(s,n1,n2)
{
  return (s*sqrt(1/n1+1/n2))
}
#function which calculates the pooled proportion using the sample proportions p1 and p2; n1, n2 - sample sizes
PooledProportion <-function(p1,p2,n1,n2) 
{
  return ((p1*n1+p2*n2)/(n1+n2))
}
#function which calculates a standard error for proportions z-test; p0 - pooled proportion; n1,n2 - sample sizes
ProportionSECombined <-function(p0,n1,n2)
{
  return (sqrt(p0*(1-p0)*(1/n1+1/n2)))
}
#function which calculates a standard error, which is used for CI's if null hypothesis with combined SE was rejected.
ProportionSENotCombined<-function(p1,p2,n1,n2)
{
  return(sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2))
}

#function performing two-sample independent tests
#type - either "z" or "t"
#tails - either "two", "left" or "right"
#alpha - significance level
#h0 - value for null hypothesis
#n1, n2  - sample sizes
#x_bar_1 and x_bar_2 - sample means
#s1, s2 - sample standard deviations
#isProp - boolean variable indicating whether test is conducted for proportions or not
TwoSampTest <-function(type=NULL, tails=NULL, alpha, h0, n1,n2, x_bar_1,x_bar_2, s1, s2, isProp = FALsE)
{
  #calculating difference between two means
  diff <- x_bar_1-x_bar_2 
  #initialising standard error with zero
  se <-0 
  if(!isProp)
  {
    #calculating standard error for the case when we do not test the proportions
    se <-EqualVarianceSE(EqualVarianceSD(s1,s2,n1,n2),n1,n2)
  }
  else
  {
    #calculating standard error for the case when we test the proportions
    se <- ProportionSECombined(PooledProportion(x_bar_1,x_bar_2,n1,n2),n1,n2)
  }
  #calculating test statistic
  test_stat <- (diff-h0)/(se)
  
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n1 + n2 - 2  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- 2*pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("Two sample", type, "test.", tails, "tailed"),n1=n1,n2=n2,Pop_D = h0,diff =diff,se_est =se,test_stat = test_stat,p= p_val,alpha = alpha, significance = sig)
  #return the list
  return( ret )
}


library('Rlab')
library('stats')

#getting a sample of 100 elements from normal distribution with mean = 4 and sd = 5
x0 <- rnorm(100,4,5)
##getting a sample of 100 elements from normal distribution with mean = 3.5 and sd = 2
x1 <- rnorm(80,3.5,2)

#performing a two sample two-tailed z test with true SD
#The test is not significant, which means that we failed to reject the null hypothesis
#Test_stat = -0.4649039, p = 0.6420003
print(TwoSampTest("z","two",0.05,1,100,80,mean(x0),mean(x1),5,2,FALSE))

#performing a two sample two-tailed z test with sample SD
#The test is not significant, which means that we failed to reject the null hypothesis
#Test_stat = 1.212253, p = 0.2254156
print(TwoSampTest("z","two",0.05,0,100,80,mean(x0),mean(x1),sd(x0),sd(x1),FALSE))

#performing a two sample two-tailed t test, with sample SD obviously. 
#The test is not significant, which means that we failed to reject the null hypothesis
#Test_stat = 1.212253, p = 0.2270222
print(TwoSampTest("t","two",0.05,0,100,80,mean(x0),mean(x1),sd(x0),sd(x1),FALSE))

#performing a two sample two-tailed t test with the assumption of equal variance using 'stats' package
#Test_statistic = 1.2123, p = 0.227, which means that we fail to reject the null hypothesis. H0 is included in the confidence interval.
print(t.test(x0,x1, alternative ="two.sided", 0,paired = FALSE,alpha = 0.05,var.equal = TRUE))

#performing a two sample two-tailed t test with the assumption of not equal variance using 'stats' package
#Test_statistic = 1.3119, p = 0.1917, which means that we fail to reject the null hypothesis. H0 is included in the confidence interval.
print(t.test(x0,x1, alternative ="two.sided", 0,paired = FALSE,alpha = 0.05,var.equal = FALSE))

#getting a sample of 100 elements from bernoulli distribution with success rate of 0.3
b0 <-rbern(100,0.3)
#getting a sample of 85 elements from bernoulli distribution with success rate of 0.7
b1 <-rbern(85,0.7)

#getting the result for two-sample two sided z test for sample proportions
prop_test_res <-TwoSampTest("z","two",0.05,0,100,85,mean(b0),mean(b1),sd(b0),sd(b1),TRUE)

#printing those results
#Test_stat = -3.896221, p = 9.770539e-05, which means that the test is significant. Therefore, we reject the null hypothesis and accept the alternative hypothesis that
#the difference between two means is not equal to 0
print(prop_test_res)

#if test was significant we need to recalculate standard error for getting the correct CI
#otherwise we just use the same formula as previously
if(prop_test_res$significance == "significant")
{
  se <-ProportionSENotCombined(mean(b0),mean(b1),100,85)
}else
{
  se <-ProportionSECombined(PooledProportion(mean(b0),mean(b1),100,85),100,85)
}
#left boundary of the CI, we use qnorm to get the critical value for CI and 0.975 is chosen since that is a 95% confidence interval
leftCI <- (mean(b0)-mean(b1)) - qnorm(0.975)*se
#right boundary of the CI, we use qnorm to get the critical value for CI and 0.975 is chosen since that is a 95% confidence interval
rightCI <- (mean(b0)-mean(b1)) + qnorm(0.975)*se
#printing the resulting CI
print(leftCI)
print(rightCI)







