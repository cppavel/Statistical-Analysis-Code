OneSampTest <-function(type=NULL, tails=NULL, alpha, mu, n, x_bar, sd)
{
  #calculate the test statistic
  se = (sd/sqrt(n))
  test_stat <- (x_bar-mu)/(sd/sqrt(n))
  
  if (type=="z") {  
    #get the p-value for this test statistic
    if (tails =="two") {  p_val <-  2*pnorm(abs(test_stat), lower.tail=FALSE) 
    } else if (tails=="left") {p_val <-  pnorm(test_stat , lower.tail=TRUE)
    } else if (tails=="right") {p_val <-  pnorm(test_stat , lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else if (type =="t") {
    #define df
    df <- n-1  
    #get the p-value for this test statistic
    if (tails =="two") { p_val <- pt(abs(test_stat), df, lower.tail=FALSE) 
    } else if (tails=="left") {p_val <- pt(test_stat , df,lower.tail=TRUE)
    } else if (tails=="right") {p_val <- pt(test_stat , df, lower.tail=FALSE)
    } else {stop("please choose tails as two, left, or right")}
  }
  else {stop("please choose z or t")}
  
  #check if significant
  if (p_val <alpha) {sig <-"significant"
  }  else {sig <-"not significant"}
  
  
  ret <- list(type=paste("One Sample", type, "test.", tails, "tailed"),test_statistics =test_stat, mu=mu, n=n, x_bar=x_bar, se=se, p = p_val, alpha = alpha, significance = sig )
  #return the list
  return( ret )
}
library('Rlab')
library('stats')
#drawing a sample of size 100 from a normal(4,5) distribution
vector <- rnorm(100,4,5)

#performing z test for that sample(type is "z", test is two tailed since the sign in H0 is equals,
#0.05 is an alpha parameter, 0 is a hypothehes (H0) mean, we use sample mean and the true population sd
#because this is the "z" test)
#The test is significant, which means that we reject null hypothesis and therefore accept the alternative
#hypothesis that the mean is not equal to 0

print(OneSampTest(type = "z",tails = "two", 0.05,0,100,mean(vector),5))

#performing t test for that sample(type is "t", test is one tailed, tail is equal to left
#since the sign in H0 is greater, and therefore the sigh in HA is less than.
#0.025 is an alpha parameter, 4.2 is a hypothehes (H0) mean, we use sample mean and sample sd
#because this is the "t" test)
#We fail to reject the null hypothesis, which means that value 4.2 lies in the 95 confidence interval

print(OneSampTest(type = "t",tails = "left", 0.025,4.2, 100,mean(vector),sd(vector)))

#here we use a function from package stats to perform the same test as the above one
#we need to pass a vector as a parameter, we do not need optional vector y, therefore we set it to NULL
#we then specify that the sign in alternative hypothesis is less and the hypotheses mean (H0) is 4.2
#we also need to say that a test is not paired and that alpha is equal to 0.025
#Again value lies in the confidence interval, which means that we fail to reject the null hypothesis

t.test(vector,y = NULL, alternative = "less",4.2,paired = FALSE,alpha = 0.025)

#we set the true proportion value to 0.3
prop<-0.3
#draw 100 values from bernoulli distribution with the chances of success equal to 0.3
bern <-rbern(100,prop)
#getting the frequencies of 1 and 0 in a sample
tab <- table(bern)
#getting the frequencies of 1 only
sample_prop <-tab[names(tab)==1]
#converting to proportion
sample_prop <-sample_prop/100

#here we do a two tailed z test since H0 sign is equals, alpha = 0.05
#proportion hypotheses value is 0.28, number of samples is 100, sample_prop is
#the mean of the sample and the last parameter is sd for proportions
#The resulting test statistic for this case is 0.2203798 p_value = 0.8255754, which
#means that it is not significant. Therefore, we fail to reject the H0, which means that we cant say that
#population mean is not equal to 0.28

print(OneSampTest(type = "z",tails = "two", 0.05,0.28,100,sample_prop,sqrt((sample_prop)*(1-sample_prop))))

#here we do a one tailed z (right) since H0 sign is less, which means that HA sign
#is greater, we use alpha = 0.05, the mean under H0 is 0.35, number of samples - 100
#and mean and proprtions are the same as in the previous test
#The resulting test statistic for this case is -1.322279  p_value = 0.9069624 , which
#means that it is not significant. Therefore, we fail to reject the H0, which means that we cant say that
#population mean is greater than 0.35
print(OneSampTest(type = "z",tails = "right", 0.05,0.35,100,sample_prop,sqrt((sample_prop)*(1-sample_prop))))
      
      