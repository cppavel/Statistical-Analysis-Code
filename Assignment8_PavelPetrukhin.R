#correlation between the variables:
#it used for calculating regression coefficient (slope)
cor( faithful$waiting,faithful$eruptions) 
#scatter plot of the variables with a smoothed line
scatter.smooth(x= faithful$waiting, y=faithful$eruptions, main="Eruptions ~ Waiting") 
#divide graph area in 2 columns
par(mfrow=c(1, 2))  
#waiting box plot
boxplot(faithful$waiting, main="Waiting", sub=paste("Outlier rows: ", boxplot.stats(faithful$waiting)$out))  
#eruptions box plot
boxplot(faithful$eruptions, main="Eruptions", sub=paste("Outlier rows: ", boxplot.stats(faithful$eruptions)$out)) 
#divide graph area in 2 columns
par(mfrow=c(1, 2))  
#density plot waiting
plot(density(faithful$waiting), main="Density Plot: Waiting", ylab="Frequency")  
#density plot fot eruptions
plot(density(faithful$eruptions), main="Density Plot: Eruptions", ylab="Frequency")
#fitting the linear regression
faithful.lm <- lm(eruptions ~ waiting, data=faithful)  
#setting the plotting area back to normal
par(mfrow=c(1, 1))  
#summary of the linear regression
summary(faithful.lm)
#plotting the linear regression
plot(faithful$waiting, faithful$eruptions)
abline(faithful.lm)
#the equation which describes the linear model I got is: y = -1.874016 + 0.075628*x, where y is the duration of eruption and
#x is the waiting period

#since the p value for the slope is almost equal to zero, we consider the test to be statistically 
#significant for the signigicance level of 0.05, which means we have the enough evidence to reject null hypothesis (slope = 0, there is no relationship betweeen
#the independent and dependent variable). And hence accept the alternative hypothesis: there is a relationship between the independent and dependent variable

#F test is a test of significance of a model overall. Its null hypothesis is " A model with an intercept only fits the data as well as the model considered."
#Basically that means that a model with zero slope fits the data as well as the model considered, which is just the same as saying that there is no relationship
#between the independent and dependent variables. Therefore, the p values for those tests are very close to each other, because they test the same thing (their null hypothesis are same).
#F test is often used when we are considering the multiple regression model with an intercept.And the null hypothesis is b_1 = ... = b_(p-1) = 0

#calculating the residuals
faithful.res <-resid(faithful.lm)

#frequency plot of the residuals
plot(density(faithful.res), main="Density Plot: residuals", ylab="Frequency") 

#residual vs fitted values. standartized residuals vs fitted values. Q-Q plot of the residuals
# Residuals vs Leverage
par(mfrow=c(2,2))
plot(faithful.lm)

#The model itself is properly specified and the Y and X variables have a direct relationship.

#the equation which describes the linear model I got is: y = -1.874016 + 0.075628*x, where y is the duration of eruption and
#x is the waiting period

#since the p value for the slope is almost equal to zero, we consider the test to be statistically 
#significant for the signigicance level of 0.05, which means we have the enough evidence to reject null hypothesis (slope = 0, there is no relationship betweeen
#the independent and dependent variable). And hence accept the alternative hypothesis: there is a relationship between the independent and dependent variable

#It is worth calculating the mean of residuals as well.
mean(faithful.res)
#It is equal to 2.529938e-18, which is basically 0. That is good.

#We should also check that waiting and residuals are uncorrelated. 

cor.test(faithful$waiting, faithful.res) 

#p_value is equal to 1, which is greater than 0.05, so there is no evidence of correlation.

#I know that the residuals must be normally distributed. Here we observe a normal distribution with quite a noticeable skew,
#however after looking at the Q-Q plot I would say that the distribution is ok, as the deviations from normal distribution
#are not as significant as they seem to be at the first glance. 

#It is also to say that residuals pattern resembles the patterns we saw in eruptions and waiting density plots, although
#in the latter ones the pattern is far more extreme. Basically, that pattern means that there is a lack of observations for
# which eruption duration takes values around 3 and waiting takes values around 65. That could be a reason for the lack of
# residuals around value -0.25 in comparison with the amount of residuals around value 0.25 approximately.

#As for the homoscedasticity of the residuals,the red lines on Residuals vs Fitted and Scale-Location graphs do not really seem to be exactly flat.
#However, it is important that there is no trend in them (increasing/decreasing), so we could say that they are approximately flat, which means
#that the graphs are appropriate.

#Now lets consider R-squared. It is generally better to look at the Adjusted R-squared, however for the model where there is only
#one independent variable there won't be much difference between them. So Adjusted R-squared is equal to 0.8108, which is quite close
#to 1. That is good.

#We need to calculate the sd of eruption durations

sd(faithful$eruptions)

#The residual standard error is 0.4965, whereas the sample standard deviation is 1.141371. Since we do not have another model to compare
#the residual standard error with we cannot say anything apart from the fact that residual standard error is less than sample
#standard deviation, which is as it should be. 

#As for the Residuals vs Leverage plot, we see that there are no points which could be considered to be outliers or points with large influence, 
#which is good.

#The model generally makes sense, since the more time passes from the moment of last eruption the more energy is accumulated, so that
#the next eruption duration is going to be larger.

#All in all, I feel that the model is appropriate even though there might be some inaccuracies.

