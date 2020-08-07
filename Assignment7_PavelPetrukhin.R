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

