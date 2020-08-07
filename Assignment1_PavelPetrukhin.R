#loading data from file
peopleData<-read.csv(file="Lab1.csv", header=TRUE)
#summary statistics for variable earn
summary(peopleData$EARN) 
#frequencies of the variable Job.class
table(peopleData$Job.class) 
#three-way cross-tabulation of the proportions of variables 
#Educational Level, Gender and Job.class
ftable(peopleData$EDUC, peopleData$Gender, peopleData$Job.class)
#basic histogram of variable EARN
hist(peopleData$EARN, xlab = "EARN", main = "Earn Histogram ") 
#basic boxplot of the variable EARN by Job Class
boxplot(peopleData$EARN~peopleData$Job.class, xlab = "JOB CLASS", ylab = "EARN")
#creating a new variable EARNx10000 which is equal to EARN/10000
peopleData$EARNx10000 = peopleData$EARN/10000
#scatterplot with EARNx10000 on the x axis and AGE on the Y axis
plot(peopleData$EARNx10000, peopleData$AGE, xlab = "EARN (in tens of thousands)", ylab = "AGE") 
