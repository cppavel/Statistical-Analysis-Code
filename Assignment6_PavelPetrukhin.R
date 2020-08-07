#loading data from file
survey<-read.csv(file="survey.csv", header=TRUE)
#creating a table for chi-square test
tbl <- table(survey$Smoke, survey$Exer)
#printing table of frequencies
tbl
#performing chi-square test
chisq.test(tbl)
#the results of chi-square test:
#Pearson's Chi-squared test
# data:  tbl
# X-squared = 5.4885, df = 6, p-value = 0.4828
# 1. For chi-square test degrees of freedom are calculate using the following formula: (number_of_rows-1)*(number_of_columns-1), where
# number_of_rows and number_of_columns are the respective values for the table, we are running the chi-square test on. 
# 2. The assumptions for chi-square test are:
#       1.  All observations must contribute to only one cell, which means that there is no observation which might be counted two or more times
#           in different places
#       2.  In a 2x2 table, all Expected values must be >5
#       3.  In a larger table, n must be >20, all Expected values must be >1 and no more than 20% of the expected values can be <5. 
# 3. As sample size increases, absolute differences become a smaller and smaller proportion of the expected value. A reasonably strong association may not come up as 
#significant if the sample size is small. In large samples, we may find statistical significance when the findings are small and uninteresting. This means that sometimes
#chi-square test might give us incorrect results, even though we did not make any mistake.
# 4. For chi-square test null hypothesis is always that there is no association between two parameters. It is chosen to be such, so that if it is rejected
#it means that there is an association between the two parameters (alternative hypothesis). (If we reject the null hypothesis, we accept the alternative one)
# 5. For a significance level of 0.05 we conclude the test is not significant, because 0.05<0.4828(p_value). Therefore, we fail to reject the null hypothesis.
# Finally, we cannot be certain that there is no association between how frequently people exercise and how much they smoke. 