
###########################################################
################## PO91Q - Solutions week 7 ###############
###########################################################


########################
#### CORE EXERCISES ####
########################


# 1. Open the European Social Survey data. Name it conveniently and attach it

library(foreign)
setwd("~/Library/CloudStorage/OneDrive-UniversityofWarwick/Warwick/Modules/PO91Q/Seminars/Week 7/Worksheet Solutions")
ESS9<-read.spss("ESS9e03.sav",to.data.frame=TRUE,max.value.labels=10)
attach(ESS9)


# 2. Univariate Analysis (revising weeks 1-2)


#     a. Identify and explore briefly variables about respondents'
#        happiness, subjective health, age and household's income
#        (we are going to look at their relationships later)

str(happy)
str(health)
str(agea)
str(hinctnta)
# ~ Refer to ESS documentation for precise meaning of these variables

table(happy)
table(health)
table(agea)
table(hinctnta)



# b. What do the values of the income variable mean?
# ~ Quantiles, that is, compartments with same number of people each
# ~ In this case, belonging to first decile means belonging to
# ~ 10% poorest of the population, 2nd decile means
# ~ the second 10% poorest, etc.
# ~ Refer to Questionnaire/codebook for details
# ~ about survey question



# c. What is the mode of 'age'?
plot(agea)
# ~ Not relevant: this gives individual values
hist(agea)
# ~ This one works. Mode seems to be somewhere around 55-60 y.o.
# ~ Check function 'hist' to modify parameters (using arguments), such as increment for bars
# ~ Can try with breaks=100, for example...
hist(agea, breaks=100) 

# ~ Same with Ggplot package ######
library(ggplot2)

ggplot(ESS, aes(x=agea)) + 
  geom_histogram(color="black", fill="purple")
# ~ With an improved scale for x and y axes...
ggplot(ESS, aes(x=agea)) + 
  geom_histogram(color="black", fill="purple")+
  scale_x_continuous(breaks = seq(0, 100, by = 5))+
  scale_y_continuous(breaks = seq(0,3000, by=500))
# ~ Exact mode is 63, with very close second mode at 54. You can also check with
# ~ the function table, although it is not always easy to detect.





# d. Try to write a function which automatically determines the mode of a variable (more advanced, don't worry if this beats you. We will discuss this in the seminar.)

get_mode <- function(x) {
  # get unique values of the input vector
  uniqv <- unique(x)
  # select the values with the highest number of occurrences
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

get_mode(agea)

###############################################################################


# e. Check the purpose of function round and, if relevant,
# apply it appropriately to your results from now onward
?round



# f. Calculate means, variances and standard deviations when relevant
# ~ In terms of variables' level of measurement,
# ~ only interval (numeric) variables are eligible,
# ~ and other variables return error messages:

mean(agea,na.rm=T)
round(mean(agea,na.rm=T),1)

sd(agea,na.rm=T)
round(sd(agea,na.rm=T),1)

# ~ However we could apply these functions to the factor variables,
# ~ as they are ordinal, and actually quasi-scales.

# ~ For this we convert them into numeric format
# ~ and exclude the invalid answers:

class(happy)
table(happy)
round(mean(happy,na.rm=T),2)
round(sd(happy,na.rm=T),2)

class(health)
table(health)

health.num<-as.numeric(health)
table(health.num)
round(mean(health.num,na.rm=T),2)
round(sd(health.num,na.rm=T),2)

hinctnta.num<-as.numeric(hinctnta)
table(hinctnta.num)
round(mean(hinctnta.num,na.rm=T),2)
round(sd(hinctnta.num,na.rm=T),2)

# ~ Mean and sd make less sense with deciles:
# ~ by construction, mean will be around 5.5.






# g. Find a suitable plotting function for all four variables

hist(happy, breaks=100)

# Mmmmm...not very good, isn't it?

hist(happy, breaks=10)
# ~ Ok, now it is better

plot(health)
plot(hinctnta)
# ~ Improved version
plot(hinctnta,horiz=T,las=1)

###################### We can also use ggplot package #####################

ggplot(ESS, aes(x=happy)) + 
  geom_histogram(color="black", fill="red", binwidth=1)

# ~ Let's add labels and title with ggplot ############

ggplot(ESS, aes(x=happy)) + 
  geom_histogram(color="black", fill="red", binwidth=1)+
  ggtitle("Happiness of the Population") +
  xlab("Happiness")+ 
  ylab("Number of people")



# 3. Crosstabulations
# a. We suspect there is a relationship between health and happiness. State the null and alternative hypothesis. Take care to correctly specify the dependent and independent variable. 
# ~ Null: There is no relationship between health and happiness
# ~ HA: The higher the level of happiness, the higher the level of health
# ~ HA is also possible the other way around, depending on your argument. Could happiness even be an attribute of health?

# b. Cross-tabulate health and happiness, using the table function (search with '?' if needed)
?table
table(health,happy)


# c. Apply the 'addmargins' function to the previous formula
addmargins(table(health,happy))


# d. Can you read the results? Can you interpret them?
# ~ We can distinguish a trend that high health goes with high happiness,
# ~ but this remains blurry. It is a big table, with big numbers inside.
# ~ We need some simplification.

# e. Display a bivariate plot with the same (search with '?' if needed). What kind of plot do you get? Does this help reading and interpreting?

plot(health,happy)
# ~ Yes multiple boxplots are very relevant here
# ~ (although unfortunately key values are rounded)
# ~ However, it seems that the worse the health, the lower the happiness

# ~ In case you do not have the right output (multiple boxplots),
# ~ you can do: 

plot(as.factor(health),as.numeric(happy))

# f. Convert the table into percentages.
#    You may function 'cprop' or 'lprop' from package 'questionr'
#    (you can install the new package through the menu "Tools" in RStudio).
#    Try and interpret the result.
#    Improve the table as much as you can.

library(questionr)

# ~ Make sure you choose the best order for X and Y and the right function
# ~ This means identifying the independent and the dependent variables

lprop(table(happy,health))
round(lprop(table(happy,health)),0)
round(lprop(table(happy,health,useNA="ifany")),0)

# g. Conduct a Chi2 test of significance and interpret the result.

table <- round(lprop(table(happy,health,useNA="ifany")),0)

chisq.test(table , correct=FALSE) 
# ~ there is a relationship, due to the small p-value


# h. Reorder the levels of Health (from 'Very bad' to 'Very good') 
#    using the 'factor' function with levels argument
#    Then display the cross-tabulation again

health1<-factor(health,levels=c("Very bad","Bad","Fair","Good","Very good"))
round(lprop(table(happy,health1,useNA="ifany")),0)

# i. Is the relationship still statistically significant?

table2 <- round(lprop(table(happy,health1,useNA="ifany")),0)
chisq.test(table2 , correct=FALSE) 
# ~the result is still statistically significant


# 4. Correlation
# a. Calculate the Pearson correlation between health and happiness
#    (you need to work only with the cases that have positive values
#    for both variables, search the help file for this)

?cor
# ~ Help file indicates 'use' argument
# ~ to select only cases with both X and Y documented

cor(happy,health.num,use="pairwise.complete.obs")
round(cor(happy,health.num,use="pairwise.complete.obs"),2)

# b. Does the result confirm your previous conclusion?
# ~ Negative result fits with contrary orientation of values
# ~ between the two variables: Happiness 'goes up', Health 'goes down'.

# c. Is the correlation coefficient statistically significant? What does this mean?
cor.test(happy,health.num,use="pairwise.complete.obs")

# ~ strongly statistically significant, which means that statistically the correlation is confirmed.

# 5. Further application
# a. Form the null und alternative hypotheses for the relationship between 
#   -  happiness and household income
#   -  happiness and age

# ~ null: there is no relationship between happiness and household income.
# - HA: the higher household income, the higher the level of happiness (possibly the other way artound, again?)
# ~ null: there is no relationship between happiness and age.
# ~ HA: the higher age, the lower the level of happiness (perhaps higher?)


# b. Repeat the crosstabulation and correlation exercises for these two pairs of variables to test your hypotheses. 

table3 <- round(lprop(table(happy,hinctnta.num,useNA="ifany")),0)
chisq.test(table3 , correct=FALSE) 

# ~ This is significant, but it is a huge table. Let us see what the expected cell counts are:

Xsq <- chisq.test(table3 , correct=FALSE) 
Xsq$expected

# ~ The expected cell count should exceed 5 which is not always met. The test might therefore be unreliable. 

# ~ Age is a continuous variable, so we cannot do a crosstabulation. You could recode it into a categorical variable, however. Excellent if you have done so!

cor.test(hinctnta.num,happy,use="pairwise.complete.obs")
cor.test(agea,happy,use="pairwise.complete.obs")

# ~ Again, both are strongly statistically significant. 
# ~ We observe that health seems to be more strongly linked with happiness,
# ~ just before income, and finally age.
# ~ Age correlation is weak,
# ~ so the relationship might not be non-linear.


########################
#### GOING FURTHER ####
########################


# SECTION 1

# 1. Use the 'prop.table' function to look at happiness
#    against health in a comparison between genders.
#    Try and compare genders as precisely as possible.

# ~ First solution involves a tri-dimensional table:
round(prop.table(table(health,happy,gndr),margin=1),2)*100
# ~ Problem is that totals are not separated between genders

# ~ Second solution (a bit more complex):
round(prop.table(table(health[which(gndr=="Male")],
			happy[which(gndr=="Male")]),margin=1),2)*100
round(prop.table(table(health[which(gndr=="Female")],
			happy[which(gndr=="Female")]),margin=1),2)*100

# ~ Or by splitting the dataset by gender:

# ~ Long version:

ESS.F<-ESS[gndr=="Male",]
ESS.M<-ESS[gndr=="Female",]

round(prop.table(table(ESS.M$health,ESS.M$happy),margin=1),2)*100
round(prop.table(table(ESS.F$health,ESS.F$happy),margin=1),2)*100

# ~ Shorter version using with():

with(ESS[gndr=="Male",],round(prop.table(table(health,happy),margin=1),2)*100)
with(ESS[gndr=="Female",],round(prop.table(table(health,happy),margin=1),2)*100)

# ~ To go further, one may calculate the difference between genders.
# ~ This enables visualising quickly where the gaps lay:

Men<-with(ESS[gndr=="Male",],round(prop.table(table(health,happy),margin=1),2)*100)
Women<-with(ESS[gndr=="Female",],round(prop.table(table(health,happy),margin=1),2)*100)
# ~ Raw differences
Men-Women

# ~ Percent difference of men, compared to women as baseline
round(100*(Men-Women)/Women,0)





# 2. Does health drive happiness more for men or women?
# ~ Previous outputs do not enable to conclude clearly.

cor(health.num[gndr=="Male"],
    happy[gndr=="Male"],use="pairwise.complete.obs")

# ~ As usual, let's get rid of some decimals

round(cor(health.num[gndr=="Male"],
   happy[gndr=="Male"],use="pairwise.complete.obs"),5)


round(cor(health.num[gndr=="Female"],
   happy[gndr=="Female"],use="pairwise.complete.obs"),5)

# ~ Women's happiness seems to depend on health more:



# 3. Identify the two countries with lowest and highest happiness
table(cntry,happy)
# ~ Approximate answer:
boxplot(happy ~ cntry)

# ~ Let's scale down the X axis to make it clearer
boxplot(happy ~ cntry,cex.axis=.6)


# ~ Precise answer (you may as well consider median instead of mean):

round(mean(happy[cntry=="AT"],na.rm=T),2)
round(mean(happy[cntry=="BE"],na.rm=T),2)
# ~ etc.

# ~ Using a new function, 'tapply', which applies a given function
# ~ across all categories of a covariable:
happy.cntry<-tapply(happy, cntry, mean, na.rm=TRUE)
happy.cntry

# ~ To visualise the result in one go, just wrap everything in brackets

(happy.cntry<-tapply(happy, cntry, mean, na.rm=TRUE))


(happy.cntry1<-round(happy.cntry,1))
(happy.cntry2<-happy.cntry1[order(happy.cntry)])
barplot(happy.cntry2,horiz=TRUE,las=1)
grid(nx=NULL,ny=NA,col="grey40")
# ~ Lowest: Bulgaria
# ~ Highest: Danemark





# 4. Compare visually happiness distributions between these two countries
# ~ First, not the best way (with hist function):
layout(1:2)

hist(happy[cntry=="BG"], col = "red")

hist(happy[cntry=="DK"],col="blue")

# ~ A better way:

layout(1:2)

plot(happy[cntry=="BG"][order(happy[cntry=="BG"])])
plot(happy[cntry=="DK"][order(happy[cntry=="DK"])])
# ~ Using par(new=T) to superimpose the lines
# ~ ylim to specify that we need the same range of Y values,
# ~ and ylab to erase the superimposed Y axis label:

layout(1)
plot(happy[cntry=="BG"][order(happy[cntry=="BG"])],col="red",ylim=c(0,10),ylab=NA)
par(new=T)
plot(happy[cntry=="DK"][order(happy[cntry=="DK"])],col="blue",ylim=c(0,10),ylab=NA)




# 5. What benefit do distributions have compared to just looking at means?
# ~ A  better understanding of where the big values are,
# ~ and of the shape of the distribution, how skewed it is.







# SECTION 2

# 1. For this section use Wave 7 (2014). Name it conveniently and attach it.
setwd("~/Library/CloudStorage/OneDrive-UniversityofWarwick/Warwick/Modules/PO91Q/Seminars/Week 4/Worksheet Solutions")
ESS7<-read.spss("ESS7e02_1.sav",to.data.frame=TRUE,max.value.labels=10)
attach(ESS7)


# 2. Calculate the Body Mass Index variable with BMI=weight/height.
BMI<-100*weight/(height*height)

# 3. Compare mean and standard deviation of BMI between the UK and Spain
round(mean(BMI[cntry=="GB"],na.rm=TRUE),3)
round(mean(BMI[cntry=="ES"],na.rm=TRUE),3)
round(sd(BMI[cntry=="GB"],na.rm=TRUE),3)
round(sd(BMI[cntry=="ES"],na.rm=TRUE),3)

# 4. Apply 't.test' function to test the difference between the two means
#    (note this is now a **two-sample** t-test)
t.test(BMI[cntry=="GB"],BMI[cntry=="ES"])

# 5. Have the inhabitants of one country a significantly better BMI?
#    Do you know why?
# ~ Yes, the difference between the two means is significant,
# ~ (thanks to high sample size) although very small.
# ~ UK inhabitants have higher BMI than Spain inhabitants.
# ~ We may hypothesise differences in diet, lunching habits, etc.



# SECTION 3

# 1. For this section use Wave 7 (2014). Name it conveniently and attach it.
setwd("~/Library/CloudStorage/OneDrive-UniversityofWarwick/Warwick/Modules/PO91Q/Seminars/Week 4/Worksheet Solutions")
ESS7<-read.spss("ESS7e02_1.sav",to.data.frame=TRUE,max.value.labels=10)
attach(ESS7)



# 2. Plot the age at which respondents completed full-time education
#    against the same for their fathers (note that these variables are only valid for the UK sample).

layout(1)
plot(edagegb,edagefgb)

# 3. There might be outlying values--can you see them?
# ~ Yes, some isolated values at the right and top of the graph
# ~ reduce visibility.

# 4. Plot the same, excluding outlying values
# ~ I fix the threshold  at 40 years old (to be discussed of course,
# ~ you may try another threshold)
edagegb1<-edagegb
edagegb1[edagegb>40]<-NA
edagefgb1<-edagefgb
edagefgb1[edagefgb>40]<-NA

plot(edagegb1,edagefgb1)
# ~ The distribution of "useful" values appears now more clearly



# 5. Is there a significant link between fathers and children regarding this variable?
cor.test(edagegb1,edagefgb1)
# ~ Yes, there is. Actually a very similar level of association
# ~ as between height and weight!



# 6. Is there a significant link between mothers and children?

# ~ Similar calculation as before, using 'edagemgb'
# ~ keeping respondents who left school over 40 y.o. out of the sample
edagemgb1<-edagemgb
edagemgb1[edagemgb1>40]<-NA
cor.test(edagegb1,edagemgb1)

# ~ Yes, there is a link. The link is stronger for fathers than mothers.
# ~ Fathers seem to make a bigger difference than mothers
# ~ in the propensity of children to stay shorter or longer in school.

# ~ However confidence intervals slightly overlap:
# ~ There is a chance (even if small) that
# ~ fathers-children correlation is lower than 0.486
# ~ (upper limit of mothers' CI)
# ~ and mothers-children correlation is higher than 0.474
# ~ (lower limit of fathers' CI).
# ~ Therefore we cannot conclude with certainty.

# 7. Are these links, if any, stronger for sons or for daughters?
#    Can you interpret?
cor.test(edagegb1[gndr=="Male"],edagefgb1[gndr=="Male"])
cor.test(edagegb1[gndr=="Male"],edagemgb1[gndr=="Male"])
cor.test(edagegb1[gndr=="Female"],edagefgb1[gndr=="Female"])
cor.test(edagegb1[gndr=="Female"],edagemgb1[gndr=="Female"])



# ~ 1. Based on p-values:
# ~ All four gender combinations give very significant results.

# ~ 2. Based on correlation coefficients
# ~ All four correlation coefficients are around 0.51,
# ~ except the one between sons and mothers, which is much lower (0.38).

# ~ 3. Based on confidence intervals (CIs):
# ~ Only sons-mothers CIs do not overlap with the other 3 CIs.
# ~ Therefore we can only conclude that mothers have a lower impact
# ~ on sons than mothers on daughters,
# ~ as well as than fathers on both sons and daughters.

