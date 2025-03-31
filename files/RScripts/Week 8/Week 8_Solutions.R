###########################################
###########################################
# WEEK 8 - EXERCISE SOLUTIONS
###########################################
###########################################



################
# CORE EXERCISES
################


rm(list = ls())
setwd('')


library(stringr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lmtest)


london <- read.csv('london_exercises.csv', header = T, stringsAsFactors = T)
attach(london)

#Exercise 1. Examine the London data. 
#What hypothesis could be tested using the variables contained in the dataset?
str(london)
names(london)
head(london)
summary(london)

#Possible answers to the question include:
#the relationship between crime and house prices
#the relationship between age and unemployment
#the relationship between population density and crime
#the relationship between unemployment and crime
#etc.





#Exercise 2. Crime rate is defined as the number of crimes committed per 1,000 people in a given area.


#a.  Evaluate the variable's distribution using a histogram and summary statistics.
ggplot(london, aes(x = crime)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), bins = 100) +
  ylab('Proportion') + xlab('Crime Rate') + theme_classic()

summary(london$crime)

#b. Which wards had the most crimes committed in them? 
#Find the names of wards in which the crime rate was higher than 
#the 99th percentile of the variable's distribution.

#Dplyr solution:
london %>% filter(crime > quantile(crime, probs = 0.99)) %>% 
  select(ward, crime) %>% arrange(-crime)
#Base solution:
outliers <- london[which(with(london, crime > quantile(crime, probs = 0.99))),
                   colnames(london) %in% c('ward','crime')]
outliers[order(-outliers$crime),]

#Exercise 3. London is divided into 32 Boroughs and the City of London. 
#Examine the variation in crime on Borough level.

#a. Calculate crime rate for each of the boroughs

london$population <- children + adults + elderly
london$crime_absolute <- london$population/1000*london$crime

crime.borough <- london %>% 
  group_by(borough) %>%
  summarise(crime = sum(crime_absolute),population = sum(population)) %>% 
  mutate(crime.rate = crime/(population/1000))


#b. Present the Borough-level crime rates using a table and a bar chart,
#with Borough order descending by crime rate.
#table
crime.borough %>% select(borough, crime.rate) %>% 
  arrange(-crime.rate) %>% print(n = 33)


ggplot(crime.borough) + 
  geom_histogram(aes(x = reorder(borough, -crime.rate), y = crime.rate), stat = 'identity') +
  ylab('Crime rate') + 
  xlab('Borough') + 
  coord_flip()+
  theme_classic()


#Exercise 4. Unemployment rate, defined as the ratio of people in full time employment to population of working age, is often said to be related to crime. 

#a. Generate an unemployment rate variable for each of the wards.

#Dplyr solution:
london <- london %>% mutate(unemp_rate = 
                              (adults - employed)/adults)
#Base solution:
london$unemp_rate = with(london, (adults - employed)/adults)

#b.Examine the distribution of the unemployment rate:
ggplot(london, aes(x = unemp_rate)) + 
  geom_histogram(aes(y = ..count../sum(..count..)), bins = 60) +
  ylab('Proportion') + 
  xlab('Unemployment rate')+
  theme_classic()

#c. Create a scatter plot of the relationship between unemployment and crime. 
#Make sure to choose the right axis for each of the variables and to label the axes correctly. 
ggplot(london, aes(x = unemp_rate, y = crime)) + 
  geom_point() + 
  xlab('Unemployment Rate') + 
  ylab('Crime Rate') +
  theme_classic()

#Interpret the results 
#The crime rate appears to increase with unemployment rate. There are several 
#outliers in the data which might exert significant influence on the OLS regression estimates

#d. Calculate the correlation coefficient between these two variables and interpret it
cor(london$unemp_rate,london$crime)
#OR
with(london, cor(unemp_rate, crime))

#There seems to be a relatively weak linear relationship between crime and unemployment in London.



#Exercise 5. Estimate a regression model for the relationship between unemployment rate and crime rate. Interpret the results.
model_crime_unemp <- lm(crime ~ unemp_rate, data = london)
summary(model_crime_unemp)

#a.Interpretation:
  #i. The intercept is significantly different from 0 at 95% significance level
  #Therefore we can assume, that
  #hypotheticallyy a borough with 0 unemployment would have crime rate of 24.63.
  
  #ii. The slope is significantly higher than 0.
  #On average, an increase of 1 in unemployment rate corresponds with increase in 
  #crime rate of 176.09. 
  
  #iii. The distributions of residuals
  ggplot() + geom_histogram(aes(x = model_crime_unemp$residuals), bins = 100)
  #There are some outlying values, especially those higher than the mean
  #which could have been expected based on the scatter plot from previous exercise
  
  #v. The R^2 - The unemployment rate explains 3.768% of variation in crime rate,
  # which is a poor R^2.

#b. Add the regression line to the scatter plot
ggplot(london, aes(x = unemp_rate, y = crime)) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  xlab('Unemployment Rate') + 
  ylab('Crime Rate')+
  theme_classic()


#c. Estimate the 95% CI for the model
confint(model_crime_unemp, level = 0.95)

#d. Using the model, predict the value of crime rate 
#for an hypothetical ward with unemployment rate of 4.
predict(model_crime_unemp, newdata = data.frame(unemp_rate = 0.4))



#Exercise 6. It can be hypothesised that a ward with higher median household income will have a lower rate of crime compared to a ward with lower median household income.

#a. Run a regression model testing this hypothesis.
model_crime_inc <- lm(crime ~ income, data = london)
summary(model_crime_inc)

#b. Interpret the coefficients, their significance levels, 
# and the $R^2$ of the model.
# Both the intercept and slope coefficients are statistically significant,
# meaning that, starting at a crime rate of 39.39 for a ward with 0 median
# household income, for every additional unit of household income in a ward
# the crime rate will increase by 0.001113. However, the R^2 for this model
# is poor, only able to explain around 1.2% of the variation in crime rate.

#c. Produce a scatter plot and add the regression line.
ggplot(london, aes(income, crime)) +
  geom_point() +
  geom_smooth(method = lm)+
  theme_classic()

#d. What conclusions can you draw from the model and plot with respect
# to the hypothesis?
# The hypothesis can be rejected in this case, since whilst there is a
# statistically significant relationship, it is in the opposite direction
# to that predicted by the hypothesis. However, visually it can be seen
# there there are some significant outliers in the data that could be
# affecting the model. Furthermore, the low R^2 suggests that the independent
# variable is not a good predictor.



# Exercise 7 The mean age of a ward can be said to influence the rate of crime
# in that ward.

#a. Come up with a hypothesis for the relationship between the mean age 
# of a ward and the rate of crime in that ward. 
# Briefly justify why you chose this hypothesis.
# For example, it can be hypothesised that as the mean age of a ward increase,
# the rate of crime will decrease. This is because wards with a higher proportion
# of older people will likely not see as much crime compared to a ward with
# a higher proportion of younger people, since younger people can be said
# to be more likely to commit crimes.
#This is just one example, however, and it can be argued the other way. 
# Alternatively, it could also be argued that age would have no affect on the
# crime rate.

#b. Run a regression model testing this hypothesis and interpret
# the results of the model.
model_crime_age <- lm(crime ~ age, data = london)
summary(model_crime_age)
# Since both the intercept and slope coefficients are statistically significant,
# it can be interpreted that starting at a crime rate of 201 for a ward with
# a mean age of 0, for every year increase in age the crime rate decreases
# by 3.287. This supports the hypothesis stated above, that an older ward
# will have a lower crime rate (although whether the hypothesis is rejected
# or not depends upon the hypothesis you formulated). However, the R^2
# is very poor, only able to explain 1.8% of the crime rate variation.







# Exercise 8 It can be hypothesised that a ward with a 
# larger number of benefit recipients will have a higher rate of crime
# compared to a ward with fewer benefit recipients.

#a. Explain some reasons why this might be the case.
# Wards with larger numbers of people receiving benefits will likely be much
# poorer wards in general, with a lower average income, which has been
# suggested to result in a higher crime rate seen by the previous model,
# leading to a higher crime rate. Furthermore, more benefit recipients likely
# means a higher unemployment rate in the ward, which is also likely to mean
# there is a higher crime rate in the ward. Furthermore, benefit recipients
# are more likely to live in council housing compared to non-benefit recipients,
# with council housing often consisting of flats in high-density areas, with
# this also being more likely to lead to higher crime rates. Etc.

#b. Run a regression model testing this hypothesis and 
# interpret the results of the model. What does this interpretation 
# suggest about your hypothesis?
model_crime_benefits <- lm(crime ~ benefits, data = london)
summary(model_crime_benefits)
# Both the intercept and slope coefficients are significant, meaning we can
# interpret that starting at a crime rate of 68.67 for a ward with 0% of
# the population receiving work-related benefits, every additional percentage
# increase in benefits recipients results in a 1.169 increase in the crime
# rate. This provides evidence that supports our hypothesis. However, the R^2
# is terrible, being able to explain less than 1% of the variation in the
# crime rate.

#c. Produce a scatter plot and add the regression line.
ggplot(london, aes(benefits, crime)) +
  geom_point() +
  geom_smooth(method = lm)+
  theme_classic()

#d. Compare the regression model with the models from Exercises 7 and 8.
# Which is better able to explain the crime rate of a ward and why?
(summary(model_crime_inc))$r.squared
(summary(model_crime_age))$r.squared
(summary(model_crime_benefits))$r.squared
# The age model has the highest R^2 (0.018) compared to the income (0.012)
# and benefits (0.006) models, meaning it is better able to explain the
# crime rate of London wards, although it is still incredibly low.


################
# GOING FURTHER
################


# Exercise 1 
# London Boroughs are classified as either 'Inner' or 'Outer' boroughs.

# a
ggplot(subset(london, crime < 500),
       aes(x = unemp_rate, y = crime, color = inner)) + 
  geom_point() + 
  xlab('Unemployment Rate') + 
  ylab('Crime Rate')+
  theme_classic()

# b.
london_safe <- filter(london, crime<=500)

ggplot(subset(london_safe),
       aes(x = unemp_rate, y = crime)) + 
  geom_point(data=london_safe[inner == "TRUE",],color="red",size=2)+
  geom_point(data=london_safe[inner == "FALSE",],color="blue",size=2)+
  xlab('Unemployment Rate') + 
  ylab('Crime Rate')+
  theme_classic()

# c.
ggplot(subset(london_safe),
       aes(x = unemp_rate, y = crime)) + 
  geom_point(data=london_safe[inner == "TRUE",],color="red",size=2)+
  geom_point(data=london_safe[inner == "FALSE",],color="blue",size=2)+
  geom_smooth(data=london_safe[inner == "TRUE",],color="red",linewidth=1, method = 'lm', se = FALSE)+
  geom_smooth(data=london_safe[inner == "FALSE",],color="blue",linewidth=1, method = 'lm', se= FALSE)+
  xlab('Unemployment Rate') + 
  ylab('Crime Rate')+
  theme_classic()


# with legend
# this is involved, because we need to feed the location of a borough into both the line and the point geom. 

#create subsets according to location
london_inner <- filter(london_safe, inner=="TRUE")
london_outer <- filter(london_safe, inner=="FALSE")

# create a new variable in each subset to specify location
london_inner$location <- "Inner"
london_outer$location <- "Outer"

# bind these together to have them in one data set again
plotdata <- rbind(london_inner,london_outer)
plotdata$location <- as.factor(plotdata$location)

# specify the colours we want for each borough location
group.colours <- c("Inner" = "red", "Outer" = "blue1")

# finally, the plot
ggplot(plotdata, aes(x = unemp_rate, y = crime, color=location)) +
  geom_smooth(method = lm, se=FALSE) +
  geom_point() +
  theme_classic() +
  xlab('Unemployment Rate') + 
  ylab('Crime Rate')+
  scale_colour_manual(values=group.colours, 
                      name="") +
  labs(colour = "") 


# d.
model_crime_unemp_inner <- lm(crime ~ unemp_rate, 
                              data = subset(london, inner == TRUE & 
                                              crime < 500))

model_crime_unemp_outer <- lm(crime ~ unemp_rate, 
                              data = subset(london, inner == FALSE &
                                              crime < 500))

summary(model_crime_unemp_inner)
summary(model_crime_unemp_outer)
# i.
# The coefficient is larger in the model of the outer boroughs, this implies that a rise in the unemployment rate is associated with a greater rise in crime rates in outer rather than inner boroughs. 

# ii. 
# The higher R-squared statistic in the outer boroughs implies that unemployment has a greater link to crime rate in the outer boroughs than the inner boroughs.

# iii.
predict(model_crime_unemp_inner, newdata = data.frame(unemp_rate = 0.45))

predict(model_crime_unemp_outer, newdata = data.frame(unemp_rate = 0.45))
# You would expect crime to be lower in an outer borough with an unemployment rate of 0.45
