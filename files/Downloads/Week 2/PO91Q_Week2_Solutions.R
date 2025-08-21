
###########################################################
################## PO91Q - Solutions week 2 ###############
###########################################################

# Topic: DESCRIPTIVE STATISTICS AND GRAPHS


# Exercises are cumulative week after week.
# Hence it is important that you have worked on Week 1 Core exercises
# before moving on to Week 2.

# Note that there are sometimes alternative solutions
# to the one(s) I offer below. If you are unsure if your solution
# is valid, feel free to share it in the Forum.


########################
#### CORE EXERCISES ####
########################

rm(list=ls())

# 1. STARTING

# Open the ESS9 dataset in R.
# Attach it

library(foreign)
setwd("~/Library/CloudStorage/OneDrive-UniversityofWarwick/Warwick/Modules/PO91Q/Seminars/Week 2")
ESS<-read.spss("ESS9e03.sav",to.data.frame=TRUE,max.value.labels=10)
attach(ESS)

ESS$lrscale
# 2.INSPECTING
# Identify the variable about "Left-right placement"
# Check its formatting in the questionnaire: what do 77, 88 and 99 mean?
# What is the level of this variable?
# Summarise it with functions 'class', 'str' and 'head'
# What do these functions return?

class(lrscale)
str(lrscale)
head(lrscale)

# ~ Respectively class, structure and first values of the variable.




# 3. Same with the variable about the "European unification:
# should go further or already gone too far"

class(euftf)
str(euftf)
head(euftf)

# Display the first 20 values of the two variables above
# Check the structure of the 'table' function and tabulate the two variables
# Can you see a major difference between them
# regarding non positive answers (ie outside proposed scale)?

lrscale[1:20]
euftf[1:20]
table(lrscale,useNA="ifany")
table(euftf,useNA="ifany")
# ~ Yes there is a difference, probably with respect to Refusals (more with LR scale)
# ~ and
# ~ which could be interpreted in terms of how sensitive the two questions are,
# ~ or as how much sense they make to respondents...
# ~ Hard to make sure what interpretation is right,
# ~ unless we perform more treatments! (not now)
# ~ Also there are important country contrasts to expect,
# ~ so elaborating a cross-country interpretation is risky.

# 4. Consider the function rm(list=ls())
# What does function 'rm' stand for?
# How useful can this function be for future exercises?

# ~ rm is for 'remove'
# ~ Can be used to clean the working environment before restarting from scratch
# ~ For example when you are lost in your calculations.



# 5. UNIVARIATE STATISTICS AND RECODING


# 5.1 Calculate the two means, using only valid values
# (check the 'mean' function beforehand)

# ~ Calculating means based only on positive values
# ~ using an argument that excludes na's:
# ~ (you can check how this argument works with ?mean)
mean(lrscale)
mean(lrscale,na.rm=T)
mean(euftf,na.rm=T)


# 5.2 In Left-right, regroup values:
# 0-1 into "far left", 2-3 into "left", 4-6 into "centre",
# 7-8 into "right" and 9-10 into "far right"

# Method:
# a. recode
# b. convert to factor format using 'as.factor'
# c. assign value labels using 'levels'

# ~ a. Recoding in numeric format using R base
lrscale1<-lrscale # Keeping the original data intact for backup and reference
lrscale1[lrscale<=1]<-1
lrscale1[lrscale>=2 & lrscale<=3]<-2
lrscale1[lrscale>=4 & lrscale<=6]<-3
lrscale1[lrscale>=7 & lrscale<=8]<-4
lrscale1[lrscale>=9]<-5

# ~ Another version using 'car' package
library(car)
lrscale2<-car::recode(lrscale,
                      "0:1='far left';
  2:3='left';
  4:6='centre';
  7:8='right';
  9:10='far right'")

# ~ Checking outcome against original variable
table(lrscale)
table(lrscale1)
table(lrscale,lrscale1)

# ~ b. Converting to factor format
lrscale3<-factor(lrscale2,levels=c("far left","left","centre","right","far right"))

# ~ Checking outcome
table(lrscale3,useNA="ifany")


# ~ Then do the same for euftf



########################
#### GOING FURTHER ####
########################


# 1. Using the new, transformed variables:

# Calculate means in the UK only
# You may use the [variable == "value"] subscript

# ~ Creating new variable and linking it with working dataset
attach(ESS$lrscale3)
attach(ESS$lrscale1)
attach(ESS$euftf3)

# ~ Checking country names
table(cntry)
# ~ Using the numeric version of the variable
mean(lrscale[cntry=="GB"],na.rm=TRUE)

# How does this mean compare with the average in Europe? and with France?
(LR.EU.mean<-mean(lrscale,na.rm=TRUE))
(LR.FR.mean<-mean(lrscale[cntry=="FR"],na.rm=TRUE))
(LR.UK.mean<-mean(lrscale[cntry=="GB"],na.rm=TRUE))

# Present the frequencies and means of these three samples in one unique table
(freqs<-
    rbind(
      (LR.FR.freq<-table(lrscale[cntry=="FR"])),
      (LR.UK.freq<-table(lrscale[cntry=="GB"])),
      (LR.EU.freq<-table(lrscale))))
(means<-
    c(LR.FR.mean,LR.UK.mean,LR.EU.mean))
(FreqsMeans<-cbind(freqs,means))

# Install and load the "questionr" package
install.packages("questionr")
library(questionr)

# Use its function 'freq' to calculate the same as above, but with percentages
(LR.FR.freq1<-freq(lrscale[cntry=="FR"]))
(LR.UK.freq1<-freq(lrscale[cntry=="GB"]))
(LR.EU.freq1<-freq(lrscale))
# ~ Selecting the second column only with [,2]:
(rbind(c(LR.UK.freq1[,2],LR.UK.mean),
       c(LR.FR.freq1[,2],LR.FR.mean),
       c(LR.EU.freq1[,2],LR.EU.mean)))

# Compare deviations from the mean in the UK and Germany
# Using function 'abs' (absolute value, which means any value
# with all negative signs deleted)
mean(abs(lrscale[cntry=="GB"]
         -mean(lrscale[cntry=="GB"],na.rm=TRUE)),na.rm=TRUE)
mean(abs(lrscale[cntry=="DE"]
         -mean(lrscale[cntry=="DE"],na.rm=TRUE)),na.rm=TRUE)

# Compare mean, median, variance and standard deviation in the UK and Germany
# Same for the mode

cbind(
  rbind(
    mean(lrscale[cntry=="GB"],na.rm=TRUE),
    median(lrscale[cntry=="GB"],na.rm=TRUE),
    var(lrscale[cntry=="GB"],na.rm=TRUE),
    sd(lrscale[cntry=="GB"],na.rm=TRUE)),
  rbind(
    mean(lrscale[cntry=="DE"],na.rm=TRUE),
    median(lrscale[cntry=="DE"],na.rm=TRUE),
    var(lrscale[cntry=="DE"],na.rm=TRUE),
    sd(lrscale[cntry=="DE"],na.rm=TRUE)))

# ~ Mode requires another approach, so better do it separately,
# ~ by spotting it in the frequency. Both modes are in 'Centre' here:
table(lrscale[cntry=="GB"])
table(lrscale[cntry=="DE"])

# ~ Duplicate section 4 for EU integration


# 2. Try 3 or more kinds or graphs with these two variables separately,
# using the most appropriate of original or transformed values
# Assess and compare the relevance of each graph

plot(lrscale3)
# ~ Nice for categorical
boxplot(lrscale)
# ~ Nice for numeric
pie(table(lrscale3))
# ~ Not often useful, although often used...


# 3. Tabulate the two variables in original format against each other using table(X,Y)
# Interpret the output.

# ~ We are moving towards a real research question:
# ~ the link between ideology and views about the EU
table(lrscale,euftf)
# ~ Cannot read much, too stuffed
# ~ But some trends appear somehow...

# Same with transformed format: Interpret the output.

table(lrscale3,euftf)
# ~ Still too stuffed, although clearer with less values

# 5. Try the same using questionr::cprop
lprop(table(lrscale3,euftf))
# ~ Much better!


# 6. Graph the two variables against each other using 'plot'
# (make sure you choose the right versions of the variables)
# What can you conclude from this graph?

plot(lrscale3,euftf)
# ~ More readable this time. We can see that the further left,
# ~ the more respondents are in favour of integration

# ~ Other graph formats should bear better results
# ~ (to be explored on your own)