
###########################################################
################## PO91Q - Solutions week 3 ###############
###########################################################


# 1. Open the European Social Survey data, 
# but this time use Wave 7 (2014) which you need to download from the ESS website.
# Name it conveniently and attach it

library(foreign)
setwd("~/Library/CloudStorage/OneDrive-UniversityofWarwick/Warwick/Modules/PO91Q/Seminars/Week 3")
ESS<-read.spss("ESS7e02.sav",to.data.frame=TRUE,max.value.labels=10)
attach(ESS)

# 2. UNIVARIATE
# a. Identify height and weight variables
names(ESS)[order(names(ESS))]
class(height)
class(weight)

# b. Use an appropriate function to summarise them in a few lines
str(height)
str(weight)

# c. Plot the two variables separately as boxplots
layout(1:2)
boxplot(height)
boxplot(weight)
# ~ Not great! Let's try this:

layout(matrix(1:2,1,2))
boxplot(height)
boxplot(weight)


# d. Plot the two variables separately as histograms using function 'hist'
layout(matrix(1:2,2,1))
hist(height)
hist(weight)

# e. Adjust the bars to 5 cm and 5 kg (or as close to these numbers as you can)
hist(height,breaks=24)
hist(weight,breaks=24)

# ~ More complicated, using the seq function to define sequences of x marks:
hist(height,breaks=seq(5,220,by=5))
hist(weight,breaks=seq(5,200,by=5))

# f. Discuss the advantage of various break values
# ~ More breaks are more precise
# ~ Too many breaks may lose sight of the trend

# g. What are weight's and height's modes?
# ~ They correspond to the peaks on the graphs
# ~ There are one for height (165-170 cm)
# ~ and two for weight (65-70 and 75-80 kg).

######### Mode function, to find this more precisely ####################

get_mode <- function(x) {
  # get unique values of the input vector
  uniqv <- unique(x)
  # select the values with the highest number of occurrences
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

get_mode(height)
get_mode(weight)

# h. Plot the same using the 'density' of each variable
# (density is a function that smoothes a distribution)
plot(density(height,na.rm=T))
plot(density(weight,na.rm=T))

# i. Calculate relevant central values for both variables. Do they match your intuition?
round(mean(height,na.rm=T),2)
round(mean(weight,na.rm=T),2)
# ~ Same as for the graph, we can verify that the values make sense
# ~ (ie they match our intuition)



# 3. SAMPLING 

# a. Execute the following three times, then explain what function 'sample' is:
sample(1:10,5)
# ~ This pulls a random sample, where each sample is different

# b. Execute the following three times, then explain what function 'set.seed' is:
set.seed(1); sample(1:10,5)
# ~ As set.seed fixes the first draw, all samples are the same (but still random). This makes it reproducible for somebody else. 

# c. Select a random sample of 100 from the ESS data frame, using "set.seed(1)".
set.seed(1)
(s<-sample(40185,100))




# 4. SAMPLING DISTRIBUTIONS

# a. Generate a vector "w" containing 10000 normally distributed numbers with mean 0 and standard deviation 1. Convert the vector into a data frame "dfw".
w <- rnorm(10000, mean = 0, sd = 1)
dfw <- as.data.frame(w)

# b. Using ggplot, plot a histogram of variable w (hint: use "after_stat(density)"), and overlay the histogram with a red density curve. Label the graph appropriately. 
library(ggplot2)
ggplot(data = dfw) +
  geom_histogram(aes(x=w, y=after_stat(density)),
                 position="identity", color = 'gray1', alpha=0.5)+
  geom_density(aes(x=w), color = 'red', alpha=0.0, linewidth=1) +
  ggtitle("Population (normal)") +
  xlab("") + ylab("Frequency") +
  theme_bw() 


# c. Create an empty vector, called "n30". Write a function which draws 9000 samples of size 30 from variable w and stores the mean of each sample in vector "n30". Convert the vector "n30" into a data frame. 
n30<-c()

n=9000

for (i in 1:n) {
  n30[i] = mean(sample(dfw$w,30, replace = TRUE))}

n30 <- as.data.frame(n30)


# d. Using ggplot, plot a histogram of variable n30 (hint: use "after_stat(density)"), and overlay the histogram with a red density curve. Label the graph appropriately. 

ggplot(data = n30) +
  geom_histogram(aes(x=n30, y=after_stat(density)),
                 position="identity", color = 'gray1', alpha=0.5)+
  geom_density(aes(x=n30), color = 'red', alpha=0.0, linewidth=1) +
  ggtitle("9,000 Samples, n=30") +
  xlab("") + ylab("Frequency") +
  theme_bw()

# e. Plot the two graphs we just created in a grid with two columns and 1 row. (Hint: use package 'gridExtra')

pop_no <- ggplot(data = dfw) +
  geom_histogram(aes(x=w, y=after_stat(density)),
                 position="identity", color = 'gray1', alpha=0.5)+
  geom_density(aes(x=w), color = 'red', alpha=0.0, linewidth=1) +
  ggtitle("Population (normal)") +
  xlab("") + ylab("Frequency") +
  theme_bw() 

no30 <- ggplot(data = n30) +
  geom_histogram(aes(x=n30, y=after_stat(density)),
                 position="identity", color = 'gray1', alpha=0.5)+
  geom_density(aes(x=n30), color = 'red', alpha=0.0, linewidth=1) +
  ggtitle("9,000 Samples, n=30") +
  xlab("") + ylab("Frequency") +
  theme_bw()

library(gridExtra)
grid.arrange(pop_no, no30, 
             ncol = 2, 
             nrow=1)

# f. Interpret the juxtaposition of these graphs. 
# ~ the first graph corresponds to the population distribution. The second graph is a sampling distribution.




# GOING FURTHER 
###########################

#1.

# Uniform Distribution

x <- runif(9000, min = 0, max = 1)
df <- as.data.frame(x)


pop_uni <- ggplot(data = df) +
  geom_histogram(aes(x=x, y=after_stat(density)),
                 position="identity", color = 'gray1', alpha=0.5)+
  geom_density(aes(x=x), color = 'red', alpha=0.0, size=1) +
  ggtitle("Population (uniform)") +
  xlab("") + ylab("Frequency") +
  theme_bw()

s30<-c()

for (i in 1:n) {
  s30[i] = mean(sample(df$x,30, replace = TRUE))}

s30 <- as.data.frame(s30)
u3 <- ggplot(data = s30) +
  geom_histogram(aes(x=s30, y=after_stat(density)),
                 position="identity", color = 'gray1', alpha=0.5)+
  geom_density(aes(x=s30), color = 'red', alpha=0.0, size=1) +
  ggtitle("9,000 Samples, n=30") +
  xlab("") + ylab("Frequency") +
  theme_bw()


# Beta Distribution (Positive Skew)
# Beta distribution is a continuous, but finite (interval [0,1]), 
# probability distribution defined as f(x,a,b) = const x^(a-1) (1-x)^(b-1)

z <- rbeta(10000,2,5)

dfz <- as.data.frame(z)

pop_beta <- ggplot(data = dfz) +
  geom_histogram(aes(x=z, y=after_stat(density)),
                 position="identity", color = 'gray1', alpha=0.5)+
  geom_density(aes(x=z), color = 'red', alpha=0.0, size=1) +
  ggtitle("Population (beta)") +
  xlab("") + ylab("Frequency") +
  theme_bw()

beta30<-c()

for (i in 1:n) {
  beta30[i] = mean(sample(dfz$z,30, replace = TRUE))}

beta30 <- as.data.frame(beta30)
betaplot3 <- ggplot(data = beta30) +
  geom_histogram(aes(x=beta30, y=after_stat(density)),
                 position="identity", color = 'gray1', alpha=0.5)+
  geom_density(aes(x=beta30), color = 'red', alpha=0.0, size=1) +
  ggtitle("9,000 Samples, n=30") +
  xlab("") + ylab("Frequency") +
  theme_bw()



# Create a graph in which each distribution type has a new row, and population and sampling distributions are laced next to each other. 

grid.arrange(pop_no, no30, 
             pop_uni, u3, 
             pop_beta, betaplot3,
             ncol = 2, 
             nrow=4)

