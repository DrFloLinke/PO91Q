knitr::opts_chunk$set(echo = FALSE, collapse=TRUE, message=FALSE, warning=FALSE, error=FALSE, size='scriptsize')
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})

## setwd("")
## 
## library(tidyverse)
## library(haven)
## library(stargazer)
## 
## sim <- read.csv("london.csv", stringsAsFactors = T)

setwd("~/Library/CloudStorage/OneDrive-UniversityofWarwick/Warwick/Modules/PO91Q/Seminars/Week 8/Week 8_Case Study/Case Study_Solutions")

library(tidyverse)
library(haven)
library(stargazer)

london <- read.csv("london.csv", stringsAsFactors = T)

names(london)
dim(london)

table(london$idaci)
class(london$idaci)

table(london$gcse)
class(london$gcse)

ggplot(london, aes(x = idaci, y = gcse)) +
  geom_smooth(method = lm, se=FALSE) +
  geom_point()

ggplot(london, aes(x = idaci, y = gcse)) +
  geom_smooth(method = lm, se=FALSE) +
  geom_point() +
  theme_classic() +
  xlab('IDACI') +
  ylab('GCSE Points') +
  ggtitle("Impact of Deprivation on GCSE Scores") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggplot(london, aes(idaci, gcse)) +
  geom_point(position='jitter', alpha = 1/5) +
  xlab('IDACI') +
  ylab('GCSE Points') +
  ggtitle("Impact of Deprivation on GCSE Scores") +
  theme_classic() +
  geom_smooth(method = 'lm', se=T, colour = 'red', lwd=0.4)+
  geom_hline(yintercept = mean(london$gcse, na.rm=TRUE), color='blue', lty='dashed', lwd=0.4)+
  geom_vline(xintercept = mean(london$idaci,na.rm=TRUE), color='blue', lty='dashed', lwd=0.4)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme(plot.title = element_text(size = 14, face = "bold"))

## # First, create an object #
## scatterplot <- ggplot(london, aes(idaci, gcse)) +
##   geom_point(position='jitter', alpha = 1/5) +
##   xlab('IDACI') +
##   ylab('GCSE Points') +
##   ggtitle("Impact of Deprivation on GCSE Scores") +
##   theme_classic() +
##   geom_smooth(method = 'lm', se=T, colour = 'red', lwd=0.4)+
##   geom_hline(yintercept = mean(london$gcse, na.rm=TRUE), color='blue', lty='dashed', lwd=0.4)+
##   geom_vline(xintercept = mean(london$idaci,na.rm=TRUE), color='blue', lty='dashed', lwd=0.4)+
##   theme(axis.text=element_text(size=12),
##         axis.title=element_text(size=14,face="bold")) +
##   theme(plot.title = element_text(size = 14, face = "bold"))

## # Then save the file #
## save_plot("scatterplot_idaci.png", scatterplot)

# Store the results in an object called model #
model<-lm(gcse ~ idaci, london)
# Visualise the regression output using summary() #
summary(model)

summary(model)[4]

## stargazer(model,
##           header=F,
##           type="html",
##           out="documentname.doc")

stargazer(model,
          header=F,
          font.size = "scriptsize")

## stargazer(model,
##           header=F,
##           font.size = "scriptsize",
##           omit.stat = c("adj.rsq", "ser", "f"),
##           dep.var.labels   = "GCSE Score",
##           covariate.labels = "IDACI",
##           type="html",
##           out="documentname.doc")

stargazer(model,
          header=F, 
          font.size = "scriptsize", 
          omit.stat = c("adj.rsq", "ser", "f"),
          dep.var.labels   = "GCSE Score",
          covariate.labels = "IDACI")
