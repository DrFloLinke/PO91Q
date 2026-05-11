################################################
# PO91Q, WORKSHEET, WEEK 9
################################################

# clear environment
rm(list = ls())

# set wd
setwd("")

# load packages
library(modelsummary)
library(tinytable)
library(tidyverse)

# load data set
wdi <- read.csv("WDI_PO91Q.csv")

# Model 1
############

wdi_life <- lm(gdppc ~ lifeexp, data = wdi)
summary(wdi_lifeexp)

# Intercept: A country with no life expectancy would have an average per capita GDP of -94306.86 USD (obviously nonsense, but that's what the model says).
# Slope: For each additional year of life expectancy, a country's per capita GDP increases on average by 1503.211 USD.


# Model 2
############

# H0: There is no relationship between urbanisation and per capita GDP.
# H1: There higher the level of urbanisation, the higher the expected level of per capita GDP.

wdi_urban <- lm(gdppc ~ urban, data = wdi)
summary(wdi_urban)

# Intercept: A country with 0% urbanisation would have an average per capita GDP of -16983.431 USD (again nonsense, but that's what the model says).
# Slope: For each additional percentage point of urbanisation, a country's per capita GDP increases on average by 542.033 USD.

# Evaluation: The slope coefficient is significant, and so we can interpret it substantively. It points into the right direction, and so we can conclude that this regression model delivers evidence in support of the alternative hypothesis (we fail to reject H0).


# Model 3
############

wdi_joint <- lm(gdppc ~ lifeexp + urban, data = wdi)
summary(wdi_joint)

# Intercept: A country with no life expectancy and 0% urbanisation would have an average per capita GDP of -74786.145 USD (again nonsense, but that's what the model says).
# Slope lifeexp: For each additional year of life expectancy, a country's per capita GDP increases on average by 1012.168 USD, holding urbanisation constant.
# Slope urban: For each additional percentage point of urbanisation, a country's per capita GDP increases on average by 273.735 USD, holding life expectancy constant.



# Modelsummary code for Table 2
# (Note: This is the code that would work to produce the table in Quarto)
##########################################################################

models <- list(
  "Bivariate<br>(1)"     = wdi_life,
  "Bivariate<br>(2)" = wdi_urban,
  "Multiple<br>(3)"     = wdi_joint
)

## Rename Coefficients

cm <- c('lifeexp'    = 'Life Expectancy (years)',
        'urban'    = 'Urbanisation',
        'literacy' = "Literacy",
        '(Intercept)' = 'Constant')

modelsummary(models, 
             estimate = "{estimate}{stars}",
             gof_omit = 'DF|Deviance|Log.Lik|F|AIC|BIC|RMSE', 
             coef_map = cm,
             escape = FALSE,
             notes = "\\vspace{0.1\\baselineskip}",
             notes_append = TRUE)|>
  group_tt(j = list(" " = 1, "Dependent Variable:<br>per capita GDP" = 2:4)) |>
  theme_latex(resize_width= 0.85, resize_direction="both")|> 
  theme_latex(outer = "label={tbl:models1}")


# How have the slope coefficients changed? Why?
####################################################
# lifeexp: from 1503.211 to 1012.168 (decreased)
# urban: from 542.033 to 273.735 (decreased)
# In Model 3 we allow both urbanisation and lifeexpectancy to explain variation in gdppc. As both have a positive impact on gdppc, some of the variation that was previously attributed to one variable is now attributed to the other. This results in smaller slope coefficients for both variables.

# Which model explains the level of GDP best? Why?
###################################################
# Model 3 (Multiple) explains the level of GDP best, as it has best model fit (Adjusted R-Squared of 0.456), and both coefficients are statistically significant. This is because it includes two independent variables that explain variation in gdppc, instead of only one.

# Specify the SRF
#####################
#gdppc_i_hat= -74786.145 + 1012.168 lifeexp expectancy_i + 273.735 urbanisation_i




#Run additional models for Table 3
#####################################

wdi_lit <- lm(gdppc ~ literacy, data = wdi)

wdi_joint1 <- lm(gdppc ~ lifeexp + urban + literacy, data = wdi)


# This produces Table 3
###################################################

models <- list(
  "(1)"     = wdi_life,
  "(2)"     = wdi_urban,
  "(3)"     = wdi_joint,
  "(4)"     = wdi_lit,
  "(5)"     = wdi_joint1
)

cm <- c('lifeexp'    = 'Life Expectancy (years)',
        'urban'    = 'Urbanisation',
        'literacy' = "Literacy",
        '(Intercept)' = 'Constant')

modelsummary(models, 
             estimate = "{estimate}{stars}",
             gof_omit = 'DF|Deviance|Log.Lik|F|AIC|BIC|RMSE', 
             coef_map = cm,
             escape = FALSE,
             notes = "\\vspace{0.1\\baselineskip}",
             notes_append = TRUE)|>
  group_tt(j = list(" " = 1, "Dependent Variable:<br>per capita GDP" = 2:6)) |>
  theme_latex(resize_width= 0.85, resize_direction="both")|> 
  theme_latex(outer = "label={tbl:models2}")


# In Model 5 the coefficient for literacy has turned insignificant. Reproduce the results in Table 4 to find out which variable takes away the significance.
#################################################################################
# We need to run two models, combining literacy with life expectancy and urbanisation, respectively, to find out which one takes significance away. This leads to Table 4:


#Run models for Table 4
########################################
wdi_1 <- lm(gdppc ~ literacy + lifeexp, data = wdi)

wdi_2 <- lm(gdppc ~ literacy + urban, data = wdi)


# This produces Table 4
##############################################
models <- list(
  "(1)"     = wdi_lit,
  "(2)"     = wdi_1,
  "(3)"     = wdi_2
)

cm <- c('lifeexp'    = 'Life Expectancy (years)',
        'urban'    = 'Urbanisation',
        'literacy' = "Literacy",
        '(Intercept)' = 'Constant')

modelsummary(models, 
             estimate = "{estimate}{stars}",
             gof_omit = 'DF|Deviance|Log.Lik|F|AIC|BIC|RMSE|.*', 
             coef_map = cm,
             escape = FALSE,
             notes = "\\vspace{0.1\\baselineskip}",
             notes_append = TRUE)|>
  group_tt(j = list(" " = 1, "Dependent Variable:<br>per capita GDP" = 2:4)) |>
  theme_latex(resize_width= 0.85, resize_direction="both")|> 
  theme_latex(outer = "label={tbl:models3}")


# What can we conclude from this investigation?
###############################################################
# It turns out that both life expectancy and urbanisation take away the significance of literacy. This is because both variables explain variation in gdppc that was previously attributed to literacy. As a result, the slope coefficient for literacy becomes statistically insignificant.


# Does the variable infant have the same effect? What do you conclude from this?
#####################################################################################

wdi_3 <- lm(gdppc ~ literacy + infant, data = wdi)
summary(wdi_3)
# Yes, it does, once again the coefficient for literacy turns insignificant.
# We conclude that regardless of measurement (lifeexp or infant), health is more important in explaining the level of wealth in a country than education. 




# Models for Table 5
#################################
wdi_1 <- lm(gdppc ~ literacy + lifeexp, data = wdi)

wdi_2 <- lm(gdppc ~ literacy + urban, data = wdi)


# This produces Table 5:
################################################
models <- list(
  "(1)"     = wdi_lit,
  "(2)"     = wdi_1,
  "(3)"     = wdi_2
)

cm <- c('lifeexp'    = 'Life Expectancy (years)',
        'urban'    = 'Urbanisation',
        'literacy' = "Literacy",
        '(Intercept)' = 'Constant')

modelsummary(models, 
             estimate = "{estimate}{stars}",
             gof_omit = 'DF|Deviance|Log.Lik|F|AIC|BIC|RMSE', 
             coef_map = cm,
             escape = FALSE,
             notes = "\\vspace{0.1\\baselineskip}",
             notes_append = TRUE)|>
  group_tt(j = list(" " = 1, "Dependent Variable:<br>per capita GDP" = 2:4)) |>
  theme_latex(resize_width= 0.85, resize_direction="both")|> 
  theme_latex(outer = "label={tbl:models3}")

# Which measurement explains GDP better, life or infant?
#################################################################
# life expectancy - the R-Squared is far higher (0.394 vs. 0.222).





###########################################
##### INDEPENDENT STEPS
###########################################

# Sample variables: gdppc, enrol, and lifeexp

model1 <- lm(polity5 ~ gdppc, data = wdi)
model2 <- lm(polity5 ~ enrol, data = wdi)
model3 <- lm(polity5 ~ lifeexp, data = wdi)
model4 <- lm(polity5 ~ gdppc + enrol, data = wdi)
model5 <- lm(polity5 ~ gdppc + lifeexp, data = wdi)
model6 <- lm(polity5 ~ enrol + lifeexp, data = wdi)
model7 <- lm(polity5 ~ gdppc + enrol + lifeexp, data = wdi)



          
###########################################
##### GOING FURTHER
###########################################

# 2D Example

ggplot(wdi, aes(x = gdppc, y = polity5)) +
  geom_point() +
  geom_smooth(method = lm, se=FALSE, colour="#e57726") +
  theme_classic() +
  scale_x_continuous(name = "per capita GDP") +
  ylab('Polity V') +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))


# Example for a Multivariate Model, using the rockchalk package
###################################################################

library(rockchalk)

plotPlane(model5, plotx1 = "gdppc", plotx2 = "lifeexp", 
          plotPoints=TRUE, 
          x1lab = "per capita GDP",
          x2lab = "Life Expectancy at Birth",
          ylab = "Polity V",
          ticktype = "detailed",
          x1floor = 2,
          x2floor = 2,
          cex.axis=0.5,
          npp=5, 
          theta= 325, phi=10)

# Or if you want to match the design on the lecture slides
#############################################################

plotPlane(model5, plotx1 = "gdppc", plotx2 = "lifeexp", 
          plotPoints=TRUE,
          pcol = "#8a1e00",
          lcol = "#e57726",
          col= "#919294",
          llwd = 0.9,
          pch = 16,
          x1lab = "per capita GDP",
          x2lab = "Life Expectancy at Birth",
          ylab = "Polity V",
          ticktype = "detailed",
          x1floor = 2,
          x2floor = 2,
          cex.axis=0.85,
          cex.lab = 0.95,
          cex.main = 0.95,
          font.main = 1,
          adj = 0,
          npp=10, 
          theta=325, phi=10)
