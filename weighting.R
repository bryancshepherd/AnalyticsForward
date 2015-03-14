# Creating weights with R supporting code for Analytics Forward 3/14/15 presentation
# Based heavily on work by Christoph Waldhauser presented here: http://tophcito.blogspot.com/2014/04/survey-computing-your-own-post.html

# Make sure we have the required packages and load them
# install.packages("survey")

library(survey)

# Bring in the data to weight
orgData = read.csv(file = "Data/weightingData.csv", stringsAsFactors = FALSE)

## We will weight based on the following variables:
# Survey var name, Census var name, Description
# age, AGEP, Age as of December 2014
# d1, SEX, Sex
# d9, HUPAC, Children under the age of 18 in HH 
# d6, MAR - Marital status

summary(orgData$age)
table(orgData$age)

## Age - restrict data to 18 and above and create three age groups for simplicity
weightingData = orgData[orgData$age >= 18, ]
weightingData$ageGroup = cut(weightingData$age, breaks = c(18, 34, 64, 90), include.lowest = TRUE, labels = FALSE)

## Based on census 5-year file the relative population percentages are:
# 18 - 34 = 30.8%
# 35 - 64 = 51.9%
# 65 and above = 17.3%

## The relative percentages in the sample are:
prop.table(table(weightingData$ageGroup, useNA = "always"))

## Sex
# Based on census 5-year file the relative percentages are:
# Female = 51.2%
# Male = 48.8%

## The relative percentages in the sample are:
prop.table(table(weightingData$d1, useNA = "always"))

## Children under the age of 18 in the household
# Recode survey variable to binary variable indicating children under 18 in the home (1) or not (0)
weightingData$childrenInHH = ifelse(weightingData$d9 >= 1, 1, 0)

# Based on census 5-year file the relative percentages are:
# No children under 18 in household = 64.3%
# Children under 18 in household = 35.7%

## The relative percentages in the sample are:
prop.table(table(weightingData$childrenInHH, useNA = "always"))

## Marital status
# Based on census 5-year file the relative percentages are:
# Married = 50.8%
# Widowed = 5.7%
# Divorced = 11.7%
# Separated = 2.3%
# Never married = 29.4%

## The relative percentages in the sample are:
prop.table(table(weightingData$d6, useNA = "always"))

## Now the magic happens
# Create an unweighted survey object
unweightedData = svydesign(ids=~1, data = weightingData)

## Create data frames that include the population information for each variable

# Age
ageGroup.dist = data.frame(ageGroup = c(1, 2, 3), Freq = nrow(weightingData) * c(.308, .519, .173))

# Sex
sex.dist = data.frame(d1 = c("Female", "Male"), Freq = nrow(weightingData) * c(.512, .488))

# Children under 18 in household
childrenInHH.dist = data.frame(childrenInHH = c(0, 1), Freq = nrow(weightingData) * c(.643, .357))


# Marital status
marital.dist = data.frame(d6 = c("Now married", "Widowed", "Divorced", "Separated", "Never married"), 
                               Freq = nrow(weightingData) * c(.508, .057, .117, .023, .294))

## Ok, now the real magic
# The rake command iteratively adjusts the weights so that the relative proportions match as closely as possible for ALL 
# weighting variables
rakedData = rake(design = unweightedData, 
                 sample.margins = list(~ageGroup, ~d1, ~childrenInHH, ~d6), 
                 population.margins = list(ageGroup.dist, sex.dist, childrenInHH.dist, marital.dist))

## It is important to check the highest and lowest weights to make sure they aren't extreme
# "Extreme" is subjective and contextual, but a good cutoff is often above 5 and below .2
summary(weights(rakedData))

## The trimweights command reweights by top/bottom coding extreme weights and reallocating the weight
rakedAndTrimmedData = trimWeights(rakedData, lower = .25, upper = 4, strict = TRUE)

# We shouldn't have any weights outside of our thresholds now
summary(weights(rakedAndTrimmedData))

# We can now use the weight as part of the survey package, or add it to our data frame for use elsewhere
weightingData$weightVar = weights(rakedAndTrimmedData)


## Test out our new weights
# Before applying weight
weighted.mean(weightingData$age)

# After applying weight
weighted.mean(weightingData$age, weightingData$weightVar)

# Before applying weight
weighted.mean(weightingData$d1 == "Male")

# After applying weight
weighted.mean(weightingData$d1 == "Male", weightingData$weightVar)

# Before applying weight
weighted.mean(weightingData$q10 %in% c("Do not plan to be using it at all",
                                       "Much less frequently",
                                       "Less frequently"))

# After applying weight
weighted.mean(weightingData$q10 %in% c("Do not plan to be using it at all",
                                       "Much less frequently",
                                       "Less frequently"), weightingData$weightVar)

# Before applying weight
weighted.mean(weightingData$q7 %in% c("Once a day or more"))

# After applying weight
weighted.mean(weightingData$q7 %in% c("Once a day or more"), weightingData$weightVar)

