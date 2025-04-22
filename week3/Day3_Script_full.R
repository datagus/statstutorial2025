# Statistics Tutorial Day 3
###################################################

####----1. Working Directory----####
getwd()

#this is how you set your working directory on a Mac, 
#you need to adjust it to the location of the txt or csv file on YOUR computer!
#This basically tells R where to find the data:
setwd("/Users/carlo/Desktop/01_Projects/02_Statistics Tutorial SoSe 2025/data")

#with a Windows computer it looks slightly different:
#remember when you copy the path from Windows you have to change the /
setwd("C:/")

####----2. Basic Data Types in R----####

## character or string: anything inside single or double quotes
a <- 'apple'

## numeric: whole numbers and decimal numbers
b <- 1.618

## integers: positive and negative whole numbers 
#             with `L` attached at the end
c <- 3L
d <- -5L

# check what type of object `b` is
typeof(b)
class(b) # numeric


####----3. Normal Distribution----####

#data to calculate the mean and median
#enter the values from the board below into the vector candy (separate by comma)
#for example name<-c(1,2,3)

candy<-c(1,	2,	3,	5,	3,	2,	4,	2,	5,	6,	8,	3,	2)
str(candy) #the structure of the data
summary(candy) #descriptive information about the data
boxplot(candy) #boxplot
hist(candy) #histogram

?hist

#let's see how we can make this more "normal"
#why logarithm?
hist(log(candy))


####----4. Load survey data----####
survey<-read.csv("survey25cleaned.csv",header=T, sep=",")

attach(survey) #why do we use attach?

detach(survey)  #what happens if we "detach"?
str(survey)
summary(survey)
View(survey)

survey$happy1 #get the happy1 column of the survey
happy1      #why the error?

####----5. Chi-Square-Test (chisq.test) for Independence----####

# H0 : The two variables are (stochastically) independent
# H1 : The two variables are related

#Link between pineapple on pizza and OS preference?

#create a table with both variables
table(Pineapple_Pizza,OS) 

#calculate X2 (chi-square) statistic
chisq.test(table(Pineapple_Pizza,OS))   
chisq.test(table(Pineapple_Pizza,OS))$expected #shows expected values

# the p-value bigger than 0.05 : we cannot reject the H0
# ==> the two variables are not related (independent)


#let's only check for Microsoft and Apple
#this is how you sub-select for Apple (does not create new variable!)
Pineapple_Pizza[OS=="Apple"]
#this is how you sub select for several factor levels
Pineapple_Pizza[OS == "Apple" | OS == "Microsoft" ]


#for the chisq.test you need to do this for both variables 
#(if you want a shorter command structure create new variables instead, see below)
table(Pineapple_Pizza[OS == "Apple" | OS == "Microsoft" ],OS[OS == "Apple" | OS == "Microsoft" ])
chisq.test(table(Pineapple_Pizza[OS == "Apple" | OS == "Microsoft" ],OS[OS == "Apple" | OS == "Microsoft" ])) 

# or
Pineapple <- Pineapple_Pizza[OS == "Apple" | OS == "Microsoft" ]
OperativeSystem <- OS[OS == "Apple" | OS == "Microsoft" ]

table(Pineapple, OperativeSystem)
chisq.test(Pineapple, OperativeSystem)

# do we reject the null hypothesis?
#Test other variables from the survey that you can test with a Chi-Square-Test

#here is the example from R
?chisq.test
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null


####----6. Student t-test (Test for equality of means) - Independent----####
# H0 : means are equal
# H1 : means are not equal
# 3 key decisions of t-test: independence, normality, equal sample size and variance

# Data set: mtcars
# Are miles/gallon and engine related?

### Independence ###
# Is the data independent?
?mtcars
#yes

## creating the data
cars<-mtcars

# we can create another column with a more explicit name
cars$engine <- ifelse(cars$vs == 0, "shaped", "straight")
table(cars$engine)

# let's create new variables where our data is stored
cars_shaped <- cars$mpg[cars$engine=="shaped"]
cars_straight <- cars$mpg[cars$engine=="straight"]

### Test for Normality ###
hist(cars_shaped)
hist(cars_straight)

shapiro.test(cars_shaped)
shapiro.test(cars_straight)

#what about with log?
log_cars_shaped <- log(cars_shaped)
log_cars_straight <- log(cars_straight)

hist(log_cars_shaped)
hist(log_cars_straight)

shapiro.test(log_cars_shaped)
shapiro.test(log_cars_straight)

### Check sample size of groups ###
table(cars$engine)

## F-Test (Test for Equality of Variance) #
# H0 : Ratio of variance is equal to 1 (they have the same variance)
# H1 : Ratio of variance is NOT equal to 1

#F-test
var.test(cars_shaped, cars_straight)
#since p-value is above 0.05 we do not reject H0

### Independent t-Test ###

t.test(cars$mpg ~ cars$engine)
#or
t.test(cars_shaped,cars_straight)
#since p-value is below 0.05 we reject H0
#means are not equal

# Boxplots for visual comparison
boxplot(cars$mpg ~ cars$engine)
boxplot(log(cars$mpg) ~ cars$engine)    #what changed?


####----7. Paired t-test----####
# Paired Samples - when we apply the 2 treatments to the same samples, 
# giving us paired combinations of samples for the 2 treatments

# Assumptions for the Paired Sample T-Test:

# (1) Differences between paired values follow a Normal distribution
# (2) Data is Continuous
# (3) We have paired or dependent samples
# (4) Simple Random Sampling - each unit has an equal probability of being selected

# Setting up our Hypotheses (Two-Tailed):

# H0: The pairwise difference between means is 0 / Paired Population Means are equal
# H1: The pairwise difference between means is not 0 / Paired Population Means are not equal

### Example: Weight loss after raising the younglings?

# Read data from .txt file
weight<-read.table("t.test.paired.txt",header=T)

# Attach object to search path
attach(weight)

# Structure of the object
str(weight)

# Boxplots for visual comparison
boxplot(before,after)

# Performing a two-tail, repeated T-Test
weight.t <- t.test(before,after, paired=T)

# Structure of output object
str(weight.t)

# Round up p-values
round(weight.t$p.value,digits=2)

# p-value is below 0.05: We reject the H0
# means for before and after treatment are not equal
