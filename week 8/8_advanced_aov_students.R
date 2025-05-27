# Statistics Tutorial Day 8
###################################################

####----1. Working Directory----####
getwd("C:/Users/YourName/Documents/Projects/MyProject")
setwd("/Users/yourname/Documents/Projects/MyProject")

####----2. Splityield Dataset----####

# 2.1 Loading data and inspection

# load and attach data
crops <-read.table("splityield.txt", header =T)
attach(crops)

# convert the all categorical variables as factors
# example for block
crops$block <- as.factor(crops$block)

# now for all other categorical variables

# investigate data

# balanced design? - look into the structure of the data
?ftable
ftable(block ~ irrigation + density + fertilizer)

# let's visualize it
?mosaicplot
mosaicplot(~ block + irrigation + density + fertilizer, data = crops, 
           color = c("brown", "orange", "yellow"),
           cex.axis = 0.6, las = 0, main = "Experiment design")

# 2.2 Preconditions of the ANOVA

# Is the response variable "yield" normally distributed?

# homoscedasticity?
boxplot(yield~irrigation*density*fertilizer,
        las=2,
        cex.axis=0.5,
        xlab="")

# 2.3 Building ANOVA model(s)

## First, let's run the null model. 
null_model <- aov(yield ~ 1)

# get summary of null_model # This model is telling that all the variation is unexplained


## Running full model
fullmodel <- aov(yield~irrigation*density*fertilizer)

# get summary of fullmodel

## but where is block? In this experiment block is variable but we are not interested for testing
## still block introduces variation
## accounting for the block effect
boxplot(yield~block) #what do you see?

fullmodel_b <- aov(yield~ block + irrigation*density*fertilizer)
summary(fullmodel_b)

## because we are not interested in the block effect we put it as Error in the formula
fullmodel_e <- aov(yield~irrigation*density*fertilizer+Error(block))
summary(fullmodel_e) #check the summary. Which factors are not significant?

## removing insignificant variables until reaching a minimum adequate model:
## your turn to remove the three-way interaction and create a model with two-way interactions
twoway_model2 <- 

# here is another way to write it
twoway_model2 <- aov(yield ~ (irrigation + density + fertilizer)^2 
                     + Error(block))
summary(twoway_model2) #which factors or interactions are not significant?

## your turn to remove the two-way interaction of density and fertilizer
twoway_model3 <- 

summary(twoway_model3)# -> we can call this our minimum adequate model for now

TukeyHSD(fullmodel) #let's compare the post hoc test with the full model

minimum_model <- aov(yield~irrigation+density+fertilizer
                     +irrigation:density+irrigation:fertilizer)

TukeyHSD(minimum_model) #models with Error() cannot be used in the TukeyHSD function.

## how much of the variation is now unexplained?
total_variation <- 8278+1758+1977+2747+953+7848
residuals_variation <- 7848
percentage_unexplained <- round(residuals_variation/total_variation * 100,0)
paste0(percentage_unexplained, "% of the total variation remains unexplained")

## how to compare anova models?
## you can use an anova to compare different anova models. However, to do this comparison,
## we cannot use the Error() in the models we want to compare. 

## Let's compare the nullmodel vs the fullmodel
anova(null_model, fullmodel) #what is the null hypothesis in this case?


# 2.4 Role play: You work for Monsanto. Build a model with which you can investigate 
### which of your fertilizers works best.
### if you don't want to account the variation introduced by some factors, you can put them in the Error.
### For example
evil_m1 <- aov(yield ~ irrigation*density*fertilizer
               +Error(block/irrigation/density))
summary(evil_m1)

### your turn to remove the three-way interaction
evil_m2 <- 
summary(evil_m2)

### removing two-way interaction of density and fertilizer
evil_m3 <- 
  
summary(evil_m3) #this the minimum adequate model
### as part your role, you have to tell the Monsanto CEO about your results.
### What would you say?


####----3. mtcars ----####

#load data
data(mtcars)
attach(mtcars)

#investigate data


#### Question: How is mpg explained by am (transmission), vs (engine) and/or gear? ###

# Let's make more readable, but let's create first a copy
mtcars2<- mtcars

# Renaming and relabeling transmission, and putting as factor.
mtcars2$transmission <- factor(mtcars2$am,
                               levels = c(0, 1),
                               labels = c("automatic", "manual"))

# Rename and relabel engine type, and putting as factor.
mtcars2$engine <- factor(mtcars2$vs,
                         levels = c(0, 1),
                         labels = c("V-shaped", "straight"))

# Relabel gear as factor with descriptive label, and putting as factor.
mtcars2$gear <- factor(mtcars2$gear,
                       levels = c(3, 4, 5),
                       labels = c("3 gears", "4 gears", "5 gears"))

detach(mtcars)
attach(mtcars2)
str(mtcars2) #check the structure again

#Initial inspection
table(engine, transmission) #ok
table(engine, gear) #5 gears vs straight has only one sample!
table(transmission, gear) #Two zeros!

#another way
ftable(engine, transmission, gear) #not a balanced design

# your turn to create 4 boxplots with each mpg and 1) engine, 2) transmission, 3) gear and 4) engine*transmission*gear



#how does it look?
mosaicplot( ~ transmission+engine+gear,  data = mtcars2, 
            color = T ,cex.axis = 0.6, las = 0, main = "Treatment Structure by Block")

##All unbalanced-> Type III Anova

##Full model
model1 <- aov(mpg~engine*transmission*gear)
summary(model1) 
#all seems ok, however, using aov() cannot tell you anything about unbalanced designs

#instead use Anova() function from car package
#if not installed, use install.packages("car")
library(car)

#running full model
model_1 <- Anova(lm(mpg~engine*transmission*gear, data=mtcars2),type="III") 
model_1 #Not enough statistical power, model fails

#Note: Use type III when you have interactions, use type II otherwise

# for a second model remove transmission factor
model_2 <- 
model_2

#for a third model remove interactions between engine and gear
model_3 <- 
model_3

#create model 4 by running an Anova for engine and transmission
model_4 <- 
model_4

#remove interactions of model 4
model_5 <- 
model_5

# create model 6 with all three factors but without interactions
model_6 <- 
model_6

# Which of those six models is the Minimum Adequate Model?