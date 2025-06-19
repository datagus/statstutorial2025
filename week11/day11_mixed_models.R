### ---- Mixed effect models ---- ###

# ---- Load data ----

# install the packages nlme and multcomp and call it by running the library command 
install.packages("nlme")
install.packages("multcomp")
library(nlme)
library(multcomp)

# set working directory
getwd()
setwd("/Users/Wanja/R_tutorial/week11")

# load the splityield data 
yields<-read.table("splityield.txt",header=T)
names(yields)


# ---- Inspect data ----

# inspect data
str(yields)

# convert the categorical variables as factors
yields$block <- as.factor(yields$block)
yields$irrigation <- as.factor(yields$irrigation)
yields$density <- as.factor(yields$density)
yields$fertilizer <- as.factor(yields$fertilizer)

# attach the dataset
attach(yields)

# understand the study design
ftable(block ~ irrigation + density + fertilizer)
ftable(fertilizer ~ irrigation + density)

# let's visualize it
mosaicplot(~ block + irrigation + density + fertilizer, data = yields, 
           color = c("brown", "orange", "yellow"),
           cex.axis = 0.6, las = 0, main = "Experiment design")

# check distribution of yield
hist(yield)

# visualize the treatments vs yield
boxplot(yield~irrigation*density*fertilizer, las=2, cex.axis=0.5, xlab="")

# ---- Run mixed effect model ---- 

# try to fit a mixed effect model, defining random and fixed effects
model1<-lme(yield~irrigation*density*fertilizer,
            random=~1|block/irrigation/density)
summary(model1)
anova(model1) # check significance of the overall treatment terms

# simplify, leaving the significant terms in the model 
model2<-lme(yield~(irrigation+density+fertilizer)^2,
            random=~1|block/irrigation/density)
summary(model2)
anova(model2)

# further reduce
model3<-lme(yield~irrigation*density+irrigation*fertilizer,
            random=~1|block/irrigation/density)
summary(model3) 
anova(model3) # minimum adequate model

# check the model 
# extract residuals
model3$residuals
# plot a histogram of residuals: normal distribution?
hist(model3$residuals)
# stars in the sky? 
plot(model3)

# the following is a deeper dive into the model structure
# find the fitted values
str(model3)
# here they are
model3$fitted

# plotting response variable against the fitted values
# i.e. comparing the yield values predicted by the model with the observed values
plot(model3$fitted[,2],yield) # block
plot(model3$fitted[,3],yield) # irrigation
plot(model3$fitted[,4],yield) # density

