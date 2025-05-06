####----1. Working Directory----####

getwd()
setwd()

####----2. Caterpillar Data ----####

# this data set is growth of caterpillars [mg] in relation to tannin concentration [micromole] in their diet
cater<- read.table("regression.txt",header=T)
attach(cater)

# let's take an intial look at the data
names(cater)
str(cater)
dim(cater)
cater
View(cater)

# data examination
plot(tannin,growth)

# changing the look of your graph
?par # -> gives you many arguments to change the appearance of a plot
plot(tannin, growth, col = "blue", xlab = "tannin concentration [micromole]", ylab = "caterpillar growth [mg]") # how about blue dots?
plot(tannin, growth, col = "blue", pch = 16,xlab = "tannin concentration [micromole]", ylab = "caterpillar growth [mg]") # how about filled dots?
plot(tannin, growth, col = "blue", pch = 8, xlab = "tannin concentration [micromole]", ylab="caterpillar growth [mg]") # how about stars?

## normal distribution of variables

# histogram of growth

# shapiro test of growth

# Kolmogorov Smirnov Test of growth
ks.test(growth, "pnorm", mean=mean(growth), sd=sd(growth)) # We may also try another test: the Kolmogorov Smirnov Test


# histogram of tannin

# shapiro test of tannin

# Kolmogorov Smirnov Test of tannin


## graphical inspection of relation

# a correlation plot of growth and tannin

## linear regression model with abline
model<-lm(growth~tannin)
abline(model,col="red")

## modeling
model<-lm(growth~tannin) #note "model" is not a command this is only a name
summary(model)

#you can also write
summary(lm(growth~tannin))

str(summary(model)) # another way of looking at the model metrics

# let's look at the residuals of the model
hist(resid(model)) # normal distribution?
plot(resid(model)) # stars in the sky?
abline(h=0, col="violet")
shapiro.test(resid(model)) # p-value > 0.05?


####----3. Ecology Practical Data ----####

# next load ecology practical data:
eco<-read.csv("ring_count_22plus.csv",header=T,sep=",")
attach(eco)
str(eco)
View(eco)
plot(diam,rings)

# please write the code for a linear model called "model" that explains "rings" with "diameter"


# plot the model
plot(diam,rings,xlab ="Stem diameter [cm]", ylab ="Number of growth rings",pch=16)
abline(model,col="red")


# summary of the model

# histogram of the residuals

# residuals vs. fitted values + abline



####----4. Multiple Linear Regression with AirBNB data ----####

# load data airbnb data (see file "airbnb_explanations.xlsx" for an explanation of the column names)
air<-read.csv("airbnb.csv",header=T, sep=",")

#inspect data
attach(air)
names(air)
str(air)
summary(air)
dim(air)

# how are the variables distributed?
par(mfrow=c(2,3))
hist(price);hist(guest_satis);hist(distance);hist(metro_dist);hist(cleanliness);hist(bedrooms)
par(mfrow=c(1,1))

# can we explain the price?
airmod <- lm(price ~ cleanliness+bedrooms+distance+metro_dist)

# are the residuals of the model normally distributed?


# can we improve the distribution of the residuals by transforming some of our variables?
hist(price)
hist(log(price))

# next attempt
airmod2 <- lm(log(price) ~ cleanliness+bedrooms+distance+metro_dist)
hist(resid(airmod2)) # looks better

summary(airmod2)


#throw out the insignificant variables
airmod3 <- 
summary(airmod3)
hist(resid(airmod3))

## Checking for multicollinearity

# Install and load the ggcorrplot package
install.packages("ggcorrplot")
library(ggcorrplot)

# Reduce air data to numeric variables from the model 
reduced_data <- subset(air, select = c(bedrooms,distance,metro_dist,cleanliness))

# Compute correlation at 2 decimal places
corr_matrix = round(cor(reduced_data, method="spearman"), 2)

# Compute and show the  result
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

#only choose independent variables that do not correlate with each other
airmod4 <- 
summary(airmod4)
hist(resid(airmod4))

#does the model perform better with metro_dist instead of distance? 
airmod5 <- 
summary(airmod5)
hist(resid(airmod5))

## Let's take a closer look at London

# select London for multiple regression
London <- air[city == 'London',]

detach(air)
attach(London)

# data inspection
dim(London)
summary(London)

# multiple regression with price as dependent and three characteristics as independent variables
london_mod1<- lm(log(price)~cleanliness+bedrooms+distance)
summary(london_mod1)
hist(resid(london_mod1)) 

# multiple regression with guest satisfaction as dependent and four characteristics as independent variables
london_mod2<- lm(guest_satis~cleanliness+bedrooms+distance+price)
summary(london_mod2)
hist(resid(london_mod2)) 

# compare London with other cities
detach(London)
attach(air)
boxplot(guest_satis ~ city)
boxplot(price ~ city)

#now try multiple regression with other cities....
