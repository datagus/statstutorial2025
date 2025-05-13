getwd()
setwd()
####------1. Boxplots -----#####
# Import the data from Khan Academy video "Constructing a box and whisker plot"
restaurants <-read.table("khan_data.txt", header =T)
attach(restaurants)

# Create a basic boxplot, the dataframe has only one column: "miles"
boxplot(miles)

# Add some style
boxplot(miles, boxwex=0.6, pch = 19, col = "lightgreen")

# Let's check the statistics, what do they say?
summary(miles)

# Another way to check statistics
quantile(miles)

# But do these results match with your results you calculated by hand?!

# Check the documentation of the function quantile
?quantile

# And now? do they match?
quantile(miles, type=6)

summary(miles, quantile.type=6)

# You can use other individual functions such as mean(), median(), length(), range()

#1.2.-Now, imagine that there are different restaurans branches
branches <- c("A","A","C","A","B","B","A","B","C","B","A","C","C","A","B","B","C")

# Add this to your dataframe
restaurants$branches <- branches

# Now create this boxplot, pay attention to which variable you write first
boxplot(miles~branches)

# Add some style
boxplot(miles~branches,
        xlab= "branches", ylab= "miles", 
        main="Restaurant distances", pch = 19, 
        col = c("lightgreen","blue", "pink"), outcol = "red")

# This is how you add more data to a dataframe
# Create a new name to keep the old dataframe
new<- rbind(restaurants, data.frame(miles = 40, branches = "C"))

# Create the boxplot
boxplot(miles~branches,data=new, 
        xlab= "branches", ylab= "miles", 
        main="Restaurant distances", 
        pch = 19, col = c("lightgreen","blue", "grey"), 
        outcol = "red")

# Let's add another extreme value or outlier
# Outlier = the values of any data points which lie beyond the extremes of the whiskers.
# Extremes = 1.5 * IQR

new2<- rbind(restaurants, data.frame(miles =1100, branches = "A"))
# Run the previous boxplot again
boxplot(miles~branches,data=new2, 
        xlab= "branches", ylab= "miles", 
        main="Restaurant distances", 
        pch = 19, col = c("lightgreen","blue", "grey"), 
        outcol = "red")

# Look how the boxplots get squashed. Play around with the restaurants dataframe and add more values


####------2. Anova -----#####
# Import data
production <-read.table("yields.txt",header=T)

# Inspecting the dataframe
str(production)
summary(production)

# Attaching dataframe
attach(production)

# Is the response variable "yield" normally distributed?
hist(yield)
shapiro.test(yield)


# Creating the boxplot
boxplot(yield~soil,ylab="yield [mg]")

# When running anovas and other modelling, converting categorical variables as factor is best practice

# R software always sorts alphabetical, if you want to sort from lowest to highest:change order by defining soil to be an ordered factor
production$soil<-factor(soil,levels=c("sand","clay","loam"))

# Plot again
boxplot(yield~soil,ylab="yield [mg]", col = c("pink", "orange", "wheat"))
table(soil)

#There are two possibilities to calculate ANOVA
# Option 1: Calculating ANOVA as a linear model
model<-lm(yield~soil)
anova(model)
summary(model)

# Option 2: Applying directly the ANOVA function
model<-aov(yield~soil)
summary(model)

# As well as in linear regression, ALWAYS check the residuals
hist(resid(model))

# You can also use the plot(model)command, just make sure you adjust it the name of your model, this model is called "model"
plot(model)

# What happens if you run a t-test to compare only sand with clay and sand with loam?
?t.test
t.test(yield[soil=="sand"],yield[soil=="clay"])
t.test(yield[soil=="sand"],yield[soil=="loam"])



# Note: You have to apply directly the ANOVA function (not using lm() function) for a posthoc-Test = to test every factor level against each other
# PostHoc test
TukeyHSD(model)

# Challenge: Try ANOVA yourself but with fertilizer
# Create boxplots, check residuals, run t-test and Post Hoc test
# Interpret and write your results


####------------3. Airbnb example-------------#####
# Load airbnb data and attach it
air<-read.csv("airbnb.csv",header=T, sep=",")
attach(air)
str(air)

# Step 1: Visually inspect your data with boxplots
# Which variables could be your factors and which your dependent variable?

boxplot(price~city) #It looks nice, doesn't it?

# Better try log() transformation
boxplot(log(price)~city) # better right?
table(city)

# Let's narrow down the data and consider only the following cities: Amsterdam, Barcelona, London and Paris
# don't get stressed with this code section
cities_subselected <- c("Amsterdam", "Barcelona", "London", "Paris") # defining cities
number_rows <- 859 #859 is the total number of rows or datapoints corresponding to Amsterdam

# Define the function
get_first_rows <- function(city_name, number_rows) {
  city_data <- subset(air, city == city_name)
  city_data[1:min(number_rows, nrow(city_data)), ]
}

# Applying function
air2 <- do.call(rbind, lapply(cities_subselected, function(city) {
  get_first_rows(city, number_rows)
}))

detach(air)
attach(air2)

# Inspecting again

table(city)


boxplot(price~city, las = 2, cex.axis = 0.8)
boxplot(log(price)~city, las = 2, cex.axis = 0.8)

# Check the normality of price
hist(price)

hist(log(price)) #let's take this one

# Converting into factor
city <- factor(air2$city)


# Running anova
modelair <-aov(log(price)~city)
summary(modelair)

# Checking the residuals
hist(resid(modelair))


#Now PostHoc Test
TukeyHSD(modelair)

# Which pair of cities can we conclude that have similar prices on average?

# Challege: now test if price significantly differs between Amsterdam, Barcelona, Berlin, Budapest, Vienna with an ANOVA
cities_subselected <- c("Athens", "Budapest", "Lisbon", "Rome", "Vienna")
number_rows <- 1000 

# Applying function
air3 <- do.call(rbind, lapply(cities_subselected, function(city) {
  get_first_rows(city, number_rows)
}))

detach(air2)
attach(air3)

city <- factor(air3$city)
table(city)


####-------HOMEWORK----------######
# With same subselected dataframe "air3", perform an anova regarding:
# price and bedrooms, 
# price and day, 
# price and room_Type
# price and person_capacity

# REMEMBER TO PRACTICE PRACTICE PRACTICE !!!!!! Use the survey data from past tutorials and practice the ANOVA

