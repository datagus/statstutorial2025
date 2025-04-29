# Statistics Tutorial Day 4
###################################################

####----1. Working Directory----####
getwd()
setwd()

####----2. Correlation with small sample ----####

# Correlation example from video "How to calculate Pearson's correlation by hand"
# defining / loading your data
Student_attitude <- c(94,73,59,80,93,85,66,79,77,91)
correct_answers <- c(17,13,12,15,16,14,16,16,18,19)

# Testing for normality
shapiro.test(Student_attitude) # p-value above 0.05 -> normal distribution, ! but small sample! 
shapiro.test(correct_answers)

hist(Student_attitude)
hist(correct_answers)
plot(Student_attitude,correct_answers) # does this look like a correlation?

# Pearson's correlation coefficient
cor(Student_attitude,correct_answers)
#Correlation test statistic
cor.test(Student_attitude,correct_answers)

####----3. Correlation with survey data ----####

#load survey data 
trivial<-read.csv("survey25cleaned.csv",header=T, sep=",")
attach(trivial)
str(trivial)
View(trivial)

cor.test(siblings_number,pets)
hist(siblings_number) # is siblings_number normally distributed?
hist(pets)            # is pets normally distributed?
summary(siblings_number)
summary(pets)

# can we use the logarithm for a transformation of pets and siblings_number?
log(0) #what is the logarithm of zero?

hist(log(siblings_number+1)) # avoiding -Inf
hist(log(pets+1))           # avoiding -Inf
shapiro.test(log(siblings_number+1)) 
shapiro.test(log(pets+1)) 
# Without a normal distribution, we cannot use Pearson correlation
# Alternative: let's try other types of correlations instead
?cor.test
cor.test(siblings_number,pets,method="spearman") # does not require normally distributed data
cor.test(siblings_number,pets,method="kendall")  # does not require normally distributed data
plot(siblings_number,pets)
table(siblings_number,pets)

#find other variables in the survey data (trivial data) suitable for a correlation
cor.test(born,travel)
plot(born,travel)

####----4. Correlation with Automobile data ----####
#load data
auto<-read.table("Automobile_R.txt",header=T)
attach(auto)
str(auto)
summary(auto)
#this shows the correlation coefficients for the entire data set (only possible if your data set contains only numbers)
cor(auto)
cor(auto,use = "complete.obs")
plot(auto)
#let's correlate automobile width with length
cor.test(width,length)

#how does this look like?
plot(width,length)
#are the two continuous variables normally distributed?
hist(width)
hist(length)
shapiro.test(width)
summary(width)
hist(log(width))
shapiro.test(log(width))
#if the data is not normally distributed there are two non-parametric rank correlations to choose from
cor.test(length,width,method="spearman")
cor.test(length,width,method="kendall")

# correlation between wheel base (= horizontal distance between the centers of the front and rear wheels) and length
hist(wheel.base)
hist(length)
cor.test(wheel.base,length)
#find a continuous variable in this data set that is normally distributed based on the Shapiro test


####----5. Correlation with base r datasets ----####

# cars dataset
str(cars) # !! different than mtcars dataset
plot(cars) # what is your assumption about a potential correlation?
hist(cars$speed)
hist(cars$dist)
cor(cars)
cor.test(cars$speed,cars$dist, method = "pearson")

# mtcars dataset
cor(mtcars)
min(cor(mtcars)) # find "the most" negative correlation coefficient
hist(mtcars$vs)
hist(mtcars$mpg)
cor.test(mtcars$vs,mtcars$mpg, method = "pearson") # !! be aware, vs cannot be analyzed via correlation, because it is binary
hist(mtcars$hp)
cor.test(mtcars$mpg,mtcars$hp, method = "spearman") 
hist(mtcars$wt)
shapiro.test(mtcars$wt)
cor.test(mtcars$mpg,mtcars$wt)
