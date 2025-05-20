getwd()
setwd()

############## Two-Way ANOVA #############################

#Let's use some data in R: toothgrowth data
data() #opens a Window with all the available data sets 
data("ToothGrowth")

#Inspect the data
?ToothGrowth
attach(ToothGrowth)
str(ToothGrowth) #how many observations per group and level?
summary(ToothGrowth)

#transforming dose into a factor
new_dose<-as.factor(dose) #dose is numeric here, but should be a factor 
str(new_dose)

# Is the response variable "length" normally distributed?
hist(len)
shapiro.test(len)

#balanced design?
table(new_dose,supp)
#variances?
boxplot(len~supp:new_dose)
#for a more advanced boxplot of this dataset go to ?boxplot
?boxplot

#two-way ANOVA
#full model
model1<-aov(len~supp*new_dose,data=ToothGrowth) #interactive effect *
summary(model1)
#reduced model
model2<-aov(len~supp+new_dose,data=ToothGrowth) #single effect WITHOUT interaction +
summary(model2)

#Residuals
hist(resid(model1))

#which of the factor levels are significantly different from each other?
TukeyHSD(model1)
TukeyHSD(model2)

####################################
#Next: Sheep Growth data, gain of sheep with diet and supplement
shaun<-read.table("growth.txt",header=T)
attach(shaun)
str(shaun)

# Is the response variable "gain" normally distributed?
hist(gain)
shapiro.test(gain)

#balanced design?
table(supplement, diet)
#variances?
boxplot(gain~diet:supplement)
#how can you change the label for the x-Axis so that it is readable?
boxplot(gain~diet:supplement,las=2, cex.axis=0.6,xlab="")
#what happens if you switch diet and supplement?
boxplot(gain~supplement:diet,las=2, cex.axis=0.6,xlab="")

####################################
#EXERCISE: NOW TRY OUT A TWO-WAY ANOVA WITH THE SHEEPGROWTH DATA FOR YOURSELF

#2-way ANOVA

#remove non significant model variables


#Residuals?


#which of the factor levels are significantly different from each other?

#visualize your data again

##### What would you feed your farm animals? 

###################### 3-Way ANOVA ##############

#the dataset npk contains information about the effect of nitrogen, phosphate and potassium on the growth of peas
data("npk")
attach(npk)
str(npk)
summary(npk)
table(N, P, K)

# Is the response variable "yield" normally distributed?
hist(yield)
shapiro.test(yield)

#let's visualize our data
par(mfrow=c(2,2))
par(mar=c(2,2,1,1))
boxplot(yield~N, main="Nitrogen")
boxplot(yield~P, main="Phosphate")
boxplot(yield~K, main="Potassium")
boxplot(yield~block, main="Block")
graphics.off()

boxplot(yield~N*P*K)

#we can construct a full model
model<-aov(yield~N*P*K)
summary(model)

#let's try to reduce the model
#the non-significant treatments and treatment interactions are subsequently removed 
#to arrive at the minimum adequate model
model2<-aov(yield~N*P*K+Error(block))
summary(model2)

######################
#EXERCISE: FIND THE MINIMUM ADEQUATE MODEL FOR NPK DATASET

