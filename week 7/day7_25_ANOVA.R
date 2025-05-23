getwd()
setwd('/Users/carlo/Desktop/01_Projects/02_Statistics Tutorial SoSe 2025/data')

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

#compare against the null model
model0 <- aov(len~1)
anova(model0, model1) # is it significantly different?

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

#2-way ANOVA
shaun_model<-aov(gain~diet*supplement)
summary(shaun_model)
#remove non significant model variables
shaun_model2<-aov(gain~diet+supplement)
summary(shaun_model2)

#compare against the null model
shaun_model0 <- aov(gain~1)
anova(shaun_model0, shaun_model2) # is it significantly different?

#Residuals?
hist(resid(shaun_model2))

#which of the factor levels are significantly different from each other?
TukeyHSD(shaun_model2)
boxplot(gain~diet)
boxplot(gain~supplement)

#what would you feed your farm animals? 

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

model3<-aov(yield~N+P+K+N:P+N:K+P:K+Error(block))
summary(model3)

model4<-aov(yield~N+P+K+N:P+N:K+Error(block))
summary(model4)

model5<-aov(yield~N+P+K+N:P+Error(block))
summary(model5)

model6<-aov(yield~N+P+K+Error(block))
summary(model6)

model7<-aov(yield~N+K+Error(block))
summary(model7)

#this is our minimum adequate model with all factors included being significant