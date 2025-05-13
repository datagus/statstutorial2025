dat<-read.table("yields.txt",header=T)

attach(dat)
boxplot(yield~soil,ylab="yield [mg]")
soil<-factor(soil,levels=c("sand","clay","loam"))
boxplot(yield~soil,ylab="yield [mg]")

#################################

model<-aov(yield~soil)
summary(model)
hist(resid(model))
shapiro.test(resid(model))

#you can also use the plot(model)command, just make sure you adjust it the the name of your model, this model is called "model"
plot(model)

#you have to chose option 2 (see above) for a posthoc-Test = to test every factor level against each other
#Post Hoc test
TukeyHSD(model)


############     THEORY SECTION   #############################################
####The following plots explain SSY (total sum of squares) AND SSE (error sum of squares)
#SSA (treatment sum of squares OR explained variation) SSA=SSY-SSE
#These plots explain the theory,you will not do this for every ANOVA!
plot(yield)
#close graphics window and then...
par(mfrow=c(1,2))
plot(yield,pch="")#to draw a plot with points step by step you first have to draw the plot with invisible points pch=""
points(yield[soil=="sand"],pch=15)
points(11:20,yield[soil=="clay"],pch=3)

points(21:30,yield[soil=="loam"],pch=24)
abline(h=mean(yield),col="red")
for(i in 1:30)lines(c(i,i),c(yield[i],mean(yield)))
text(3,18,"SSY")


mean(yield) - 14

plot(yield,pch="")
points(yield[soil=="sand"],pch=15)
points(11:20,yield[soil=="clay"],pch=3)
points(21:30,yield[soil=="loam"],pch=24)
SANDmean<-mean(yield[soil=="sand"])
lines(c(0,10),c(SANDmean,SANDmean))
CLAYmean<-mean(yield[soil=="clay"])
lines(c(11,20),c(CLAYmean,CLAYmean))
LOAMmean<-mean(yield[soil=="loam"])
lines(c(21,30),c(LOAMmean,LOAMmean))
for(i in 1:10)lines(c(i,i),c(yield[i],mean(yield[soil=="sand"])))
for(i in 11:20)lines(c(i,i),c(yield[i],mean(yield[soil=="clay"])))
for(i in 21:30)lines(c(i,i),c(yield[i],mean(yield[soil=="loam"])))
text(3,18,"SSE")

####plots to explain SSY (total sum of squares) AND SSE (error sum of squares), when SSE is zero
#SSA (treatment sum of squares OR explained variation) SSA=SSY-SSE
xvc<-1:15
yvs<-rep(c(10,12,14),each=5)
par(mfrow=c(1,2))
plot(xvc,yvs,ylim=c(5,16),pch=(15+(xvc>5)+(xvc>10)))
for(i in 1:15)lines(c(i,i),c(yvs[i],mean(yvs)))
abline(h=mean(yvs),col="red")

text(3,15,"SSY")
plot(xvc,yvs,ylim=c(5,16),pch=(15+(xvc>5)+(xvc>10)))
lines(c(1,5),c(10,10),col="red")
lines(c(6,10),c(12,12),col="red")
lines(c(11,15),c(14,14),col="red")
text(3,15,"SSE")


##### Sum of squares calculation by hand #####

## Total sum of squares (SSY) ##

# example: distance of first point to overall mean
yield[1]
mean(yield)
yield[1]-mean(yield)

# distances from all points to overall mean
yield-mean(yield)

# square the distances
(yield-mean(yield))**2

# sum the squared distances (-> total sum of squares)
SSY <- sum((yield-mean(yield))**2)
SSY


## Error sum of squares (SSE) ##

# example: distances of points in the sand group to mean of sand group
soil_subset <- subset(dat, soil == "sand", select = yield)
mean(soil_subset$yield)
residuals_soil <- soil_subset$yield - mean(soil_subset$yield)
residuals_soil

# ANOVA calculates distances of all points to the mean of their group (= residuals)
residuals(model)

# square the residuals
residuals(model)**2

# sum the squared residuals (-> error sum of squares)
SSE <- sum(residuals(model)**2)
SSE


## Treatment sum of squares (SSA) ##

# Total sum of squares - error sum of squares
SSX <- SSY - SSE
SSX

# ANOVA leads to the same results
model<-aov(yield~soil)
summary(model)




################## END OF THEORY SECTION  ######################################


pf(4.245, df1 = 2, df2 = 27, lower.tail = FALSE)
