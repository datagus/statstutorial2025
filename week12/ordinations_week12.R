#install.packages("vegan")
#install.packages("cluster")

library(vegan)
library(cluster)

#----Detrended Correspondance Analysis----
#Load  classic data from Jongman
data(dune);data(dune.env)

?dune

#inspect the data
str(dune)
?dune
View(dune)

#Make a DCA of the dataset
modeldca<-decorana(dune)
#How much does the model explain
modeldca
#How does it look?
plot(modeldca)

#How to interpret the plot?
#Species that are close to each other tend to occur in similar samples.
#Samples that are close together have similar species compositions.

#choose two species distant to each other and find a description/ picture
#Plot only the sites
plot(modeldca,display=c("sites"), col="purple", cex=0.5)


#Make a cluster analysis
?agnes
modelclust<-agnes(dune,method="ward")
plot(modelclust)
# Compare dendrogram with the DCA
par(mfrow=c(1,2))
dune_cluster <- as.hclust(modelclust)
plot (dune_cluster)
plot(modeldca,display=c("sites"), col="purple", cex=0.5)
par(mfrow=c(1,1))

###----Principal Component Analysis ----
#Here is a PCA of the swiss data
data(swiss)
#inspect data....

modelpca<-prcomp(swiss, scale=T)
summary(modelpca)
plot(modelpca)
par(xpd=T)
biplot(modelpca) #What can you say about this plot? 
#Why some villages are closer together and why others are farther away?



#---------Homework: Interpret the following ordinations ------

#Now a DCA with environmental data fitted
data(varechem);data(varespec)

#inspect data

modeldca2<-decorana(varespec)
plot(modeldca2)

modelfit<-envfit(modeldca2,varechem,perm=1000)
plot(modelfit)

#example with mtcars
data(mtcars)
modelpca2<-prcomp(mtcars,scale=T)
biplot(modelpca2)