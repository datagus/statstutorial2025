#Tutorial day 2
#if you type a hashtag you can make notes and the program recognises that this is not code

#where is R currently storing and loading your data from?
getwd()
"/Users/henrikvonwehrden"

#this is how you set your working directory on a Mac, 
#you need to adjust it to the location of the txt or csv file on YOUR computer!This basically tells R where to find the data:
setwd("/Users/henrikvonwehrden/Tutorial_Stats")
#with a Windows computer it looks slightly different:
setwd("C:\\")
#for Windows Rstudio:
setwd("C:/")



# load the survey data!
#for a csv file the command is slightly different
survey<-read.csv("survey25cleaned.csv",header=T, sep=",")
attach(survey)
?attach

#this returns the structure of your data
str(survey)

#inspect the data
#if you type head you can see the first few lines
head(survey)

#this will create a histogram of the distance data
hist(travel)

#this will create a histogram of the pinky finger data
hist(Pinky,col="pink")
barplot(table(season),ylab="anything",col=c("orange","yellow","gold","white"))
table(season)
?na.omit
survey2<-na.omit(survey)
str(survey2)

##############################################

#next load titanic data:
day2<-read.table("Titanic_R.txt",header=T)

#next attach data (after this you can refer to the data as "day2", this could have been any name)
attach(day2)

#if you just type the name you can see the table
day2
#if you type head you can see the first few lines
head(day2)
#this returns the structure of your data
str(day2)

#summarizing statistics
summary(day2)

#this will create a histogram of the age data
hist(age)

#if you want help for a certain command just type a "?" before the command, here the information under the heading "Details" could be interesting for you
?hist

boxplot(day2$age)

#for a barplot the data has to be in a matrix format, therefore the "table" command
table(class_level)

#drawing a barplot
barplot(table(class_level),ylab="counts",col=c("orange","yellow","gold"))

#if you want to look at the colour options in R:
colours()

#a table out of two variables
table(survived,class_level)

