# Statistics Tutorial Day 10
###################################################

####----1. Working Directory----####

getwd()
setwd('/Users/carlo/Desktop/01_Projects/02_Statistics Tutorial SoSe 2025/data')

####----2. Data Inspection----####

# telling R which file to work with

ice <- read.csv("https://raw.githubusercontent.com/datagus/statstutorial2025/main/week10/Ice_cream.txt",
                header = TRUE, sep = "")

# The data was collected on 200 random citizens and are scores on various tests, including a video game and a puzzle. 
# The data also includes the person’s favorite flavor of ice cream – vanilla (1), chocolate (2) or strawberry (3).

str(ice)
names(ice)
summary(ice)

head(ice)
ice
View(ice)

attach(ice)

####----3. Ice Cream Graphs----####

# here are four commands you can use to create graphs
?plot

?boxplot

?hist

?barplot

# this gives you an overview of different PARameters that you can change in any graph
# font size of axis labels, margins, colours...
?par


# create a graph with this data set, please make it colorful and use a diversity of symbols!
# you have 10 min. 
# you can work in groups.


# here is a cool color palette
install.packages("wesanderson")
library(wesanderson)
?wes_palette

# now try to re-create the graphs you see up front

# first graph has purple triangles, main title, x axis and y axis label

# second graph has three different colors, a x axis an y axis label

# third graph has three different colors, main tittle, y axis label and a legend 


