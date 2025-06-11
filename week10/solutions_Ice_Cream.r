# Statistics Tutorial Day 10
###################################################

####----1. Working Directory----####

getwd()
setwd('/Users/carlo/Desktop/01_Projects/02_Statistics Tutorial SoSe 2025/data')

####----2. Data Inspection----####

# telling R which file to work with

ice<-read.table("Ice_cream.txt",header=T)

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


####----SOLUTIONS for Tutors only!----####

# 1st graph
plot(puzzle,video,
     ylab="video score",
     xlab ="puzzle score",
     main="Video vs. puzzle score",
     pch=17,
     col="purple")

# 2nd graph
boxplot(video~ice_cream, 
        xlab="ice cream flavor",
        ylab="video score",
        col=c("white","chocolate","pink"),
        boxwex=0.5)

# 3rd graph
pal<-wes_palette("Moonrise3",3,type="discrete")
barplot(table(puzzle_type,ice_cream),
        beside=T,
        main="Puzzle type meets ice cream flavour",
        ylab="number of people",
        names=c("vanilla", "chocolate", "strawberry"),
        col=pal,
        legend.text=TRUE)

####----Extra----####

# showing ice cream preferences with one "layer" for each datapoint
barplot(table(id,ice_cream))

# sorting puzzle types! was asked many times
# this can be in 2 steps or 1 step

puzzle_type <- factor(puzzle_type, levels=c("rookie", "average", "pro"))
boxplot(puzzle~puzzle_type)

# or
boxplot(puzzle~as.factor(factor(puzzle_type,levels=c("rookie","average","pro"))))

# here is a way of combining chocolate and strawberry

table(ice_cream)
vanilla<-(ice_cream[ice_cream==1])
not_vanilla<-(ice_cream[ice_cream>1])

####----AirBnB Graphs----####


# install ggplot package. Installation only necessary once
install.packages("ggplot2")
# load library. loading a library is always necessary when you want to use the code
library(ggplot2)

air<-read.csv("airbnb.csv",header=T, sep=",")
attach(air)
str(air)

# for each city distances to nearest metro station and city center, each listing colorized by its price
pal2<-wes_palette("GrandBudapest1",3,type="discrete")
pal2
ggplot(air, aes(x = distance, y = metro_dist, color = price)) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(low = pal2[1], mid = pal2[2],high = pal2[3]) +
  labs(title = "AirBnB prices in 10 European cities",
       subtitle = "price listings with distance to city and nearest metro",
       x = "Distance to city center (km)",
       y = "Distance to nearest metro (km)",
       color = "Price (EUR)") +
  facet_wrap(~ city) +
  theme_minimal() +
  theme(legend.position = "bottom")

# boxplots for prices by room type and superhost status 
ggplot(air, aes(x = room_type, y = price, fill = factor(superhost))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "orange"), 
                    labels = c("0" = "Host", "1" = "Superhost")) +
  labs(title = "AirBnB prices by room type and superhost status",
       x = "room type",
       y = "price (EUR)",
       fill = "superhost status") +
  theme_minimal() +
  theme(legend.position = "bottom")



