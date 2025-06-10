### Airbnb Data Analysis Script for Tutors Only ###
# DO NOT DISTRIBUTE TO STUDENTS

# ===============================
# Load necessary packages
# ===============================
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggthemes)
library(car)

# Optional: Wes Anderson-inspired color palette
wes_palette <- c("#FFD700", "#FF6F61", "#6B5B95", "#88B04B", "#92A8D1", "#955251")

# set working directory
setwd()

# ===============================
# Load and inspect the dataset
# ===============================
air <- read.csv("airbnb.csv", header = TRUE, sep = ",")
str(air)

# ===============================
# 1. Price comparison: Rome, London, Paris
# Do Rome, London, and Paris have significant price differences in the Airbnb listings?
# ===============================

# What are the relevant variables from air data set?

# Filter relevant cities
rlp<- subset(air, city %in% c("Rome","London","Paris"))

# Alternative for filter
rlp <- air %>% filter(city %in% c("Rome", "London", "Paris"))


# Investigate dataset
attach(rlp)
str(rlp) # What data formats do they have? 
table(city) #balanced design?

# What kind of test can I perform with these data formats to answer the question?

# Checking distributions
hist(price) #normally distributed?
hist(log(price))

# ANOVA: Price ~ City
rlp$city <- as.factor(rlp$city)
rlp_model <- aov(log(price) ~ city, data = rlp) 
summary(rlp_model) # Does city has an effect on price?
TukeyHSD(rlp_model) # Is there a difference between cities?
hist(resid(rlp_model))

# Type 3 Anova for unbalanced design
library(car)
rlp_model_type3 <- Anova(lm(log(price) ~ city * room_type, data = bb), type = "III")
rlp_model_type3

# Simple: Visualizing price differences across cities
boxplot(log(price)~city) # Do you recognize a difference between cities based on boxplots?

# Advanced: Visualize log-transformed price by city
ggplot(rlp, aes(x = city, y = log(price), fill = city)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette[1:3]) +
  theme_minimal() +
  labs(title = "Price Comparison Across Cities",
       y = "Log(Price)", x = "City")

detach(rlp)

# ===============================
# 2. Room types in Berlin and Budapest
# How many shared, private and entire homes are in Berlin and Budapest? Do Airbn prices depend on room category in Berlin and Budapest? Is the effect different in different cities?
# ===============================

# What are the relevant variables from air data set?

# Two options to filter relevant cities
bb <- air %>% filter(city %in% c("Berlin", "Budapest"))
bb<- subset(air, city %in% c("Berlin","Budapest"))
attach(bb)

# Investigate data
str(bb) # What data formats do the relevant variables have?

# What kind of test can I perform with these data formats to answer the question?

# Count of listings by room type
table(city, room_type)
# How many shared, private and entire homes are in Berlin and Budapest?

# ANOVA: Price ~ City * Room Type
table(city, room_type) # Balanced design?
hist(price) # Normally distributed?
hist(log(price))
bb$city <- as.factor(bb$city)
bb$room_type <- as.factor(bb$room_type)

# Type 3 Anova for unbalanced design
library(car)
bb_model_type3 <- Anova(lm(log(price) ~ city * room_type, data = bb), type = "III")
bb_model_type3

# Do Airbn prices depend on room category in Berlin and Budapest?
TukeyHSD(bb_model)
# Is the effect different in the two different cities?
hist(resid(bb_model))

# Visual
ggplot(bb, aes(x = room_type, y = log(price), fill = city)) +
  geom_boxplot() +
  scale_fill_manual(values = wes_palette[4:5]) +
  theme_minimal() +
  labs(title = "Room Type & City Impact on Airbnb Prices",
       x = "Room Type", y = "Log(Price)")

detach(bb)
# ===============================
# 3. Distance from City Centre
# Which city has the most listings furthest away from the city centre? Does the price depend on distance to the city centre or metro distance?
# ===============================
attach(air)
# What are the relevant variables from air data set?

# Investigate data
str(air) # What data formats do the relevant variables have?

# What kind of test can I perform with these data formats to answer the question?

# Boxplot of distance to city center by city
boxplot(distance~city, las = 2)
# Which city has the most listings furthest away from the city centre?

# Reorder city factor by **maximum** distance
air$city <- with(air, reorder(city, distance, FUN = max))

# Create boxplot
boxplot(distance ~ city, data = air,
        las = 2, col = "lightblue",
        main = "Cities Ordered by Maximum Distance",
        ylab = "Distance (km)")

# distributions
hist(price) #normally distributed?
hist(log(price))

hist(distance) #normally distributed?
hist(log(distance))

hist(metro_dist) #normally distributed?
hist(log(metro_dist+1))

## Simple Regression with each independent variable
# Price and distance to city center with regression
dist_model1 <- lm(log(price) ~ log(distance), data = air)
summary(dist_model1) # Does the price depend on distance to the city centre?
plot(log(distance),log(price))
abline(dist_model1, col="red")
hist(resid(dist_model1))

# Price and distance to metro station with regression
dist_model2 <- lm(log(price) ~ log(metro_dist+1), data = air)
summary(dist_model2) # Does the price depend on distance to the metro?
plot(log(metro_dist+1),log(price))
abline(dist_model2, col="red")
hist(resid(dist_model2))

## Regression with both independent variables
# Price and both distance types
model_price <- lm(log(price) ~ log(distance) + log(metro_dist+1), data = air)
summary(model_price) # Does the price depend on distance to the city centre or metro distance?
hist(resid(model_price))
detach(air)

# ===============================
# 4. Bedrooms vs City 
# Are cities of AirBnb listings independent from number of bedrooms?
# Does this relationship change when only including up to 3 bedrooms?
# ===============================

attach(air)

# What are the relevant variables from air data set?

# Investigate data
str(air) # What data formats do the relevant variables have?

# What kind of test can I perform with these data formats to answer the question?

# Chi-square test across cities
table(city, bedrooms)
bedroom_all <- table(city, bedrooms)
chisq.test(bedroom_all)
# Are cities of AirBnb listings independent from number of bedrooms?

# Chi-Square test for only 3 bedrooms across cities
bedroom_0to3 <- bedroom_all[, colnames(bedroom_all) %in% c("0", "1", "2", "3")]
chisq.test(bedroom_0to3)
# Does this relationship change when only including up to 3 bedrooms?

#Alternative Code:
city_03<-city[bedrooms<4]
table(city_03)
bed_03<-bedrooms[bedrooms<4]
table(bed_03)
chisq.test(table(city_03,bed_03))

detach(air)

# ===============================
# 5. Superhost Impact
# Which city has the most, which has the least super hosts?
# Does the price of the Airbnb listing depend on the host status?
# ===============================

attach(air)

# What are the relevant variables from air data set?

# Investigate data
str(air) # What data formats do the relevant variables have?

# What kind of test can I perform with these data formats to answer the question?

# Superhost count per city
table(superhost, city)
# Which city has the most, which has the least super hosts?

# Superhost impact on price
hist(price) # normally distributed?
hist(log(price))

new_superhost<-as.factor(superhost)
host_model <- aov(log(price) ~ new_superhost, data = air)
summary(host_model) # Does the price of the Airbnb listing depend on the host status?
TukeyHSD(host_model)
hist(resid(host_model))

# Visualizing the price by host status
boxplot(log(price)~superhost)

# Is there another test apart from an ANOVA that you could have performed?

detach(air)

# ===============================
# 6. Cleanliness vs Superhost
# Which city has the most, which has the least super hosts?
# Are there significant more clean apartments that are also super host apartments?
# ===============================

attach(air)

# What are the relevant variables from air data set?

# Investigate data
str(air) # What data formats do the relevant variables have?

# What kind of test can I perform with these data formats to answer the question?

# Superhost count per city
table(superhost, city)
# Which city has the most, which has the least super hosts?

# Association of cleanliness and host status
table(superhost, cleanliness)
chisq.test(table(superhost, cleanliness))
chisq.test(table(superhost, cleanliness))$expected

# Are there significantly more clean apartments that are also super host apartments?

# ===============================
# 7. Distance to Metro vs Distance to Centre
# Do Airbnb listings further away from the city centre have shorter distances to metro stations?
# ===============================
attach(air)

# What are the relevant variables from air data set?

# Investigate data
str(air) # What data formats do the relevant variables have?

# What kind of test can I perform with these data formats to answer the question?

# Checking distributions
hist(distance) # normally distributed?
hist(metro_dist) # normally distributed?

# Performing correlations
cor.test(air$distance, air$metro_dist, method = "spearman") # Significant relationship? 
cor.test(air$distance, air$metro_dist, method = "kendall") # What kind of relationship?

# Do Airbnb listings further away from the city centre have shorter distances to metro stations?

# Visualizing the relationship
ggplot(air, aes(x = distance, y = metro_dist)) +
  geom_point(alpha = 0.4, color = "#88B04B") +
  geom_smooth(method = "lm", color = "#955251") +
  theme_minimal() +
  labs(title = "Distance to Metro vs Distance to City Centre",
       x = "Distance to City Centre", y = "Distance to Metro")

# ===============================
# BONUS: Gentrification
# Can you tell based on this data if cities are gentrified? 

# In which city is a high dispersion of AirBnB's  throughout the whole city?
# In which cities are the listing more concentrated on the city center?
# ===============================

attach(air)

# What are the relevant variables from air data set?

# Investigate data
str(air) # What data formats do the relevant variables have?

# How can I approach the question based on this information?

# Plot number of listings vs IQR of distance
boxplot(distance~city)
iqr_values <- tapply(distance, city, IQR) # IQR interquartile range
city_counts <- table(city)

plot(iqr_values, as.numeric(city_counts), type = "n",
     xlab = "IQR of Distance", ylab = "Number of Listings",
     main = "City Spread vs Listings")
text(iqr_values, city_counts, labels = names(iqr_values), col = "darkblue")

# Final distance boxplot
boxplot(distance ~ city, data = air, las = 2, col = wes_palette,
        main = "Distance to City Centre by City", ylab = "Distance (km)")

# we look at the dispersion of distance to city center and number of listings. 
# Berlin and London have high dispersion = listings are scattered across the city 
# -> regardless the number of listings, apartments are not centered. 
# Athens and Budapest have low dispersion/IQR of distance
# -> Apartments are in the city center and displace residents.

detach(air)

# ===============================
# Just for Fun: Create map
# ===============================
attach(air)
plot(longitude,latitude)

ggplot(air, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.3, color = "#FF6F61") +
  theme_void() +
  labs(title = "Map of Airbnb Listings")

detach(air)
