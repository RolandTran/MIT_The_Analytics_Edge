# extra problem Class lec 1.3.6 (YouTube)
# https://www.youtube.com/watch?v=MvERdFp8mvI&list=PLUl4u3cNGP61Q_FSXJUGkDJs1SMj5teGq&index=10
# modify data structure

setwd("D:/Coding Exercises/R/The Analytics Edge/Assignment 1/W1 Lectures")
getwd()
w1dir <- getwd()



Country = c("Brazil","China","India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)
Country
LifeExpectancy
CountryData =data.frame(Country,LifeExpectancy)
CountryData
CountryData$Population = c(199000, 1390000, 1240000, 7997, 318000)
CountryData
Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country, LifeExpectancy, Population)
NewCountryData
AllCountryData = rbind(CountryData, NewCountryData)
AllCountryData

# 1.3.8 lecture
WHO <- read.csv("WHO.csv")
str(WHO)
head(WHO)
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)
head(WHO)
write.csv(WHO_Europe, "WHO_Europe.csv")
ls()
rm(WHO_Europe)
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO, GNI>10000 & FertilityRate>2.5)
nrow(Outliers)
Outliers[c("Country","GNI","FertilityRate")]
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region)
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="Life Expectancy")
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="Life Expectancy", main="Life Expectancy of Countries by Region")
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$Over60, WHO$Region, min)
tapply(WHO$LiteracyRate, WHO$Region, min)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE )

#1.4.3 
USDA = read.csv("USDA.csv")
head(USDA)
str(USDA)
summary(USDA)
which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]
HighSodium = subset(USDA, Sodium>10000)
nrow(HighSodium)
HighSodium$Description
match("CAVIAR", USDA$Description)
USDA$Sodium[4154]
USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium)
plot(USDA$Protein, USDA$TotalFat)
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab = "fat", main = "Protein vs Fat", col = "red")
hist(USDA$VitaminC, xlab ="Vitamin C (mg)", main ="Histogram of Vitamin C Levels")
hist(USDA$VitaminC, xlab ="Vitamin C (mg)", main ="Histogram o(0,100f Vitamin C Levels", xlim=c(0,100))
hist(USDA$VitaminC, xlab ="Vitamin C (mg)", main ="Histogram o(0,100f Vitamin C Levels", xlim=c(0,100), breaks = 100)
hist(USDA$VitaminC, xlab ="Vitamin C (mg)", main ="Histogram o(0,100f Vitamin C Levels", xlim=c(0,100), breaks = 2000)
boxplot(USDA$Sugar, main = "Boxplot of Sugar Levels", ylab = "Sugar(g)")
USDA$Sodium[1] > mean(USDA$Sodium, na.rm=TRUE)
USDA$Sodium[50] > mean(USDA$Sodium, na.rm=TRUE)
HighSodium = USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE)
str(HighSodium)
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
str(HighSodium)
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
USDA$HighProtein = as.numeric(USDA$Sodium> mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
str(USDA)
table(USDA$HighSodium)
# average amt of iron group by high protein
tapply(USDA$Iron, USDA$HighProtein, mean, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, mean, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, max, na.rm=TRUE)
tapply(USDA$VitaminC, USDA$HighCarbs, summary, na.rm=TRUE)






