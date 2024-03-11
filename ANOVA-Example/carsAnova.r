## Chose a data set containing data on cars
# 	Running an ANOVA to determine if the type of the car body affects the dimensions of the car

# Looking at car data set from Kaggle; running an ANOVA on length of car based on the body type of the car (SUV, coupe, etc.)
library(ggplot2)
library(reshape2)

carsData <- read.csv("cars-dataset.csv")
carsSizeData <- carsData[c('Body.style','Length','Width','Height')]
carsSizeData <- carsSizeData[!apply(carsSizeData == "", 1, all),]

lengthSplit <- colsplit(string=carsSizeData$Length, pattern=" ", names=c("LengthInches","LengthExtra"))
# suppress warnings about NA values that will be cleaned out later
carsSizeData$LengthNumeric <- suppressWarnings(as.numeric(lengthSplit$LengthInches))

widthSplit <- colsplit(string=carsSizeData$Width, pattern=" ", names=c("WidthInches","WidthExtra"))
# suppress warnings about NA values that will be cleaned out later
carsSizeData$WidthNumeric <- suppressWarnings(as.numeric(widthSplit$WidthInches))

heightSplit <- colsplit(string=carsSizeData$Height, pattern=" ", names=c("HeightInches","HeightExtra"))
# suppress warnings about NA values that will be cleaned out later
carsSizeData$HeightNumeric <- suppressWarnings(as.numeric(heightSplit$HeightInches))

# clean out NA values
carsSizeData <- carsSizeData[complete.cases(carsSizeData), ]

# filtering out physically unrealistic values from data set
# assume any length value less than 5 feet or greater than 25 feet is a data entry error
carsSizeData <- carsSizeData[(carsSizeData$LengthNumeric > 60 & carsSizeData$LengthNumeric < 300),]
# assume any width value less than 3 feet or greater than 10 feet is a data entry error
carsSizeData <- carsSizeData[(carsSizeData$WidthNumeric > 36 & carsSizeData$WidthNumeric < 120),]
# assume any height value less than 3 feet or greater than 12 feet is a data entry error
carsSizeData <- carsSizeData[(carsSizeData$HeightNumeric > 36 & carsSizeData$HeightNumeric < 144),]
print(summary(carsSizeData))

print("==========================")

res_aov_l <- aov(LengthNumeric ~ Body.style, data=carsSizeData)
res_aov_welch_l <- oneway.test(LengthNumeric ~ Body.style, data=carsSizeData, var.equal=FALSE)

print("")
print("ANOVA: Length of cars")
print(res_aov_l)
print("")
print("==========================")
print("")
print("Welch ANOVA: Length of cars")
print(res_aov_welch_l)
print("")

print("==========================")

res_aov_w <- aov(WidthNumeric ~ Body.style, data=carsSizeData)
res_aov_welch_w <- oneway.test(WidthNumeric ~ Body.style, data=carsSizeData, var.equal=FALSE)

print("\n")
print("ANOVA: Width of cars")
print(res_aov_w)
print("\n")
print("==========================")
print("\n")
print("Welch ANOVA: Width of cars")
print(res_aov_welch_w)
print("\n")

print("==========================")

res_aov_h <- aov(HeightNumeric ~ Body.style, data=carsSizeData)
res_aov_welch_h <- oneway.test(HeightNumeric ~ Body.style, data=carsSizeData, var.equal=FALSE)

print("")
print("ANOVA: Height of cars")
print(res_aov_h)
print("")
print("==========================")
print("")
print("Welch ANOVA: Height of cars")
print(res_aov_welch_h)
print("")

print("==========================")