##  Assignment 3, problem 1
## Christopher Peters
install.packages("foreach")
install.packages("ggplot2")
library(foreach)
library(ggplot2)
library(xtable)

bleed.data <- read.csv("C:\\Users\\Chris\\School\\EXST_Reliability_and_Survival_Analysis\\JMPCourseReliabilityDataSets\\JMPCourseReliabilityDataSets\\BleedSystem.csv")

# Calculate the number that enter each time period
num.begin.units <- sum(bleed.data$Weight)

bleed.data$units.entered <- rep(num.begin.units, 60)

for(i in 1:59) {
  bleed.data[i+1, 5] <- (bleed.data[i, 5] - bleed.data[i, 3])
}


for(i in 1:60){
  if(bleed.data[ i, 2] == "Failed") 
       (bleed.data[ i, 6] <- 1) else
       (bleed.data[ i, 6] <- 0)
}

for(i in 1:60){
  bleed.data[i , 7] <-(bleed.data[i, 5] - bleed.data[i , 6]) / bleed.data[i, 5]
}

names(bleed.data)[names(bleed.data) == "V6"] <- "d"
names(bleed.data)[names(bleed.data) == "V7"] <- "pi"

bleed.data[1, 8] <- 1
foreach(i = 2:60) %do%
  (bleed.data[ i , 8] <- (bleed.data[ i, 7] * bleed.data[ i - 1, 8]))

names(bleed.data)[names(bleed.data) == "V8"] <- "KM"

ggplot(bleed.data, aes(Hours, KM)) + geom_point() + opts(aspect.ratio = 2/(1 + sqrt(5)) )

