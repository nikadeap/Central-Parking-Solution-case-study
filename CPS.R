#Question 3: Read the case "Central Parking Solutions", and answer the following questions.
#1.Explore the data for weekday and weekend. What inferences can you make based on descriptive statistics?
#Load CPS Weekday and Weekend data

#Set Directory
setwd('C:/Users/Harish/Desktop/BDA/Assignment')
# load library readxl to read xls file
library(readxl)

#Store Weekend data in CPS_Weekend dataset and weekday data in CPS_Weekday in dataset
CPS_Weekend <- read_excel("CPS-Data Set.xls", sheet = 'Weekend')
CPS_Weekday <- read_excel("CPS-Data Set.xls", sheet = 'Weekday')


#Rename the columns

colnames(CPS_Weekday) <-
  c(
    'Vehicle',
    'Equipment',
    'dateIn',
    'TimeIn',
    'DateOut',
    'TimeOut',
    'Amount',
    'TimeDiff',
    'TicketType',
    'Weekday'
  )
colnames(CPS_Weekend) <-
  c(
    'Vehicle',
    'Equipment',
    'dateIn',
    'TimeIn',
    'DateOut',
    'TimeOut',
    'Amount',
    'TimeDiff',
    'TicketType',
    'Weekday'
  )


CPS_Weekday <- data.frame(CPS_Weekday)
CPS_Weekend <- data.frame(CPS_Weekend)

#Remove Pass holder and Loss of Ticket records
weekDay <-
  subset(CPS_Weekday,
         CPS_Weekday$TicketType != 'Pass' & CPS_Weekday$Amount != 300)
weekEnd <-
  subset(CPS_Weekend,
         CPS_Weekend$TicketType != 'Pass' & CPS_Weekend$Amount != 300)

weekDay <- weekDay[, 1:10]
weekEnd <- weekEnd[, 1:10]

print("***************Question 3.1************************")

#Descriptive Analysis for CPS
#Weekday
meanAmount_weekday <- mean(weekDay$Amount)
cat("Average amount collected on weekday", meanAmount_weekday)
meanTimmDiff_weekday <- mean(weekDay$TimeDiff)
cat("Average length of stay during weekday is ", meanTimmDiff_weekday)

sdAmount_weekday <- sd(weekDay$Amount)
cat("standard deviation of amount on weekday is ", sdAmount_weekday)
sdTimeDiff_weekday <- sd(weekDay$TimeDiff)
cat("standard deviation of length of stay on weekday is ",
    sdTimeDiff_weekday)

print(summary(weekDay$Amount))
print(summary(weekDay$TimeDiff))

#Weekend

meanAmount_weekend <- mean(weekEnd$Amount)
cat("Aerage amount collected on weekend is ", meanAmount_weekend)
meanTimmDiff_weekend <- mean(weekEnd$TimeDiff)
cat("Average length of stay during weekend is ", meanTimmDiff_weekend)

sdAmount_weekend <- sd(weekEnd$Amount)
cat("Standard deviation in amount over weekend is ", sdAmount_weekend)
sdTimeDiff_weekend <- sd(weekEnd$TimeDiff)
cat("standard deviation in length of stay over weekend is ",
    sdTimeDiff_weekend)

print(summary(weekEnd$Amount))
print(summary(weekEnd$TimeDiff))

#Draw boxolots
par(mfrow = c(1, 2))
boxplot(
  weekDay$TimeDiff,
  axes = TRUE,
  col = "beige",
  pch = 20,
  cex = 0.7,
  outline = TRUE,
  horizontal = TRUE,
  main = "Length of stay-weekday"
)
boxplot(
  weekEnd$TimeDiff,
  axes = TRUE,
  col = "beige",
  pch = 20,
  cex = 0.7,
  outline = TRUE,
  horizontal = TRUE,
  main = "Length of stay-weekend"
)

#********************************************************************************************************************
print("***************Question 3.2************************")

#Q3.2 Test whether random variable length of stay for weekday and weekend follow normal distibution
set.seed(700)
weekday_var <-
  rnorm(100, mean = meanTimmDiff_weekday, sd = sdTimeDiff_weekday)
weekend_var <-
  rnorm(100, mean = meanTimmDiff_weekend, sd = sdTimeDiff_weekend)



#qqnorm for random variables of length of stay
par(mfrow = c(1, 2))
qqnorm(weekday_var, col = "red", main = "Length of stay-weekday")
qqline(weekday_var, col = "blue", lwd = 2)
print("random variable length of stay(weekday) follow normal distribution")
qqnorm(weekend_var, col = "blue", main = "Length of stay-weekend")
qqline(weekend_var, col = "red", lwd = 2)

#****************************************************************************************************************************************
print("***************Question 3.3************************")

#3.3 Between normal and Weibull distribution, which distribution should be used to reresent length of stay variable in weekend
library(MASS)

library(car)

#Find maximum likelihood estimates for shape and scale
print(fitdistr(weekEnd$TimeDiff, densfun = "weibull", lower = 0))

#Generate random variable length of stay for weilbull distribution
set.seed(700)
weilbull_var <- rweibull(100,shape=171,scale=1.92)
#Approximate shape=1.9 and scale=171
#draw a QQPlot for weibull distribution with shape=1.9 and scale=171

qqPlot(
  weilbull_var,
  distribution = "weibull",
  shape = 6,
  scale = 150,
  las = 1,
  pch = 19,
  main="Weilbull Distribution"
)

#Conclusion:The fit of normal distribution is better than that of weilbull distribution.
#Hence normal distribution should be used to represent length of stay during weekend.

#*************************************************************************************************
print("***************Question 3.4************************")

# 4) Hypothesis testing for length of stay duration for weekend is greater than that of weekday

result <- t.test(
  weekEnd$TimeDiff,
  weekDay$TimeDiff,
  var.equal = FALSE,
  alternative = "greater",
  conf.level = 0.95
)

print(result)

#******************************************************************************************************************
print("***************Question 3.5************************")

#Question 5:5 Determine if average length of stay varies between time periods

library(lubridate)

#Weekday

#Combine date and time to calculate arraival time as per categories
weekDay$TimeInHour <- as.numeric(hour(weekDay$TimeIn))
weekDay$dateIn <- as.Date(weekDay$dateIn, format = "%d-%m-%y")

weekDay$temp_inTime <- paste(weekDay$dateIn, weekDay$TimeInHour)

weekDay$temp_inTime <- ymd_h(weekDay$temp_inTime, tz = "EST")

#Slice the time in three categories namely, 10-2,2-6,6-10.
#In below code slicing is done in 24 hour format using cut function

weekDay$bin_weekDay <-
  cut(hour(weekDay$temp_inTime), c(10, 14, 18, 22), include.lowest = FALSE)
print("Average length of stay in three time periods for weekday")
print(tapply(weekDay$TimeDiff, weekDay$bin_weekDay, FUN = mean,na.rm =TRUE))

#Weekend
#Combine date and time to calculate arraival time as per categories
weekEnd$TimeInHour <- as.numeric(hour(weekEnd$TimeIn))
weekEnd$dateIn <- as.Date(weekEnd$dateIn, format = "%d-%m-%y")

weekEnd$temp_inTime <- paste(weekEnd$dateIn, weekEnd$TimeInHour)

weekEnd$temp_inTime <- ymd_h(weekEnd$temp_inTime, tz = "EST")

#Slice the time in three categories namely, 10-2,2-6,6-10.
#In below code slicing is done in 24 hour format using cut function

weekEnd$bin_weekEnd <-
  cut(hour(weekEnd$temp_inTime), c(10, 14, 18, 22), include.lowest = FALSE)
print("Average length of stay in three time periods for weekend")
print(tapply(weekEnd$TimeDiff, weekEnd$bin_weekEnd, FUN = mean,na.rm =TRUE))
print("Conclusion:Average length of stay varies between three slots for both weekday and weekend and it is maximum for morning slot for both weekday and weekend")
#**************************************************************************************************

print("***************Question 3.6************************")


#Question 6.Impact on weekday data if pricing model is changed
#Calculate the chnage in weekday data if pricing model changes in the new column new_Amount

weekDay$new_amount <- with(weekDay, ifelse((TimeDiff - 120) == 0, 20,
                                           ifelse((TimeDiff - 120) > 0, 20 + 10 * ((
                                             TimeDiff - 120
                                           ) / 60), 20)))

cat("Total amount collected before changing pricing model is ",sum(weekDay$Amount))
cat("Total amount collected after changing pricing model is ",sum(weekDay$new_amount))
print("Financial impact: Total Amount collected for weekday is reduced after changing pricing model")
#***********************************************************************************************************************
print("***************Question 3.7************************")
#Question3.7 Comparison of additional worker on Sunday and Monday.
maxEmp <- read.csv("CPS_additionalWorker.csv", header = TRUE)
# one additional employee after 30 vehicles
NumEmp <- 30
countMonday <- c()
countSunday <- c()

for (i in 1:length(maxEmp$TimeInterval)) {
  countMonday[i] <- ceiling(maxEmp$Monday[i] / NumEmp)
  countSunday[i] <-  ceiling(maxEmp$Sunday[i] / NumEmp)
}

maxMonday <- max(countMonday)
print(paste("Additional workers required on Monday is ", maxMonday))
maxSunday <- max(countSunday)
print(paste("Additional workers required on Sunday is ", maxSunday))

print(paste("Additional worker required on Sunday as compared to monday is ",
      maxSunday - maxMonday))

#************************************************************************************************************************
print("***************Question 3.8************************")

#Question 3.8

#Null Hypothesis:Average occupancy on weekday is utmost 2 hours
#Alternate Hypothesis: Average occupancy on weekDay is greater than 2 hours

#Lets u is average occupancy on weekday
#Ho:u<=2
#Ha:u>2
#Conduct one sample t-test
print("Hypothesis test for question 3.8")
print(t.test(weekDay$TimeDiff,mu=120,alternative = "greater"))
print("p-value is very small hence we can not retain null hypothesis")
print ("Satistically average occupancy on weekday is greater than 2 hours" )

