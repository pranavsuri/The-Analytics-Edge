# 1 - LOADING THE DATA
mvt = read.csv("mvtWeek1.csv")

## 1.1 How many variables are in this dataset?
## 1.2 How many rows of data (observations) are in this dataset?
str(mvt)

## 1.3 Using the "max" function, what is the maximum value of the
## variable "ID"?
max(mvt$ID)

## 1.4 What is the minimum value of the variable "Beat"?
min(mvt$Beat)

## 1.5 How many observations have value TRUE in the Arrest variable
## (this is the number of crimes for which an arrest was made)?
summary(mvt)
sum(mvt$Arrest == TRUE)

## 1.6 How many observations have a LocationDescription value of ALLEY?
sum(mvt$LocationDescription == "ALLEY")

# 2 - UNDERSTANDING DATES IN R

## 2.1 In what format are the entries in the variable Date?
mvt$Date[1]

## 2.2 What is the month and year of the median date in our dataset?
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

## 2.3 In which month did the fewest motor vehicle thefts occur?
which.min(table(mvt$Month))
sort(table(mvt$Month))

## 2.4 On which weekday did the most motor vehicle thefts occur?
which.max(table(mvt$Weekday))
table(mvt$weekday)

## 2.5 Which month has the largest number of motor vehicle thefts
## for which an arrest was made?
table(mvt$Arrest, mvt$Month)

# 3 - VISUALIZING CRIME TRENDS
hist(mvt$Date, breaks=100)

## In general, does it look like crime increases or decreases from 2002 - 2012?
## In general, does it look like crime increases or decreases from 2005 - 2008?
## In general, does it look like crime increases or decreases from 2009 - 2011?

## 3.2 Does it look like there were more crimes for which arrests were made
## in the first half of the time period or the second half of the time period?
## Note that the time period is from 2001 to 2012, so the middle of the
## time period is the beginning of 2007.
boxplot(mvt$Date~mvt$Arrest)

## 3.3 For what proportion of motor vehicle thefts in 2001 was an arrest made?
mvt$Year = format(DateConvert,"%Y")
table(mvt$Arrest, mvt$Year)

## 3.4 For what proportion of motor vehicle thefts in 2007 was an arrest made?
## 3.5 For what proportion of motor vehicle thefts in 2012 was an arrest made?

# 4 - POPULAR LOCATIONS

## 4.1 Which locations are the top five locations for motor vehicle thefts,
## excluding the "Other" category?
sort(table(mvt$LocationDescription), decreasing = TRUE)[1:6]

## 4.2 How many observations are in Top5?
Top5 = subset(mvt, LocationDescription=="STREET" |
                LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" |
                LocationDescription=="ALLEY" |
                LocationDescription=="GAS STATION" |
                LocationDescription=="DRIVEWAY - RESIDENTIAL")
str(Top5)

## 4.3 One of the locations has a much higher arrest rate than the other
## locations. Which is it?
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$LocationDescription, Top5$Arrest)

## 4.4 On which day of the week do the most motor vehicle thefts at gas
## stations happen?
table(Top5$LocationDescription, Top5$Weekday)

# 4.5 On which day of the week do the fewest motor vehicle thefts in
## residential driveways happen?
table(Top5$LocationDescription, Top5$Weekday)
