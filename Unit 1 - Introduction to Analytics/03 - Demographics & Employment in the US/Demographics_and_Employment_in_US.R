# 1 - SUMMARIZING THE DATASET

## 1.1 How many interviewees are there in the dataset?
nrow(CPS)

## 1.2 Among the interviewees with a value reported for the Industry variable,
## what is the most common industry of employment?
which.max(table(CPS$Industry))

## 1.3 Which state has the fewest interviewees?
which.min(table(CPS$State))
## 1.3 Which state has the largest number of interviewees?
which.max(table(CPS$State))

## 1.4 What proportion of interviewees are citizens of the United States?
table(CPS$Citizenship)

## 1.5 For which races are there at least 250 interviewws are of Hipsanic
## enthicity?
table(CPS$Race, CPS$Hipsanic)

# 2 - EVALUATING MISSING VALUES

## 2.1 Which variables have at least one intervieww with a missing (NA) value?
summary(CPS)

## 2.2 Which is the most accurate in terms of missing values?
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

## 2.3 How many states had all interviewees living in a non-metropolitan area?
## (aka they have a missing MetroAreaCode value)? For this question, treat the
## District of Columbia as a state (even though it is not technically a state)
table(CPS$State, is.na(CPS$MetroAreaCode))
sum(table(CPS$State, is.na(CPS$MetroAreaCode))[,'FALSE'] == 0)

## 2.3 How many states had all interviewees living in a metropolitan area?
sum(table(CPS$State, is.na(CPS$MetroAreaCode))[,'TRUE'] == 0)

## 2.4 Which region of the United States has the largest proportion of
## interviewees living in a non-metropolitan area?
table(CPS$Region, is.na(CPS$MetroAreaCode))

## 2.5 Which state has a proportion of interviewees living in a
## non-metropolitan area closest to 30%?
## 2.5 Which state has the largest proportion of non-metropolitan interviewees,
## ignoring states where all interviewees were non-metropolitan?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

# 3 - INTEGRATING METROPOLITAN AREA DATA
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryOfBirthCode <- read.csv("CountryCodes.csv")

## 3.1 What are the number of observations in both the data frames?
nrow(MetroAreaMap)
nrow(CountryMap)

## 3.2 Updating the CPS Data Frame.
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

## 3.2 What is the name of the variable that was added to the data frame by
## the merge() operation?
summary(CPS)
str(CPS)

## 3.2 How many interviewees have a missing value for the new metropolitan
## area variable?
table(is.na(CPS$MetroArea))['TRUE']

## 3.3 Which metropolitan area has the largest number of interviewees?
sort(table(CPS$MetroArea), decreasing = TRUE)[1:8]

## 3.4 Which metro-area has the highest proportion of interviewws of
## Hispanic ethnicity?
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean), decreasing = TRUE)[1:3]

## 3.5 Determine the number of metro areas in the US from which at least
## 20% interviewees are Asian.
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean), decreasing = TRUE)[1:6]

## 3.6 Which metropolitan area has the smallest proportion of interviewees who
## have received no high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,
            na.rm=T))

# 4 - INTEGRATING COUNTRY OF BIRTH DATA
CPS <- merge(CPS, CountryOfBirthCode,
              by.x="CountryOfBirthCode", by.y="Code",
              all.x=TRUE)

## 4.1 Answer the following questions on the new updated Data Frame.
## 4.1 What is the name of the variable added to the CPS data frame by this
## merge operation?
str(CPS)

## 4.1 How many interviewees have a missing value for the new country of birth
## variable?
summary(CPS)

## 4.2 Among all interviewees born outside of North America, which country was
## the most common place of birth?
sort(table(CPS$Country), decreasing=TRUE)[1:3]

## 4.3 What proportion of interviewees from the NY-NJ-PA metropolitan area have
## a country of birth that is not US? For this computation, don't include
## people with missing data.
table((CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",
        CPS$Country != "United States"))

## 4.4 Which metropolitan area has the largest number of interviewees with a
## country of birth ____?

### in India?
sort(tapply(CPS$Country=="India", CPS$MetroArea, sum, na.rm=T),
      decreasing = TRUE)[1]

### in Brazil?
sort(tapply(CPS$Country=="Brazil", CPS$MetroArea, sum, na.rm=T),
      decreasing = TRUE)[1]

### in Somalia?
sort(tapply(CPS$Country=="Somalia", CPS$MetroArea, sum, na.rm=T),
      decreasing = TRUE)[1]
