# 1 - SUMMARY STATISTICS

IBM = read.csv("IBMStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
GE = read.csv("GEStock.csv")
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

## 1.1 How many observations are there in each dataset
## (all have the same number of observations)?
nrow(IBM)

## 1.2 What is the earliest year in our datasets?
min(IBM$Date)

## 1.3 What is the latest year in our datasets?
max(IBM$Date)

## 1.4 What is the mean stock price of IBM over this time period?
mean(IBM$StockPrice)

## 1.5 What is the minimum stock price of General Electric over this
## time period?
min(GE$StockPrice)

## 1.6 What is the maximum stock price of Coca-Cola over this time period?
max(CocaCola$StockPrice)

## 1.7 What is the median stock price of Boeing over this time period?
median(Boeing$StockPrice)

## 1.8 What is the standard deviation of the stock price of Procter & Gamble
## over this time period?
sd(ProcterGamble$StockPrice)

# 2 - VISUALIZING STOCK DYNAMICS

## 2.1 Around what year did Coca-Cola has its highest stock price
## in this time period?
plot(CocaCola$Date, CocaCola$StockPrice, type="l")

## 2.2 Around what year did Coca-Cola has its lowest stock price
## in this time period?
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

## 2.2 Around 1983, the stock for one of these companies (Coca-Cola or
## Procter and Gamble) was going up, while the other was going down. Which one
## was going up?
plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("1983-03-01")), lwd=2)

# 3 - VISUALIZING STOCK DYNAMICS 1995-2005

## 3.1 Which stock fell the most right after the technology bubble burst
## in March 2000?
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432],
     type="l", col="red", ylim=c(0,210))

lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")

abline(v=as.Date("2000-03-01"), lwd=2)

## 3.2 Which stock reaches the highest value in the time period 1995-2005?

## 3.3 In October of 1997, there was a global stock market crash that was
## caused by an economic crisis in Asia. Comparing September 1997 to
## November 1997, which companies saw a decreasing trend in their stock price?
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

## 3.4 In the last two years of this time period (2004 and 2005) which stock
## seems to be performing the best, in terms of increasing stock price?
abline(v=as.Date("2004-01-01"))
abline(v=as.Date("2005-01-01"))

# 4 - MONTHLY TRENDS

## 4.1 For IBM, compare the monthly averages to the overall average stock
## price. In which months has IBM historically had a higher stock price
## (on average)?
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)

## 4.2 General Electric and Coca-Cola both have their highest average stock
## price in the same month. Which month is this?
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)

## 4.3 For the months of December and January, every company's average stock
## is higher in one month and lower in the other. In which month are the stock
## prices lower?
tapply(IBM$StockPrice, months(IBM$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)
