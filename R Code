## Install and load the necessary packages for the analysis

install.packages('knitr')
install.packages('easystats')
install.packages('dplyr')
install.packages('tidyverse')
install.packages('MASS')
install.packages("rmarkdown",type="win.binary")

library(knitr)
library(easystats)
library(dplyr)
library(tidyverse)
library(MASS)
library(rmarkdown)


## load data with independent variables

data <- read.csv("fundamentals.csv")
data <- data %>% rename("date" = "Period.Ending", "symbol" = "Ticker.Symbol")
View(data)


## load data with response variable

data2 <- read.csv("prices.csv")
View(data2)


## merge data for analysis

stocks <- merge(data, data2, by = c("date", "symbol"))
View(filter(data2, symbol == "AAL"))


## exploratory data analysis

head(stocks)
summary(stocks$close)


## develop linear regression models

index <- sample(nrow(stocks), nrow(stocks) * 0.90)
stocks_train = stocks[index,] 
stocks_test = stocks[-index,]

stocks_close <- glm(close ~ Accounts.Payable + Accounts.Receivable + 
                     Capital.Expenditures + Capital.Surplus + 
                     Cash.and.Cash.Equivalents + Cost.of.Revenue + Current.Ratio + 
                     Depreciation + Earnings.Before.Interest.and.Tax + 
                     Earnings.Before.Tax + Fixed.Assets + Goodwill + Gross.Profit + 
                     Intangible.Assets + Interest.Expense + Inventory + 
                     Investments + Liabilities + Long.Term.Debt + 
                     Long.Term.Investments + Net.Borrowings + Net.Cash.Flow + 
                     Net.Income + Net.Receivables + Operating.Income + 
                     Research.and.Development + Retained.Earnings + 
                     Sale.and.Purchase.of.Stock + Total.Assets + 
                     Total.Current.Assets + Total.Current.Liabilities + 
                     Total.Equity + Total.Liabilities + Total.Revenue + 
                     Earnings.Per.Share + Estimated.Shares.Outstanding, 
                   family = gaussian, data=stocks_train)

stocks_market_cap <- glm(close*Estimated.Shares.Outstanding ~ Accounts.Payable + Accounts.Receivable + 
                     Capital.Expenditures + Capital.Surplus + 
                     Cash.and.Cash.Equivalents + Cost.of.Revenue + Current.Ratio + 
                     Depreciation + Earnings.Before.Interest.and.Tax + 
                     Earnings.Before.Tax + Fixed.Assets + Goodwill + Gross.Profit + 
                     Intangible.Assets + Interest.Expense + Inventory + 
                     Investments + Liabilities + Long.Term.Debt + 
                     Long.Term.Investments + Net.Borrowings + Net.Cash.Flow + 
                     Net.Income + Net.Receivables + Operating.Income + 
                     Research.and.Development + Retained.Earnings + 
                     Sale.and.Purchase.of.Stock + Total.Assets + 
                     Total.Current.Assets + Total.Current.Liabilities + 
                     Total.Equity + Total.Liabilities + Total.Revenue + 
                     Earnings.Per.Share + Estimated.Shares.Outstanding, 
                   family = gaussian, data=stocks_train)


## check the accuracy of the models

r2(stocks_close)
r2(stocks_market_cap)
summary(stocks_glm01)


## plotting predicted closing stock price based on lm

hist(predict(stocks_glm0))
plot(predict(stocks_glm0), ylim = c(0, 600))


## creating a better model

stocks_glm <- glm(close ~ Accounts.Payable + Current.Ratio + 
                     Depreciation + Fixed.Assets + Goodwill + Gross.Profit + 
                     Intangible.Assets + Inventory + Liabilities + Long.Term.Debt + 
                     Net.Income + Net.Receivables + Retained.Earnings + 
                     Earnings.Per.Share + Estimated.Shares.Outstanding, 
                   family = gaussian, data=stocks_train)

summary(stocks_glm)


## analyzing predicted closing stock price based on new model / testing new model

pred_stocks <- predict(stocks_glm, stocks_test)
pred_stocks <- (pred_stocks)
View(pred_stocks)
mse(stocks_glm)   #4669.338
r2(stocks_glm)    #0.52


## visualizing the testing data to check for accuracy

hist(pred_stocks)
plot(pred_stocks, ylim = c(0, 600))
plot(stocks$close)
nrow(stocks_test)
length(pred_stocks)

View(pred_stocks)

