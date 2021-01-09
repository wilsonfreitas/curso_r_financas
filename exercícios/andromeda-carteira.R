
suppressPackageStartupMessages(library(googledrive))
suppressPackageStartupMessages(library(googlesheets))


library(purrr)
library(dplyr)
library(xts)
library(PerformanceAnalytics)
library(quantmod)

# load data ----

stocks_names <- c(
  "ABEV3.SA",
  "B3SA3.SA",
  "CNTO3.SA",
  "EGIE3.SA",
  "LEVE3.SA",
  "LREN3.SA",
  "PSSA3.SA",
  "RAIL3.SA",
  "FLRY3.SA",
  "ARZZ3.SA",
  "MDIA3.SA",
  "EZTC3.SA"
)

getSymbols(stocks_names, from = "2009-01-01", to = Sys.Date() - 1,
           auto.assign = TRUE, warnings = FALSE)


prices <- stocks_names[-3] %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(stocks_names[-3])

prices_rets <- Return.calculate(prices, method = "log")

# correlation study ----

chart.Correlation(prices_rets["2020-01/2020-02"])

# table.Correlation(prices_rets_monthly, prices_rets_monthly)
# write.csv2(.Last.value, "andromeda-correlation-analysis.csv")
# # gs_new("andromeda-correlation-analysis", input = .Last.value, trim = TRUE, verbose = FALSE)

# carteira 1 ----

symbols <- c("ABEV3.SA",
             "CNTO3.SA",
             "B3SA3.SA",
             "MDIA3.SA")

prices <- symbols %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

prices_rets <- Return.calculate(prices, method = "log") %>% na.omit()
prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
prices_rets_monthly <- Return.calculate(prices_monthly,  method = "log") %>%  na.omit()

chart.Correlation(prices_rets_monthly)

# ----

andromeda <- c("TCSA3.SA", "RAIL3.SA", "VVAR3.SA", "B3SA3.SA", "LREN3.SA", "VALE3.SA", "OIBR3.SA", "LCAM3.SA")
stocks <- c(andromeda, "WEGE3.SA", "LEVE3.SA", "CVCB3.SA", "FLRY3.SA", "PSSA3.SA", "ENBR3.SA", "^BVSP")
getSymbols(stocks, from = "2009-01-01", to = Sys.Date() - 1,
                     auto.assign = TRUE, warnings = FALSE)

stocks <- sub("\\^", "", stocks)

stocks <- stocks[!(stocks %in% c("BVSP", "BOVA11.SA"))] # excluding BVSP, BOVA11

stocks %>%
  map(~ Ad(get(.))) %>%
  map_dbl(~ length(.)) %>%
  set_names(stocks)

stocks %>%
  map(~Ad(get(.))) %>%
  map_dbl(~sum(is.na(.))) %>%
  set_names(stocks)

# stocks <- c("TCSA3.SA", "RAIL3.SA", "ITSA4.SA", "VVAR3.SA", "B3SA3.SA", "LREN3.SA", "VALE3.SA", "WEGE3.SA", "VULC3.SA", "OIBR3.SA", "FLRY3.SA", "ABEV3.SA")

prices <- stocks %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(stocks)

andromeda_prices <- andromeda %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(andromeda)

plot(prices, legend.loc = "topleft")

prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
andromeda_prices_monthly <- to.monthly(andromeda_prices, indexAt = "lastof", OHLC = FALSE)
plot(andromeda_prices_monthly, legend.loc = "topleft")

prices_rets <- Return.calculate(prices,  method = "log") %>%  na.omit()
andromeda_prices_monthly_rets <- Return.calculate(andromeda_prices_monthly,  method = "log") %>%  na.omit()
prices_monthly_rets <-  Return.calculate(prices_monthly,  method = "log") %>%  na.omit()

index_prices <- "BVSP" %>%
  map(~Ad(get(.))) %>%
  `[[`(1) %>%
  `colnames<-`("BVSP")

index_prices_monthly <- to.monthly(index_prices, indexAt = "lastof", OHLC = FALSE)

index_rets <- Return.calculate(index_prices,  method = "log") %>%  na.omit()
index_prices_monthly_rets <- Return.calculate(index_prices_monthly,  method = "log") %>%  na.omit()

table.Correlation(index_prices_monthly_rets, prices_monthly_rets)
table.Correlation(index_prices_monthly_rets, andromeda_prices_monthly_rets)

table.Correlation(prices_monthly_rets, prices_monthly_rets)
chart.Correlation(prices_monthly_rets)
chart.Correlation(andromeda_prices_monthly_rets)

apply(prices_monthly_rets, 2, mean)
sqrt(apply(prices_monthly_rets, 2, var))

apply(prices_monthly_rets, 2, mean) * 12
apply(xts::last(prices_monthly_rets, "5 years"), 2, mean) * 12

set_names(as.numeric(xts::last(xts::last(prices_monthly, "5 years"))) / as.numeric(xts::first(xts::last(prices_monthly, "5 years"))), names(prices_monthly))
set_names(as.numeric(xts::last(prices_monthly)) / as.numeric(xts::first(prices_monthly)), names(prices_monthly))

(1 + apply(prices_monthly_rets, 2, mean))^12 - 1
sqrt(apply(prices_monthly_rets, 2, var))*sqrt(12)

stocks %>%
  map( ~ lm(reformulate("BOVA11.SA", .),
            data = merge(prices_monthly_rets[,.] - 0.005, index_prices_monthly_rets - 0.005))) %>%
  map(coefficients) %>%
  set_names(stocks) %>%
  bind_cols()
  

library(fPortfolio)
library(timeSeries)

ret <- as.timeSeries(prices_monthly_rets)
ret <- ret[,c("ITSA4.SA", "B3SA3.SA", "LREN3.SA")]
n_col <- ncol(ret)
min_max <- c("minW[1:n_col]=0.1", "maxW[1:n_col]=0.5")
minvarp <- minvariancePortfolio(ret, constraints = min_max)
summary(minvarp)
# eff <- portfolioFrontier(ret)
# plot(eff)

ret <- as.timeSeries(prices_monthly_rets[,c("ITSA4.SA", "B3SA3.SA", "LREN3.SA", "VALE3.SA")])
n_col <- ncol(ret)
minvarp <- minvariancePortfolio(ret)
summary(minvarp)
eff <- portfolioFrontier(ret)
plot(eff)

ret <- as.timeSeries(prices_monthly_rets[,c("ITSA4.SA", "B3SA3.SA", "LREN3.SA", "WEGE3.SA")])
n_col <- ncol(ret)
minvarp <- minvariancePortfolio(ret)
summary(minvarp)
rbind(
  mean = apply(ret, 2, mean)*12,
  stdev = sqrt(apply(ret, 2, var))*sqrt(12)
)

ret <- as.timeSeries(prices_monthly_rets[,c("ITSA4.SA", "B3SA3.SA", "LREN3.SA", "WEGE3.SA", "VALE3.SA")])
n_col <- ncol(ret)
minvarp <- minvariancePortfolio(ret)
summary(minvarp)
rbind(
  mean = apply(ret, 2, mean)*12,
  stdev = sqrt(apply(ret, 2, var))*sqrt(12)
)

ret <- as.timeSeries(prices_monthly_rets[,c("B3SA3.SA", "LREN3.SA", "WEGE3.SA", "VALE3.SA")])
n_col <- ncol(ret)
minvarp <- minvariancePortfolio(ret)
summary(minvarp)
rbind(
  mean = apply(ret, 2, mean)*12,
  stdev = sqrt(apply(ret, 2, var))*sqrt(12)
)

chart.Correlation(ret)

ret <- as.timeSeries(prices_monthly_rets[,c("B3SA3.SA", "LREN3.SA")])
minvariancePortfolio(ret)
rbind(
  mean = apply(ret, 2, mean)*12,
  stdev = sqrt(apply(ret, 2, var))*sqrt(12)
)

ret <- as.timeSeries(prices_monthly_rets["2017/",c("VALE3.SA", "LREN3.SA", "ITSA4.SA", "RAIL3.SA", "B3SA3.SA")])
minvariancePortfolio(ret)
rbind(
  mean = apply(ret, 2, mean)*12,
  stdev = sqrt(apply(ret, 2, var))*sqrt(12)
)

ret <- as.timeSeries(prices_monthly_rets["2017/",c("VALE3.SA", "LREN3.SA", "ITSA4.SA", "RAIL3.SA", "B3SA3.SA", "WEGE3.SA", "FLRY3.SA")])
minvariancePortfolio(ret)
chart.Correlation(ret)
