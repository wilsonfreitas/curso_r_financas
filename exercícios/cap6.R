
library(fPortfolio)
library(tidyverse)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(bizdays)

r_c <- function(alpha, R) {
  # desanualizando
  R <- R / 252
  r <- alpha*R[1] + (1 - alpha)*R[2]
  r * 252
}

sigma_c <- function(alpha, SIGMA, RHO) {
  # desanualizando
  SIGMA <- SIGMA / sqrt(252)
  s <- alpha * alpha * SIGMA[1] * SIGMA[1] +
    (1 - alpha) * (1 - alpha) * SIGMA[2] * SIGMA[2] +
    2 * alpha * (1 - alpha) * SIGMA[1] * SIGMA[2] * RHO
  sqrt(s * 252)
}

# ABEV3.SA x ITUB4.SA

symbols <- c("ABEV3.SA", "ITUB4.SA")
prices <- getSymbols(symbols, from = "2016-01-01",
                     to = "2020-11-30",
                     auto.assign = TRUE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

rets <- log(prices) %>% diff() %>% na.omit()

r_i <- colMeans(rets) * 252

sigma_i <- apply(rets, 2, sd) * sqrt(252)

rho_ij <- cor(rets[,1], rets[,2]) %>% as.numeric()

alpha <- seq(0, 1, 0.01)
risk_ <- sigma_c(alpha, sigma_i, rho_ij)
return_ <- r_c(alpha, r_i)

plot(risk_, return_,
     # xlim = c(0.30, 0.65),
     type = "b", pch=16, col = "firebrick2",
     xlab = "Risco", ylab = "Retorno")
text(x = sigma_i, y = r_i, labels=names(r_i), pos=4)
points(x = sigma_i, y = r_i, col="orange3", pch=16, cex=1.5)


min_var_port <- c(alpha[which.min(risk_)], 1 - alpha[which.min(risk_)])
names(min_var_port) <- symbols
min_var_port

head(rets)

port_1 <- Return.portfolio(rets, min_var_port, wealth.index = TRUE)

port_1 %>% head()

plot(port_1, main = "Carteira de mínima variância")

# Forma matricial

# ABEV3.SA x ITUB4.SA

symbols <- c("ABEV3.SA", "ITUB4.SA", "MGLU3.SA", "IRBR3.SA", "B3SA3.SA", "COGN3.SA")
prices <- getSymbols(symbols, from = "2016-01-01",
                     to = "2020-11-30",
                     auto.assign = TRUE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

rets <- log(prices) %>% diff() %>% na.omit()

r_i <- colMeans(rets) * 252

# sigma_i <- apply(rets, 2, sd) * sqrt(252)
# 
# rho_ij <- cor(rets[,1], rets[,2]) %>% as.numeric()

m_cov <- cov(rets)

minvarp <- minvariancePortfolio(as.timeSeries(rets))

weights <- minvarp@portfolio@portfolio$weights

sqrt( t(weights) %*% m_cov %*% t(t(weights)) * 252 )

summary(minvarp)

plot(minvarp)

front <- portfolioFrontier(as.timeSeries(rets))
plot(front)
#

n_col <- ncol(rets)

min_max <- c("minW[1:6]=0.1", "maxW[1:6]=0.5")

minvarp <- minvariancePortfolio(as.timeSeries(rets), constraints = min_max)

weights <- minvarp@portfolio@portfolio$weights
# 
sqrt( t(weights) %*% m_cov %*% t(t(weights)) * 252 )

summary(minvarp)

plot(minvarp)
