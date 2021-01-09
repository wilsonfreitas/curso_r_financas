
library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(quantmod)
library(rbcb)

IBOVESPA <- getSymbols("^BVSP",
                       from = "2009-12-31",
                       to = "2019-12-31",
                       auto.assign = FALSE)

IBOVESPA %>%
  Ad() %>%
  autoplot()

# EDA retornos

IBOVESPA_rets <- IBOVESPA %>%
  Cl() %>%
  log() %>%
  diff()


IBOVESPA_rets %>% autoplot()

summary(IBOVESPA_rets %>% coredata())

var(IBOVESPA_rets %>% as.numeric(), na.rm = TRUE)

sd(IBOVESPA_rets, na.rm = TRUE)

skewness(IBOVESPA_rets, na.rm = TRUE)

kurtosis(IBOVESPA_rets, na.rm = TRUE, method = "moment")

x_norm <- rnorm(length(IBOVESPA_rets))

plot(x_norm, type = "l")

mean(x_norm, na.rm = TRUE)

sd(x_norm, na.rm = TRUE)

skewness(x_norm, na.rm = TRUE)

kurtosis(x_norm, na.rm = TRUE, method = "moment")


IBOVESPA_rets %>%
  chart.Histogram(breaks = 50,
                  main = "Histograma de retornos do IBOVESPA",
                  show.outliers = TRUE,
                  methods = "add.normal", lwd = 1)

x_norm %>%
  chart.Histogram(breaks = 50,
                  main = "Histograma de retornos da Normal",
                  show.outliers = TRUE,
                  methods = "add.normal", lwd = 1)


# medidas de risco

BOVA11 <- getSymbols("BOVA11.SA",
                     to = "2019-12-31",
                     auto.assign = FALSE)
BOVA11_rets <- BOVA11 %>%
  Cl() %>%
  log() %>%
  diff()

BOVA11_monthly <- BOVA11 %>%
  Ad() %>%
  to.period()

BOVA11_monthly_rets <- BOVA11_monthly %>%
  Cl() %>%
  log() %>%
  diff()

BOVA11_monthly_rets %>%
  chart.Histogram(breaks = 30,
                  main = "Histograma de retornos do IBOVESPA",
                  show.outliers = TRUE,
                  methods = "add.normal", lwd = 1)


mu <- BOVA11_monthly_rets %>%
  na.omit() %>%
  mean()

sigma <- BOVA11_monthly_rets %>%
  na.omit() %>%
  sd()

qnorm(0.025, mean = mu, sd = sigma)

qnorm(0.0005, mean = mu, sd = sigma)

r_t <- qnorm(0.025, mean = mu, sd = sigma)

P_t <- 111.23

1000 * (P_t - P_t * exp(r_t))

R_t <- (1 - exp(r_t))

1000 * P_t * R_t

1000 * P_t * r_t

# Drawdown

chart.Drawdown(BOVA11_rets["2019"])

table.Drawdowns(BOVA11_rets["2019"])


# Short-fall risk

length(BOVA11_rets)

BOVA11_rets[BOVA11_rets < -0.05]

sum(BOVA11_rets < -0.05, na.rm = TRUE)

100 * sum(BOVA11_rets < -0.05, na.rm = TRUE) / length(BOVA11_rets)

length(BOVA11_monthly_rets)

sum(BOVA11_monthly_rets < -0.05, na.rm = TRUE)

100 * sum(BOVA11_monthly_rets < -0.05, na.rm = TRUE) / length(BOVA11_monthly_rets)

c(
  mean = IBOVESPA_rets %>% na.omit %>% mean(),
  sd = IBOVESPA_rets %>% na.omit %>% sd()
)

IBOVESPA_rets %>% autoplot()

rollmean(IBOVESPA_rets, 21) %>%
  autoplot() +
  geom_hline(yintercept = IBOVESPA_rets %>% na.omit %>% mean(), colour = 'red')

rollapply(IBOVESPA_rets, 21, sd) %>%
  autoplot() +
  geom_hline(yintercept = IBOVESPA_rets %>% na.omit %>% sd(), colour = 'red')

plot(index(IBOVESPA_rets), coredata(IBOVESPA_rets),
     type = "l",
     ylim = c(-0.1, 0.1),
     cex.axis = .7,
     lwd = 1,
     xlab = "Date", ylab = "Retorno",
     main = "Retorno diário do IBOVESPA com intervalo de confiança de 2 desvios")
volatility <- rollapply(IBOVESPA_rets, 21, sd)
lines(index(volatility), coredata(volatility) * 3, col = "red")
lines(index(volatility), -coredata(volatility) * 3, col = "red")


symbols <- c("BOVA11.SA", "SMAL11.SA", "SPXI11.SA", "PETR4.SA", "VALE3.SA", "B3SA3.SA", "ABEV3.SA", "ITUB4.SA", "VVAR3.SA")
prices <- getSymbols(symbols, from = "2016-01-01",
                     to = "2019-12-31",
                     auto.assign = TRUE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)


plot(prices, legend.loc = "topleft")


prices_monthly <- to.monthly(prices, indexAt = "lastof", OHLC = FALSE)
head(prices_monthly)

prices_monthly_rets <- Return.calculate(prices_monthly, "log")
head(prices_monthly_rets)


exp_returns <- colMeans(prices_monthly_rets, na.rm = TRUE)
exp_returns * 252


exp_risk <- StdDev(prices_monthly_rets)
exp_risk


plot(exp_risk, exp_returns, 
     xlim = c(0.05, 0.2),
     ylim = c(0.0, 0.05),
     pch = 16)
text(x = exp_risk,
     y = exp_returns,
     labels = colnames(exp_risk),
     pos=4)


chart.RiskReturnScatter(prices_monthly_rets)

prices_monthly_rets %>%
  data.frame(Date = index(.)) %>%
  remove_rownames() %>%
  gather(Symbol, Returns, -Date) %>%
  ggplot(aes(x = Symbol, y = Returns)) +
  geom_boxplot()


prices_monthly_rets %>%
  data.frame(Date = index(.)) %>%
  remove_rownames() %>%
  gather(Symbol, Returns, -Date) %>%
  ggplot(aes(x = Symbol, y = Returns)) +
  geom_violin(fill = "blue")


cor(prices_monthly_rets %>% na.omit())


chart.Correlation(prices_monthly_rets)

symbols <- c("BOVA11.SA", "SPXI11.SA")
prices <- getSymbols(symbols, from = "2016-01-01",
                     to = "2019-12-31",
                     auto.assign = TRUE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)
prices %>% head()

rets <- log(prices) %>% diff() %>% na.omit()
head(rets)

r_i <- colMeans(rets) * 252
r_i

sigma_i <- apply(rets, 2, sd) * sqrt(252)
sigma_i

rho_ij <- cor(rets[,1], rets[,2]) %>% as.numeric()
rho_ij

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

alpha <- seq(0, 1, 0.01)
risk_ <- sigma_c(alpha, sigma_i, rho_ij)
return_ <- r_c(alpha, r_i)

plot(risk_, return_,
     xlim = c(0.12, 0.25),
     type = "b", pch=16, col = "firebrick2",
     xlab = "Risco", ylab = "Retorno")
text(x = sigma_i, y = r_i, labels=names(r_i), pos=4)
points(x = sigma_i, y = r_i, col="orange3", pch=16, cex=1.5)

alpha <- seq(0, 1, 0.05)
risk_ <- sigma_c(alpha, sigma_i, 1)
return_ <- r_c(alpha, r_i)

plot(risk_, return_,
     xlim = c(0.12, 0.25),
     type = "b", pch=16, col = "firebrick3",
     xlab = "Risco", ylab = "Retorno")
text(x = sigma_i, y = r_i, labels=names(r_i), pos=4)
points(x = sigma_i, y = r_i, col="orange3", pch=16, cex=1.5)


alpha <- seq(0, 1, 0.01)
risk_ <- sigma_c(alpha, sigma_i, -1)
return_ <- r_c(alpha, r_i)

plot(risk_, return_,
     xlim = c(0, 0.27),
     type = "b", pch=16, col = "firebrick4",
     xlab = "Risco", ylab = "Retorno")
text(x = sigma_i, y = r_i, labels=names(r_i), pos=4)
points(x = sigma_i, y = r_i, col="orange3", pch=16, cex=1.5)


# Totalmente diversificada (correlação = -1)
alpha <- seq(0, 1, 0.01)
risk_ <- sigma_c(alpha, sigma_i, -1)
return_ <- r_c(alpha, r_i)

plot(risk_, return_,
     xlim = c(0, 0.27),
     type = "p", pch=16, col = "firebrick2",
     xlab = "Risco", ylab = "Retorno")

# Correlação estimada entre os ativos
alpha <- seq(0, 1, 0.02)
risk_ <- sigma_c(alpha, sigma_i, rho_ij)
return_ <- r_c(alpha, r_i)

points(risk_, return_, col = "firebrick3", pch = 16)

# Sem diversificação
alpha <- seq(0, 1, 0.05)
risk_ <- sigma_c(alpha, sigma_i, 1)
return_ <- r_c(alpha, r_i)

points(risk_, return_, col = "firebrick4", pch=16)

# labels
text(x = sigma_i, y = r_i, labels = names(r_i), pos = 4)
points(x = sigma_i, y = r_i, col="orange3", pch = 16, cex = 1.5)


alpha <- seq(0, 1, 0.01)
risk <- sigma_c(alpha, sigma_i, rho_ij)

min_var_weight <- alpha[which.min(risk)]

min_var_weight


weights_c <- c(BOVA11.SA = min_var_weight, SPXI11.SA = 1 - min_var_weight)
port_1 <- Return.portfolio(rets, weights_c, wealth.index = TRUE)
plot(port_1, main = "Carteira de mínima variância")

weights_c <- c(BOVA11.SA = min_var_weight, SPXI11.SA = 1 - min_var_weight)
port_2 <- Return.portfolio(rets, weights_c, rebalance_on = "months", wealth.index = TRUE)
ports <- merge(port_1, port_2) %>%
  `colnames<-`(c("Sem rebalanceamento", "Com rebalanceamento"))
plot(ports, legend.loc = "topleft", main = "Simulando rebalanceamento da carteira")
