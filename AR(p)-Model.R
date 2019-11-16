#                   ############################################

#                   Master's Thesis - Matthieu Nicolas Rüttimann
#                   
#                   ############################################
#                   #####           AR(p)-model            #####
#                   ############################################

# ------------------------------------ Packages ---------------------------------------

library(data.table)
library(pryr)
library(microbenchmark)
library(xts)
library(timeSeries)
library(forecast)
library(plm)
library(biglm)
library(lmtest)
library(sandwich)
library(KFAS)
library(stargazer)


# ----------------------------------- AR(p)-Model --------------------------------------

# Estimating the AR(p)-model by AIC or BIC which is used to decompose the informational
# from an uninformational component in ETF flows. Before the actual estimation, a look
# is taken at the characteristics of the time series. This helps guiding the estimation
# process and checking its plausibility.

acf(Stocks$`ETF Flows`)

acf(Stocks$`ETF Flows`, plot = F, lag.max = 120, na.action = na.pass)

pacf(Stocks$`ETF Flows`)

# Ljung-Box test: Null-hypothesis of first-order independence is clearly rejected

Box.test(Stocks$`ETF Flows`, lag = 1, type = "Ljung-Box", fitdf = 1)

# Next, the AR(p)-model is estimated.

Return_Effects <- cbind(Stocks$Return, shift(Stocks$Return, n = 1), shift(Stocks$Return, n = 2),
                        shift(Stocks$Return, n = 3), shift(Stocks$Return, n = 4),
                        shift(Stocks$Return, n = 5))

colnames(Return_Effects) <- c("r_t", "r_t-1", "r_t-2", "r_t-3", "r_t-4", "r_t-5")

tmp <- tempfile()
Rprof(tmp)

Fit_AR_10 <- arima(Stocks$`ETF Flows`, order = c(10, 0, 0), include.mean = T, xreg = Return_Effects,
                   method = "CSS-ML", SSinit = "Rossignol2011")

Rprof(NULL)
a <- summaryRprof(tmp)
unlink(tmp)

Fit_AR <- ar(Stocks$`ETF Flows`, aic = T, order.max = 10, method = "mle",
             na.action = na.pass)

Fit_AR <- auto.arima(Stocks$`ETF Flows`, max.p = 10, max.q = 0, ic = "bic")

# Goodness-of-fit of the AR(p)-model

AIC(Fit_AR_1, Fit_AR_2, Fit_AR_5, Fit_AR_6, Fit_AR_8, Fit_AR_10)

BIC(Fit_AR_1, Fit_AR_2, Fit_AR_5, Fit_AR_6, Fit_AR_8, Fit_AR_10)

# Diagnostics of the fit of the AR(p)-model

acf(residuals(Fit_AR_10))

tsdiag()

# Storing fitted and residual series as informational and uninformational component,
# respectively.

Informed_ETF_Flows <- residuals(Fit_AR_10)

Uninformed_ETF_Flows <- fitted(Fit_AR_10)

# Adding the informed and uninformed flow component to the main stock data table

TmpA <- as.data.table(cbind(Informed_ETF_Flows, Uninformed_ETF_Flows))

Stocks <- cbind(Stocks, TmpA)

colnames(Stocks)[23:24] <- c("Informed ETF Flows", "Uninformed ETF Flows")


