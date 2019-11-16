#                   Master's Thesis - Matthieu Nicolas Ruettimann
#                   
#                   ############# State Space Trial #############


# ------------------------------------ Packages ---------------------------------------

library(timeSeries)
library(tseries)
library(KFAS)
library(forecast)
library(lmtest)

# ------------------------------- Connecting to WRDS ----------------------------------

# WRDS can be accessed remotely from a personal computer using the SQL language in R
# Studio. The code shows the loging-in process to the WRDS database with one's personal
# user name and password. The package "RPostgres" is needed for this purpose, which
# enables SQL queries within R Studio.

WRDS <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  user = "mruettim",
                  password = "MfDb1362",
                  sslmode='require')


# ----------------------------- Data Download from WRDS -------------------------------

RES <- dbSendQuery(WRDS, "select *
                   from crsp.dsf
                   where date between '2000-01-01'
                   and '2018-01-01'
                   and cusip = '48783610'")
Kellogg_Stock_Price <- dbFetch(RES, n=-1)
dbClearResult(RES)
Kellogg_Stock_Price

# -------------------------- #### State Space Models #### ------------------------------
# 
# ------------------------- Alcohol Deaths Series from KFAS ----------------------------


# Loading the data

data("alcohol")
deaths <- window(alcohol[ , 2], end = 2007)
population <- window(alcohol[ , 6], end = 2007)

# Setting the system matrices for the wanted model

Zt <- matrix(c(1, 0), 1, 2)
Ht <- matrix(NA)
Tt <- matrix(c(1, 0, 1, 1), 2, 2)
Rt <- matrix(c(1, 0), 2, 1)
Qt <- matrix(NA)
a1 <- matrix(c(1, 0), 2, 1)
P1 <- matrix(0, 2, 2)
P1inf <- diag(2)

# Defining local linear trend model

model_gaussian <- SSModel(deaths / population ~ -1 +
                            SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1,
                                      P1inf = P1inf), H = Ht)

model_gaussian <- SSModel(deaths / population ~ SSMtrend(degree = 2, Q = list(matrix(NA), matrix(0))),
                          H = matrix(NA))

# Fitting the model, i.e. estimating the parameters of the defined model

fit_gaussian <- fitSSM(model_gaussian, inits = c(0, 0), method = "BFGS")

# Filtering and smoothing recursions

out_gaussian <- KFS(fit_gaussian$model)

# Returning the estimated parameters, where fit_gaussian$model["H"] = variance of observation
# disturbances and fit_gaussian$model["Q"] = variance of state disturbances

out_gaussian

fit_gaussian$model["H"]
fit_gaussian$model["Q"]
fit_gaussian$model["Q", etas = "level"]
fit_gaussian$model["Q", etas = "slope"]
out_gaussian$F

fit_gaussian

out_gaussian$logLik / length(deaths / population)

# Ploting the estimated model (filtered and smoothed estimates)

plot(cbind(deaths/population, out_gaussian$att[ , -2], out_gaussian$alphahat[ , -2]),
     plot.type = "single", main = "Alcohol Related Deaths",  col = c(1, "orangered3", "dodgerblue4"),
     lty = c("solid", "longdash", "dotdash"), lwd = c(1, 1, 2),
     ylab = "Alcohol related deaths per 10'000 people")

legend(x = 1995, y = 15, legend = c("One-Step-Ahead Predictions", "Smoothed Estimates"),
       col = c("orangered3", "dodgerblue4"), lty=1:2, cex = 0.8)

# Plotting estimated states (level and slope)

plot(coef(out_gaussian), main = "Smoothed States")

# Diagnostics

plot(residuals(out_gaussian))
plot(rstandard(out_gaussian))
acf(rstandard(out_gaussian), na.action = na.pass)

plot(rstandard(KFS(fit_gaussian$model, filtering = "mean", smoothing = "none", nsim = 1000)))



# -------------------------------- UK Seatbelt Model -----------------------------------

# Data and transformation into ts object

UK_Seatbelt_Data <- readSeries("/Users/Matthieu/Desktop/HSG/Master's Thesis/ETPs and Systemic Risk/Data/UK Seatbelt Data/UK Seatbelt Data.csv",
                             header = T, sep = ",", format = "%Y-%m-%d")

UK_Seatbelt_Data <- as.ts(UK_Seatbelt_Data)

colnames(UK_Seatbelt_Data) <- c("Drivers KSI", "Petrol Price")

# Log-Tranformation

UK_Seatbelt_Data <- log(UK_Seatbelt_Data)

# Plot

plot.ts(UK_Seatbelt_Data[ , "Drivers KSI"])
plot.ts(UK_Seatbelt_Data)


                              #### Local Level Model ####


# 2.1 Defining the deterministic local level model (intercept term is automatically omitted
# when using the SSMtrend auxillary function)

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = 0, a1 = 0, P1 = 0), H = NA)


# 2.2 Defining the stochastic local level model

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = NA), H = NA)

# Fitting the model, i.e. estimating the parameters of the defined model

fit_UK_Seatbelt <- fitSSM(model_UK_Seatbelt, inits = c(0, 0), method = "BFGS")

exp(fit_UK_Seatbelt$optim.out$par)

# Filtering and smoothing recursions

out_UK_Seatbelt <- KFS(fit_UK_Seatbelt$model)

# Returning the estimated parameters

fit_UK_Seatbelt
out_UK_Seatbelt

fit_UK_Seatbelt$model["H"]    # Variance of observation error disturbance
fit_UK_Seatbelt$model["Q"]    # Variance of state disturbance
fit_UK_Seatbelt$model["Q", etas = "level"]
out_UK_Seatbelt$P             # Variance of filtered state
out_UK_Seatbelt$F             # Variance of observation / prediction error


logLik(fit_UK_Seatbelt$model)
out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data)

# Ploting the estimated model (filtered and smoothed estimates)

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], out_UK_Seatbelt$att, out_UK_Seatbelt$alphahat),
     plot.type = "single", main = "UK Drivers KSI",  col = c(1, "orangered3", "dodgerblue4"),
     lty = c("solid", "longdash", "dotdash"), lwd = c(1, 1, 2),
     ylab = "Drivers KSI")

legend(x = 3200, y = 2, legend = c("UK Drivers KSI", "One-Step-Ahead Predictions",
                                   "Smoothed Estimates"),
       col = c(1, "orangered3", "dodgerblue4"), lty = c("solid", "longdash", "dotdash"),
       lwd = c(1, 1, 2), cex = 0.8)

# Information criteria

AIC_UK <- (1 / nrow(UK_Seatbelt_Data)) * (-2 * nrow(UK_Seatbelt_Data)
                                          * (out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data))
                                          + 2 * (1 + 2))

# Diagnostics

plot.ts(residuals(out_UK_Seatbelt)[-1])   # Error disturbances / irregular component / residuals

plot(rstandard(out_UK_Seatbelt))          # Standardised residuals

Box.test(rstandard(out_UK_Seatbelt), lag = 15, type = "Ljung-Box", fitdf = 0)

acf(rstandard(out_UK_Seatbelt), na.action = na.pass)

# H-test is missing

jarque.bera.test(na.omit(rstandard(out_UK_Seatbelt)))

# One-step-ahead predicitions

predict()


                          #### Local Linear Trend Model ####

# 3.1 Defining the deterministic local linear trend model

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 2, Q = list(0, 0)), H = NA)

# 3.2 Defining the stochastic local linear trend model

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 2, Q = list(NA, NA)), H = NA)

# 3.3 Defining the stochastic local linear trend model with deterministic slope

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 2, Q = list(NA, 0)), H = NA)

# Fitting the model

fit_UK_Seatbelt <- fitSSM(model_UK_Seatbelt, inits = c(0, 0, 0), method = "L-BFGS-B")

# Filtering and smoothing recursions

out_UK_Seatbelt <- KFS(fit_UK_Seatbelt$model)

# Returning the estimated parameters

fit_UK_Seatbelt
out_UK_Seatbelt
out_UK_Seatbelt$alphahat

fit_UK_Seatbelt$model["H"]    # Variance of observation error disturbance
fit_UK_Seatbelt$model["Q"]    # Variance of state disturbance
fit_UK_Seatbelt$model["Q", etas = "level"]
out_UK_Seatbelt$P             # Variance of filtered state
out_UK_Seatbelt$F             # Variance of observation / prediction error

out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data)

# Ploting the estimated model (filtered and smoothed estimates)

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], out_UK_Seatbelt$att[ , "level"],
           out_UK_Seatbelt$alphahat[ , "level"]),
     plot.type = "single", main = "UK Drivers KSI",  col = c(1, "orangered3", "dodgerblue4"),
     lty = c("solid", "longdash", "dotdash"), lwd = c(1, 1, 2),
     ylab = "Drivers KSI")

plot(out_UK_Seatbelt$alphahat[ , "slope"],
     plot.type = "single", main = "Stochastic Linear Trend Model",  col = "dodgerblue4",
     lty = "solid", lwd = 1, ylab = "Slope Component")

legend(x = 3200, y = 2, legend = c("UK Drivers KSI", "One-Step-Ahead Predictions",
                                   "Smoothed Estimates"),
       col = c(1, "orangered3", "dodgerblue4"), lty = c("solid", "longdash", "dotdash"),
       lwd = c(1, 1, 2), cex = 0.8)

# Information criteria

AIC_UK <- (1 / nrow(UK_Seatbelt_Data)) * (-2 * nrow(UK_Seatbelt_Data)
                                          * (out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data))
                                          + 2 * (2 + 2))

# Diagnostics

plot.ts(residuals(out_UK_Seatbelt)[-(1:2)])   # Error disturbances / irregular component / residuals

plot(rstandard(out_UK_Seatbelt))          # Standardised residuals

Box.test(rstandard(out_UK_Seatbelt), lag = 15, type = "Ljung-Box", fitdf = 0)

acf(residuals(out_UK_Seatbelt)[-(1:2)], na.action = na.pass)

# H-test is missing

jarque.bera.test(na.omit(rstandard(out_UK_Seatbelt)))


                      #### Local Level Model with Seasonal ####

# 4.1 Defining the deterministic local linear trend model with deterministic seasonal

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = 0)
                             + SSMseasonal(period = 12, Q = 0, sea.type = "dummy"),
                             H = NA)

# 4.2 Defining the stochastic local linear trend model with stochastic seasonal

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = NA)
                             + SSMseasonal(period = 12, Q = NA, sea.type = "dummy"),
                             H = NA)

# 4.3 Defining the stochastic local linear trend model with deterministic seasonal

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = NA)
                             + SSMseasonal(period = 12, Q = 0, sea.type = "dummy"),
                             H = NA)

# Fitting the model

fit_UK_Seatbelt <- fitSSM(model_UK_Seatbelt, inits = c(0, 0, 0), method = "BFGS")

exp(fit_UK_Seatbelt$optim.out$par)
fit_UK_Seatbelt$optim.out$convergence
fit_UK_Seatbelt

# Filtering and smoothing recursions

out_UK_Seatbelt <- KFS(fit_UK_Seatbelt$model)

# Returning the estimated parameters

out_UK_Seatbelt
out_UK_Seatbelt$alphahat

fit_UK_Seatbelt$model["H"]    # Variance of observation error disturbance
fit_UK_Seatbelt$model["Q"]    # Variance of state disturbance
fit_UK_Seatbelt$model["Q", etas = "level"]
out_UK_Seatbelt$P             # Variance of filtered state
out_UK_Seatbelt$F             # Variance of observation / prediction error

fit_UK_Seatbelt$optim.out$value
out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data)

# Ploting the estimated model (filtered and smoothed estimates)

# Combined model

smoothed_signal_UK <- signal(out_UK_Seatbelt, states = "all", filtered = F)
filtered_signal_UK <- signal(out_UK_Seatbelt, states = "all", filtered = T)

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], smoothed_signal_UK$signal),
     plot.type = "single", main = "UK Drivers KSI - Stochastic Local Level Model with Seasonal",
     col = c(1, "dodgerblue4"), lty = c("solid", "longdash"),
     lwd = c(1, 2), ylab = "Combined Model")

# Level component

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], coef(out_UK_Seatbelt, states = "level")),
     plot.type = "single", main = "Stochastic Level Component",
     col = c(1, "dodgerblue4"), lwd = c(1, 2),
     lty = c("solid", "solid"), ylab = "Stochastic Level")

# Seasonal component

seasonal_signal_UK <- signal(out_UK_Seatbelt, states = "seasonal", filtered = F)

plot(seasonal_signal_UK$signal,
     plot.type = "single", main = "Deterministic Seasonal Component",
     col = "dodgerblue4", ylab = "Deterministic Seasonal")
abline(v = 1969:2003, lty = "dotted")

legend(x = 3200, y = 2, legend = c("UK Drivers KSI", "One-Step-Ahead Predictions",
                                   "Smoothed Estimates"),
       col = c(1, "orangered3", "dodgerblue4"), lty = c("solid", "longdash", "dotdash"),
       lwd = c(1, 1, 2), cex = 0.8)

# Information criteria

AIC_UK <- (1 / nrow(UK_Seatbelt_Data)) * (-2 * nrow(UK_Seatbelt_Data)
          * (out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data)) + 2 * (12 + 2))

BIC_UK

# Diagnostics

plot.ts(residuals(out_UK_Seatbelt)[-(1:12)])   # Error disturbances / irregular component / residuals

plot(rstandard(out_UK_Seatbelt))          # Standardised residuals

Box.test(rstandard(out_UK_Seatbelt), lag = 15, type = "Ljung-Box", fitdf = 12)

acf(rstandard(out_UK_Seatbelt), na.action = na.pass)

# H-test is missing -> There are several good graphical methods for assessing the homoscedasticity etc.
# of the residuals!

sum((residuals(out_UK_Seatbelt)[134:192])^2) / sum((residuals(out_UK_Seatbelt)[13:72])^2)

gqtest(model_UK_Seatbelt, point = 0.5, fraction = 0, alternative = "two.sided", data = list())

jarque.bera.test(na.omit(rstandard(out_UK_Seatbelt)))
shapiro.test(rstandard(out_UK_Seatbelt))


                #### Local Level Model with Explanatory Variable ####

# 5.1 Defining the deterministic local level model with deterministic explanatory variable
# (classical linear regression model), once regressed on time and once on the log petrol price

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = 0)
                             + c(1:192),
                             H = NA)

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = 0)
                             + SSMregression(~ UK_Seatbelt_Data[ , "Petrol Price"]),
                             H = NA)

# 5.2 Defining the stochastic local level model with deterministic explanatory variable

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = NA)
                             + SSMregression(~ UK_Seatbelt_Data[ , "Petrol Price"], Q = 0),
                             H = NA)

# Fitting the model

fit_UK_Seatbelt <- fitSSM(model_UK_Seatbelt, inits = c(0, 0), method = "BFGS")

exp(fit_UK_Seatbelt$optim.out$par)
fit_UK_Seatbelt$optim.out$convergence
fit_UK_Seatbelt

# Filtering and smoothing recursions

out_UK_Seatbelt <- KFS(fit_UK_Seatbelt$model)

# Returning the estimated parameters

out_UK_Seatbelt
out_UK_Seatbelt$alphahat

fit_UK_Seatbelt$model["H"]    # Variance of observation error disturbance
fit_UK_Seatbelt$model["Q"]    # Variance of state disturbance
fit_UK_Seatbelt$model["Q", etas = "level"]
out_UK_Seatbelt$P             # Variance of filtered state
out_UK_Seatbelt$F             # Variance of observation / prediction error

fit_UK_Seatbelt$optim.out$value
out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data)

# Ploting the estimated model (filtered and smoothed estimates)

# Combined model

smoothed_signal_UK <- signal(out_UK_Seatbelt, states = "all", filtered = F)
filtered_signal_UK <- signal(out_UK_Seatbelt, states = "all", filtered = T)

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], smoothed_signal_UK$signal), plot.type = "single",
     main = "UK Drivers KSI - Stochastic Local Level Model with Explanatory Variable",
     col = c(1, "dodgerblue4"), lty = c("solid", "longdash"),
     lwd = c(1, 2), ylab = "Combined Model")

# Level component

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], coef(out_UK_Seatbelt, states = "level")),
     plot.type = "single", main = "Stochastic Level Component",
     col = c(1, "dodgerblue4"), lwd = c(1, 2),
     lty = c("solid", "solid"), ylab = "Stochastic Level")

# Explanatory variable component

explanatory_signal_UK <- signal(out_UK_Seatbelt, states = "regression",
                             filtered = F)

plot(explanatory_signal_UK$signal,
     plot.type = "single", main = "Deterministic Explanatory Variable Component",
     col = "dodgerblue4", ylab = "Deterministic Explanatory Variable")

legend(x = 3200, y = 2, legend = c("UK Drivers KSI", "One-Step-Ahead Predictions",
                                   "Smoothed Estimates"),
       col = c(1, "orangered3", "dodgerblue4"), lty = c("solid", "longdash", "dotdash"),
       lwd = c(1, 1, 2), cex = 0.8)

# Information criteria

AIC_UK <- (1 / nrow(UK_Seatbelt_Data)) * (-2 * nrow(UK_Seatbelt_Data)
                                          * (out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data))
                                          + 2 * (2 + 3))

BIC_UK

# Diagnostics

plot.ts(UK_Seatbelt_Data[ , "Drivers KSI"] - fitted(out_UK_Seatbelt)) 

plot(rstandard(out_UK_Seatbelt))          # Standardised residuals

Box.test(rstandard(out_UK_Seatbelt), lag = 15, type = "Ljung-Box", fitdf = 2)

acf(rstandard(out_UK_Seatbelt), na.action = na.pass)

# H-test is missing -> There are several good graphical methods for assessing the homoscedasticity etc.
# of the residuals!

sum((residuals(out_UK_Seatbelt)[134:192])^2) / sum((residuals(out_UK_Seatbelt)[13:72])^2)

gqtest(model_UK_Seatbelt, point = 0.5, fraction = 0, alternative = "two.sided", data = list())

jarque.bera.test(na.omit(rstandard(out_UK_Seatbelt)))
shapiro.test(rstandard(out_UK_Seatbelt))


        #### Local Level Model with Explanatory Variable in Matrix Form ####

# Setting the system matrices

Zt <- array(1, dim = c(1, 2, 192))
Zt[1, 2, ] <- UK_Seatbelt_Data[ , "Petrol Price"]
Ht <- matrix(NA)
Tt <- diag(2)
Rt <- diag(2)
Qt <- matrix(c(NA, 0, 0, 0), 2, 2)
a1 <- matrix(c(0, 0), 2, 1)
P1 <- matrix(0, 2, 2)
P1inf <- diag(2)

# Defining the stochastic local level model with explanatory variable

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[, "Drivers KSI"] ~ -1
                             + SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt,
                                         a1 = a1, P1 = P1, P1inf = P1inf,
                                         state_names = c("Level", "Petrol Price")),
                             H = Ht)

# Fitting the model

fit_UK_Seatbelt <- fitSSM(model_UK_Seatbelt, inits = c(0, 0), method = "BFGS")

exp(fit_UK_Seatbelt$optim.out$par)

# Filtering and smoothing recursions

out_UK_Seatbelt <- KFS(fit_UK_Seatbelt$model)


                #### Local Level Model with Intervention Variable ####

# 6.0 Creating a dummy variable for the introduction of the UK setabelt law

Intervention_Dummy <- UK_Seatbelt_Data[ , "Drivers KSI"]
window(Intervention_Dummy, start = c(1969,1), end = c(1983,1)) <- 0
window(Intervention_Dummy, start = c(1983,2), end = c(1984,12)) <- 1

# 6.1 Defining the deterministic local level model with deterministic intervention variable
# (classical linear dummy regression model)

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = 0)
                             + SSMregression(~ Intervention_Dummy, Q = 0),
                             H = NA)

# 6.2 Defining the stochastic local level model with deterministic intervention variable
# (classical linear dummy regression model)

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = NA)
                             + SSMregression(~ Intervention_Dummy, Q = 0),
                             H = NA)

# Fitting the model

fit_UK_Seatbelt <- fitSSM(model_UK_Seatbelt, inits = c(0, 0), method = "BFGS")

exp(fit_UK_Seatbelt$optim.out$par)
fit_UK_Seatbelt$optim.out$convergence
fit_UK_Seatbelt

# Filtering and smoothing recursions

out_UK_Seatbelt <- KFS(fit_UK_Seatbelt$model)

# Returning the estimated parameters

out_UK_Seatbelt
out_UK_Seatbelt$alphahat

fit_UK_Seatbelt$model["H"]    # Variance of observation error disturbance
fit_UK_Seatbelt$model["Q"]    # Variance of state disturbance
fit_UK_Seatbelt$model["Q", etas = "level"]
out_UK_Seatbelt$P             # Variance of filtered state
out_UK_Seatbelt$F             # Variance of observation / prediction error

fit_UK_Seatbelt$optim.out$value
out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data)

# Ploting the estimated model (filtered and smoothed estimates)

# Combined model

smoothed_signal_UK <- signal(out_UK_Seatbelt, states = "all", filtered = F)
filtered_signal_UK <- signal(out_UK_Seatbelt, states = "all", filtered = T)

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], smoothed_signal_UK$signal), plot.type = "single",
     main = "UK Drivers KSI - Stochastic Local Level Model with Intervention Variable",
     col = c(1, "dodgerblue4"), lty = c("solid", "longdash"),
     lwd = c(1, 2), ylab = "Combined Model")

# Level component

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], coef(out_UK_Seatbelt, states = "level")),
     plot.type = "single", main = "Stochastic Level Component",
     col = c(1, "dodgerblue4"), lwd = c(1, 2),
     lty = c("solid", "solid"), ylab = "Stochastic Level")

# Intervention variable component

intervention_signal_UK <- signal(out_UK_Seatbelt, states = "regression",
                                filtered = F)

plot(intervention_signal_UK$signal,
     plot.type = "single", main = "Deterministic Intervention Variable Component",
     col = "dodgerblue4", ylab = "Deterministic Intervention Variable")

legend(x = 3200, y = 2, legend = c("UK Drivers KSI", "One-Step-Ahead Predictions",
                                   "Smoothed Estimates"),
       col = c(1, "orangered3", "dodgerblue4"), lty = c("solid", "longdash", "dotdash"),
       lwd = c(1, 1, 2), cex = 0.8)

# Information criteria

AIC_UK <- (1 / nrow(UK_Seatbelt_Data)) * (-2 * nrow(UK_Seatbelt_Data)
                                          * (out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data))
                                          + 2 * (2 + 2))

BIC_UK

# Diagnostics

plot.ts(ts(residuals(out_UK_Seatbelt)[-1], start = c(1969,2), frequency = 12))

plot(rstandard(out_UK_Seatbelt))          # Standardised residuals

Box.test(rstandard(out_UK_Seatbelt), lag = 15, type = "Ljung-Box", fitdf = 2)

acf(rstandard(out_UK_Seatbelt), na.action = na.pass)

# H-test is missing -> There are several good graphical methods for assessing the homoscedasticity etc.
# of the residuals!

sum((residuals(out_UK_Seatbelt)[134:192])^2) / sum((residuals(out_UK_Seatbelt)[13:72])^2)

gqtest(model_UK_Seatbelt, point = 0.5, fraction = 0, alternative = "two.sided", data = list())

jarque.bera.test(na.omit(rstandard(out_UK_Seatbelt)))
shapiro.test(rstandard(out_UK_Seatbelt))


                        #### Combined UK Seatbelt Model ####

# 7.1 Defining the deterministic combined model (classical multivariate regression model)

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = 0)
                             + SSMseasonal(period = 12, Q = 0, sea.type = "dummy")
                             + SSMregression(~ UK_Seatbelt_Data[ , "Petrol Price"], Q = 0)
                             + SSMregression(~ Intervention_Dummy, Q = 0),
                             H = NA)

# 7.2 Defining the stochastic combined model

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = NA)
                             + SSMseasonal(period = 12, Q = NA, sea.type = "dummy")
                             + SSMregression(~ UK_Seatbelt_Data[ , "Petrol Price"], Q = 0)
                             + SSMregression(~ Intervention_Dummy, Q = 0),
                             H = NA)

# 7.3 Defining the stochastic combined model with deterministic seasonal

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[ , "Drivers KSI"] ~
                               SSMtrend(degree = 1, Q = NA)
                             + SSMseasonal(period = 12, Q = 0, sea.type = "dummy")
                             + SSMregression(~ UK_Seatbelt_Data[ , "Petrol Price"], Q = 0)
                             + SSMregression(~ Intervention_Dummy, Q = 0),
                             H = NA)

# Fitting the model

fit_UK_Seatbelt <- fitSSM(model_UK_Seatbelt, inits = c(0, 0, 0), method = "BFGS")

exp(fit_UK_Seatbelt$optim.out$par)
fit_UK_Seatbelt$optim.out$convergence
fit_UK_Seatbelt

# Filtering and smoothing recursions

out_UK_Seatbelt <- KFS(fit_UK_Seatbelt$model)

# Returning the estimated parameters

out_UK_Seatbelt
out_UK_Seatbelt$alphahat

fit_UK_Seatbelt$model["H"]    # Variance of observation error disturbance
fit_UK_Seatbelt$model["Q"]    # Variance of state disturbance
fit_UK_Seatbelt$model["Q", etas = "level"]
out_UK_Seatbelt$P             # Variance of filtered state
out_UK_Seatbelt$F             # Variance of observation / prediction error

fit_UK_Seatbelt$optim.out$value
out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data)

# Ploting the estimated model (filtered and smoothed estimates)

# Combined model

smoothed_signal_UK <- signal(out_UK_Seatbelt, states = "all", filtered = F)
filtered_signal_UK <- signal(out_UK_Seatbelt, states = "all", filtered = T)

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], smoothed_signal_UK$signal), plot.type = "single",
     main = "UK Drivers KSI - Combined UK Seatbelt Model",
     col = c(1, "dodgerblue4"), lty = c("solid", "longdash"),
     lwd = c(1, 2), ylab = "Combined Model")

# Combined model without seasonal

smoothed_signal_UK <- signal(out_UK_Seatbelt, states = c(1:3), filtered = F)

plot(cbind(UK_Seatbelt_Data[ , "Drivers KSI"], smoothed_signal_UK$signal), plot.type = "single",
     main = "UK Drivers KSI - Combined UK Seatbelt Model Without Seasonal",
     col = c(1, "dodgerblue4"), lty = c("solid", "longdash"),
     lwd = c(1, 2), ylab = "Combined Model Without Seasonal")

# Seasonal component

seasonal_signal_UK <- signal(out_UK_Seatbelt, states = "seasonal", filtered = F)

plot(seasonal_signal_UK$signal,
     plot.type = "single", main = "Stochastic Seasonal Component",
     col = "dodgerblue4", ylab = "Stochastic Seasonal")
abline(v = 1969:2003, lty = "dotted")

legend(x = 3200, y = 2, legend = c("UK Drivers KSI", "One-Step-Ahead Predictions",
                                   "Smoothed Estimates"),
       col = c(1, "orangered3", "dodgerblue4"), lty = c("solid", "longdash", "dotdash"),
       lwd = c(1, 1, 2), cex = 0.8)

# Information criteria

AIC_UK <- (1 / nrow(UK_Seatbelt_Data)) * (-2 * nrow(UK_Seatbelt_Data)
                                          * (out_UK_Seatbelt$logLik / nrow(UK_Seatbelt_Data))
                                          + 2 * (14 + 3))

BIC_UK

# Diagnostics

plot.ts(UK_Seatbelt_Data[ , "Drivers KSI"] - fitted(out_UK_Seatbelt))

plot(rstandard(out_UK_Seatbelt))          # Standardised residuals

Box.test(out_UK_Seatbelt$v/sqrt(t(out_UK_Seatbelt$F)), lag = 15, type = "Ljung-Box", fitdf = 14)

acf(UK_Seatbelt_Data[ , "Drivers KSI"] - fitted(out_UK_Seatbelt), na.action = na.pass)

# H-test is missing -> There are several good graphical methods for assessing the homoscedasticity etc.
# of the residuals!

sum((residuals(out_UK_Seatbelt)[134:192])^2) / sum((residuals(out_UK_Seatbelt)[13:72])^2)

gqtest(model_UK_Seatbelt, point = 0.5, fraction = 0, alternative = "two.sided", data = list())

jarque.bera.test(na.omit(out_UK_Seatbelt$v/sqrt(t(out_UK_Seatbelt$F))))
shapiro.test(rstandard(out_UK_Seatbelt))


                #### Combined UK Seatbelt Model in Matrix Form ####

# Setting the system matrices

Zt <- array(0, dim = c(1, 14, 192))
Zt[1, 1:2, ] <- 1
Zt[1, 13, ] <- UK_Seatbelt_Data[ , "Petrol Price"]
Zt[1, 14, ] <- Intervention_Dummy

Ht <- matrix(NA)

Tt <- matrix(0, 14, 14)
Tt[1, 1] <- 1
Tt[2, 2:12] <- -1
Tt[3:12, 2:11] <- diag(10)
Tt[13:14, 13:14] <- diag(2)

Rt <- matrix(0, 14, 2)
Rt[1:2, 1:2] <- diag(2)

Qt <- matrix(c(NA, 0, 0, NA), 2, 2)

a1 <- matrix(0, 14, 1)
P1 <- matrix(0, 14, 14)
P1inf <- diag(14)

# Defining the stochastic local level model with explanatory variable

model_UK_Seatbelt <- SSModel(UK_Seatbelt_Data[, "Drivers KSI"] ~ -1
                             + SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt,
                                         a1 = a1, P1 = P1, P1inf = P1inf,
                                         state_names = c("Level", "Seasonal Dummy 1",
                                                         "Seasonal Dummy 2", "Seasonal Dummy 3",
                                                         "Seasonal Dummy 4", "Seasonal Dummy 5",
                                                         "Seasonal Dummy 6", "Seasonal Dummy 7",
                                                         "Seasonal Dummy 8", "Seasonal Dummy 9",
                                                         "Seasonal Dummy 10", "Seasonal Dummy 11",
                                                         "Petrol Price", "Intervention Seatbelt Law")),
                             H = Ht)

# Fitting the model

fit_UK_Seatbelt <- fitSSM(model_UK_Seatbelt, inits = c(0, 0, 0), method = "BFGS")

exp(fit_UK_Seatbelt$optim.out$par)

# Filtering and smoothing recursions

out_UK_Seatbelt <- KFS(fit_UK_Seatbelt$model)



# ------------------------------ Kellogg's Stock Price ---------------------------------


# Creating a time series object or ts object - Kellog stock price

Kellogg_Stock_Price[, "date"] <- as.character(Kellogg_Stock_Price[, "date"])

Kellogg_Stock_Price <- as.timeSeries(Kellogg_Stock_Price[ , c("date", "prc")], format = "%Y/%m/%d")

# Creating a time series object or ts object - CRSP excess market return

FF_Factors[, "date"] <- as.character(FF_Factors[, "date"])

Mktr <- as.timeSeries(FF_Factors[ , c("date", "mktrf")], format = "%Y/%m/%d")

range(time(Mktr))

Mktr <- window(Mktr, start = "2000-01-03", end = "2017-12-29")

# Log-transformation

Kellogg_Stock_Price <- log(Kellogg_Stock_Price)

Mktr <- log(Mktr + 1)   # The plus one is added to transform simple into continuous returns


# Plot of Kellogg's stock price and CRSP (excess) market return

plot(cbind(Kellogg_Stock_Price, Mktr))


# Defining the state space model

model_Kellogg <- SSModel(Kellogg_Stock_Price ~
                               SSMtrend(degree = 1, Q = NA)
                             + SSMregression(~ Mktr, Q = 0),
                             H = NA)

summary(model_Kellogg)

# Fitting the model, i.e. estimating the parameters of the defined model

fit_Kellogg <- fitSSM(model_Kellogg, inits = c(0, 0), method = "BFGS")

fit_Kellogg$optim.out$convergence

exp(fit_Kellogg$optim.out$par)

# Filtering and smoothing recursions

out_Kellogg <- KFS(fit_Kellogg$model)

out_Kellogg$logLik / nrow(Kellogg_Stock_Price)

# Ploting the estimated model (filtered and smoothed estimates)

smoothed_signal_Kellogg <- signal(out_Kellogg, states = "all", filtered = F)
filtered_signal_Kellogg <- signal(out_Kellogg, states = "all", filtered = T)

plot(cbind(Kellogg_Stock_Price, smoothed_signal_Kellogg$signal), plot.type = "single",
     main = "Kellogg's Stock price - State Space Model",
     col = c(1, "dodgerblue4"), lty = c("solid", "longdash"),
     lwd = c(1, 2), ylab = "Combined Stochastic Trend and Regression Component")


legend(x = 3200, y = 2, legend = c("Kellogg", "One-Step-Ahead Predictions", "Smoothed Estimates"),
       col = c(1, "orangered3", "dodgerblue4"), lty = c("solid", "longdash", "dotdash"),
       lwd = c(1, 1, 2), cex = 0.8)


plot(cbind(as.ts(log(Kellogg_Stock_Price$prc[1000:1100])), out_Kellogg$att[1000:1100, -2],
           out_Kellogg$alphahat[1000:1100, -2]),
     plot.type = "single", main = "Kellogg's Stock Price",  col = c(1, "orangered3", "dodgerblue4"),
     lty = c("solid", "longdash", "dotdash"), lwd = c(1, 1, 2),
     ylab = "Stock Price")

legend(x = 70, y = 3.66, legend = c("Kellogg", "One-Step-Ahead Predictions", "Smoothed Estimates"),
       col = c(1, "orangered3", "dodgerblue4"), lty = c("solid", "longdash", "dotdash"),
       lwd = c(1, 1, 2), cex = 0.8)

# Information criteria

AIC_Kellogg <- (1 / nrow(Kellogg_Stock_Price)) * (-2 * nrow(Kellogg_Stock_Price)
                                          * (out_Kellogg$logLik / nrow(Kellogg_Stock_Price))
                                          + 2 * (2 + 2))

BIC_Kellogg

# Diagnostics

Residuals_Kellogg <- Kellogg_Stock_Price - fitted(out_Kellogg)
colnames(Residuals_Kellogg) <- "Residuals"

plot(Residuals_Kellogg)

plot(rstandard(out_Kellogg))          # Standardised residuals

Box.test(rstandard(out_Kellogg), lag = 15, type = "Ljung-Box", fitdf = 2)

acf(Residuals_Kellogg)

# H-test is missing -> There are several good graphical methods for assessing the homoscedasticity etc.
# of the residuals!

sum((residuals(out_UK_Seatbelt)[134:192])^2) / sum((residuals(out_UK_Seatbelt)[13:72])^2)

gqtest(model_UK_Seatbelt, point = 0.5, fraction = 0, alternative = "two.sided", data = list())

jarque.bera.test(na.omit(rstandard(out_Kellogg)))
shapiro.test(rstandard(out_Kellogg))


