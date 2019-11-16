#                   ############################################

#                   Master's Thesis - Matthieu Nicolas Rüttimann
#                   
#                   ############################################
#                   # Panel Regressions - Asset Pricing Models #
#                   ############################################


# ------------------------------------ Packages ---------------------------------------

library(microbenchmark)
library(tidyverse)
library(data.table)
library(plm)
library(biglm)
library(lmtest)
library(sandwich)
library(stargazer)


# ------------------------ Return Autocorrelations for Each Year ----------------------

# Function for splitting sample across years

subset.sample.years <- function(x, year) {
  
  subset(x, Date >= paste0(as.character(year), "-01-01")
         & Date < paste0(as.character(year + 1), "-01-01"))
  
}

# Split sample in years and store it in a list. Each list element then represents a
# separate year.

Autocorrelation_Years <- list()

for (i in 1993:2017) {
  
  Autocorrelation_Years[[length(Autocorrelation_Years) + 1]] <- subset.sample.years(
    Stocks[ , c("Date", "Return")], year = i)
  
}

names(Autocorrelation_Years) <- 1993:2017

# Compute the autocorrelations for up to 10 lags for each year separately

Tmp <- lapply(Autocorrelation_Years,
              function(x) acf(x$Return, lag.max = 10, type = "correlation",
                              plot = F, na.action = na.pass,
                              demean = T)$acf[-1]
              )

# Create a data table which holds the autocorrelations across years and lags and fill
# it up

Autocorrelation_Years <- matrix(0, 25, 10)

for (i in 1:length(Tmp)) {
  Autocorrelation_Years[i, ] <- Tmp[[i]]
}

rownames(Autocorrelation_Years) <- 1993:2017

colnames(Autocorrelation_Years) <- 1:10

# Round the results to three digits

Autocorrelation_Years <- round(Autocorrelation_Years, digits = 3)


# -------------------- Return Autocorrelations for Stock Size Bins --------------------

summary(lm(Return ~ -1 + shift(Return), data = Autocorrelation_Years$`1993`))

summary(lm(`ETF Flows` ~ -1 + shift(`ETF Flows`), data = Autocorrelation_ETF_Years$`1996`))


# ------------------------- Autocorrelations for ETF Flows ----------------------------

# Split sample in years and store it in a list. Each list element then represents a
# separate year.

Autocorrelation_ETF_Years <- list()

for (i in 1996:2017) {
  
  Autocorrelation_ETF_Years[[length(Autocorrelation_ETF_Years) + 1]] <- subset.sample.years(
    Stocks[ , c("Date", "ETF Flows")], year = i)
  
}

names(Autocorrelation_ETF_Years) <- 1996:2017

# Compute the autocorrelations for up to 10 lags for each year separately

Tmp <- lapply(Autocorrelation_ETF_Years,
              function(x) acf(x$`ETF Flows`, lag.max = 10, type = "correlation",
                              plot = F, na.action = na.pass,
                              demean = T)$acf[-1]
              )

# Create a data table which holds the autocorrelations across years and lags and fill
# it up

Autocorrelation_ETF_Years <- matrix(0, 22, 10)

for (i in 1:length(Tmp)) {
  Autocorrelation_ETF_Years[i, ] <- Tmp[[i]]
}

rownames(Autocorrelation_ETF_Years) <- 1996:2017

colnames(Autocorrelation_ETF_Years) <- 1:10

# Round the results to three digits

Autocorrelation_ETF_Years <- round(Autocorrelation_ETF_Years, digits = 3)


# ----------------------------- Panel Data Preparation --------------------------------

# Create a pdata.frame object for the panel estimation with plm

Stocks_Panel <- pdata.frame(Stocks, index = c("Permno", "Date"), drop.index = T,
                            row.names = T, drop.NA.series = F, drop.const.series = F,
                            drop.unused.levels = F)

# Reducing the panel data.frame to solely variables needed for the regression models

Stocks_Panel <- Stocks_Panel[ , c("Return", "RF", "MKTRF", "SMB", "HML", "UMD",
                                  "ETF Ownership", "ETF Flows", "Aggregated ETF Flows")]


# --------------------------------- OLS Regressions -----------------------------------

# CAPM regression model for holding-period stock returns

reg_CAPM <- plm((Return - RF) ~ MKTRF,
               data = Stocks_Panel
               )

# Carhart regression model

reg_Carhart <- plm((Return - RF) ~ MKTRF + SMB + HML + UMD,
                  data = Stocks_Panel
                  )

# Carhart regression model with three lags for each factor

reg_Carhart_lagged <- lm((Return - RF) ~
                           MKTRF + shift(MKTRF) + shift(MKTRF, n = 2) + shift(MKTRF, n = 3)
                         + SMB + shift(SMB) + shift(SMB, n = 2) + shift(SMB, n = 3)
                         + HML + shift(HML) + shift(HML, n = 2) + shift(HML, n = 3)
                         + UMD + shift(UMD) + shift(UMD, n = 2) + shift(UMD, n = 3),
                         data = Stocks)



# My asset pricing regressions

regression_ETF <- lm(formula = (Return - RF) ~ MKTRF + SMB + HML + UMD + `ETF Ownership`
                     + shift(`ETF Ownership`) + shift(`ETF Ownership`, n = 2),
                     data = Stocks)

regression_ETF_Lags_1 <- lm(formula = (Return - RF) ~ MKTRF + shift(MKTRF)
                            + SMB + shift(SMB) + HML + shift(HML)
                            + UMD + shift(UMD) + `ETF Ownership`
                            + shift(`ETF Ownership`),
                            data = Stocks)

regression_ETF_Lags_2 <- lm(formula = (Return - RF) ~ MKTRF + shift(MKTRF)
                            + shift(MKTRF, n = 2) + SMB + shift(SMB) + HML + shift(HML)
                            + UMD + shift(UMD) + `ETF Ownership`
                            + shift(`ETF Ownership`)
                            + shift(`ETF Ownership`, n = 2),
                            data = Stocks)


# ----------------- Return Autocorrelations for Entire Sample Period ------------------

# Setting up the autocorrelations table as a data.table object

Autocorrelations_Table <- data.table("Simple Returns" = 1:20, "t-Statistic" = 1:20,
                                     "CAPM Returns" = 1:20, "t-Statistic" = 1:20,
                                     "Carhart Returns" = 1:20, "t-Statistic" = 1:20
                                     )

# Autocorrelations of holding-period stock returns for 20 lags

Autocorrelations_Table[ , 1] <- acf(Stocks$Return, lag.max = 20, type = "correlation",
                                    plot = F, na.action = na.pass, demean = T)$acf[-1]

# Autocorrelations of excess returns (residuals) of the market model (i.e. CAPM) for
# 20 lags

Autocorrelations_Table[ , 3] <- acf(residuals(reg_CAPM), lag.max = 20,
                                    type = "correlation", plot = F, na.action = na.pass,
                                    demean = T)$acf[-1]

# Autocorrelations of excess returns (residuals) of the Carhart four-factor model
# for 20 lags

Autocorrelations_Table[ , 5] <- acf(residuals(reg_Carhart), lag.max = 20,
                                    type = "correlation", plot = F, na.action = na.pass,
                                    demean = T)$acf[-1]

# Robust t-stats, clustered along the group- and time-dimension

t_Stats <- data.table("t-Statistic" = as.numeric(1:20))

for (i in 1:nrow(t_Stats)) {
  
  # Computes autocorrelations for all 20 acf-lags
  reg_ACs <- plm(Return ~ 0 + lag(Return, k = i),
                 data = Stocks_Panel,
                 model = "pooling"
                 )
  
  # Computes double-clustered t-stats and writes results into corresponding t_Stats data.table
  t_Stats[i] <- coeftest(reg_ACs, vcov = vcovDC(reg_ACs, type = "HC0"))[[3]]
  
}

reg_CAPM_res <- residuals(reg_CAPM)

for (i in 1:nrow(t_Stats)) {
  
  # Computes autocorrelations for all 20 acf-lags
  reg_ACs <- plm(x ~ 0 + plm::lag(x, k = i, shift = "row"),
                 data  = reg_CAPM_res,
                 model = "pooling"
                 )
  
  # Computes double-clustered t-stats and writes results into corresponding t_Stats data.table
  t_Stats[i] <- coeftest(reg_ACs, vcov = vcovDC(reg_ACs, type = "HC0"))[[3]]
  
}

Autocorrelations_Table[ , 2] <- t_Stats

# Rounding the values to 3 digits

Autocorrelations_Table <- round(Autocorrelations_Table, digits = 3)



# ---------------------------------- Hausman Test -------------------------------------

phtest(x, data, model = c("within", "random"),
       method = c("chisq", "aux"),
       index = NULL, vcov = NULL, ...)


# ------------------------ Panel Regressions: ETF Ownership ---------------------------

# Fixed effects (within) model with individual-fixed effects:

# Carhart model (only with stock-fixed effects since the time dimension is constant for
# the asset pricing factors)

FE_Reg_Carhart <- plm((Return - RF) ~ MKTRF + SMB + HML + UMD,
                      data = Stocks_Panel,
                      model = "within",
                      effect = "individual"
                      )

FE_Reg_Carhart_t <- coeftest(FE_Reg_Carhart, vcov = vcovDC(FE_Reg_Carhart, type = "HC0"))

# Carhart model and contemporaneous ETF ownership with stock-fixed effects

FE_Reg_ETF_Own <- plm((Return - RF) ~ MKTRF + SMB + HML + UMD
                      + Stocks_Panel$`ETF Ownership`,
                      data = Stocks_Panel,
                      model = "within",
                      effect = "individual"
                      )

FE_Reg_ETF_Own_t <- coeftest(FE_Reg_ETF_Own, vcov = vcovDC(FE_Reg_ETF_Own, type = "HC0"))

# Carhart model and contemporaneous and lagged ETF ownership with stock-fixed effects

FE_Reg_ETF_Own_Lag <- plm((Return - RF) ~ MKTRF + SMB + HML + UMD
                      + Stocks_Panel$`ETF Ownership`
                      + lag(Stocks_Panel$`ETF Ownership`)
                      + lag(Stocks_Panel$`ETF Ownership`, k = 2),
                      data = Stocks_Panel,
                      model = "within",
                      effect = "individual"
                      )

system.time(FE_Reg_ETF_Own_Lag_t <- coeftest(FE_Reg_ETF_Own_Lag, vcov = vcovDC(FE_Reg_ETF_Own_Lag, type = "HC0")))

# Carhart model and ETF ownership variables, all contemporaneous and lagged with stock-fixed effects

FE_Reg_ETF_Own_All <- plm((Return - RF) ~
                            MKTRF + lag(MKTRF)
                          + SMB + lag(SMB)
                          + HML + lag(HML)
                          + UMD + lag(UMD)
                          + Stocks_Panel$`ETF Ownership`
                          + lag(Stocks_Panel$`ETF Ownership`)
                          + lag(Stocks_Panel$`ETF Ownership`, k = 2),
                          data = Stocks_Panel,
                          model = "within",
                          effect = "individual"
                          )

FE_Reg_ETF_Own_All_t <- coeftest(FE_Reg_ETF_Own_All, vcov = vcovDC(FE_Reg_ETF_Own_All, type = "HC0"))


# -------------------------- Panel Regressions: ETF Flows -----------------------------

# Fixed effects (within) model with individual-fixed effects:

# Carhart model and contemporaneous ETF flows with stock-fixed effects

system.time(
FE_Reg_ETF_Flows <- plm((Return - RF) ~ MKTRF + SMB + HML + UMD
                        + Stocks_Panel$`ETF Flows`,
                        data = Stocks_Panel,
                        model = "within",
                        effect = "individual"
                        )
)

system.time(
FE_Reg_ETF_Flows_t <- coeftest(FE_Reg_ETF_Flows, vcov = vcovDC(FE_Reg_ETF_Flows, type = "HC0"))
)

# Carhart model and contemporaneous and lagged ETF flows with stock-fixed effects

system.time(
FE_Reg_ETF_Flows_Lag <- plm((Return - RF) ~ MKTRF + SMB + HML + UMD
                            + Stocks_Panel$`ETF Flows`
                            + plm::lag(Stocks_Panel$`ETF Flows`)
                            + plm::lag(Stocks_Panel$`ETF Flows`, k = 2),
                            data = Stocks_Panel,
                            model = "within",
                            effect = "individual"
                            )
)

system.time(FE_Reg_ETF_Flows_Lag_t <- coeftest(FE_Reg_ETF_Flows_Lag,
                                               vcov = vcovDC(FE_Reg_ETF_Flows_Lag, type = "HC0")))

# Carhart model and ETF flow variables, all contemporaneous and lagged with stock-fixed effects

system.time(
FE_Reg_ETF_Flows_All <- plm((Return - RF) ~
                            MKTRF + lag(MKTRF, shift = "row")
                          + SMB + lag(SMB, shift = "row")
                          + HML + lag(HML, shift = "row")
                          + UMD + lag(UMD, shift = "row")
                          + Stocks_Panel$`ETF Flows`
                          + lag(Stocks_Panel$`ETF Flows`, shift = "row")
                          + lag(Stocks_Panel$`ETF Flows`, shift = "row", k = 2),
                          data = Stocks_Panel,
                          model = "within",
                          effect = "individual"
                          )
)

system.time(
FE_Reg_ETF_Flows_All_t <- coeftest(FE_Reg_ETF_Flows_All, vcov = vcovDC(FE_Reg_ETF_Flows_All, type = "HC0"))
)


# ----------------------------- Panel Regression Tables -------------------------------

# Fixed-Effects Regression Model for ETF Ownership (Individual-Fixed Effects)

stargazer(FE_Reg_Carhart, FE_Reg_ETF_Own, FE_Reg_ETF_Own_Lag, FE_Reg_ETF_Own_All,
          title = "FE Panel Regressions: Stock Returns and ETF Ownership",
          dep.var.labels = "Daily excess returns (r$_{t}$ - r$^{f}_{t}$)",
          covariate.labels = c("MKTRF$_{t}$", "MKTRF$_{t-1}$", "SMB$_{t}$", "SMB$_{t-1}$",
                               "HML$_{t}$", "HML$_{t-1}$", "UMD$_{t}$", "UMD$_{t-1}$",
                               "ETF$_{t}$", "ETF$_{t-1}$", "ETF$_{t-2}$"),
          font.size = "small", align = T, column.sep.width = "-15pt",
          report = "vc*t",
          omit.stat = "f",
          type = "latex"
          )


# ------------------------- Random Effects Model: ETF Flows ---------------------------

# Random effects model with group and time random effects

RE_Regression <- plm(formula = (Return - RF) ~ MKTRF + lag(MKTRF) + lag(MKTRF, k = 2)
                     + SMB + lag(SMB)
                     + HML + lag(HML)
                     + UMD + lag(UMD)
                     + Stocks_Panel$`ETF Flows` + lag(Stocks_Panel$`ETF Flows`)
                     + lag(Stocks_Panel$`ETF Flows`, k = 2),
                     data = Stocks_Panel,
                     effects = "twoways",
                     model = "random")


# ------------------------ Fixed Effects Model: All Variables -------------------------

system.time(
FE_Reg_Full <- plm(formula = (Return - RF) ~ MKTRF + SMB + HML + UMD + Stocks_Panel$`Informed ETF Flows`
                   + Stocks_Panel$`Bid-Ask Spread` + Stocks_Panel$`Log(Market Cap)`
                   + Stocks_Panel$`Log(Volume)` + Stocks_Panel$`1/Price` + Stocks_Panel$Amihud
                   + Stocks_Panel$Volatility + Stocks_Panel$`Uninformed ETF Flows`,
                   data    = Stocks_Panel,
                   model   = "within",
                   effect  = "individual")
)

system.time(
  FE_Reg_Full_Lag <- plm(formula = (Return - RF) ~ MKTRF + SMB + HML + UMD + lag(Stocks_Panel$`Informed ETF Flows`, shift = "row")
                         + lag(Stocks_Panel$`Bid-Ask Spread`, shift = "row") + lag(Stocks_Panel$`Log(Market Cap)`, shift = "row")
                         + lag(Stocks_Panel$`Log(Volume)`, shift = "row") + lag(Stocks_Panel$`1/Price`, shift = "row") + lag(Stocks_Panel$Amihud, shift = "row")
                         + lag(Stocks_Panel$Volatility, shift = "row") + lag(Stocks_Panel$`Uninformed ETF Flows`, shift = "row"),
                         data    = Stocks_Panel,
                         model   = "within",
                         effect  = "individual")
)


# ----------------------- Preliminary Evidence on Price Pressure ----------------------

# Setting common names for the independent variables (for the regression tables in latex)

b <- cbind(a$`Daily ETF Ownership`[-(1:4)], lag.xts(a$`Daily ETF Ownership`[-(1:4)]),
           lag.xts(a$`Daily ETF Ownership`[-(1:4)], k = 2),
           lag.xts(a$`Daily ETF Ownership`[-(1:4)], k = 3))

b <- cbind(a$`Daily ETF Ownership`, lag.xts(a$`Daily ETF Ownership`),
           lag.xts(a$`Daily ETF Ownership`, k = 2),
           lag.xts(a$`Daily ETF Ownership`, k = 3))

colnames(b) <- c("ETF Ownerhsip$_{t}$", "ETF Ownerhsip$_{t-1}$", "ETF Ownerhsip$_{t-2}$",
                 "ETF Ownerhsip$_{t-3}$")

b <- as.data.table(b)

# Preliminary evidence on price pressure from idiosyncratic returns



regression_CAPM_Price_Pressure <- lm(formula = residuals(regression_CAPM_Lagged)
                                     ~ `Daily ETF Ownership`[-(1:4)]
                                     + lag.xts(`Daily ETF Ownership`[-(1:4)])
                                     + lag.xts(`Daily ETF Ownership`[-(1:4)], k = 2)
                                     + lag.xts(`Daily ETF Ownership`[-(1:4)], k = 3),
                                     data = a)

regression_CAPM_Price_Pressure <- lm(formula = residuals(regression_CAPM_Lagged)
                                     ~ b$`ETF Ownerhsip$_{t}$`
                                     + b$`ETF Ownerhsip$_{t-1}$`
                                     + b$`ETF Ownerhsip$_{t-2}$`
                                     + b$`ETF Ownerhsip$_{t-3}$`,
                                     data = b)

# Preliminary evidence on price pressure from Carhart model

regression_Carhart_Price_Pressure <- lm(formula = residuals(regression_Carhart)
                                        ~ `Daily ETF Ownership`
                                        + lag.xts(`Daily ETF Ownership`)
                                        + lag.xts(`Daily ETF Ownership`, k = 2)
                                        + lag.xts(`Daily ETF Ownership`, k = 3),
                                        data = a)

regression_Carhart_Price_Pressure <- lm(formula = residuals(regression_Carhart)
                                        ~ b$`ETF Ownerhsip$_{t}$`
                                        + b$`ETF Ownerhsip$_{t-1}$`
                                        + b$`ETF Ownerhsip$_{t-2}$`
                                        + b$`ETF Ownerhsip$_{t-3}$`,
                                        data = b)


# ------------------------------- Summary Statistics ----------------------------------

# Summary statistics

stargazer(Stocks[ , -c(1:3)], title = "Summary Statistics",
          summary = T, column.sep.width = "-5pt", font.size = "small", digits = 3,
          align = T, add.lines = T, type = "latex", summary.logical = T,
          summary.stat = c("n", "mean", "median", "sd", "min", "p25", "p75", "max"))

# Correlation matrix

Cor_Table_Stocks <- round(cor(Stocks[ , -c(1:3, 8:9, 11)], use = "pairwise.complete.obs",
                              method = "pearson"), digits = 3)

stargazer(Cor_Table_Stocks, title = "Correlation Matrix",
          column.sep.width = "-10pt", font.size = "footnotesize", digits = 3,
          align = T, add.lines = T, type = "latex")


# ----------------------------- Autocorrelation Tables --------------------------------

stargazer(Autocorrelation_Years, title = "Return Autocorrelations Across Sample Years",
          summary = F, column.sep.width = "-10pt", font.size = "small", digits = 3,
          align = T, add.lines = T, type = "latex")

stargazer(Autocorrelation_ETF_Years, title = "ETF Flow Autocorrelations Across Sample Years",
          summary = F, column.sep.width = "-10pt", font.size = "small", digits = 3,
          align = T, add.lines = T, type = "latex")

stargazer(Autocorrelations_Table, title = "Return Autocorrelations for Entire Sample Period",
          summary = F, column.sep.width = "-15pt", font.size = "small", digits = 3,
          align = T, add.lines = T, type = "latex")


# -------------------------------- Regression Tables ----------------------------------

stargazer(FE_Reg_Carhart,
          title = "FE Panel Regressions: Stock Returns and ETF Flows",
          dep.var.labels = "Daily Excess Stock Returns (r$_{it}$ - r$^f_{it}$)",
          font.size = "small", align = T, column.sep.width = "-15pt",
          report = "vc*t",
          omit.stat = "f",
          type = "latex")

# Show t-stats instead of p-values

stargazer(FE_Reg_Carhart, FE_Reg_ETF, regression_ETF_Lags_1, regression_ETF_Lags_2,
          title = "FE Panel Regressions: Stock Returns and ETF Flows",
          dep.var.labels = "Daily Excess Stock Returns (r$_t$ - r$_{ft}$)",
          covariate.labels = c("Market$_{t}$", "Market$_{t-1}$", "Market$_{t-2}$", "SMB$_{t}$",
                               "SMB$_{t-1}$", "HML$_{t}$", "HML$_{t-1}$", "UMD$_{t}$",
                               "UMD$_{t-1}$", "ETF Flows$_{t}$", "ETF Flows$_{t-1}$",
                               "ETF Flows$_{t-2}$"),
          font.size = "small", align = T, column.sep.width = "-15pt",
          report = "vc*t",
          omit.stat = "f",
          type = "latex")

# OLS regression of idiosyncratic excess returns on ETF ownership

stargazer(regression_CAPM_Price_Pressure, regression_Carhart_Price_Pressure,
          title = "OLS Regressions: CAPM and Carhart Four-Factor Daily Excess Returns on ETF Ownership",
          dep.var.labels = c("CAPM Excess Returns", "Carhart Excess Returns"),
          covariate.labels = c("ETF Ownerhsip$_{t}$", "ETF Ownerhsip$_{t-1}$",
                               "ETF Ownerhsip$_{t-2}$", "ETF Ownerhsip$_{t-3}$"),
          font.size = "small", align = T,
          type = "latex")

