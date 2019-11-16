#                   ############################################

#                   Master's Thesis - Matthieu Nicolas Rüttimann
#                   
#                   ############################################
#                   ######## State Space Model - Final #########
#                   ############################################

# ------------------------------------ Packages ---------------------------------------

library(pryr)
library(tidyverse)
library(reshape2)
library(data.table)
library(stargazer)
library(KFAS)
library(gridExtra)
library(ggExtra)
library(ggpubr)
library(plotly)
library(RColorBrewer)

# -------------------------------- State Space Model ----------------------------------

# Create a small trial sample

stocks_sample <- sample(Stocks$Permno, size = 20, replace = F)

stocks_sample <- Stocks[Permno %in% stocks_sample]

# Sample batches for estimation procedure

# Create sample for first 200 stocks

# stocks_sample <- unique(Stocks$Permno)[1:200]

# stocks_sample <- unique(Stocks$Permno)[201:400]

# stocks_sample <- unique(Stocks$Permno)[401:600]

# stocks_sample <- unique(Stocks$Permno)[601:800]

# stocks_sample <- unique(Stocks$Permno)[801:1000]

# stocks_sample <- unique(Stocks$Permno)[1001:1200]

# stocks_sample <- unique(Stocks$Permno)[1201:1400]

# stocks_sample <- unique(Stocks$Permno)[1401:1600]

# stocks_sample <- unique(Stocks$Permno)[1601:1800]

# stocks_sample <- unique(Stocks$Permno)[1801:2000]

# stocks_sample <- unique(Stocks$Permno)[2001:2200]

# stocks_sample <- unique(Stocks$Permno)[2201:2400]

# stocks_sample <- unique(Stocks$Permno)[2401:2600]

# stocks_sample <- unique(Stocks$Permno)[2601:2800]

# stocks_sample <- unique(Stocks$Permno)[2801:3000]

# stocks_sample <- unique(Stocks$Permno)[3001:3200]

# stocks_sample <- unique(Stocks$Permno)[3201:3400]

# stocks_sample <- unique(Stocks$Permno)[3401:3600]

# stocks_sample <- unique(Stocks$Permno)[3601:3800]

# stocks_sample <- unique(Stocks$Permno)[3801:4000]

# stocks_sample <- unique(Stocks$Permno)[4001:4200]

# stocks_sample <- unique(Stocks$Permno)[4201:4400]

# stocks_sample <- unique(Stocks$Permno)[4401:4600]

# stocks_sample <- unique(Stocks$Permno)[4601:4800]

# stocks_sample <- unique(Stocks$Permno)[4801:5000]

# stocks_sample <- unique(Stocks$Permno)[5001:5200]

# stocks_sample <- unique(Stocks$Permno)[5201:5400]

# stocks_sample <- unique(Stocks$Permno)[5401:5600]

# stocks_sample <- unique(Stocks$Permno)[5601:5800]

# stocks_sample <- unique(Stocks$Permno)[5801:6000]

# stocks_sample <- unique(Stocks$Permno)[6001:6200]

# stocks_sample <- unique(Stocks$Permno)[6201:6400]

# stocks_sample <- unique(Stocks$Permno)[6401:6600]

# stocks_sample <- unique(Stocks$Permno)[6601:6800]

# stocks_sample <- unique(Stocks$Permno)[6801:7000]

# stocks_sample <- unique(Stocks$Permno)[7001:7200]

# stocks_sample <- unique(Stocks$Permno)[7201:7400]

# stocks_sample <- unique(Stocks$Permno)[7401:7600]

# stocks_sample <- unique(Stocks$Permno)[7601:7800]

# stocks_sample <- unique(Stocks$Permno)[7801:8000]

# stocks_sample <- unique(Stocks$Permno)[8001:8200]

# stocks_sample <- unique(Stocks$Permno)[8201:8400]

# stocks_sample <- unique(Stocks$Permno)[8401:8600]

# stocks_sample <- unique(Stocks$Permno)[8601:8800]

# stocks_sample <- unique(Stocks$Permno)[8801:9000]

# stocks_sample <- unique(Stocks$Permno)[9001:9200]

# stocks_sample <- unique(Stocks$Permno)[9201:9400]

# stocks_sample <- unique(Stocks$Permno)[9401:9600]

stocks_sample <- unique(Stocks$Permno)[9601:9668]

stocks_sample <- Stocks[Permno %in% stocks_sample]


# ----------------------------- State Space Model Function -------------------------------

SSMestimation <- function(data) {
  
  # Print permno number for following information on conversion of algorithm
  
  print(dplyr::distinct(data, Permno))
  
  # Cast time series of stock prices and lag it, since the SSM requires
  # a lag in the dependent variable
  
  stock_prices <- dplyr::lag(data$Price)
  
  # ------------------------------- SSM Matrices ---------------------------------
  
  Zt <- matrix(c(1, 1, rep(0, 17)), 1, 19)
  
  Ht <- matrix(0)
  
  # Set system matrix Tt
  
  Tt <- array(diag(19), dim = c(19, 19, length(stock_prices)))
  Tt[1, 3, ]  <- 1
  Tt[1, 4, ]  <- data$MKTRF
  Tt[1, 5, ]  <- data$SMB
  Tt[1, 6, ]  <- data$HML
  Tt[1, 7, ]  <- data$UMD
  Tt[1, 8, ]  <- data$`Informed ETF Flows`                        # Change from stock to stock
  Tt[1, 9, ]  <- dplyr::lag(data$`Informed ETF Flows`)            # Change from stock to stock
  Tt[1, 10, ] <- dplyr::lag(data$`Informed ETF Flows`, n = 2)     # Change from stock to stock
  Tt[2, 2, ]  <- 0
  Tt[2, 11, ] <- dplyr::lag(data$`1/Price`)                       # Change from stock to stock
  Tt[2, 12, ] <- dplyr::lag(data$`Log(Market Cap)`)               # Change from stock to stock
  Tt[2, 13, ] <- dplyr::lag(data$`Bid-Ask Spread`)                # Change from stock to stock
  Tt[2, 14, ] <- dplyr::lag(data$Amihud)                          # Change from stock to stock
  Tt[2, 15, ] <- dplyr::lag(data$`Log(Volume)`)                   # Change from stock to stock
  Tt[2, 16, ] <- dplyr::lag(data$Volatility)                      # Change from stock to stock
  Tt[2, 17, ] <- data$`Uninformed ETF Flows`                      # Change from stock to stock
  Tt[2, 18, ] <- dplyr::lag(data$`Uninformed ETF Flows`)          # Change from stock to stock
  Tt[2, 19, ] <- dplyr::lag(data$`Uninformed ETF Flows`, n = 2)   # Change from stock to stock
  
  Tt[is.na(Tt)] <- 0
  
  # Selection matrix
  
  Rt <- matrix(0, 19, 2)
  Rt[1, 1] <- 1
  Rt[2, 2] <- 1
  
  # Efficient price and pricing error are not correlated here (could theoretically be estimated from data)
  
  # Qt <- matrix(NA, 2, 2)
  Qt <- matrix(0, 2, 2)
  Qt[1, 1] <- NA
  Qt[2, 2] <- NA
  
  a1 <- matrix(0, 19, 1)
  P1 <- matrix(0, 19, 19)
  P1inf <- diag(19)
  
  # Setting the model components with the logarithm of stock prices as dependent
  # variables
  
  # ------------------------------- SSM Model ---------------------------------
  
  model <- SSModel(log(stock_prices) ~
                     -1 + SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt,
                                    a1 = a1, P1 = P1, P1inf = P1inf,
                                    state_names = c("Efficient Price",
                                                    "Pricing Error",
                                                    "Intercept", "MKT", "SMB", "HML", "UMD",
                                                    "Informed ETF Flows", "Informed ETF Flows_t-1",
                                                    "Informed ETF Flows_t-2",
                                                    "1/Price", "Log(Market Cap)", "Bid-Ask Spread", "Amihud",
                                                    "Log(Volume)", "Volatility",
                                                    "Uninformed ETF Flows", "Uninformed ETF Flows_t-1",
                                                    "Uninformed ETF Flows_t-2"
                                    )
                     ),
                   H = Ht)
  
  # ------------------------------- SSM Estimation ---------------------------------
  
  # Fitting the model (estimating the parameters)
  
  # system.time(
  #   fit_model <- fitSSM(model, inits = c(rep(0, times = 2)), method = "BFGS")
  # )
  
  print(
    system.time(
      fit_model <- fitSSM(model, inits = c(rep(0, times = 2)), method = "L-BFGS-B")
    )
  )
  
  print(fit_model$optim.out$convergence)
  
  if (fit_model$optim.out$convergence != 0) {
    print("Warning: MLE did not converge!")
  }
  
  print(fit_model$model["Q"])
  
  # Filtering and smoothing recursions
  
  print(
    system.time(
      out_model <- KFS(fit_model$model)
    )
  )
  
  # Returning model estimates
  
  return(out_model)
  
}


# ------------------------------ Stock-Wise SSM Estimation ---------------------------------

# Estimate model separately for each stock

system.time(
  
  SSM_estimates <- stocks_sample %>%
    group_by(Permno) %>%
    nest() %>%
    mutate(model = map(data, SSMestimation)) %>%
    select(-data)
  
)


# ------------------------------ SSM Parameter Extraction ---------------------------------

# Parameter extraction function for KFS objects

SSM_param_extract <- function(KFS) {
  
  # Setting dimensions of SSM
  
  p <- KFS$dims$p
  m <- KFS$dims$m
  n <- KFS$dims$n
  pdiag <- 1 + 0:(p - 1) * (p + 1)
  mdiag <- 1 + 0:(m - 1) * (m + 1)
  
  # Extracting parameter estimates and corresponding standard errors
  
  SSM_parameters <- cbind(KFS$alphahat[n, ], sqrt(KFS$V[ , , n][mdiag]))
  
  # Computing t-stats
  
  SSM_parameters <- cbind(SSM_parameters, SSM_parameters[ , 1] / SSM_parameters[ , 2])
  
  colnames(SSM_parameters) <- c("Estimate", "Std. Error", "t value")
  
  # Adding significance level as a new column
  
  SSM_parameters <- as.data.frame(SSM_parameters)
  
  SSM_parameters$Significance <- if_else(abs(SSM_parameters$`t value`) > 2.576, "***",
                                         if_else(abs(SSM_parameters$`t value`) > 1.96, "**",
                                                 if_else(abs(SSM_parameters$`t value`) > 1.645, "*", "")
                                                 )
                                         )
  
  # Returning SSM parameter estimates as a data.frame
  
  return(SSM_parameters)
  
}

# Extracting estimated parameters of all SSMs

SSM_estimates_parameters <- SSM_estimates %>%
  mutate(parameters = map(model, SSM_param_extract))

# Storing estimation results batch-wise

# save(SSM_estimates_parameters,
#      file = "~/Desktop/HSG/Master's Thesis/ETFs and Systemic Risk/Data/R Data/SSM_Estimates_1-200.RData"
#      )

# Extrating each parameter estimate separately into a data.frame

df_efficient_price <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[1, 1]}),
            `Std. Error` = map(parameters, function(i) {i[1, 2]}),
            `t value`    = map(parameters, function(i) {i[1, 3]}),
            Significance = map(parameters, function(i) {i[1, 4]})
            ) %>%
  unnest()

df_pricing_error <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[2, 1]}),
            `Std. Error` = map(parameters, function(i) {i[2, 2]}),
            `t value`    = map(parameters, function(i) {i[2, 3]}),
            Significance = map(parameters, function(i) {i[2, 4]})
            ) %>%
  unnest()

df_intercept <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[3, 1]}),
            `Std. Error` = map(parameters, function(i) {i[3, 2]}),
            `t value`    = map(parameters, function(i) {i[3, 3]}),
            Significance = map(parameters, function(i) {i[3, 4]})
  ) %>%
  unnest()

df_mkt <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[4, 1]}),
            `Std. Error` = map(parameters, function(i) {i[4, 2]}),
            `t value`    = map(parameters, function(i) {i[4, 3]}),
            Significance = map(parameters, function(i) {i[4, 4]})
  ) %>%
  unnest()

df_smb <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[5, 1]}),
            `Std. Error` = map(parameters, function(i) {i[5, 2]}),
            `t value`    = map(parameters, function(i) {i[5, 3]}),
            Significance = map(parameters, function(i) {i[5, 4]})
  ) %>%
  unnest()

df_hml <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[6, 1]}),
            `Std. Error` = map(parameters, function(i) {i[6, 2]}),
            `t value`    = map(parameters, function(i) {i[6, 3]}),
            Significance = map(parameters, function(i) {i[6, 4]})
  ) %>%
  unnest()

df_umd <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[7, 1]}),
            `Std. Error` = map(parameters, function(i) {i[7, 2]}),
            `t value`    = map(parameters, function(i) {i[7, 3]}),
            Significance = map(parameters, function(i) {i[7, 4]})
  ) %>%
  unnest()

df_informed_ETF_flows <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[8, 1]}),
            `Std. Error` = map(parameters, function(i) {i[8, 2]}),
            `t value`    = map(parameters, function(i) {i[8, 3]}),
            Significance = map(parameters, function(i) {i[8, 4]})
  ) %>%
  unnest()

df_informed_ETF_flows_t1 <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[9, 1]}),
            `Std. Error` = map(parameters, function(i) {i[9, 2]}),
            `t value`    = map(parameters, function(i) {i[9, 3]}),
            Significance = map(parameters, function(i) {i[9, 4]})
  ) %>%
  unnest()

df_informed_ETF_flows_t2 <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[10, 1]}),
            `Std. Error` = map(parameters, function(i) {i[10, 2]}),
            `t value`    = map(parameters, function(i) {i[10, 3]}),
            Significance = map(parameters, function(i) {i[10, 4]})
  ) %>%
  unnest()

df_price_inverse <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[11, 1]}),
            `Std. Error` = map(parameters, function(i) {i[11, 2]}),
            `t value`    = map(parameters, function(i) {i[11, 3]}),
            Significance = map(parameters, function(i) {i[11, 4]})
  ) %>%
  unnest()

df_mc_log <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[12, 1]}),
            `Std. Error` = map(parameters, function(i) {i[12, 2]}),
            `t value`    = map(parameters, function(i) {i[12, 3]}),
            Significance = map(parameters, function(i) {i[12, 4]})
  ) %>%
  unnest()

df_bid_ask <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[13, 1]}),
            `Std. Error` = map(parameters, function(i) {i[13, 2]}),
            `t value`    = map(parameters, function(i) {i[13, 3]}),
            Significance = map(parameters, function(i) {i[13, 4]})
  ) %>%
  unnest()

df_amihud <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[14, 1]}),
            `Std. Error` = map(parameters, function(i) {i[14, 2]}),
            `t value`    = map(parameters, function(i) {i[14, 3]}),
            Significance = map(parameters, function(i) {i[14, 4]})
  ) %>%
  unnest()

df_volume_log <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[15, 1]}),
            `Std. Error` = map(parameters, function(i) {i[15, 2]}),
            `t value`    = map(parameters, function(i) {i[15, 3]}),
            Significance = map(parameters, function(i) {i[15, 4]})
  ) %>%
  unnest()

df_volatility <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[16, 1]}),
            `Std. Error` = map(parameters, function(i) {i[16, 2]}),
            `t value`    = map(parameters, function(i) {i[16, 3]}),
            Significance = map(parameters, function(i) {i[16, 4]})
  ) %>%
  unnest()

df_uninformed_ETF_flows <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[17, 1]}),
            `Std. Error` = map(parameters, function(i) {i[17, 2]}),
            `t value`    = map(parameters, function(i) {i[17, 3]}),
            Significance = map(parameters, function(i) {i[17, 4]})
  ) %>%
  unnest()

df_uninformed_ETF_flows_t1 <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[18, 1]}),
            `Std. Error` = map(parameters, function(i) {i[18, 2]}),
            `t value`    = map(parameters, function(i) {i[18, 3]}),
            Significance = map(parameters, function(i) {i[18, 4]})
  ) %>%
  unnest()

df_uninformed_ETF_flows_t2 <- SSM_estimates_parameters %>%
  select(parameters) %>%
  transmute(Estimate     = map(parameters, function(i) {i[19, 1]}),
            `Std. Error` = map(parameters, function(i) {i[19, 2]}),
            `t value`    = map(parameters, function(i) {i[19, 3]}),
            Significance = map(parameters, function(i) {i[19, 4]})
  ) %>%
  unnest()

# Save parameter results into a tibble with list columns for better access

SSM_results <- tibble(Name       = c("Efficient Price", "Pricing Error", "Intercept", "MKT", "SMB", 
                                     "HML", "UMD", "Informed ETF Flows", "Informed ETF Flows_t-1",
                                     "Informed ETF Flows_t-2", "1/Price", "Log(Market Cap)",
                                     "Bid-Ask Spread", "Amihud", "Log(Volume)", "Volatility",
                                     "Uninformed ETF Flows", "Uninformed ETF Flows_t-1",
                                     "Uninformed ETF Flows_t-2"),
                      Parameters = list(df_efficient_price, df_pricing_error, df_intercept,
                                        df_mkt, df_smb, df_hml, df_umd, df_informed_ETF_flows,
                                        df_informed_ETF_flows_t1, df_informed_ETF_flows_t2,
                                        df_price_inverse, df_mc_log, df_bid_ask, df_amihud,
                                        df_volume_log, df_volatility, df_uninformed_ETF_flows,
                                        df_uninformed_ETF_flows_t1, df_uninformed_ETF_flows_t2)
                      )

# Rename SSM parameter results and load previous results table

SSM_results_2 <- SSM_results

load("~/Desktop/HSG/Master's Thesis/ETFs and Systemic Risk/Data/R Data/SSM_results.RData")

# Pasting results together and extending them with each estimation batch

SSM_results <- SSM_results %>% add_column(SSM_results_2$Parameters)

colnames(SSM_results)[3] <- "Parameters_2"

SSM_results <- SSM_results %>%
  mutate(Parameters = map2(Parameters, Parameters_2, rbind)) %>%
  select(-Parameters_2)

# Storing estimation results batch-wise

save(SSM_results,
     file = "~/Desktop/HSG/Master's Thesis/ETFs and Systemic Risk/Data/R Data/SSM_results.RData"
     )

# Summary statistics of SSM parameters

summary(df_mkt)


# ------------------------------------- SSM Results ----------------------------------------

# Results distribution table (with distribution of parameter estimates)

SSM_results_dist <- unnest(SSM_results) %>%
  group_by(Name) %>%
  summarise("Min"     = min(Estimate),
            "1st Qu." = quantile(Estimate, probs = 0.25),
            "Median"  = median(Estimate),
            "Mean"    = mean(Estimate),
            "3rd Qu." = quantile(Estimate, probs = 0.75),
            "Max"     = max(Estimate),
            "SD"      = sd(Estimate),
            "Var"     = var(Estimate)
            )

# Adding significance to the results distribution table

SSM_results_sign <- unnest(SSM_results) %>% group_by(Name) %>% count(Significance, name = "Count")

# SSM_results_sign_NA <- SSM_results_sign %>%
#   filter(is.na(Significance)) %>%
#   select(Name, Count) %>%
#   rename("NA" = Count)

SSM_results_sign_NS <- SSM_results_sign %>%
  filter(Significance == "") %>%
  select(Name, Count) %>%
  rename(NS = Count)

SSM_results_sign_1 <- SSM_results_sign %>%
  filter(Significance == "*") %>%
  select(Name, Count) %>%
  rename("*" = Count)

SSM_results_sign_2 <- SSM_results_sign %>%
  filter(Significance == "**") %>%
  select(Name, Count) %>%
  rename("**" = Count)

SSM_results_sign_3 <- SSM_results_sign %>%
  filter(Significance == "***") %>%
  select(Name, Count) %>%
  rename("***" = Count)

SSM_results_sign <- full_join(SSM_results_dist, SSM_results_sign_NS, by = "Name")

SSM_results_sign <- full_join(SSM_results_sign, SSM_results_sign_1, by = "Name")

SSM_results_sign <- full_join(SSM_results_sign, SSM_results_sign_2, by = "Name")

SSM_results_dist <- full_join(SSM_results_sign, SSM_results_sign_3, by = "Name")

# Stargazer table for SSM parameter estimates

stargazer(SSM_results_dist, title = "State Space Parameter Estimates",
          summary = F, column.sep.width = "-10pt", font.size = "small", digits = 3,
          align = T, add.lines = T, type = "latex")


# ---------------------------------- SSM Result Plots --------------------------------------

# Parameter estimate point plot function

plot_SSM_param <- function(data, subtitle = "") {
  
  data$Stock <- 1:nrow(data)
  
  p_SSM_param <- ggplot(data = data, aes(x = Estimate, y = Stock, col = Significance, shape = Significance)
                        ) +
    geom_point() +
    # geom_density(alpha = 0.1) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(title    = "SSM Parameter Estimates",
         subtitle = subtitle,
         x        = "Estimates",
         y        = "Stock Time Series"
         ) +
    scale_color_manual(values = c("gray80", "lightskyblue2", "deepskyblue3", "forestgreen")) +
    scale_shape_manual(values = c(1, 3, 18, 19))
  # + coord_cartesian(xlim = c(-10, 10))
  
  p_SSM_param <- ggMarginal(p_SSM_param, type = "histogram",
                            size = 10,
                            groupColour = T,
                            xparams = list(bins = 50),
                            yparams = list(bins = 50),
                            )
  
  # TODO(): Return ggplot or plotly object?

  return(p_SSM_param)
  
  # return(ggplotly(p_SSM_param))
  
}

# Parameter estimate density plot function

plot_SSM_param <- function(data, title = "SSM Parameter Estimates", subtitle = "") {
  
  # Computing the median and the corresponding 95%-confidence interval
  
  median <- median(data$Estimate, na.rm = T)
  
  median_conf_inter_l <- quantile(data$Estimate, probs = 0.475)
  
  median_conf_inter_u <- quantile(data$Estimate, probs = 0.525)
  
  median_col <- brewer.pal(n = 11, name = "RdYlBu")[2]
  
  # Computing the mean and the corresponding 95%-confidence interval
  
  mean   <- mean(data$Estimate, na.rm = T)
  
  mean_se <- sd(data$Estimate) / sqrt(length(data$Estimate))
  
  mean_conf_inter_l <- mean - 1.96 * mean_se
  
  mean_conf_inter_u <- mean + 1.96 * mean_se
  
  mean_col <- brewer.pal(n = 11, name = "RdYlBu")[9]
  
  # Prepare text box with median, mean, and confidence interval values
  
  text   <- c(paste0("Median = ", round(median, digits = 3)),
              paste0("95%-CI: [", round(median_conf_inter_l, digits = 3), ", ",
                     round(median_conf_inter_u, digits = 3), "]"),
              paste0("Mean = ", round(mean, digits = 3)),
              paste0("95%-CI: [", round(mean_conf_inter_l, digits = 3), ", ",
                     round(mean_conf_inter_u, digits = 3), "]")
              )
  
  text_position_x <- min(data$Estimate) + 0.15 * (max(data$Estimate) - min(data$Estimate))
  
  # Parameter density plot 
  
  p <- ggplot(data, aes(x = Estimate)) +
  stat_density(geom = "line", aes(y = ..scaled..), size = 0.5, col = "black", adjust = 1) +
  annotate("text", x = text_position_x, y = c(0.8, 0.717, 0.633, 0.55),
           # color = c(rep(mean_col, 2), rep(median_col, 2)),
           label = text, size = 3.5
           ) +
  labs(title    = title,
       # subtitle = subtitle,
       x        = "Estimate",
       y        = "Density"
       ) +
    theme_bw()
  
  # Determine y-axis density position of median, mean, and confidence intervals
  
  filter_tol <- sum(abs(range(data$Estimate))) / 1000
  
  median_pos_y <- ggplot_build(p)$data[[1]] %>%
    filter(near(x, median, tol = filter_tol)) %>%
    pull(ndensity)
  
  median_conf_inter_l_pos_y <- ggplot_build(p)$data[[1]] %>%
    filter(near(x, median_conf_inter_l, tol = filter_tol)) %>%
    pull(ndensity)
  
  median_conf_inter_u_pos_y <- ggplot_build(p)$data[[1]] %>%
    filter(near(x, median_conf_inter_u, tol = filter_tol)) %>%
    pull(ndensity)
  
  df_median_conf <- data.frame(x = c(median_conf_inter_l, median_conf_inter_u),
                               y = c(median_conf_inter_l_pos_y, median_conf_inter_u_pos_y))
  
  mean_pos_y <- ggplot_build(p)$data[[1]] %>%
    filter(near(x, mean, tol = filter_tol)) %>%
    pull(ndensity)
  
  mean_conf_inter_l_pos_y <- ggplot_build(p)$data[[1]] %>%
    filter(near(x, mean_conf_inter_l, tol = filter_tol)) %>%
    pull(ndensity)
  
  mean_conf_inter_u_pos_y <- ggplot_build(p)$data[[1]] %>%
    filter(near(x, mean_conf_inter_u, tol = filter_tol)) %>%
    pull(ndensity)
  
  df_mean_conf <- data.frame(x = c(mean_conf_inter_l, mean_conf_inter_u),
                             y = c(mean_conf_inter_l_pos_y, mean_conf_inter_u_pos_y))
  
  # Add median and mean lines and points and 95%-confidence intervals to the plot
  
  p <- p +
    geom_segment(x = median, xend = median,
                 y = 0, yend = median_pos_y,
                 linetype = "solid", color = mean_col, size = 0.4) +
    geom_segment(x = mean, xend = mean,
                 y = 0, yend = mean_pos_y,
                 linetype = "solid", color = median_col, size = 0.4) +
    geom_point(aes(x = median),
               y = median_pos_y,
               color = mean_col, size = 2) +
    geom_point(aes(x = mean),
               y = mean_pos_y,
               color = median_col, size = 2) +
    # geom_segment(x = median_conf_inter_l, xend = median_conf_inter_l,
    #              y = 0, yend = median_conf_inter_l_pos_y,
    #              color = mean_col, linetype = "dotted", size = 0.1, alpha = 0.25) +
    # geom_segment(x = median_conf_inter_u, xend = median_conf_inter_u,
    #              y = 0, yend = median_conf_inter_u_pos_y,
    #              color = mean_col, linetype = "dotted", size = 0.1, alpha = 0.25) +
    # geom_segment(x = mean_conf_inter_l, xend = mean_conf_inter_l,
    #              y = 0, yend = mean_conf_inter_l_pos_y,
    #              color = median_col, linetype = "dotted", size = 0.1, alpha = 0.25) +
    # geom_segment(x = mean_conf_inter_u, xend = mean_conf_inter_u,
    #              y = 0, yend = mean_conf_inter_u_pos_y,
    #              color = median_col, linetype = "dotted", size = 0.1, alpha = 0.25) +
    geom_area(data = df_median_conf, aes(x = x, y = y),
              fill = mean_col, alpha = 0.5, col = NA) +
    geom_area(data = df_mean_conf, aes(x = x, y = y),
              fill = median_col, alpha = 0.5, col = NA)
    
  # Return plot
  
  return(p)

}

# Parameter estimate density plot

p1 <- plot_SSM_param(SSM_results$Parameters[[1]], title = expression("Efficient Price"["t"]))

p2 <- plot_SSM_param(SSM_results$Parameters[[2]], title = expression("Pricing Error"["t"]))

p3 <- plot_SSM_param(SSM_results$Parameters[[3]], title = expression("Intercept"["t"]))

p4 <- plot_SSM_param(SSM_results$Parameters[[4]], title = expression("MKT"["t"]))

p5 <- plot_SSM_param(SSM_results$Parameters[[5]], title = expression("SMB"["t"]))

p6 <- plot_SSM_param(SSM_results$Parameters[[6]], title = expression("HML"["t"]))

p7 <- plot_SSM_param(SSM_results$Parameters[[7]], title = expression("UMD"["t"]))

p8 <- plot_SSM_param(SSM_results$Parameters[[8]], title = expression("Informed ETF Flows"["t"]))

p9 <- plot_SSM_param(SSM_results$Parameters[[9]], title = expression("Informed ETF Flows"["t-1"]))

p10 <- plot_SSM_param(SSM_results$Parameters[[10]], title = expression("Informed ETF Flows"["t-2"]))

p11 <- plot_SSM_param(SSM_results$Parameters[[11]], title = expression("1/Price"["t-1"]))

p12 <- plot_SSM_param(SSM_results$Parameters[[12]], title = expression("Log(Market Cap)"["t-1"]))

p13 <- plot_SSM_param(SSM_results$Parameters[[13]], title = expression("Bid-Ask Spread"["t-1"]))

p14 <- plot_SSM_param(SSM_results$Parameters[[14]], title = expression("Amihud"["t-1"]))

p15 <- plot_SSM_param(SSM_results$Parameters[[15]], title = expression("Log(Volume)"["t-1"]))

p16 <- plot_SSM_param(SSM_results$Parameters[[16]], title = expression("Volatility"["t-1"]))

p17 <- plot_SSM_param(SSM_results$Parameters[[17]], title = expression("Uninformed ETF Flows"["t"]))

p18 <- plot_SSM_param(SSM_results$Parameters[[18]], title = expression("Uninformed ETF Flows"["t-1"]))

p19 <- plot_SSM_param(SSM_results$Parameters[[19]], title = expression("Uninformed ETF Flows"["t-2"]))

plot_title <- grid::textGrob("State Space Model: Parameter Distributions \n",
                             gp = grid::gpar(fontsize = 30))

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
             p11, p12, p13, p14, p15, p16, p17, p18, p19,
             ncol = 4,
             top = plot_title
             )

# MKT

df_mkt$Stock <- 1:nrow(df_mkt)

ggplot(df_uninformed_ETF_flows, aes(x = Estimate)) +
  geom_histogram(bins = 100, col = "black", fill = "white") +
  theme_bw()

# Dotplot

ggplot(data, aes(x = Estimate)) + geom_dotplot()



# ------------------------------------ JPM Example -----------------------------------------

# Time series plot for representative stock (JPM)

df_JPM <- Stocks[Permno == 47896, ]

# Smoothed signal and smoothed states

df_JPM$Signal         <- signal(SSM_estimates$model[[1]], states = "all", filtered = F)$signal

df_JPM$EfficientPrice <- signal(SSM_estimates$model[[1]], states = 1, filtered = F)$signal

df_JPM$PricingError   <- signal(SSM_estimates$model[[1]], states = 2, filtered = F)$signal

# filtered_signal_UK <- signal(SSM_estimates$model[[1]], states = "all", filtered = T)

# Setting colours for the plot

col_Efficient_Price      <- brewer.pal(n = 11, name = "RdYlBu")[9]

col_Pricing_Error        <- brewer.pal(n = 11, name = "RdYlBu")[2]

col_Informed_ETF_Flows   <- brewer.pal(n = 11, name = "PiYG")[10]

col_Uninformed_ETF_Flows <- brewer.pal(n = 9, name = "YlOrBr")[6]

# JPM time series plot with stock price and efficient price

p_ts_stock_price <- ggplot(df_JPM, aes(x = Date, y = log(Price))) +
  geom_line(size = 0.5) +
  # geom_point(shape = 4) +
  geom_line(aes(y = EfficientPrice), col = col_Efficient_Price) +
  # geom_point(aes(y = EfficientPrice), col = col_Efficient_Price, shape = 4) +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  labs(title = "JP Morgan (Ticker: JPM)",
       subtitle = "Log Stock Price and Efficient Price",
       x = "", y = "Basis Points") +
  theme_bw()

# JPM time series plot with pricing error

p_ts_pricing_error <- ggplot(df_JPM, aes(x = Date, y = PricingError)) +
  geom_hline(yintercept = 0, size = 1, col = "gray") +
  geom_line(col = col_Pricing_Error) +
  # geom_point(col = col_Pricing_Error, shape = 4) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  labs(subtitle = "Pricing Error",
       x = "", y = "Basis Points") +
  theme_bw()

# JPM time series plot with ETF flows

p_ts_Informed_ETF_Flows <- ggplot(df_JPM, aes(x = Date, y = `Informed ETF Flows`)) +
  geom_hline(yintercept = 0, size = 1, col = "gray") +
  geom_col(col = col_Informed_ETF_Flows) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  labs(subtitle = "Informed ETF Flows",
       x = "", y = "Percentage Points") +
  theme_bw()

p_ts_Uninformed_ETF_Flows <- ggplot(df_JPM, aes(x = Date, y = (`Uninformed ETF Flows` - mean(`Uninformed ETF Flows`)))) +
  geom_hline(yintercept = 0, size = 1, col = "gray") +
  geom_col(col = col_Uninformed_ETF_Flows) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  labs(subtitle = "Uninformed ETF Flows",
       y = "Percentage Points") +
  theme_bw()

# Combined time series plot for JPM

p_ts <- ggarrange(p_ts_stock_price, p_ts_pricing_error,
                  p_ts_Informed_ETF_Flows, p_ts_Uninformed_ETF_Flows,
                  heights = c(4, 1, 1, 1),
                  ncol = 1, nrow = 4)


# --------------------------------- Model Diagnostics --------------------------------------

# Information criteria

AIC <- (1 / SSM_estimates$model[[1]]$dims$n) * (-2 * SSM_estimates$model[[1]]$dims$n
                                                * (SSM_estimates$model[[1]]$logLik / SSM_estimates$model[[1]]$dims$n)
                                                + 2 * (19 + 2))

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



