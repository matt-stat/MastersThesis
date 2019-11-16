#                   ############################################

#                   Master's Thesis - Matthieu Nicolas Rüttimann
#                   
#                   ############################################
#                   ##########   Computed Variables   ##########
#                   ############################################

# ------------------------------------ Packages ---------------------------------------

library(tidyverse)
library(data.table)
library(pryr)
library(microbenchmark)
library(xts)
library(timeSeries)
library(stargazer)
library(ggExtra)
library(ggpubr)
library(RColorBrewer)


# ---------------------------- ETF-Fund-Level Variables -------------------------------

#                   ############################################
#                   ###########   ETF AUM and TNA   ############
#                   ############################################

# ETF AUM is equal to ETFs' market capitalisation. It is computed from daily ETF prices
# and shares outstanding in the ETF time series data table. ETF AUM is denoted in USD
# millions since shares outstanding from Bloomberg are in units of millions.

ETF_Time_Series <- cbind(ETF_Time_Series,
                         ETF_Time_Series$Price * ETF_Time_Series$`Shares Outstanding`)

colnames(ETF_Time_Series)[15] <- "AUM"

# By using NAV per share, also ETF TNA is copmuted. ETF NAV is only available beginning
# in 1 September 1998 and serves as a cross-check for ETF AUM. It is again denoted in
# USD millions.

ETF_Time_Series <- cbind(ETF_Time_Series,
                         ETF_Time_Series$NAV * ETF_Time_Series$`Shares Outstanding`)

colnames(ETF_Time_Series)[16] <- "TNA"

# It can be quickly checked for which ETFs AUM and TNA greatly differ. For around 1'400
# out of 3'904'232 observations ETF AUM and TNA diverge, so ETF ownership does not have
# to be separately computed with TNA.

subset(ETF_Time_Series, (AUM - TNA) >= 1000)


#                   ############################################
#                   ##########   ETF Capital Flows   ###########
#                   ############################################

# ETF capital flows can be computed from ETFs' daily creation and redemptions, i.e. the
# first difference of their shares outstanding. Hence, the shares outstanding variable
# coming from the ETF Time Series data table is cast back from long to wide format.

ETF_Flows <- ETF_Time_Series[ , c("Date", "CRSP Fundno", "Shares Outstanding")]

ETF_Flows <- ETF_Flows[!is.na(`CRSP Fundno`) & !is.na(`Shares Outstanding`)]

ETF_Flows <- melt(ETF_Flows, id.vars = c("Date", "CRSP Fundno"), measure.vars = "Shares Outstanding",
          value.name = "Shares Outstanding")

ETF_Flows <- as.data.table(dcast(ETF_Flows, Date ~ `CRSP Fundno`,
                                 fun.aggregate = function(x) mean(x, na.rm = T),
                                 value.var = "Shares Outstanding", drop = T))

# Next, daily ETF flows (in percentage) are calculated from first differences of shares
# outstanding and then aggregated to yield average ETF flows for the market on a given
# day.

ETF_Flows <- cbind(ETF_Flows[ , "Date"], as.data.table(apply(ETF_Flows[ , -1], 2,
                                                             function(x) (x - lag.xts(x)) / lag.xts(x))))

ETF_Aggregated_Flows <- cbind(ETF_Flows[ , "Date"], as.data.table(apply(ETF_Flows[ , -1], 1,
                                                                        function(x) mean(x, na.rm = T))))

colnames(ETF_Aggregated_Flows)[2] <- "Aggregated ETF Flows" 


# ------------------------------ Stock-Level Variables --------------------------------

#                   ############################################
#                   #######   Stock Split Adjustments   ########
#                   ############################################

# While stock returns in CRSP are already stock split adjusted, stock prices need to be
# corrected with the Cumulative Factor to Adjust Prices from CRSP.

Stock_Time_Series$Price <- Stock_Time_Series$Price / Stock_Time_Series$Cfacpr

Stock_Time_Series <- Stock_Time_Series[ , c("Date", "Permno", "CUSIP", "Price", "Bid", "Ask",
                                            "Return", "Shares Outstanding", "Volume")]


#                   ############################################
#                   #####   Stock Market Capitalisation   ######
#                   ############################################

# The market capitalisation for each stock at each point in time is calculated and added as
# a new column to the stock data table. Stock market capitalisation is denoted in USD
# millions since shares outstanding were recorded in millions.

Stock_Time_Series <- cbind(Stock_Time_Series,
                           Stock_Time_Series$Price * Stock_Time_Series$`Shares Outstanding`)

colnames(Stock_Time_Series)[10] <- "Market Cap"


#                   ############################################
#                   ########   Asset Pricing Factors   #########
#                   ############################################

# Adding the Fama-French asset pricing factors to the stock time series in a new stock data
# table.

Stock_Time_Series <- subset(Stock_Time_Series, !is.na(Permno) & !is.na(Date))

Stocks <- merge(Stock_Time_Series, FF_Factors, by = "Date", all.x = T)


#                   ############################################
#                   ############   Bid-Ask Spread   ############
#                   ############################################

# The percentage bid-ask spreads are computed from the difference of ask and bid prices, divided
# by the absolute stock price (midquote is taken here, since the CRSP price has already been
# adjusted for stock splits). This yields percentage bid-ask spreads.

Stocks <- cbind(Stocks, (Stocks$Ask - Stocks$Bid) / (0.5 * Stocks$Bid + 0.5 * Stocks$Ask))

colnames(Stocks)[18] <- "Bid-Ask Spread"


#                   ############################################
#                   ############   Price Inverse   #############
#                   ############################################

# Next, the price inverse is added to the stock data table by dividing 1 by the stock price.

Stocks <- cbind(Stocks, 1 / Stocks$Price)

colnames(Stocks)[19] <- "1/Price"


#                   ############################################
#                   ######   Log Market Capitalisation   #######
#                   ############################################

# The natural logarithm of the stock market capitalisation is also added to the stock
# data table, as a control variable for the later analysis. To avoid taking the logarithm
# of zero, a one is added to the market cap. Otherwise, negative infinte values would be
# the result.

Stocks <- cbind(Stocks, log(Stocks$`Market Cap` + 1))

colnames(Stocks)[20] <- "Log(Market Cap)"

# The original share volume variable is transformed from units of one share to a scale of
# million shares.

Stocks$Volume <- Stocks$Volume / 1e+06


#                   ############################################
#                   #######   Log Share Trading Volume   #######
#                   ############################################

# Also the logged share trading volume serves as a control variable. To avoid taking the
# logarithm of zero, a one is added to the volume. Otherwise, negative infinte values
# would be the result.

Stocks <- cbind(Stocks, log(Stocks$Volume + 1))

colnames(Stocks)[21] <- "Log(Volume)"


#                   ############################################
#                   #######   Amihud Illiquidity Ratio   #######
#                   ############################################

# The Amihud (2002) illiquidity measure is estimated by the ratio of absolute daily stock
# return over dollar volume. It roughly measures the price impact per dollar of trading.
# Share volume from CRSP is in number of shares traded on a given day. Thus, it must first
# be converted to daily dollar volume by multiplying with the price variable.

Stocks <- cbind(Stocks, abs(Stocks$Return) / (Stocks$Price * Stocks$Volume))

colnames(Stocks)[22] <- "Amihud"

# Some infinite and NaN values are created in the process. They are replaced by NAs.

Stocks$Amihud[which(is.infinite(Stocks$Amihud))] <- NA

Stocks$Amihud[which(is.nan(Stocks$Amihud))] <- NA


#                   ############################################
#                   ## Daily Volatility at Monthly Frequency ###
#                   ############################################

# Next, daily stock volatility is calculated using a monthly rolling window. The rolling window
# is applied to a period of one month (i.e. 25 days here) and then subsequently shifted forward
# in time by one day. First however, the returns of the different stocks need to be cast in wide
# format.

TmpA <- melt(Stocks[ , c("Date", "Permno", "Return")], id.vars = c("Date", "Permno"),
             measure.vars = "Return")

TmpA <- dcast(TmpA, Date ~ Permno)

# In order to apply the rolling window function, the data is first transformed into a timeSeries
# object. This enables using the built-in rolling window functions of the timeSeries package.

TmpA$Date <- as.character(TmpA$Date)

TmpA <- as.timeSeries(TmpA)

# Now, a monthly rolling window can be applied to calculate past daily volatility with the help
# of the sd function.

TmpA <- rollDailySeries(TmpA, period = "25d", FUN = function(x) apply(x, 2, sd, na.rm = T))

# Quickly checking the plausibility of the results with a function from the xts package (only
# computes end-of-month volatility, but is good enough for a cross-check).

Monthly_Index <- endpoints(TmpA, on = 'months')

period.apply(TmpA, INDEX = Monthly_Index, FUN = function(x) apply(x, 2, sd, na.rm = T))

# The resulting volatility data needs to be cast back from wide to long format.

TmpB <- as.data.table(TmpA)

colnames(TmpB) <- colnames(TmpA)

TmpB <- cbind(as.data.table(time(TmpA)), TmpB)

colnames(TmpB)[1] <- "Date"

TmpB$Date <- as.Date(TmpB$Date)

TmpA <- melt(TmpB, id.vars = "Date", measure.vars = colnames(TmpB)[-1],
             variable.name = "Permno", value.name = "Volatility")

TmpA$Permno <- as.numeric(levels(TmpA$Permno)[TmpA$Permno])

# Finally, the computed volatility is added to the main stock data table.

Stocks <- merge(Stocks, TmpA, by = c("Permno", "Date"), all.x = T)


#                   ############################################
#                   #########   Aggregated ETF Flows   #########
#                   ############################################

# The previously calculated total ETF flows for the market on a given day are added to the
# stock-level variables.

Stocks <- merge(Stocks, ETF_Aggregated_Flows, by = "Date", all.x = T)


#                   ############################################
#                   ###   ETF Ownership (Ben-David et al.)   ###
#                   ############################################

# In order to correct for non-standalone ETF portfolios, the ETF ownership variable is
# computed according to the method of Ben-David et al (2018). First, the portfolio weight
# of each stock in each fund portfolio has to be calculated. The drawback of this method is
# that many observations of total fund portfolio assets are missing (1'050'601 NAs), and the
# potential inaccuracy of the price and total fund portfolio asset variables coming from
# the s12 database.

ETF_Holdings <- cbind(ETF_Holdings, ETF_Holdings$shares * ETF_Holdings$prc /
                        (ETF_Holdings$assets * 10000))

colnames(ETF_Holdings)[17] <- "ETF Portfolio Weight"

ETF_Holdings <- ETF_Holdings[order(fundno, fdate)]

# Then, the protfolio weights are checked for their plausibility.

quantile(ETF_Holdings$`ETF Portfolio Weight`, probs = seq(0, 1, 0.1), na.rm = T)

subset(ETF_Holdings, ETF_Holdings$`ETF Portfolio Weight` >= 0.5)

# Since a number of ETF fund portfolio weights for individual stocks are obviously erroneous,
# these are cut off at the 50% level and set to NA instead.

ETF_Holdings$`ETF Portfolio Weight`[which(ETF_Holdings$`ETF Portfolio Weight` >= 0.5)] <- NA


##### Vectorized Trial #####

# Vectorized trial
# Setting up ETF AUM matrix

TmpAUM <- subset(ETF_Time_Series[ , c("Date", "Thomson Fundno", "AUM")], !is.na(`Thomson Fundno`))

TmpAUM <- melt(TmpAUM, id.vars = c("Date", "Thomson Fundno"), measure.vars = "AUM", value.name = "AUM")

TmpAUM <- as.data.table(dcast(data = TmpAUM, formula = Date ~ `Thomson Fundno`,
                         fun.aggregate = function(x) mean(x, na.rm = T),
                         value.var = "AUM", drop = F))

# Vectorized trial
# Preparing data for weight martrix

TmpETF <- ETF_Holdings[ , c("fdate", "fundno", "cusip", "ETF Portfolio Weight")]

TmpETF <- TmpETF[order(fundno, cusip, fdate)]

TmpETF <- subset(TmpETF, !is.na(cusip))

# Vectorized trial
# Setting up date vector

TmpDate <- na.omit(unique(c(ETF_Time_Series$Date, ETF_Holdings$fdate, Stock_Time_Series$Date)))

TmpDate <- TmpDate[order(TmpDate)]

TmpB <- as.data.table(cbind(TmpDate, matrix(NA, length(TmpDate), 1)))

TmpB[ , 1] <- TmpDate

colnames(TmpB)[1] <- "fdate"

# Vectorized trial
# Aligning the length of the ETF AUM matrix with the length of the stock market cap matrix
# and other matrices

TmpAUM <- merge(TmpAUM, TmpB[ , 1], by.x = "Date", by.y = "fdate", all = T)

# Vectorized trial
# Setting up output matrix (ETF ownership)

ETF_Own <- TmpB[ , 1]


##### For-Loop #####

for (i in unique(TmpETF$cusip)) {

# Vectorized trial
# Weight matrix

TmpA <- subset(TmpETF, cusip == i)

TmpA <- TmpA[order(fundno)]

# Long to wide format

TmpA <- melt(TmpA, measure.vars = "ETF Portfolio Weight", value.name = "ETF Portfolio Weight")

# if (nrow(TmpA) != 0) {

TmpA <- as.data.table(dcast(data = TmpA, formula = fdate ~ fundno, value.var = "ETF Portfolio Weight", drop = F))

# } else

# Merging 

TmpA <- merge(TmpA, TmpB[ , 1], all = T)

# Expand ETF portfolio weights from quarterly to daily, if portfolio weights are missing for more than
# three years, NA values are retained

TmpA <- na.locf(TmpA, na.rm = F, fromLast = F, maxgap = 1095)

# Vectorized trial
# Conversion of weight and ETF AUM matrices to same dimensions

TmpD <- colnames(TmpAUM)[-1][!(colnames(TmpAUM)[-1] %in% colnames(TmpA)[-1])]

TmpE <- as.data.table(matrix(NA, length(TmpDate), length(TmpD) + 1))

TmpE[ , 1] <- TmpDate

colnames(TmpE)[1] <- "fdate"

colnames(TmpE)[-1] <- TmpD

TmpD <- merge(TmpA, TmpE, by = "fdate", all.x = T)

TmpD <- subset(TmpD, fdate %in% TmpAUM$Date)

# Vectorized trial
# Matrix multiplication

TmpA <- TmpAUM[ , -1] * TmpD[ , -1]

# Vectorized trial
# Summing the whole thing up

TmpA <- apply(TmpA, 1, function(x) sum(x, na.rm = T))

# Vectorized trial
# Dividing by MktCap vector

TmpC <- subset(Stock_Time_Series, CUSIP == i)

TmpC <- TmpC[ , c("Date", "Market Cap")]

TmpC <- merge(TmpC, TmpB[ , 1], by.x = "Date", by.y = "fdate", all = T)

TmpETF_Own <- TmpA / TmpC[ , -1]

colnames(TmpETF_Own) <- i

ETF_Own <- cbind(ETF_Own, TmpETF_Own)

}

# From wide back to long format

ETF_Own <- melt(ETF_Own, id.vars = "fdate", measure.vars = colnames(ETF_Own[ , -1]),
                variable.name = "CUSIP", value.name = "ETF Ownership")

colnames(ETF_Own)[1] <- "Date"

# Merging with stocks data table

Stocks <- merge(Stocks, ETF_Own, by = c("CUSIP", "Date"), all.x = T)

Stocks <- Stocks[ , c("Date", "Permno", "CUSIP", "Price", "Bid", "Ask", "Return",
                      "Shares Outstanding", "Volume", "Market Cap", "MKTRF", "SMB",
                      "HML", "RF", "UMD", "Aggregated ETF Flows", "ETF Ownership")]

Stocks <- Stocks[order(Permno, Date)]


#                   ############################################
#                   ####   ETF Flows from ETF Ownership    #####
#                   ############################################

# Finally, from the percentage of ETF ownership for each stock, the total ETF capital flows
# to a given stock can be inferred. Again, a conversion from long to wide format is needed.

TmpA <- melt(Stocks[ , c("Date", "Permno", "ETF Ownership")], id.vars = c("Date", "Permno"),
             measure.vars = "ETF Ownership")

TmpA <- dcast(TmpA, Date ~ Permno)

# The (percentage) ETF flows are computed from first differences of the ETF ownership variable.

TmpB <- as.data.table(apply(TmpA[ , -1], 2, function(x) diff(x, lag = 1)))

TmpB <- cbind(TmpA[-1, 1], TmpB)

# The wide format is cast back into long format.

TmpA <- melt(TmpB, id.vars = "Date", measure.vars = colnames(TmpB)[-1],
             variable.name = "Permno", value.name = "ETF Flows")

TmpA$Permno <- as.numeric(levels(TmpA$Permno)[TmpA$Permno])

# The percentage ETF flows are then added to the main stock data table as a new variable.

Stocks <- merge(Stocks, TmpA, by = c("Permno", "Date"), all.x = T)

Stocks <- Stocks[ , c("Date", "Permno", "CUSIP", "Price", "Return", "Volatility", "Bid-Ask Spread",
                      "Shares Outstanding", "Market Cap", "Log(Market Cap)", "Volume", "Log(Volume)",
                      "RF", "MKTRF", "SMB", "HML", "UMD", "1/Price", "Amihud", "ETF Ownership",
                      "ETF Flows", "Aggregated ETF Flows")]


#                   ############################################
#                   ##########   Variable Cleaning    ##########
#                   ############################################

# First, a look is taken at the distribution of the ETF ownership variable by means of the
# respective quantiles.

round(quantile(Stocks$`ETF Ownership`, probs = seq(0, 1, 0.025), na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Ownership`, probs = 0.025, na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Ownership`, probs = 0.975, na.rm = T), digits = 3)

# An ETF ownership value above 40% seems highly implausible. Approximately 2% of all non-NA
# values of the ETF ownership variable seem to be erroneous (i.e. higher than what seems
# plausible). Therefore, ETF ownership values above 40% are set to 40% instead.

subset(Stocks, `ETF Ownership` >= 0.4)

length(which(Stocks$`ETF Ownership` >= 0.4)) / length(which(!is.na(Stocks$`ETF Ownership`)))

Stocks$`ETF Ownership`[which(Stocks$`ETF Ownership` >= 0.4)] <- 0.4

# Finally, for stocks with missing ETF ownership, the value is set to zero.

Stocks$`ETF Ownership`[is.na(Stocks$`ETF Ownership`)] <- 0

# Next, a look is taken at the distribution of the ETF flow variable.

round(quantile(Stocks$`ETF Flows`, probs = seq(0, 1, 0.025), na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Flows`, probs = 0.025, na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Flows`, probs = 0.00005, na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Flows`, probs = 0.975, na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Flows`, probs = 0.99995, na.rm = T), digits = 3)

# Flows which are below -50% or above +50% of ETF ownership seem implausible and too extreme.
# Only around 0.01% of ETF flow values seem to be that extreme. Replacing these flows by the
# minimum quantile value of -50% and a maximum quantile value of +50% is equivalent to a
# winsorization at approximately the 99.99%-level.

subset(Stocks, abs(`ETF Flows`) >= 0.5)

length(which(abs(Stocks$`ETF Flows`) >= 0.5)) / length(which(!is.na(Stocks$`ETF Flows`)))

Stocks$`ETF Flows`[which(Stocks$`ETF Flows` >= 0.5)] <- 0.5

Stocks$`ETF Flows`[which(Stocks$`ETF Flows` <= -0.5)] <- -0.5

# The remaining ETF flow values which are missing are set to zero to be able to include them
# in the later analysis.

Stocks$`ETF Flows`[which(is.na(Stocks$`ETF Flows`))] <- 0



################################## LETF Ownership #####################################

##### Vectorized Trial #####

# Setting up ETF AUM matrix

TmpAUM <- subset(ETF_Time_Series[ , c("Date", "Thomson Fundno", "AUM")], !is.na(`Thomson Fundno`))

TmpAUM <- TmpAUM %>% filter(`Thomson Fundno` %in% LETF_sample$fundno)

TmpAUM <- melt(TmpAUM, id.vars = c("Date", "Thomson Fundno"), measure.vars = "AUM", value.name = "AUM")

TmpAUM <- as.data.table(dcast(data = TmpAUM, formula = Date ~ `Thomson Fundno`,
                              fun.aggregate = function(x) mean(x, na.rm = T),
                              value.var = "AUM", drop = F))

# Vectorized trial
# Preparing data for weight martrix

TmpETF <- ETF_Holdings[ , c("fdate", "fundno", "cusip", "ETF Portfolio Weight")]

TmpETF <- TmpETF[order(fundno, cusip, fdate)]

TmpETF <- subset(TmpETF, !is.na(cusip))

# Vectorized trial
# Setting up date vector

TmpDate <- na.omit(unique(c(ETF_Time_Series$Date, ETF_Holdings$fdate, Stock_Time_Series$Date)))

TmpDate <- TmpDate[order(TmpDate)]

TmpB <- as.data.table(cbind(TmpDate, matrix(NA, length(TmpDate), 1)))

TmpB[ , 1] <- TmpDate

colnames(TmpB)[1] <- "fdate"

# Vectorized trial
# Aligning the length of the ETF AUM matrix with the length of the stock market cap matrix
# and other matrices

TmpAUM <- merge(TmpAUM, TmpB[ , 1], by.x = "Date", by.y = "fdate", all = T)

# Vectorized trial
# Setting up output matrix (ETF ownership)

ETF_Own <- TmpB[ , 1]


##### For-Loop #####

for (i in unique(TmpETF$cusip)) {
  
  # Vectorized trial
  # Weight matrix
  
  TmpA <- subset(TmpETF, cusip == i)
  
  TmpA <- TmpA[order(fundno)]
  
  # Long to wide format
  
  TmpA <- melt(TmpA, measure.vars = "ETF Portfolio Weight", value.name = "ETF Portfolio Weight")
  
  # if (nrow(TmpA) != 0) {
  
  TmpA <- as.data.table(dcast(data = TmpA, formula = fdate ~ fundno, value.var = "ETF Portfolio Weight", drop = F))
  
  # } else
  
  # Merging 
  
  TmpA <- merge(TmpA, TmpB[ , 1], all = T)
  
  # Expand ETF portfolio weights from quarterly to daily, if portfolio weights are missing for more than
  # three years, NA values are retained
  
  TmpA <- na.locf(TmpA, na.rm = F, fromLast = F, maxgap = 1095)
  
  # Vectorized trial
  # Conversion of weight and ETF AUM matrices to same dimensions
  
  TmpD <- colnames(TmpAUM)[-1][!(colnames(TmpAUM)[-1] %in% colnames(TmpA)[-1])]
  
  TmpE <- as.data.table(matrix(NA, length(TmpDate), length(TmpD) + 1))
  
  TmpE[ , 1] <- TmpDate
  
  colnames(TmpE)[1] <- "fdate"
  
  colnames(TmpE)[-1] <- TmpD
  
  TmpD <- merge(TmpA, TmpE, by = "fdate", all.x = T)
  
  TmpD <- subset(TmpD, fdate %in% TmpAUM$Date)
  
  TmpD <- TmpD %>% select(fdate, colnames(TmpAUM)[-1])
  
  # Vectorized trial
  # Matrix multiplication
  
  TmpA <- TmpAUM[ , -1] * TmpD[ , -1]
  
  # Vectorized trial
  # Summing the whole thing up
  
  TmpA <- apply(TmpA, 1, function(x) sum(x, na.rm = T))
  
  # Vectorized trial
  # Dividing by MktCap vector
  
  TmpC <- subset(Stock_Time_Series, CUSIP == i)
  
  TmpC <- TmpC[ , c("Date", "Market Cap")]
  
  TmpC <- merge(TmpC, TmpB[ , 1], by.x = "Date", by.y = "fdate", all = T)
  
  TmpETF_Own <- TmpA / TmpC[ , -1]
  
  colnames(TmpETF_Own) <- i
  
  ETF_Own <- cbind(ETF_Own, TmpETF_Own)
  
}

# From wide back to long format

ETF_Own <- melt(ETF_Own, id.vars = "fdate", measure.vars = colnames(ETF_Own[ , -1]),
                variable.name = "CUSIP", value.name = "ETF Ownership")

colnames(ETF_Own)[1] <- "Date"

# Merging with stocks data table

Stocks <- merge(Stocks, ETF_Own, by = c("CUSIP", "Date"), all.x = T)

Stocks <- Stocks[ , c("Date", "Permno", "CUSIP", "Price", "Bid", "Ask", "Return",
                      "Shares Outstanding", "Volume", "Market Cap", "MKTRF", "SMB",
                      "HML", "RF", "UMD", "Aggregated ETF Flows", "ETF Ownership")]

Stocks <- Stocks[order(Permno, Date)]


#                   ############################################
#                   ####   ETF Flows from ETF Ownership    #####
#                   ############################################

# Finally, from the percentage of ETF ownership for each stock, the total ETF capital flows
# to a given stock can be inferred. Again, a conversion from long to wide format is needed.

TmpA <- melt(Stocks[ , c("Date", "Permno", "ETF Ownership")], id.vars = c("Date", "Permno"),
             measure.vars = "ETF Ownership")

TmpA <- dcast(TmpA, Date ~ Permno)

# The (percentage) ETF flows are computed from first differences of the ETF ownership variable.

TmpB <- as.data.table(apply(TmpA[ , -1], 2, function(x) diff(x, lag = 1)))

TmpB <- cbind(TmpA[-1, 1], TmpB)

# The wide format is cast back into long format.

TmpA <- melt(TmpB, id.vars = "Date", measure.vars = colnames(TmpB)[-1],
             variable.name = "Permno", value.name = "ETF Flows")

TmpA$Permno <- as.numeric(levels(TmpA$Permno)[TmpA$Permno])

# The percentage ETF flows are then added to the main stock data table as a new variable.

Stocks <- merge(Stocks, TmpA, by = c("Permno", "Date"), all.x = T)

Stocks <- Stocks[ , c("Date", "Permno", "CUSIP", "Price", "Return", "Volatility", "Bid-Ask Spread",
                      "Shares Outstanding", "Market Cap", "Log(Market Cap)", "Volume", "Log(Volume)",
                      "RF", "MKTRF", "SMB", "HML", "UMD", "1/Price", "Amihud", "ETF Ownership",
                      "ETF Flows", "Aggregated ETF Flows")]


#                   ############################################
#                   ##########   Variable Cleaning    ##########
#                   ############################################

# First, a look is taken at the distribution of the ETF ownership variable by means of the
# respective quantiles.

round(quantile(Stocks$`ETF Ownership`, probs = seq(0, 1, 0.025), na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Ownership`, probs = 0.025, na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Ownership`, probs = 0.975, na.rm = T), digits = 3)

# An ETF ownership value above 40% seems highly implausible. Approximately 2% of all non-NA
# values of the ETF ownership variable seem to be erroneous (i.e. higher than what seems
# plausible). Therefore, ETF ownership values above 40% are set to 40% instead.

subset(Stocks, `ETF Ownership` >= 0.4)

length(which(Stocks$`ETF Ownership` >= 0.4)) / length(which(!is.na(Stocks$`ETF Ownership`)))

Stocks$`ETF Ownership`[which(Stocks$`ETF Ownership` >= 0.4)] <- 0.4

# Finally, for stocks with missing ETF ownership, the value is set to zero.

Stocks$`ETF Ownership`[is.na(Stocks$`ETF Ownership`)] <- 0

# Next, a look is taken at the distribution of the ETF flow variable.

round(quantile(Stocks$`ETF Flows`, probs = seq(0, 1, 0.025), na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Flows`, probs = 0.025, na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Flows`, probs = 0.00005, na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Flows`, probs = 0.975, na.rm = T), digits = 3)

round(quantile(Stocks$`ETF Flows`, probs = 0.99995, na.rm = T), digits = 3)

# Flows which are below -50% or above +50% of ETF ownership seem implausible and too extreme.
# Only around 0.01% of ETF flow values seem to be that extreme. Replacing these flows by the
# minimum quantile value of -50% and a maximum quantile value of +50% is equivalent to a
# winsorization at approximately the 99.99%-level.

subset(Stocks, abs(`ETF Flows`) >= 0.5)

length(which(abs(Stocks$`ETF Flows`) >= 0.5)) / length(which(!is.na(Stocks$`ETF Flows`)))

Stocks$`ETF Flows`[which(Stocks$`ETF Flows` >= 0.5)] <- 0.5

Stocks$`ETF Flows`[which(Stocks$`ETF Flows` <= -0.5)] <- -0.5

# The remaining ETF flow values which are missing are set to zero to be able to include them
# in the later analysis.

Stocks$`ETF Flows`[which(is.na(Stocks$`ETF Flows`))] <- 0






# --------------------------- ETF vs. Stock Market Plots ------------------------------

# Computing time series for total stock market capitalisation and trading volume
# in the sample

Stocks_MC <- Stocks %>%
  group_by(Date) %>%
  summarise(`Market Cap`  = sum(`Market Cap`, na.rm = T),
            Volume_Stocks = sum(Volume, na.rm = T))

# Computing time series for total ETF AUM and trading volume in the sample

ETF_AUM <- ETF_Time_Series %>%
  group_by(Date) %>%
  summarise(AUM        = sum(AUM, na.rm = T),
            TNA        = sum(TNA, na.rm = T),
            Volume_ETF = sum(Volume, na.rm = T))

# Joining the two time series and adding year, month, and day columns

TS_MC <- Stocks_MC %>%
  full_join(ETF_AUM, by = "Date")

TS_MC <- TS_MC %>% mutate(year  = lubridate::year(Date),
                          month = lubridate::month(Date),
                          day   = lubridate::day(Date))

# Computing yearly time series for yearly average market cap and yearly trading volume

TS_MC_Yearly <- TS_MC %>%
  group_by(year) %>% 
  summarise(Date          = last(Date),
            `Market Cap`  = mean(`Market Cap`, na.rm = T),
            Volume_Stocks = sum(Volume_Stocks, na.rm = T),
            AUM           = mean(AUM, na.rm = T),
            TNA           = mean(TNA, na.rm = T),
            Volume_ETF    = sum(Volume_ETF / 1e6, na.rm = T)) %>%
  filter(!is.na(year))
  
# Setting colours for the plots

col_blue <- brewer.pal(n = 11, name = "RdYlBu")[9]

col_red  <- brewer.pal(n = 11, name = "RdYlBu")[2]

# Time series plot for daily market capitalisation

p_MC <- ggplot(TS_MC, aes(x = Date)) +
  geom_area(aes(y = `Market Cap` / 1e6, fill = "Stocks"), alpha = 0.7) +
  geom_line(aes(y = `Market Cap` / 1e6), col = col_blue) +
  geom_area(aes(y = AUM / 1e6, fill = "ETFs"), alpha = 0.7) +
  geom_line(aes(y = AUM / 1e6), col = col_red) +
  geom_hline(yintercept = 0, size = 1, col = "gray") +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(from = 0, to = 30, by = 5)) +
  scale_fill_manual(values = c("Stocks" = col_blue, "ETFs" = col_red)) +
  labs(title    = "Total Market Capitalisation",
       subtitle = "",
       x        = "Date",
       y        = "Market Cap (USD tn)") +
  theme_minimal() +
  theme(title            = element_text(family = "CMU Bright"),
        plot.title       = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle    = element_text(size = 15, hjust = 0.5),
        legend.title     = element_blank(),
        legend.position  = "none")

# Stacked bar plot for yearly average market capitalisation

p_MC_stacked <- TS_MC_Yearly %>%
  select(Date, Stocks = `Market Cap`, ETFs = AUM) %>%
  gather(key = "Stock/ETF", value = "MC", -Date) %>%
  mutate(Date = Date - lubridate::dyears(1),
         MC   = MC / 1e6) %>% 
  ggplot(aes(x = Date, y = MC, fill = `Stock/ETF`)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_hline(yintercept = 0, size = 1, col = "gray") +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(from = 0, to = 30, by = 5)) +
  scale_fill_manual(values = c("Stocks" = col_blue, "ETFs" = col_red)) +
  labs(title    = "",
       subtitle = "",
       x        = "Date",
       y        = "Market Cap (USD tn)") +
  theme_minimal() +
  theme(title            = element_text(family = "CMU Bright"),
        plot.title       = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle    = element_text(size = 15, hjust = 0.5),
        legend.title     = element_blank(),
        legend.position  = "none")

# Time series plot for daily trading volume

p_volume <- ggplot(TS_MC, aes(x = Date)) +
  geom_area(aes(y = Volume_Stocks / 1e3, fill = "Stocks")) +
  geom_area(aes(y = Volume_ETF / 1e9, fill = "ETFs")) +
  geom_hline(yintercept = 0, size = 1, col = "gray") +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(from = 0, to = 15, by = 2.5)) +
  scale_fill_manual(values = c("Stocks" = col_blue, "ETFs" = col_red)) +
  labs(title    = "Total Trading Volume",
       subtitle = "",
       x        = "Date",
       y        = "Volume (bn)") +
  theme_minimal() +
  theme(title            = element_text(family = "CMU Bright"),
        plot.title       = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle    = element_text(size = 15, hjust = 0.5),
        legend.title     = element_blank(),
        legend.position  = "none")

# Stacked bar plot for yearly aggregated trading volume

p_volume_stacked <- TS_MC_Yearly %>%
  select(Date, Stocks = Volume_Stocks, ETFs = Volume_ETF) %>%
  gather(key = "Stock/ETF", value = "Volume", -Date) %>%
  mutate(Date     = Date - lubridate::dyears(1)) %>% 
  ggplot(aes(x = Date, y = Volume / 1e6, fill = `Stock/ETF`)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_hline(yintercept = 0, size = 1, col = "gray") +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  # scale_y_continuous(breaks = seq(from = 0, to = 2.5, by = 0.5)) +
  scale_fill_manual(values = c("Stocks" = col_blue, "ETFs" = col_red)) +
  labs(title    = "",
       subtitle = "",
       x        = "Date",
       y        = "Volume (tn)") +
  theme_minimal() +
  theme(title            = element_text(family = "CMU Bright"),
        plot.title       = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle    = element_text(size = 15, hjust = 0.5),
        legend.title     = element_blank(),
        legend.position  = "bottom")

# Plotting both plots in one with separate panels

ggarrange(p_MC, p_MC_stacked, p_volume, p_volume_stacked, nrow = 4)

# Table for time series of market cap and volume at year end

TS_MC_Yearly %>%
  select(-Date) %>%
  round(digits = 0) %>%
  stargazer(title = "Time Series of Market Capitalisation and Volume for ETFs and Stocks",
            summary = F, rownames = F, column.sep.width = "-10pt", font.size = "footnotesize",
            digits = 3, align = T, add.lines = T, type = "latex")

