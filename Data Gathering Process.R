#                   ############################################

#                   Master's Thesis - Matthieu Nicolas Rüttimann
#                   
#                   ############################################
#                   #####   Data Gathering and Clearning   #####
#                   ############################################


# ------------------------------------ Packages ---------------------------------------

library(RPostgres)
library(pryr)
library(data.table)
library(microbenchmark)
library(xts)
library(timeSeries)
library(forecast)

# ------------------------------- Connecting to WRDS ----------------------------------

# WRDS can be accessed remotely from a personal computer using the SQL language in R
# Studio. The code shows the loging-in process to the WRDS database with one's personal
# user name and password. The package "RPostgres" is needed for this purpose, which
# enables SQL queries within R Studio.

WRDS <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  dbname = 'wrds',
                  user = "mruettim",
                  password = "MfDb1362",
                  sslmode = 'require')


# ----------------------------- Querying WRDS Metadata --------------------------------

# WRDS offers a range of different libraries to download data from, ordered by vendor
# name. By quering metadata on the available libraries in WRDS, one can examine the
# structure of the different libraries and find the corresponding one to download data
# from. The following code lines show all available WRDS libraries with the currently
# active subscription of the university.

RES <- dbSendQuery(WRDS, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema")
WRDS_Libraries <- dbFetch(RES, n=-1)
dbClearResult(RES)
WRDS_Libraries

# Once the right WRDS library is found, one has to determine the needed datasets within
# the chosen library. This is done by selecting a particular library with "where table_
# schema='library'" in the following code, which shows the available datasets within the
# selected library.

RES <- dbSendQuery(WRDS, "select distinct table_name
                   from information_schema.columns
                   where table_schema='crsp'
                   order by table_name")
WRDS_Datasets <- dbFetch(RES, n=-1)
dbClearResult(RES)
WRDS_Datasets

# Next, one has to determine the WRDS column headers (variables) within a given dataset.
# This can be done by adding "and table_name='variable'" in the query code below the line
# for the selected library.

RES <- dbSendQuery(WRDS, "select column_name
                   from information_schema.columns
                   where table_schema='crsp'
                   and table_name='dsf'
                   order by column_name")
WRDS_Variables <- dbFetch(RES, n=-1)
dbClearResult(RES)
WRDS_Variables

# Note: A comprehensive list of all WRDS libraries is available via the WRDS Dataset List.
# This online resource provides a listing of each library, their component datasets and
# variables, as well as a tabular database preview feature, and is helpful in establishing
# the structure of the data one is looking for in an easy, web-friendly manner.


# ----------------------------- Data Download from WRDS -------------------------------

# Once the required library, dataset, and variables are known, one can download the actual
# data itself directly into the R Studio environment. Unlike the above metadata queries,
# where table_schema and table_name are separately used to seach libraries and datasets
# respectively, data queries instead use the two together to identify the data location.
# So, for example, a data query for the dataset dsf within the library crsp would be
# referred to as crsp.dsf. Setting n = 10 artificially limits the results to 10
# observations, while n = -1 downloads all available observations within the dataset. The
# code example below shows how to get five different variables from the CRSP Daily Stock
# File for the time period between 1 January 1960 and 1 January 1980 of stocks with an ask
# price high of greater than 2'500 and a bid price low of lower than 2'000 for 10
# observations.

RES <- dbSendQuery(WRDS, "select cusip, permno, date, bidlo, askhi
                   from crsp.dsf
                   where date between '1960-01-01'
                   and '1980-01-01'
                   and askhi > 2500
                   and bidlo < 2000")
Data <- dbFetch(RES, n=10)
dbClearResult(RES)
Data


# ---------------------- Identifying All Available ETFs in WRDS -----------------------

#                         #####################################
#                         ########## CRSP Securities ##########
#                         #####################################


# Using the CRSP Daily Stock Names (crsp.stocknames) file to download all available ETFs
# in the CRSP database. ETFs are identified by setting the Share Code (shrcd) variable
# equal to 73.

RES <- dbSendQuery(WRDS, "select *
                   from crsp.stocknames
                   where shrcd = 73")
ETF_CRSP <- dbFetch(RES, n=-1)
dbClearResult(RES)
ETF_CRSP

# CRSP’s Permanent Issue Identifier (PERMNO) for a security is used to uniquely identify
# an ETF, and to separate it from observations of the same ETF for other points in time,
# which contain the same PERMNO. Additionally, also the security CUSIP is used as the next
# best common identifier. It is checked whether every ETF CUSIP possesses a unique PERMNO
# and vice versa, which is the case.

ETF_CRSP_Identifiers <- subset(ETF_CRSP, duplicated(ETF_CRSP$permno, fromLast = T) == F)

ETF_CRSP_Identifiers <- ETF_CRSP_Identifiers[ , c("permno", "cusip", "ncusip", "ticker",
                                                  "comnam", "permco")]


#                         #####################################
#                         ########## OptionMetrics ############
#                         #####################################

# Using the OptionMetrics Security (optionm.securd) file to download all available ETFs in
# the OptionMetrics database. ETFs are identified by setting the Issue Type (issue_type)
# variable equal to %.

RES <- dbSendQuery(WRDS, "select *
                   from optionm.securd
                   where issue_type = '%'")
ETF_OptionMetrics <- dbFetch(RES, n=-1)
dbClearResult(RES)
ETF_OptionMetrics

# Security ID (secid) and CUSIP serve as the unique identifiers for ETFs in the
# OptionMetrics database. It is checked whether every ETF CUSIP possesses a unique
# Security ID and vice versa, which is the case.

ETF_OptionMetrics_Identifiers <- subset(ETF_OptionMetrics, duplicated(ETF_OptionMetrics$secid,
                                                                      fromLast = T) == F)

ETF_OptionMetrics_Identifiers <- ETF_OptionMetrics_Identifiers[ , c("secid", "cusip", "ticker")]


#                         #####################################
#                         ############# Compustat #############
#                         #####################################

# Using the Compustat IQ Security (comp.secd) file to download all available ETFsin the
# Compustat database. ETFs are identified by setting the Issue Type Code (tpci) variable
# equal to %.

RES <- dbSendQuery(WRDS, "select *
                   from comp.secd
                   where tpci = '%'")
ETF_Compustat <- dbFetch(RES, n=-1)
dbClearResult(RES)
ETF_Compustat

# The Global Company Key (gvkey) and CUSIP serve as the unique identifiers for ETFs in
# the Compustat database. It is checked whether every ETF CUSIP possesses a unique
# Global Company Key and vice versa, which is NOT the case in Compustat. Hence, since
# multiple CUSIPs exists for the same Global Company Key, CUSIP is used as a unique
# identifier instead of the Global Company Key.

ETF_Compustat_Identifiers <- subset(ETF_Compustat, duplicated(ETF_Compustat$cusip,
                                                                  fromLast = T) == F)

ETF_Compustat_Identifiers <- ETF_Compustat_Identifiers[ , c("gvkey", "cusip", "tic", "conm")]


#                         #####################################
#                         ######### CRSP Mutual Funds #########
#                         #####################################

# First, the entire CRSP Mutual Fund database for the period between 1 January 1993 and
# 31 March 2018 is downloaded into R Studio. This is done using the CRSP Mutual Funds
# Summary (crsp.fund_summary2) file.

RES <- dbSendQuery(WRDS, "select *
                   from crsp.fund_summary2
                   where caldt between '1993-01-01'
                   and '2018-03-31'")
CRSPmf <- dbFetch(RES, n=-1)
dbClearResult(RES)
CRSPmf

# Next, all ETFs (and ETNs) are identified by setting the ETF Flag (et_flag) variable
# equal to F (for ET(F)) or N (for ET(N)).

ETF_CRSPmf <- subset(CRSPmf, CRSPmf$et_flag == "F" | CRSPmf$et_flag == "N")

# CUSIP and the CRSP Fund Number (crsp_fundno) serve as the unique identifier for ETFs in
# the CRSP Mutual Funds database. Again, it is checked whether the number of unique ETF
# CUSIPs is identical to the number of unique CRSP Fund Numbers. As in the Compustat database,
# this is NOT the case here. So, CUSIP is used as the final identifier instead of the CRSP
# Fund Number.

ETF_CRSPmf <- subset(ETF_CRSPmf, duplicated(ETF_CRSPmf$cusip8, fromLast = T) == F)

ETF_CRSPmf_Identifiers <- ETF_CRSPmf[ , c("crsp_fundno", "cusip8", "ncusip", "ticker",
                                          "fund_name", "mgmt_name", "crsp_portno",
                                          "first_offer_dt", "crsp_obj_cd", "lipper_obj_cd",
                                          "tna_latest_dt", "tna_latest")]


#                         #####################################
#                         ####### Final ETF Identifier ########
#                         #####################################

# Since the ETF CUSIPs from the Compustat database are nine-digit instead of the standard
# eight-digit CUSIPs, only the first eight digits of the nine-digit CUSIPs are kept.

ETF_Compustat_Identifiers$cusip <- substr(ETF_Compustat_Identifiers$cusip, start = 1, stop = 8)

# The final list containing all available ETFs in the WRDS universe is constructed from the
# CRSP Securities, OptionMetrics, Compustat, and CRSP Mutual Funds ETF identifiers, by
# matching all unique CUSIPs from these files as the identifying variable.

ETF_Identifiers <- unique(c(ETF_CRSP_Identifiers$cusip,
                            ETF_OptionMetrics_Identifiers$cusip,
                            ETF_Compustat_Identifiers$cusip,
                            ETF_CRSPmf_Identifiers$cusip8))

# Now, this final ETF identifier list is used to find all available ETFs in the CRSP
# Mutual Fund database. For this purpose, first a list containing only unique fund entries
# from the CRSP Mutual Fund database is created.

CRSPmf_UniqueCusip <- subset(CRSPmf, duplicated(CRSPmf$cusip8, fromLast = T) == F)

CRSPmf_UniqueCusip <- CRSPmf_UniqueCusip[ , c("crsp_fundno", "cusip8", "ticker", "fund_name",
                                              "et_flag", "crsp_obj_cd", "lipper_obj_cd",
                                              "tna_latest")]

# Then, the ETFs are extracted from the CRSP Mutual Funds database. To test the validity of
# the code, the reverse is tested as well, which yields the exact same number of results.

ETF_Identifiers_Matched <- CRSPmf_UniqueCusip$cusip8[CRSPmf_UniqueCusip$cusip8 %in% ETF_Identifiers]

ETF_all <- subset(CRSPmf_UniqueCusip, (CRSPmf_UniqueCusip$cusip8 %in% ETF_Identifiers_Matched) == T)

Reverse_Test <- ETF_Identifiers[ETF_Identifiers %in% CRSPmf_UniqueCusip$cusip8]

# It is checked whether the found ETFs not marked as such in the CRSP Mutual Fund database
# are indeed ETFs and not just Mutual Funds. Their names alone make it pretty clear that
# they actually are ETFs and were mistakenly not classified as such in the CRSP Mutual Fund
# database.

table(ETF_all$et_flag, exclude = NULL)

Missing_ETF <- subset(ETF_all, is.na(ETF_all$et_flag) == T)

table(Missing_ETF$crsp_obj_cd, exclude = NULL)

# Finally, a list of ETFs sorted by TNA is created, starting with the largest ETF and then
# descending in TNA. Only variables containing useful information on the nature of ETFs
# are kept in this list.

ETF_all <- ETF_all[ , c("crsp_fundno", "cusip8", "ticker", "fund_name", "tna_latest_dt",
                            "tna_latest", "mgmt_name","et_flag", "nav_latest_dt", "nav_latest",
                            "first_offer_dt", "per_com", "per_pref", "per_conv", "per_corp",
                            "per_muni", "per_govt", "per_oth", "per_cash", "per_bond", "per_abs",
                            "per_mbs", "per_eq_oth", "per_fi_oth", "open_to_inv", "retail_fund",
                            "inst_fund", "index_fund_flag", "dead_flag", "delist_cd", "crsp_obj_cd",
                            "lipper_obj_cd", "lipper_obj_name", "lipper_class", "lipper_class_name",
                            "si_obj_cd", "wbrger_obj_cd", "lipper_asset_cd")]

ETF_all <- ETF_all[order(ETF_all$tna_latest, decreasing = T, na.last = T), ]

ETF_all <- as.data.table(ETF_all)


# ----------------------- Identifying Leveraged and Inverse ETFs ----------------------

# Search pattern for leveraged, inverse, and inverse leveraged ETFs

LETF <- c("leverage", "levered", "inverse", "short", "bear", "bull", "ultra",
          "enhanced", "1x", "2x", "3x", "4x", "-1x", "-2x", "-3x")

# Filtering the list of previously identified ETFs for leveraged, inverse,
# and inverse leveraged ETFs

LETF <- ETF_all %>%
  filter(str_detect(fund_name, regex(paste(LETF, collapse = "|"), ignore_case = T)))

# Saving a list with all identified LETFS

write.csv(LETF, file = "/Users/Matthieu/Desktop/HSG/Master's Thesis/ETFs and Systemic Risk/Data/LETFS_List.csv")

LETF_Time_Series <- ETF_Time_Series %>%
  filter(`CRSP Fundno` %in% LETF$crsp_fundno)

# Identifying LETFs which are also included in ETF holdings sample and classifying them
# into leveraged and inverse ETFs

LETF <- LETF %>%
  filter(fundno %in% ETF_Holdings$fundno) %>%
  arrange(crsp_fundno) %>%
  mutate(type = c("I", "L", "L", "L", "L", "L", "L", "L", "L", "L", "I", "I", "I", "L",
                  "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L", "L",
                  "L", "L", "L", "I", "I"))

LETF_sample <- LETF %>%
  filter(type == "L")

IETF_sample <- LETF %>%
  filter(type == "I")
  
  

# ---------------------------------- ETF Holdings -------------------------------------

#                         #####################################
#                         ######### Mutual Fund Links #########
#                         #####################################

# In order to access the equity portfolio holdings data for ETFs in the Thomson Reuters
# Mutual Fund Holdings database, first, the Thomson Reuters Fund Number for each ETF is
# needed. CRSP Fund Numbers can be converted to Thomson Reuters Fund Numbers by tapping
# the Mutual Fund Links (MFLinks) Tables. In a first step, the MFLink Table 1 yields the
# corresponding Wharton Financial Institution Center Number (WFICN) for each available
# CRSP Fund Number. Thus, the MFLink Table 1 is first downloaded into R Studio.

RES <- dbSendQuery(WRDS, "select *
                   from mfl.mflink1")
MFLink1 <- dbFetch(RES, n=-1)
dbClearResult(RES)
MFLink1

# Then, the Wharton Financial Institution Center Number (WFICN) for each ETF available
# in the MFLink Table 1 is extracted by searching with the CRSP Fund Number as an
# identifier.

ETF_WFICN <- MFLink1$crsp_fundno[MFLink1$crsp_fundno %in% ETF_all$crsp_fundno]

ETF_WFICN <- subset(MFLink1, (MFLink1$crsp_fundno %in% ETF_WFICN) == T)

# Next, the corresponding WFCINs are added to the existing ETF list. Multiple CRSP Fund Numbers
# sometimes have the exact same WFCIN since the CRSP Fund Numbers often represent different share
# classes of one and the same fund, while WFCIN tracks the fund portfolio instead, which may be
# identical for a fund with different share classes.

ETF_all <- merge(ETF_all, ETF_WFICN, by = "crsp_fundno", all.x = T)

# In a second step, the Mutual Fund Links (MFLinks) Table 2 is used to match the found WFICNs
# to the Thomson Reuters Fund Numbers. Again, first the entire MFLink Table 2 is downloaded
# into R Studio.

RES <- dbSendQuery(WRDS, "select *
                   from mfl.mflink2")
MFLink2 <- dbFetch(RES, n=-1)
dbClearResult(RES)
MFLink2

# Then, all available Thomson Reuters Fund Numbers for the ETFs are extracted from the MFLink 
# Table 2. The Thomson Reuters Fund Number is later needed in order to retrieve the equity
# holdings of the ETFs in the Thomson Reuters Mutual Fund Holdings. Each Thomson Reuters Fund
# Number represent a single fund portfolio, which can correspond to different share classes.

ETF_TR_Fundno <- MFLink2$wficn[MFLink2$wficn %in% ETF_WFICN$wficn]

ETF_TR_Fundno <- subset(MFLink2, (MFLink2$wficn %in% ETF_TR_Fundno) == T)

ETF_TR_Fundno <- subset(ETF_TR_Fundno, duplicated(ETF_TR_Fundno$fundno, fromLast = T) == F)

# Before adding the found Thomson Reuters Fund Numbers to the existing ETF list it is tested
# whether the matching CRSP Fund Number and Thomson Reuters Fund Number indeed represent the
# same fund by comparing their respective names and Fund Number Date (fdate).

Matching_Test_Fundno <- merge(ETF_all[ , c("crsp_fundno", "wficn", "ticker", "fund_name",
                                           "tna_latest")],
                              ETF_TR_Fundno[ , c("wficn", "fundno", "fundname", "fdate",
                                                 "country")], by = "wficn")

Matching_Test_Fundno <- Matching_Test_Fundno[order(Matching_Test_Fundno$tna_latest,
                                                   decreasing = T, na.last = T), ]

# There is one erroneous Thomson Reuters Fund Number (wficn = 102224, fundno = 41500), which
# is subsequently removed.

ETF_TR_Fundno <- ETF_TR_Fundno[-(ETF_TR_Fundno$fundno == 41500), ]

# Once it is ensured that the matching fund numbers are valid, the corresponding Thomson
# Reuters Fund Number is added to the existing ETF list.

ETF_all <- merge(ETF_all, ETF_TR_Fundno[ , c("wficn", "fundno", "country", "num_holdings")],
                 by = "wficn", all.x = T)

ETF_all <- ETF_all[ , c("crsp_fundno","wficn", "fundno", "cusip8", "ticker", "fund_name",
                        "tna_latest_dt", "tna_latest", "num_holdings", "mgmt_name","et_flag",
                        "nav_latest_dt", "nav_latest", "first_offer_dt", "per_com", "per_pref",
                        "per_conv", "per_corp", "per_muni", "per_govt", "per_oth", "per_cash",
                        "per_bond", "per_abs", "per_mbs", "per_eq_oth", "per_fi_oth", "open_to_inv",
                        "retail_fund", "inst_fund", "index_fund_flag", "dead_flag", "delist_cd",
                        "crsp_obj_cd", "lipper_obj_cd", "lipper_obj_name", "lipper_class",
                        "lipper_class_name", "si_obj_cd", "wbrger_obj_cd", "lipper_asset_cd",
                        "country")]

ETF_all <- ETF_all[order(ETF_all$tna_latest, decreasing = T, na.last = T), ]

# The finished ETF list with all identifiers and the Thomson Reuters Fund Numbers needed for
# extracting the portfolio holdings is saved in an external CSV file.

write.csv(ETF_all, file = "/Users/Matthieu/Desktop/HSG/Master's Thesis/ETPs and Systemic Risk/Data/Final_ETF_List.csv")


#                         ##########################################
#                         ## Thomson Reuters Mutual Fund Holdings ##
#                         ##########################################

# The ETF equity portfolio holdings come from the Thomson Reuters Mutual Fund Holdings (tfn.s12)
# database. It is important to keep in mind that the Thomson Reuters Fund Number is at portfolio
# level while the CRSP Fund Number is at share class level. All mutual fund holdings between
# 1 January 1993 and 1 June 2018 are downloaded into R Studio.

RES <- dbSendQuery(WRDS, "select fdate, rdate, fundno, fundname, assets, cusip, ticker, stkname,
                          country, stkcd, stkcdesc, shares, change, prc, shrout1, shrout2
                   from tfn.s12
                   where fdate between '1993-01-01'
                   and '2018-01-01'")
TR_Holdings <- dbFetch(RES, n=-1)
dbClearResult(RES)
TR_Holdings

TR_Holdings <- as.data.table(TR_Holdings)

# Then, only the fund holdings of ETFs are kept, while the remaining mutual fund holdings are
# removed due to storage constraints.

ETF_Holdings <- subset(TR_Holdings, (TR_Holdings$fundno %in% ETF_TR_Fundno$fundno) == T)

rm(TR_Holdings)

ETF_Holdings$cusip <- as.factor(ETF_Holdings$cusip)

# It is quickly checked in which country the stocks held by US ETFs are listed. It would obviously
# make sense to only keep US-listed stocks for further analysis in the sample. However, a quick
# check reveals that the stocks listed in countries differing from the US are in fact often
# mislabelled with the wrong country and are actually US stocks. Hence, the sample is kept as it
# is at the moment.

table(ETF_Holdings$country)

table((subset(ETF_Holdings, duplicated(ETF_Holdings$cusip, fromLast = T) == F))$stkcd)


# Historical fund tickers in s12type8 Table

RES <- dbSendQuery(WRDS, "select *
                   from tfn.s12type8")
TR_Fund_Ticker <- dbFetch(RES, n=-1)
dbClearResult(RES)
TR_Fund_Ticker

# Find all US equity ETFs or all ETFs available in the s12type8 Table by historical
# fund ticker




# ------------------------------ ETF Time Series Data ---------------------------------

#                   ############################################
#                   ########   ETF Prices and Returns   ########
#                   ############################################

#                   ############################################
#                   ########   ETF Bid and Ask Prices   ########
#                   ############################################

#                   ############################################
#                   ###########   ETF Share Volume   ###########
#                   ############################################

# ETF Prices are needed in order to compute the market capitalisation of ETFs, i.e. their
# AUM. AUM is later used to compute the ETF ownership variable. Simulatenously, closing
# prices, closing bid and ask prices, holding-period returns, share volume, and the cumulative
# factors to adjust prices and shares outstanding for all ETFs are gathered from the CRSP
# Daily Stock File (crsp.dsf). The cumulative factors to adjust prices and shares outstanding
# are needed to later correct ETF prices for share splits and other corporate actions.

RES <- dbSendQuery(WRDS, "select date, permno, cusip, prc, bid, ask, ret, vol, cfacpr, cfacshr
                   from crsp.dsf
                   where date between '1993-01-01'
                   and '2018-01-01'")
ETF_Time_Series <- dbFetch(RES, n=-1)
dbClearResult(RES)
ETF_Time_Series

ETF_Time_Series <- as.data.table(ETF_Time_Series)

ETF_Time_Series <- subset(ETF_Time_Series, ETF_Time_Series$cusip %in% ETF_all$cusip8)

colnames(ETF_Time_Series) <- c("Date", "Permno", "CUSIP", "Price", "Bid", "Ask", "Return",
                               "Volume", "Cfacpr", "Cfacshr")

ETF_Time_Series <- ETF_Time_Series[order(Permno, Date)]

# Some prices from CRSP represent bid/ask averages instead of actual closing prices. These are
# marked with a negative sign in front of the actual price and need to be transformed into
# absolute values.

ETF_Time_Series$Price <- abs(ETF_Time_Series$Price)


#                   ############################################
#                   ################   ETF NAV   ###############
#                   ############################################

# Daily ETF NAV per share is downloaded from the CRSP Mutual Fund Database (crsp.daily_nav_ret).
# NAV is only available beginning from 1 September 1998.

RES <- dbSendQuery(WRDS, "select caldt, crsp_fundno, dnav
                   from crsp.daily_nav_ret
                   where caldt between '1998-01-01'
                   and '2018-01-01'")
ETF_NAV <- dbFetch(RES, n=-1)
dbClearResult(RES)
ETF_NAV

ETF_NAV <- as.data.table(ETF_NAV)

colnames(ETF_NAV) <- c("Date", "CRSP Fundno", "NAV")

ETF_NAV <- subset(ETF_NAV, ETF_NAV$`CRSP Fundno` %in% ETF_all$crsp_fundno)

ETF_NAV <- ETF_NAV[order(`CRSP Fundno`, Date)]


#                   ############################################
#                   ########   ETF Shares Outstanding   ########
#                   ############################################

# Daily shares outstanding for ETFs come from Bloomberg and represent ETFs' daily creation and
# redemption units. Bloomberg is a more accurate source of shares outstanding than CRSP. The
# shares outstanding variable is recorded in millions of shares.

ETF_Shares_Outstanding <- fread("/Users/Matthieu/Desktop/HSG/Master's Thesis/ETFs and Systemic Risk/Data/Bloomberg/ETF Shrout/ETF Shrout R.csv",
                                header = T, sep = ",", skip = 1)

# Quickly checking which CUSIPs are not in standard eight digit format.

subset((colnames(ETF_Shares_Outstanding)),
       nchar((colnames(ETF_Shares_Outstanding)), type = "chars", allowNA = T, keepNA = NA) != 8)

# Only the date variable and one missing CUSIP are not read into R correctly from the Bloomberg
# Excel file.

colnames(ETF_Shares_Outstanding)[which(colnames(ETF_Shares_Outstanding) == "V2881")] <- "26922A51"

colnames(ETF_Shares_Outstanding)[1] <- "Date"

ETF_Shares_Outstanding$Date <- as.Date(ETF_Shares_Outstanding$Date)

# Then, the shares outstanding data is converted from wide to long format.

ETF_Shares_Outstanding <- melt(ETF_Shares_Outstanding,
                               measure.vars = colnames(ETF_Shares_Outstanding[ , -1]),
                               variable.name = "CUSIP", value.name = "Shares Outstanding")


# Next, all zero, and strikingly low or high values for ETF shares outstanding are cross-checked
# with data from OptionMetrics and Computstat. Missing values are replaced from these alternative
# sources if available. 10 ETFs have zero shares oustanding. These zero values are replaced by NAs
# to later prevent erroneous calculations.

subset(ETF_Time_Series, `Shares Outstanding` == 0)

unique(subset(ETF_Time_Series, `Shares Outstanding` == 0)$CUSIP)

ETF_Time_Series[which(`Shares Outstanding` == 0)] <- NA


subset(ETF_Time_Series, `Shares Outstanding` <= 0.001)

subset(ETF_Time_Series, `Shares Outstanding` >= 1000)


#                   ############################################
#                   ###   Final ETF Time Series Data Table   ###
#                   ############################################

# All ETF time series data, including daily prices, closing bid and ask prices, returns,
# share volume, and the cumulative factors to adjust prices and shares outstanding are
# joined in one data table.

ETF_Time_Series <- merge(ETF_Time_Series, ETF_all[ , c("cusip8", "crsp_fundno")],
                         by.x = "CUSIP", by.y = "cusip8", all.x = T)

ETF_Time_Series <- merge(ETF_Time_Series, ETF_NAV, by.x = c("crsp_fundno", "Date"),
                         by.y = c("CRSP Fundno", "Date"), all = T)

ETF_Time_Series <- merge(ETF_Time_Series, ETF_Shares_Outstanding,
                         by = c("CUSIP", "Date"), all.x = T)

# The corresponding Thomson Fundno (if available) as well as other identifiers such as the
# CRSP Fundno and the ticker are added to the ETF time series table.

ETF_Time_Series <- merge(ETF_Time_Series, subset(ETF_all[ , c("crsp_fundno", "cusip8", "fundno", "ticker")],
                                                 duplicated(crsp_fundno, fromLast = F) == F),
                         by.x = "CRSP Fundno", by.y = "crsp_fundno", all.x = T)

colnames(ETF_Time_Series)[14:16] <- c("cusip8", "Thomson Fundno", "Ticker")

ETF_Time_Series <- ETF_Time_Series[ , c("Date", "Permno", "CUSIP", "cusip8", "Ticker", "CRSP Fundno",
                                        "Thomson Fundno", "Price", "Bid", "Ask", "Return",
                                        "Shares Outstanding", "Volume", "NAV", "Cfacpr", "Cfacshr")]

ETF_Time_Series <- ETF_Time_Series[order(`CRSP Fundno`, Date)]


# --------------------------------- US Equity ETFs ------------------------------------

# Next, the sample of ETFs is narrowed down to contain only US equity ETFs. This is
# achieved by only keeping ETFs with a CRSP Objective Code (crsp_obj_cd) beginning with
# the letters "ED", which stands for Equity Domestic.

US_Equity_ETF <- subset(ETF_all, substr(ETF_all$crsp_obj_cd, start = 1, stop = 2) == "ED")

# An alternative procedure for selecting US equity-focused ETFs can be found in Ben-David
# et al. (2017). They use the Lipper Objective Code (lipper_obj_cd) to select Broad Based
# US Equity Funds and Sector Funds which invest in US companies.

US_Equity_ETF <- subset(ETF_all, (ETF_all$lipper_obj_cd %in% c("CA", "EI", "G", "GI","MC",
                                                               "MR", "SG", "SP", "BM", "CG",
                                                               "CS", "FS", "H", "ID","NR",
                                                               "RE", "TK", "TL", "S",
                                                               "UT")) == T)

# Check whether these funds are indeed US equity ETFs (e.g. by inspecting country variable)

table(US_Equity_ETF$country, exclude = NULL)


# ----------------------------------- Stock Data --------------------------------------

#                   ################################################
#                   ## Identifying Ordinary Common Stocks in CRSP ##
#                   ################################################

# Using the CRSP Daily Stock Names (crsp.stocknames) file to download all available common
# stocks in the CRSP database. The ordinary common shares are identified by setting the
# share code variable equal to 10 or 11.

RES <- dbSendQuery(WRDS, "select *
                   from crsp.stocknames
                   where shrcd = 10
                   or shrcd = 11")
Stock_Identifiers <- dbFetch(RES, n=-1)
dbClearResult(RES)
Stock_Identifiers

# I'm only interested in stock data beginning in January 1993 since that is the time when the
# first ETFs were launched. Thus, stocks which changed their ticker names before that date are
# of no interest and excluded.

Stock_Identifiers <- subset(Stock_Identifiers,
                            (Stock_Identifiers$nameenddt > as.Date("1993-01-01")) == T)

# The CUSIP then serves as a unique identifier for these ordinary common shares. The resulting
# list is saved externally and used to download data on shares outstanding for these stocks from
# Bloomberg.

Stock_Identifiers <- subset(Stock_Identifiers,
                            duplicated(Stock_Identifiers$cusip, fromLast = T) == F)

Stock_Identifiers <- Stock_Identifiers[order(Stock_Identifiers$cusip, decreasing = F, na.last = T), ]

write.csv(Stock_Identifiers, file = "/Users/Matthieu/Desktop/HSG/Master's Thesis/ETPs and Systemic Risk/Data/Final_Stock_List.csv")


#                   ############################################
#                   #######   Stock Prices and Returns   #######
#                   ############################################

#                   ############################################
#                   #######   Stock Bid and Ask Prices   #######
#                   ############################################

#                   ############################################
#                   ##########   Stock Share Volume   ##########
#                   ############################################

# The CRSP Daily Stock file (crsp.dsf) is now used to get the closing stock prices, closing bid and ask
# prices, holding-period returns, share volume, and the cumulative factors to adjust prices and shares
# outstanding. The cumulative factors to adjust prices and shares outstanding are needed to later correct
# prices for stock splits and other corporate actions.

RES <- dbSendQuery(WRDS, "select date, permno, cusip, prc, bid, ask, ret, vol, cfacpr, cfacshr
                   from crsp.dsf
                   where date between '1993-01-01'
                   and '2018-01-01'")
Stock_All <- dbFetch(RES, n=-1)
dbClearResult(RES)
Stock_All

Stock_All <- as.data.table(Stock_All)

Stock_All$date <- as.Date(Stock_All$date)

# Only the prices and returns of the common stocks identified in the previous step are kept,
# CRSP Permno serves as an identifier.

Stock_Time_Series <- subset(Stock_All, (Stock_All$permno %in% Stock_Identifiers$permno))

# Whenever the actual closing price is not available, a bid-ask average is used instead by
# CRSP. In order to indicate that it is a bid-ask price and not an actual closing price,
# CRSP adds a negative sign (-) to the price. This negative sign must be removed in order
# to accurately process the data.

Stock_Time_Series$prc <- abs(Stock_Time_Series$prc)

Stock_Time_Series <- Stock_Time_Series[order(Stock_Time_Series$permno), ]


#                   ############################################
#                   #######   Stock Shares Outstanding   #######
#                   ############################################

# The shares outstanding for all identified ordinary common stocks come from Bloomberg, since
# Bloomberg provides a more reliable source for daily shares outstanding than CRSP does. Shares
# outstanding are in millions.

Stock_Shares_Outstanding <- fread("/Users/Matthieu/Desktop/HSG/Master's Thesis/ETFs and Systemic Risk/Data/Bloomberg/Stock Shrout/Stock Shrout R.csv",
                                  header = T, sep = ",", skip = 1)

# Quickly checking which CUSIPs are not in standard eight digit format, which is for non the
# case.

subset((colnames(Stock_Shares_Outstanding)),
       nchar((colnames(Stock_Shares_Outstanding)), type = "chars", allowNA = T, keepNA = NA) != 8)

colnames(Stock_Shares_Outstanding)[1] <- "Date"

# Converting the date variable to date format in R. Then, the data table is converted from
# wide to long format.

Stock_Shares_Outstanding$Date <- as.Date(Stock_Shares_Outstanding$Date)

Stock_Shares_Outstanding <- melt(Stock_Shares_Outstanding,
                                 measure.vars = colnames(Stock_Shares_Outstanding[ , -1]),
                                 variable.name = "CUSIP", value.name = "Shares Outstanding")

# Next, all zero, and strikingly low or high values for stock shares outstanding are cross-checked
# with data from OptionMetrics and Computstat. Missing values are replaced from these alternative
# sources if available. 15 stocks have zero values for shares outstanding. These zero values are
# replaced by NAs to later prevent erroneous calculations. 

subset(Stock_Time_Series, `Shares Outstanding` == 0)

unique(subset(Stock_Time_Series, `Shares Outstanding` == 0)$Permno)

Stock_Time_Series[which(`Shares Outstanding` == 0)] <- NA


#                   ############################################
#                   #######   Final Stock Data Table   #########
#                   ############################################

# The final data table containing all stock-related variables for the following reseach
# analysis is created. The data table holds information on stock prices, total returns,
# bid and ask prices, share volume, the CRSP cumulative factors to adjust prices and
# shares outstanding, and shares outstanding.

Stock_Time_Series <- merge(Stock_Time_Series, Stock_Shares_Outstanding, 
                           by.x = c("cusip", "date"), by.y = c("CUSIP", "Date"), all.x = T)

Stock_Time_Series <- Stock_Time_Series[ , c("date", "permno", "cusip", "prc", "bid", "ask",
                                            "ret", "Shares Outstanding", "vol", "cfacpr",
                                            "cfacshr")]

colnames(Stock_Time_Series) <- c("Date", "Permno", "CUSIP", "Price", "Bid", "Ask", "Return",
                                 "Shares Outstanding", "Volume", "Cfacpr", "Cfashr")


#                   ############################################
#                   #############   Stock Filter   #############
#                   ############################################

# Stocks with an average share price below USD 2 and an average market capitalisation of under
# USD 100 millions across the sample period are excluded from the analysis. In order to apply
# this stock filter, first, the average share price for each stock across the sample period is
# computed.

TmpA <- melt(subset(Stock_Time_Series[ , c("Date", "Permno", "Price")], !is.na(Permno)
                    & !is.na(Date) & !is.na(Price)), id.vars = c("Date", "Permno"),
             measure.vars = "Price")

# Transformation from long to wide format.

TmpA <- dcast(TmpA, formula = Date ~ Permno)

# Computing the average price for each stock across the sample period, then extracting the
# Permnos of the stocks with an average share price below USD 2 in a separate list.

TmpA <- apply(TmpA[ , -1], 2, function(x) mean(x, na.rm = T))

Excluded_Stocks <- names(TmpA[TmpA < 2])

length(Excluded_Stocks)

# The list is then used to exclude stocks from the overall sample. The remaining number of
# unique stocks is 14'384 (out of 15'214).

Stock_Time_Series <- subset(Stock_Time_Series, !(Stock_Time_Series$Permno %in% Excluded_Stocks))

# Next, shares with an an average market capitalisation of under USD 100 millions are excluded.
# (This step assumes that the market capitalisation for each stock has already been computed in
# the file "Computed Variables.R")

TmpA <- melt(subset(Stock_Time_Series[ , c("Date", "Permno", "Market Cap")], !is.na(Permno)
                    & !is.na(Date) & !is.na(`Market Cap`)), id.vars = c("Date", "Permno"),
             measure.vars = "Market Cap")

# Transformation from long to wide format.

TmpA <- dcast(TmpA, formula = Date ~ Permno)

# Computing the average market capitalisation for each stock across the sample period, then extracting
# the Permnos of the stocks with an average market cap below USD 100 millions in a separate list.

TmpA <- apply(TmpA[ , -1], 2, function(x) mean(x, na.rm = T))

Excluded_Stocks <- names(TmpA[TmpA < 100])

length(Excluded_Stocks)

# The list is then used to exclude stocks from the overall sample. The remaining number of unique
# stocks is 9'668 (out of 14'384) after excluding those with an average share price below USD 2 and
# a market capitalisation of less than USD 100 millions.

Stock_Time_Series <- subset(Stock_Time_Series, !(Stock_Time_Series$Permno %in% Excluded_Stocks))

# In order to create a balanced panel of data, only observations containing non-NA values
# for prices, returns, and shares outstanding are kept.

# Stock_Time_Series <- subset(Stock_Time_Series, !is.na(Price) & !is.na(Return)
#                            & !is.na(`Shares Outstanding`))


# ------------------------- Fama-French Asset Pricing Factors --------------------------

# The Fama-French Factors are downloaded for a daily frequency from the WRDS database
# (ff.factors_daily).

RES <- dbSendQuery(WRDS, "select *
                   from ff.factors_daily
                   where date between '1993-01-01'
                   and '2018-01-01'")
FF_Factors <- dbFetch(RES, n=-1)
dbClearResult(RES)
FF_Factors

FF_Factors <- as.data.table(FF_Factors)

colnames(FF_Factors) <- c("Date", "MKTRF", "SMB", "HML", "RF", "UMD")

