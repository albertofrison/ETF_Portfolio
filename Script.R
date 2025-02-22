#####
# A. INTRODUCTION
# R Project to better understand the composition of your ETF Portfolio
# I download the components of some ETFs listed in Xetra or Borsa Italiana from a number of providers (composing my Portfolio) such as iShares or Vanguard and I 
# analyse them from the Market, Area, Sector, Currency, ... point of view and hopefully provide more insights.
# Created - February 2025


#####
# B. LIBRARIES
library (tidyverse)


#####
# C. INITIALIZATION / CLEANUP
rm (list = ls()) # cleans up objects in the environment


# ETF LIST with TICKERS


# IUSN	iShares MSCI World Small Cap UCITS ETF	WDSC	iShares
# EXUS	Xtrackers MSCI World ex USA UCITS ETF 1C	EXUS	Xtrackers
# EIMI	iShares Core MSCI Emerging Markets IMI UCITS ETF (Acc) [MSCI Emerging Markets Investable Market (IMI)]	EIMI	iShares
# IWSZ	iShares MSCI World Mid-Cap Equal Weight UCITS ETF	IWSZ	iShares

#####
# D. LOADING THE DATASET - XDEW	Xtrackers S&P 500 Equal Weight UCITS ETF 1C	XDEW	Xtrackers

# We will download locally the .xlsx file and open through an excel object
url <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BLNMYC90/" # url to the excel costituent file in DWS / Xtrackers website

destfile = paste("./data/","XDEW.xslx", sep ="")
download.file(url, destfile , method="auto")

data_XDEW <-readxl::read_excel(destfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 3) # open the first sheet and skips the first 3 rows
