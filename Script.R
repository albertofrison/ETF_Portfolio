#####
# A. INTRODUCTION
# R Project to better understand the composition of your ETF Portfolio
# I download the components of some ETFs listed in Xetra or Borsa Italiana from a number of providers (composing my Portfolio) such as iShares or Vanguard and I 
# analyse them from the Market, Area, Sector, Currency, ... point of view and hopefully provide more insights.
# Created - February 2025


#####
# B. LIBRARIES
library (tidyverse)
library (readxl)


#####
# C. INITIALIZATION / CLEANUP
rm (list = ls()) # cleans up objects in the environment


# ETF LIST with TICKERS



# IWSZ	iShares MSCI World Mid-Cap Equal Weight UCITS ETF	IWSZ	iShares

#####
# D. LOADING THE DATASET
# We will download locally the .xlsx file (through an excel object) of all the ETFs we need 
# then we will select the information (columns we need) from all the ETFs and store into a dataframe
# while creating the dataframe we will convert the data in the correct class (character, integer, ...)

# XDEW	Xtrackers S&P 500 Equal Weight UCITS ETF 1C	XDEW	Xtrackers
url <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BLNMYC90/"
destfile = paste("./data/","XDEW.xslx", sep ="")
download.file(url, destfile , method="auto")
data_XDEW <-read_xlsx(destfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 3) # open the first sheet and skips the first 3 rows

# IUSN	iShares MSCI World Small Cap UCITS ETF	WDSC	iShares
url <- "https://www.ishares.com/it/investitore-privato/it/prodotti/296576/fund/1538022822418.ajax?fileType=xls&fileName=iShares-MSCI-World-Small-Cap-UCITS-ETF-USD-Acc_fund&dataType=fund"
destfile = paste("./data/","IUSN.xls", sep ="")
download.file(url, destfile , method="auto")
# for some reasons it is needed to open and save the file again before importing it
data_IUSN <- read_xls(destfile, sheet = "Partecipazioni", col_names = TRUE, col_types = NULL, na = "", skip = 7) # open the first sheet and skips the first 7 rows

# EXUS	Xtrackers MSCI World ex USA UCITS ETF 1C	EXUS	Xtrackers
url <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE0006WW1TQ4/"
destfile = paste("./data/","EXUS.xlsx", sep ="")
download.file(url, destfile , method="auto")
data_EXUS <- read_xlsx(destfile, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 3) # open the first sheet and skips the first 7 rows

# EIMI	iShares Core MSCI Emerging Markets IMI UCITS ETF (Acc)
url <- "https://www.ishares.com/it/investitore-privato/it/prodotti/264659/ishares-msci-emerging-markets-imi-ucits-etf/1538022822418.ajax?fileType=xls&fileName=iShares-Core-MSCI-EM-IMI-UCITS-ETF-USD-Acc_fund&dataType=fund"
destfile = paste("./data/","EIMI.xls", sep ="")
download.file(url, destfile , method="auto")
# for some reasons it is needed to open and save the file again before importing it
data_EIMI <- read_xls(destfile, sheet = "Partecipazioni", col_names = TRUE, col_types = NULL, na = "", skip = 7) # open the first sheet and skips the first 7 rows

# IWSZ	iShares MSCI World Mid-Cap Equal Weight UCITS ETF	IWSZ	iShares
url <- "https://www.ishares.com/it/investitore-privato/it/prodotti/270057/ishares-msci-world-size-factor-ucits-etf/1538022822418.ajax?fileType=xls&fileName=IWSZ&dataType=fund"
destfile = paste("./data/","IWSZ.xls", sep ="")
download.file(url, destfile , method="auto")
# for some reasons it is needed to open and save the file again before importing it
data_IWSZ <- read_xls(destfile, sheet = "Partecipazioni", col_names = TRUE, col_types = NULL, na = "", skip = 7) # open the first sheet and skips the first 7 rows

#####
# E. CREATING AND FORMATTING THE DATAFRAME

