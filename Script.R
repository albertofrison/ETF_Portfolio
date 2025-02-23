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
install.packages("openxlsx")
library(openxlsx)
install.packages("rio")
library(rio)


#####
# C. INITIALIZATION / CLEANUP
rm (list = ls()) # cleans up objects in the environment



#####
# D. LOADING THE DATASET
# We will download locally the .xlsx file (through an excel object) of all the ETFs we need 
# then we will select the information (columns we need) from all the ETFs and store into a dataframe
# while creating the dataframe we will convert the data in the correct class (character, integer, ...)

# XDEW	Xtrackers S&P 500 Equal Weight UCITS ETF 1C	XDEW	Xtrackers
url <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BLNMYC90/"
destfile = paste("./data/","XDEW.xlsx", sep ="")
download.file(url, destfile, method="auto", mode ="wb")
data_XDEW <- read_excel(destfile, sheet = 1, col_names = TRUE, skip = 3)



# IUSN	iShares MSCI World Small Cap UCITS ETF	WDSC	iShares
url <- "https://www.ishares.com/it/investitore-privato/it/prodotti/296576/fund/1538022822418.ajax?fileType=xls&fileName=iShares-MSCI-World-Small-Cap-UCITS-ETF-USD-Acc_fund&dataType=fund"
destfile = paste("./data/","IUSN.xls", sep ="")
download.file(url, destfile, method="auto", mode ="wb")
data_IUSN <- read_excel(destfile, sheet = 1, col_names = TRUE, skip = 7)




# EXUS	Xtrackers MSCI World ex USA UCITS ETF 1C	EXUS	Xtrackers
url <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE0006WW1TQ4/"
destfile = paste("./data/","EXUS.xlsx", sep ="")
download.file(url, destfile, method="auto", mode ="wb")
data_EXUS <- read_excel(destfile, sheet = 1, col_names = TRUE, skip = 3)




# EIMI	iShares Core MSCI Emerging Markets IMI UCITS ETF (Acc)
url <- "https://www.ishares.com/it/investitore-privato/it/prodotti/264659/ishares-msci-emerging-markets-imi-ucits-etf/1538022822418.ajax?fileType=xls&fileName=iShares-Core-MSCI-EM-IMI-UCITS-ETF-USD-Acc_fund&dataType=fund"
destfile = paste("./data/","EIMI.xls", sep ="")
download.file(url, destfile, method="auto", mode ="wb")
data_EIMI <- read_excel(destfile, sheet = 1, col_names = TRUE, skip = 7)




# IWSZ	iShares MSCI World Mid-Cap Equal Weight UCITS ETF	IWSZ	iShares
url <- "https://www.ishares.com/it/investitore-privato/it/prodotti/270057/ishares-msci-world-size-factor-ucits-etf/1538022822418.ajax?fileType=xls&fileName=IWSZ&dataType=fund"
destfile = paste("./data/","IWSZ.xls", sep ="")
download.file(url, destfile, method="auto", mode ="wb")
data_IWSZ <- read_excel(destfile, sheet = 1, col_names = TRUE, skip = 7)


#####
# E. CREATING AND FORMATTING THE DATAFRAME


#keeping the necessary columns
data_XDEW <- data_XDEW[c(2,4,5,10,11)] 
data_IUSN <- data_IUSN[c(2,3,6,10,12)]
data_EXUS <- data_EXUS[c(2,4,5,10,11)]
data_EIMI <- data_EIMI[c(2,3,6,10,12)]
data_IWSZ <- data_IWSZ[c(2,3,6,10,12)]

data_XDEW$ETF <- "XDEW"
data_IUSN$ETF <- "IUSN"
data_EXUS$ETF <- "EXUS"
data_EIMI$ETF <- "EIMI"
data_IWSZ$ETF <- "IWSZ"

data_XDEW$PTF_W <- 0.25
data_IUSN$PTF_W <- 0.10
data_EXUS$PTF_W <- 0.37
data_EIMI$PTF_W <- 0.15
data_IWSZ$PTF_W <- 0.13


data_IUSN$`Ponderazione (%)` <- as.numeric(data_IUSN$`Ponderazione (%)`/100)
data_EXUS$Weighting <- as.numeric(data_EXUS$Weighting)
data_EIMI$`Ponderazione (%)` <- as.numeric(data_EIMI$`Ponderazione (%)`/100)
data_IWSZ$`Ponderazione (%)` <- as.numeric(data_IWSZ$`Ponderazione (%)`/100)


colnames(data_XDEW) <- c("Name","Country","Currency","Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight")
colnames(data_IUSN) <- c("Name","Industry","Weight","Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight")
colnames(data_EXUS) <- c("Name","Country","Currency","Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight")
colnames(data_EIMI) <- c("Name","Industry","Weight","Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight")
colnames(data_IWSZ) <- c("Name","Industry","Weight","Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight")


data_XDEW$Effective_Weight <- as.numeric(data_XDEW$PTF_Weight) * as.numeric(data_XDEW$Weight)
data_IUSN$Effective_Weight <- as.numeric(data_IUSN$PTF_Weight) * as.numeric(data_IUSN$Weight)
data_EXUS$Effective_Weight <- as.numeric(data_EXUS$PTF_Weight) * as.numeric(data_EXUS$Weight)
data_EIMI$Effective_Weight <- as.numeric(data_EIMI$PTF_Weight) * as.numeric(data_EIMI$Weight)
data_IWSZ$Effective_Weight <- as.numeric(data_IWSZ$PTF_Weight) * as.numeric(data_IWSZ$Weight)

portfolio <- ""

columns <- c("Name","Country","Currency","Industry", "Weight", "ETF", "PTF_Weight", "EffectiveWeight")
portfolio = data.frame(matrix(nrow = 0, ncol = length(columns))) 

colnames(portfolio) <- columns

portfolio <- rbind(portfolio,data_XDEW)
portfolio <- rbind(portfolio,data_IUSN)
portfolio <- rbind(portfolio,data_EXUS)
portfolio <- rbind(portfolio,data_EIMI)
portfolio <- rbind(portfolio,data_IWSZ)


portfolio$Country <- ifelse (portfolio$Country == "Stati Uniti d'America", "Stati Uniti", portfolio$Country)
portfolio$Country <- as.factor(portfolio$Country)
portfolio$Currency <- as.factor (portfolio$Currency)
portfolio$Weight <- as.numeric (portfolio$Weight)
portfolio$ETF <- as.factor(portfolio$ETF)
  
summary (portfolio)


portfolio %>%
  group_by (Country) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE)

portfolio %>%
  group_by (Currency) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  ggplot (aes (x = reorder (Currency, -total), y = total, fill = Currency)) +
  geom_bar(stat = "identity") +
  theme_minimal()

