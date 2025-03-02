# A. INTRODUCTION --------------------------------------------------------------
# R Project to better understand the composition of my  ETF Portfolio
# I download the components of some ETFs listed in Xetra or Borsa Italiana from a number of providers (composing my Portfolio) such as iShares or Vanguard and I 
# analyse them from the Market, Area, Sector, Currency, ... point of view and hopefully provide more insights.
# For some reasons some XLS files need to be opened and saved in Excel prior opening them into R.
# Created - February 2025


# B. LIBRARIES -----------------------------------------------------------------
library (tidyverse)
library (httr)
library (readxl)
library (rvest) 
library (forcats)


# C. INITIALIZATION / CLEANUP --------------------------------------------------
rm (list = ls()) # cleans up objects in the environment


# D. LOADING THE DATASET -------------------------------------------------------
# We will download locally the .xlsx file (through an excel object) of all the ETFs we need 
# then we will select the information (columns we need) from all the ETFs and store into a dataframe
# while creating the dataframe we will convert the data in the correct class (character, integer, ...)


# URL del file Excel dei datasets
# Azionari
url1 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BLNMYC90/" #XDEW
url2 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/296576/fund/1538022822418.ajax?fileType=xls&fileName=iShares-MSCI-World-Small-Cap-UCITS-ETF-USD-Acc_fund&dataType=fund" #IUSN
url3 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE0006WW1TQ4/" #EXUS
url4 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/264659/ishares-msci-emerging-markets-imi-ucits-etf/1538022822418.ajax?fileType=xls&fileName=iShares-Core-MSCI-EM-IMI-UCITS-ETF-USD-Acc_fund&dataType=fund" #EIMI
url5 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/270057/ishares-msci-world-size-factor-ucits-etf/1538022822418.ajax?fileType=xls&fileName=IWSZ&dataType=fund" # IWSZ
# Obbligazionari  
url6 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/291770/fund/1538022822418.ajax?fileType=xls&fileName=iShares-Core-Global-Aggregate-Bond-UCITS-ETF-EUR-Hedged-Acc_fund&dataType=fund" #AGGH

urls <- c(url1, url2, url3, url4, url5, url6)
skip_rows <- c(3,7,3,7,7,7) #each xls has a different number of rows to be skipped when opened 
keep_columns <- list(c(2,4,5,7,10,11),c(2,3,4,6,10,12),c(2,4,5,7,10,11), c(2,3,4,6,10,12), c(2,3,4,6,10,12),c(2,3,4,6,11,16))
etf_names <- c("XDEW", "IUSN", "EXUS", "EIMI", "IWSZ", "AGGH")
etf_weights <- c(0.25,0.0,0.30,0.13,0.15, 0.17)
columns_name <- list(c("Name","Country","Currency","Asset_Class","Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Industry","Asset_Class","Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Country","Currency","Asset_Class","Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Industry","Asset_Class","Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Industry","Asset_Class","Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Industry","Asset_Class","Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"))
                  

# Here we prepare the dataframe containing all data necessary to download the files
df_urls = data.frame (url = urls,
                      skip_row = skip_rows,
                      etf_name = etf_names,
                      etf_weight = etf_weights,
                      stringsAsFactors = FALSE)  
df_urls$keep_column <- keep_columns  
df_urls$column_name <- columns_name 


# portfolio will contain the consolidated data, from all ETFs
portfolio <- ""
columns <- c("Name","Country","Currency","Asset_Class","Industry", "Weight", "ETF", "PTF_Weight", "EffectiveWeight")
portfolio = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(portfolio) <- columns


# automated download of the excels and inclusion into the portfolio dataframe
for (i in 1:nrow(df_urls)) {
  # Crea i file temporanei
  temp_file <- tempfile(fileext = ".xls")
  converted_file <- tempfile(fileext = ".xlsx")  # File finale in formato xlsx
  
  current_url <- df_urls$url[i]
  # Scaricare il file
  res <- GET(current_url, write_disk(temp_file, overwrite = TRUE), add_headers("User-Agent" = "Mozilla/5.0"))
  
  # Convertire il file usando LibreOffice (headless mode)
  system(paste("libreoffice --headless --convert-to xlsx", shQuote(temp_file), "--outdir", shQuote(dirname(converted_file))), wait = TRUE)
  
  # Ottenere il nome effettivo del file convertito
  converted_file <- sub("\\.xls$", ".xlsx", temp_file)
  
  # Leggere il file Excel convertito
  df <- try(read_xlsx(converted_file, skip = df_urls$skip_row[i]), silent = TRUE)
  

  df <- df[df_urls$keep_column[[i]]]
  df$ETF <- df_urls$etf_name[i]
  df$PTF_W <- df_urls$etf_weight[i]
  df$EffectiveWeight <- 0
  colnames(df) <- df_urls$column_name[[i]]
   
    
  # Appending each ETF into the global dataframe
  portfolio <- rbind(portfolio,df)

}


# Formatting the Name of the Countries
portfolio$Country <- ifelse (portfolio$Country == "Stati Uniti d'America", "Stati Uniti", portfolio$Country)
portfolio$Country <- ifelse (portfolio$Country == "Paesi Bassi (Olanda)", "Olanda", ifelse (portfolio$Country == "Paesi Bassi", "Olanda", portfolio$Country))
portfolio$Country <- ifelse (portfolio$Country == "Repubblica di Corea (Corea del Sud)", "Corea", portfolio$Country)
portfolio$Country <- ifelse (portfolio$Country == "Regno Unito", "Regno unito", portfolio$Country)


# Creating the MacroArea information 
portfolio <- portfolio %>%
  mutate (MacroArea = case_when(
    Country == "Stati Uniti" ~ "USA",
    Country == "Italia" ~ "Europe",
    Country == "Francia" ~ "Europe",
    Country == "Germania" ~ "Europe",
    Country == "Irlanda" ~ "Europe",
    Country == "Spagna" ~ "Europe",
    Country == "Danimarca" ~ "Europe",
    Country == "Finlandia" ~ "Europe",
    Country == "Norvegia" ~ "Europe",
    Country == "Belgio" ~ "Europe",
    Country == "Lussemburgo" ~ "Europe",
    Country == "Polonia" ~ "Europe",
    Country == "Austria" ~ "Europe",
    Country == "Portogallo" ~ "Europe",
    Country == "Grecia" ~ "Europe",
    Country == "Ungheria" ~ "Europe",
    Country == "Olanda" ~ "Europe",
    Country == "Svezia" ~ "Europe",
    Country == "Repubblica Ceca" ~ "Europe",
    Country == "Unione Europea" ~ "Europe",
    Country == "Svizzera" ~ "Europe",
    Country == "Regno Unito" ~ "Europe",
    Country == "Russia" ~ "Europe",
    Country == "Regno unito" ~ "Europe",
    Country == "Giappone" ~ "Giappone",
    Country == "Canada" ~ "Other Developed",
    Country == "Australia" ~ "Other Developed",
    Country == "Hong Kong" ~ "Other Developed",
    Country == "Singapore" ~ "Other Developed",
    Country == "Corea" ~ "Other Developed",
    Country == "Nuova Zelanda" ~ "Other Developed",
    Country == "Taiwan" ~ "Other Developed",
    Country == "Israele" ~ "Other Developed",
    Country == "Cina" ~ "Cina",
    TRUE ~ "Developing"
  )
)


# Formatting the Industry
portfolio$Industry <- ifelse (portfolio$Industry == "Finanziari", "Finanza", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Tecnologia dell'informazione", "IT", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Servizi di comunicazione", "Comunicazione", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Immobili", "Immobiliare", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Prodotti industriali", "Industriali", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Altro", "Liquidità e/o derivati", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Beni di prima necessità", "Generi di largo consumo", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Sanità", "Salute", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Beni voluttuari", "Consumi Discrezionali", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Imprese di servizi di pubblica utilità", "Utilities", portfolio$Industry)
portfolio$Industry <- ifelse (portfolio$Industry == "Servizi di pubblica utilità", "Utilities", portfolio$Industry)

# Formatting the Asset Class
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Azioni", "Azionario", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Future", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Futures", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Cash", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Contanti", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "FX", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Money Market", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Cash Collateral and Margins", "Other", portfolio$Asset_Class)


# Trasformation of character data into Factors or Numbers, where it makes sense
portfolio$Country <- as.factor(portfolio$Country)
portfolio$Currency <- as.factor (portfolio$Currency)
portfolio$Weight <- as.numeric (portfolio$Weight)
portfolio$ETF <- as.factor(portfolio$ETF)
portfolio$Industry <- as.factor(portfolio$Industry)
portfolio$MacroArea <- as.factor(portfolio$MacroArea)
portfolio$Asset_Class <-as.factor(portfolio$Asset_Class)

summary (portfolio)


portfolio <- portfolio %>%
  group_by(ETF) %>%
  mutate(Weight_A = Weight / unique(sum(Weight, na.rm = TRUE))) %>%
  ungroup()

portfolio$Effective_Weight <- portfolio$Weight_A * portfolio$PTF_Weight

portfolio %>%
  group_by(ETF) %>%
  summarize (w = sum (Weight_A, na.rm = T), t = sum (Effective_Weight, na.rm = T))

summary (portfolio)             

# F. Analysis ------------------------------------------------------------------
portfolio %>%
  group_by (Country) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE)


# CURRENCY ---------------------------------------------------------------------
# Bar Chart to illustrate the relative importance, the exposure of the portfolio, to each Currency
portfolio %>%
  group_by (Currency) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total)) %>%
  ggplot (aes (x = reorder (Currency, -total), y = total, fill = Currency)) +
  geom_bar(stat = "identity") +
  geom_text (aes(label = format (round (total*100, digits = 2), digits = 2, scientific = FALSE) ),vjust = -1, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none") +
  labs(title = "Esposizione del portafoglio alle Valute",
       subtitle ="Dati in %",
       x = "Valuta",
       y ="% sul Portafoglio")

# ... here in pie-chart form
portfolio %>%
  group_by (Currency) %>%
  summarise (Weight = sum(Effective_Weight, na.rm = TRUE)) %>%
  arrange(desc(Weight)) %>%
  mutate(Currency = case_when(
    row_number() <= 6 ~ Currency,
    TRUE ~ "Others")) %>%
  group_by(Currency) %>%
  summarise(Weight = sum(Weight)) %>%
  arrange(desc(Weight), by_group = TRUE) %>%
  mutate(Currency = factor(Currency, levels = rev(c(setdiff(unique(Currency), "Others"), "Others")))) %>%
  ggplot (aes (x = "", y = Weight, fill = Currency)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.15) +
  coord_polar ("y", start = 0) +
  geom_text(aes(label = paste0(Currency, " ", round(Weight*100), "%")), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values=c("#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#D47309", "#AAABBB")) +
  labs(title = "Esposizione del portafoglio alle Valute",
       subtitle ="Dati in %",
       x = "Valuta",
       y ="% sul Portafoglio")


portfolio %>%
  group_by (Currency) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE)

# recupero i dati dal sito trading economics
url <- "https://tradingeconomics.com/country-list/interest-rate"
webpage <- read_html(url)
table <- webpage %>%
  html_element("table") %>%
  html_table(fill = TRUE)

# pulisco i dati e aggiusto il dataframe per il plotting
df_interest_rates <- table %>%
  select (Country, Last, Previous) %>%
  filter (Country %in% c("United States", "Euro Area", "Japan", "United Kindgom", "Canada")) %>%
  mutate (across(c(Last, Previous), ~  as.numeric(gsub("%", "", .)), .names = "clean_{.col}")) %>%
  select (Country, clean_Last, clean_Previous) %>%
  pivot_longer(cols = c("clean_Last", "clean_Previous"), names_to = "Period", values_to = "Rate")

# plot dei chart
df_interest_rates %>%
  ggplot (aes(x = Country, y = Rate, fill = Period))+
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round (Rate, 2)), 
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 4) +
  theme_minimal() +
  scale_fill_manual (values = c("clean_Last" = "blue", "clean_Previous" = "darkgreen"))


#####
# COUNTRY
portfolio %>%
  group_by (Country) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  ggplot (aes (x = reorder (Country, -total), y = total, fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text (aes(label = format (round (total*100, digits = 2), digits = 2, scientific = FALSE) ),vjust = -1, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), legend.position = "none")


#####
# INDUSTRY
portfolio %>%
  group_by (Industry) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  ggplot (aes (x = reorder (Industry, -total), y = total, fill = Industry)) +
  geom_bar(stat = "identity") +
  geom_text (aes(label = format (round (total*100, digits = 2), digits = 2, scientific = FALSE) ),vjust = -1, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), legend.position = "none")



#####
# MACRO AREA
portfolio %>%
  group_by (MacroArea) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  ggplot (aes (x = reorder (MacroArea, -total), y = total, fill = MacroArea)) +
  geom_bar(stat = "identity") +
  geom_text (aes(label = format (round (total*100, digits = 2), digits = 2, scientific = FALSE) ),vjust = -1, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), legend.position = "none")



#####
# ASSET CLASS
portfolio %>%
  group_by (Asset_Class) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  ggplot (aes (x = reorder (Asset_Class, -total), y = total, fill = Asset_Class)) +
  geom_bar(stat = "identity") +
  geom_text (aes(label = format (round (total*100, digits = 2), digits = 2, scientific = FALSE) ),vjust = -1, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), legend.position = "none")








##### --------------------------------------------------------------------------
# Examples of double entry tables - experimental
a <- portfolio %>%
  group_by (MacroArea) %>%
  summarise (Weight = as.numeric(format (round(sum(Effective_Weight, na.rm = T), digits = 3), digits = 2)),
             Number = as.numeric(format (round(n(), digits = 3), digits = 2))
  ) %>%
  arrange (desc(Weight))
a


xtabs (round (Effective_Weight, digits = 3) ~ MacroArea + Industry, data = portfolio)

#install.packages("janitor")
library(janitor)
library(scales)

df_pivot <- portfolio %>%
  group_by(MacroArea, Industry) %>%
  summarise(Total = sum(Effective_Weight, na.rm = TRUE)) %>%
  pivot_wider(names_from = Industry, values_from = Total, values_fill = 0) %>%
  adorn_totals(where = c("row", "col")) %>%
  mutate(across(where(is.numeric), ~ percent(round(., 2))))

print(df_pivot)
#view(df_pivot)


##### Retrieve Historical data for simulations ---------------------------------
library(quantmod)
library (tidyverse)

# scarico i dati storici da yahoo
getSymbols(Symbols = c("^SPXEW", "^GSPC", "WSML.L"), src = "yahoo", from = "1900-01-01", to = Sys.Date())
head (SPXEW)
head(GSPC)
head(WSML.L)


# converto SPXEW in un dataframe
df_SPXEW <- data.frame (Date = index(SPXEW), coredata(SPXEW))
df_GSPC <- data.frame (Date = index(GSPC), coredata(GSPC))


colnames(df_SPXEW) <- c ("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(df_GSPC) <- c ("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

df_SPXEW$Index <- "S&P500 Equal Weight"
df_GSPC$Index <- "S&P500 Market Weight"

df_binded <- inner_join(df_SPXEW, df_GSPC, by = "Date", suffix = c("_SP500_EW", "_SP500_MW"))
first_date <- min(df_binded$Date)

head(df_binded)

df_binded <- df_binded %>%
  mutate (Close_SPXEW_Norm = (Close_SP500_EW / Close_SP500_EW [Date == first_date])*100,
          Close_GSPC_Norm = (Close_SP500_MW / Close_SP500_MW [Date == first_date])*100
  ) %>%
  pivot_longer(cols = starts_with("Close_"), names_to = "Index", values_to = "Price") %>%
  mutate (Index = recode(Index,
                         "Close_SP500_EW" = "S&P 500 Equal Weight",
                         "Close_SP500_MW" = "S&P 500 Market Weight",
                         "Close_SPXEW_Norm" = "S&P 500 Equal Weight Standard",
                         "Close_GSPC_Norm" = "S&P 500 Market Weight Standard"
  ))

df_binded %>%
  filter (Index %in% c("S&P 500 Market Weight Standard", "S&P 500 Equal Weight Standard")) %>%
  ggplot(aes (x = Date, y = Price, color = Index)) +
  geom_line()

#### Fx Exchange Rates ---------------------------------------------------------
# Exchange Rates are useful to understand wether to hedge or not the portfolio
getSymbols(Symbols = c("EURUSD=X", "EURJPY=X"), src = "yahoo", from = "1900-01-01", to = Sys.Date())
df_EURUSD <- data.frame (Date = index(`EURUSD=X`), coredata(`EURUSD=X`))
df_EURJPY <- data.frame (Date = index(`EURJPY=X`), coredata(`EURJPY=X`))

df_Currencies <- inner_join(df_EURUSD,df_EURJPY, by = "Date")
#first_date <- min(df_Currencies$Date)

start_date <- "2020-01-01"

df_Currencies %>%
  select(Date, EURUSD.X.Close, EURJPY.X.Close) %>%
  #pivot_longer(cols = c("EURUSD.X.Close", "EURJPY.X.Close"), names_to = "Currency", values_to = "FxRate") %>%
  filter (Date >= start_date) %>%
  mutate (EURUSD_Normalized = (EURUSD.X.Close / EURUSD.X.Close[Date == start_date])*100,
          EURJPY_Normalized = (EURJPY.X.Close / EURJPY.X.Close[Date == start_date])*100,
  ) %>%
  select(Date, EURUSD_Normalized, EURJPY_Normalized) %>%
  pivot_longer(cols = c("EURUSD_Normalized", "EURJPY_Normalized"), names_to = "Currency", values_to = "FxRate") %>%
  ggplot(aes (x = Date, y = FxRate)) +
  geom_line() +
  facet_wrap(~ Currency, nrow = 1)





##### ALTRI ESPERIEMENTI PER SCARICARE DATI DEGLI ETF, COME IL PE --------------
library(rvest)

# URL dell'ETF su Morningstar (sostituisci con l'ETF che ti interessa)
url <- "https://tools.morningstar.co.uk/uk/xray/default.aspx?LanguageId=en-GB&PortfolioType=2&SecurityTokenList=F00000ZN35]2]0]FOGBR$$ALL,FOCHI$$ONS&values=100.00&CurrencyId=GBP&from=editholding"

# Scarica la pagina HTML
pagina <- read_html(url)

# Estrai il valore del P/E ratio
pe_ratio <- pagina %>%
  html_nodes(xpath = "//td[contains(text(), 'Price/Earnings Ratio')]//following-sibling::td") %>%
  html_text() %>%
  trimws()

# Mostra il P/E r
print(paste("P/E Ratio:", pe_ratio))



library(rvest)

url <- "https://tools.morningstar.co.uk/it/xray/default.aspx?LanguageId=en-GB&PortfolioType=2&SecurityTokenList=0P0001SY1G]2]0]ETALL$$ALL&values=100&CurrencyId=EUR&from=editholding"

page <- read_html(url)

# Estrarre il valore nella cella accanto a "Price/Earnings Ratio"
pe_ratio <- page %>%
  html_nodes(xpath = "//th[contains(text(), 'Price/Earnings Ratio')]/following-sibling::td") %>%
  html_text() %>%
  trimws() %>%
  .[1]

pb_ratio <- page %>%
  html_nodes(xpath = "//th[contains(text(), 'Price/Book Ratio')]/following-sibling::td") %>%
  html_text() %>%
  trimws() %>%
  .[1]

print(as.numeric(pe_ratio))
print(as.numeric(pb_ratio))
