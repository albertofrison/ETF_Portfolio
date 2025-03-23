# A. INTRODUCTION --------------------------------------------------------------
# R Project to better understand the composition of my  ETF Portfolio
# I download the components of some ETFs listed in Xetra or Borsa Italiana from a number of providers (composing my Portfolio) such as iShares or Vanguard and I 
# analyse them from the Market, Area, Sector, Currency, ... point of view and hopefully provide more insights.
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


# URL del file Excel dei datasets ----------------------------------------------
# Azionari
url1 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BLNMYC90/" #XDEW
url2 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/296576/fund/1538022822418.ajax?fileType=xls&fileName=iShares-MSCI-World-Small-Cap-UCITS-ETF-USD-Acc_fund&dataType=fund" #IUSN
url3 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE0006WW1TQ4/" #EXUS
url4 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/264659/ishares-msci-emerging-markets-imi-ucits-etf/1538022822418.ajax?fileType=xls&fileName=iShares-Core-MSCI-EM-IMI-UCITS-ETF-USD-Acc_fund&dataType=fund" #EIMI
url5 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/270057/ishares-msci-world-size-factor-ucits-etf/1538022822418.ajax?fileType=xls&fileName=IWSZ&dataType=fund" # IWSZ
# Obbligazionari  
url6 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/291770/fund/1538022822418.ajax?fileType=xls&fileName=iShares-Core-Global-Aggregate-Bond-UCITS-ETF-EUR-Hedged-Acc_fund&dataType=fund" #AGGH
#Azionario
url7 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BL25JM42/"
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# preparation of the parameters for automated load of the portfolio data
urls <- c(url1, url2, url3, url4, url5, url6, url7) # list of urls
skip_rows <- c(3,7,3,7,7,7,3) #each xls has a different number of rows to be skipped when opened 
keep_columns <- list(c(2,4,5,7,10,11),c(2,3,4,6,10,12),c(2,4,5,7,10,11), c(2,3,4,6,10,12), c(2,3,4,6,10,12),c(2,3,4,6,11,16), c(2,4,5,7,10,11)) # each issuer has a different xls structure
etf_names <- c("XDEW", "IUSN", "EXUS", "EIMI", "IWSZ", "AGGH", "XDEV") # names of the ETFs
etf_weights <- c(0.23,0.05,0.28,0.08,0.08,0.20,0.08) # weight in my portfolio
columns_name <- list(c("Name","Country","Currency","Asset_Class","Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Industry","Asset_Class","Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Country","Currency","Asset_Class","Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Industry","Asset_Class","Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Industry","Asset_Class","Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Industry","Asset_Class","Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                  c("Name","Country","Currency","Asset_Class","Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight"))
                  

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
# WARNING: at least in my system Ubuntu + LibreOffice - you need to have Libre Office opened before downloading the files
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
################################################################################
# All data is now loaded, starting to clean the data

################################################################################
# DATA CLEANING
# Formatting the Name of the Countries
portfolio$Country <- ifelse (portfolio$Country == "Stati Uniti d'America", "Stati Uniti", portfolio$Country)
portfolio$Country <- ifelse (portfolio$Country == "Paesi Bassi (Olanda)", "Olanda", ifelse (portfolio$Country == "Paesi Bassi", "Olanda", portfolio$Country))
portfolio$Country <- ifelse (portfolio$Country == "Repubblica di Corea (Corea del Sud)", "Corea", portfolio$Country)
portfolio$Country <- ifelse (portfolio$Country == "Regno Unito", "Regno unito", portfolio$Country)
portfolio$Country <- ifelse (nchar(as.character(portfolio$Country)) <= 2, "Other", portfolio$Country)

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
# note: some work should be done for the "Obbligazionario" as multiple and different industries are mentioned


# Formatting the Asset Class
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Azioni", "Azionario", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Future", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Futures", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Cash", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Contanti", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "FX", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Money Market", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Cash Collateral and Margins", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Real Estate Investment Trust", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Depository Receipts", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Preferred Stock", "Obbligazionario", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Fondi comuni", "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Forwards", "Other", portfolio$Asset_Class)


# Trasformation of character data into Factors or Numbers, where it makes sense
portfolio$Country <- as.factor(portfolio$Country)
portfolio$Currency <- as.factor (portfolio$Currency)
portfolio$Weight <- as.numeric (portfolio$Weight)
portfolio$ETF <- as.factor(portfolio$ETF)
portfolio$Industry <- as.factor(portfolio$Industry)
portfolio$MacroArea <- as.factor(portfolio$MacroArea)
portfolio$Asset_Class <-as.factor(portfolio$Asset_Class)

# Calculating exact, correct weight for each asset in the portfolios
portfolio <- portfolio %>%
  group_by(ETF) %>%
  mutate(Weight_A = Weight / unique(sum(Weight, na.rm = TRUE))) %>%
  ungroup()

portfolio$Effective_Weight <- portfolio$Weight_A * portfolio$PTF_Weight

portfolio %>%
  group_by(ETF) %>%
  summarize (w = sum (Weight_A, na.rm = T), t = sum (Effective_Weight, na.rm = T))

summary (portfolio)             


# ##############################################################################
# END OF DATA LOAD 
# ##############################################################################


################################################################################
# START OF ANALYSIS
################################################################################

# COUNTRY ----------------------------------------------------------------------
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


# OFFICIAL INTEREST RATES ------------------------------------------------------
# useful to try to understand how 
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
  filter (Asset_Class != "Other") %>%
  group_by (Country, Asset_Class) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  ggplot (aes (x = reorder (Country, -total), y = total, fill = Country)) +
  geom_bar(stat = "identity") +
  geom_text (aes(label = format (round (total*100, digits = 2), digits = 2, scientific = FALSE) ),vjust = -1, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), legend.position = "none") +
  facet_wrap(~ Asset_Class, nrow = 2)


#####
# INDUSTRY
portfolio %>%
  filter (Asset_Class %in% c("Azionario", "Obbligazionario")) %>%
  group_by (Industry, Asset_Class) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  ggplot (aes (x = reorder (Industry, -total), y = total, fill = Industry)) +
  geom_bar(stat = "identity") +
  geom_text (aes(label = format (round (total*100, digits = 2), digits = 2, scientific = FALSE) ),vjust = -1, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), legend.position = "none") +
  facet_wrap(~ Asset_Class, nrow = 2)


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





################################################################################
# SINGLE ASSET

# since the same company can appear in multiple ETFs I want to normalize / standardize the names before aggregating them
library(stringdist)

# Creiamo una funzione per trovare il nome più simile
match_names <- function(name, name_list) {
  distances <- stringdist::stringdist(name, name_list, method = "jw")  
  name_list[which.min(distances)]  # Restituisce il nome più vicino
}

# puliamo in nomi delle varie società da elementi come: INC, SPA, CLASSE A, etc, etc
clean_portfolio_names <- str_trim(str_remove_all(portfolio$Name, "\\b(S.A.|/S|NON VOTING|NON VOTING  PRE|NON-V|NON-VOTING|PC|DR|A|B|C|CL A|CL B|CL C|FXD|PCL|AS|A.S|LLC|PLC|RegS|MTN|FXD-TO-FLT|FXD-FLT|FLAT|SA|SpA|INC|CORP|THE|CO|CLASS A|REG|AG REG|SE|LTD|SPA|NV|AB|CLASS B|AG|CLASS C|A/S|CLS A|CLS B|CLS C|144A|(FXD-FXN)|FXD-FRN|BV)\\b"))
unique_names <- unique(clean_portfolio_names)

portfolio$Name_Normalized <- sapply(portfolio$Name , match_names, name_list = unique_names)

portfolio_aggregated <- portfolio %>%
  filter (Asset_Class %in% c("Obbligazionario", "Other")) %>%
  group_by (Name_Normalized) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  slice_head(n = 20)

# concentrazione del portafoglio nei primi x titoli
x <- 898

portfolio %>%
  filter (Asset_Class == "Azionario") %>%
  group_by (Name_Normalized) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE) %>%
  slice_head(n = x) %>%
  pull() %>%
  sum()
# 0.1914502 @ 23/03/2025

################################################################################
# creiamo una tabella che stratifichi la concentrazione ogni 10% di titoli
# Filtra i titoli azionari
portfolio_selected <- portfolio
portfolio_selected <- portfolio %>% filter(Asset_Class == "Obbligazionario")
# Calcola la somma dei pesi dei primi 'n' titoli azionari ordinati per peso decrescente
n <- nrow(portfolio_selected)


# Calcola la concentrazione per i percentuali da 10% a 100%
concentrazione <- sapply(seq(0.1, 1, by = 0.1), function(p) {
  n_top <- floor(p * n)  # Numero di titoli da includere per questa percentuale
  portfolio_selected %>%
    arrange(desc(Effective_Weight)) %>%
    slice_head(n = n_top) %>%
    summarise(concentrazione = sum(Effective_Weight, na.rm = TRUE)) %>%
    pull(concentrazione)
})

# Crea la tabella della concentrazione
concentrazione_tabella <- data.frame(
  Percentuale = seq(10, 100, by = 10),
  Concentrazione = concentrazione
)

plot (concentrazione_tabella)
?plot

# Visualizza la tabella
print(concentrazione_tabella)


################################################################################


# analisi ABC
portfolio %>%
  filter (Asset_Class == "Azionario") %>%
  group_by (Name_Normalized) %>%
  summarise (total = sum(Effective_Weight, na.rm = T)) %>%
  arrange(desc(total), by_group = TRUE)
  
# # grafico AGC  
# portfolio %>%
# #  filter (Asset_Class == "Azionario") %>%
#   group_by(Name_Normalized) %>%
#   summarise(total = sum(Effective_Weight, na.rm = TRUE)) %>%
#   arrange(desc(total)) %>%
#   mutate(cum_sum = cumsum(total*100),  # Somma cumulata
#        rank = row_number()/n()*100) %>%  # Posizione nel ranking
#   ggplot(aes(x = rank, y = cum_sum)) +
#   geom_line(size = 1, color = "blue") +
#   labs(title = "ABC Analysis",
#        x = "Ennesimo Titolo (%)",
#        y = "Percentuale cumulata (%)") +
#   theme_minimal()

portfolio %>%
  filter(Asset_Class == "Azionario") %>%
  group_by(ETF, Name_Normalized) %>%
  summarise(total = sum(Effective_Weight, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  mutate(cum_sum = cumsum(total * 100),  
         rank = row_number() / n() * 100) %>%  
  ggplot(aes(x = rank, y = cum_sum)) +
  
  # Linea con gradiente di colore
  geom_line(size = 1.5, color = "#0073C2") +  
  #geom_point(color = "#D55E00", size = 1, alpha = 0.7) +  
  
  # Tema personalizzato
  theme_minimal(base_family = "Arial") +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
    axis.title = element_text(size = 14, face = "bold", color = "#333333"),
    axis.text = element_text(size = 12, color = "#555555"),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  ) +
  
  # Titoli e label migliorati
  labs(title = "ETF Portfolio - analisi di concentrazione",  
       x = "Ennesimo Titolo (%)",  
       y = "Percentuale cumulata (%)",
       caption  = "Code by Alberto Frison available in GitHub - https://github.com/albertofrison/ETF_Portfolio") +  
  
  # Limiti per una migliore leggibilità
  scale_x_continuous(expand = c(0, 0), limits = c(0, 110)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 110)) +
  facet_wrap(~ETF)




##### --------------------------------------------------------------------------
# Examples of double entry tables - experimental
a <- portfolio %>%
  group_by (MacroArea) %>%
  summarise (Weight = as.numeric(format (round(sum(Effective_Weight, na.rm = T), digits = 3), digits = 2)),
             Number = as.numeric(format (round(n(), digits = 3), digits = 2))
  ) %>%
  arrange (desc(Weight))


xtabs (round (Effective_Weight, digits = 3) ~ MacroArea + Industry, data = portfolio)

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
getSymbols(Symbols = c("^SPXEW", "^GSPC", "WSML.L"), src = "yahoo", from = "2024-01-01", to = Sys.Date())
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
  geom_line() +
  theme (legend.position = "bottom")

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


##### CURVE DEI RENDIMENTI -----------------------------------------------------
# USD
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# URL del CSV
url_csv <- "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/daily-treasury-rates.csv/2025/all?type=daily_treasury_yield_curve&field_tdr_date_value=2025&page&_format=csv"

# Leggi il CSV
yield_data <- read_csv(url_csv)

# Converti Date in formato data
yield_data <- yield_data %>% mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Trasforma le colonne delle scadenze in righe (pivot_longer)
data_long <- yield_data %>%
  pivot_longer(cols = -Date, names_to = "Maturity", values_to = "Yield") %>%
  filter(Maturity != "Date")  # Rimuove la colonna Date ripetuta

# Pulizia nomi scadenze (es. "1 YR" -> "1Y", "10 YR" -> "10Y")
data_long$Maturity <- gsub(" YR", "Y", data_long$Maturity)
data_long$Maturity <- gsub(" MO", "M", data_long$Maturity)

# Converte Maturity in fattore ordinato per il grafico
data_long$Maturity <- factor(data_long$Maturity, levels = unique(data_long$Maturity))

# Grafico della yield curve (selezionabile con facet_wrap)
data_long %>%
  filter (Date %in% c("2025-03-17", "2025-02-18")) %>%
  mutate(Date = as.factor(Date)) %>%
  ggplot(aes(x = Maturity, y = Yield, group = Date, color = Date)) +
    geom_line(size = 1, alpha = 0.5) +
    geom_point(size = 1) +
    scale_color_manual(values = c("2025-03-17" = "blue", "2025-02-18" = "red")) + 
    labs(title = "Yield Curve - USA Treasury", x = "Maturity", y = "Yield (%)") +
    theme_minimal() 

# EUR -- to do 
url_csv_eur <- "https://data-api.ecb.europa.eu/service/data/YC/B.U2.EUR.4F.G_N_A+G_N_C.SV_C_YM.?lastNObservations=1&format=csvdata"


##### Historical PE RATIOS -----------------------------------------------------
library(rvest)

# PE RATIO
url <- "https://www.multpl.com/s-p-500-pe-ratio/table/by-month"
pagina <- read_html(url)
data <- pagina %>%
  html_table()

pe_ratio <- data.frame(data)
pe_ratio_clean <- pe_ratio %>%
  mutate (Date = mdy (Date), 
          Value = as.numeric(gsub("[^0-9.]", "", Value)))

pe_ratio_clean$Date <- ceiling_date(pe_ratio_clean$Date, "month") - days(1)
  

# PRICE TO BOOK VALUE (QUARTERLY)
url <- "https://www.multpl.com/s-p-500-price-to-book/table/by-quarter"
pagina <- read_html(url)
data <- pagina %>%
  html_table()

pb_ratio <- data.frame(data)
pb_ratio_clean <- pb_ratio %>%
  mutate (Date = mdy (Date), 
          Value = as.numeric(gsub("[^0-9.]", "", Value)))
pb_ratio_clean$Date <- ceiling_date(pb_ratio_clean$Date, "month") - days(1)

# PRICE TO SALES (QUARTERLY)
url <- "https://www.multpl.com/s-p-500-price-to-sales/table/by-quarter"
pagina <- read_html(url)
data <- pagina %>%
  html_table()

ps_ratio <- data.frame(data)
ps_ratio_clean <- ps_ratio %>%
  mutate (Date = mdy (Date), 
          Value = as.numeric(gsub("[^0-9.]", "", Value)))
ps_ratio_clean$Date <- ceiling_date(ps_ratio_clean$Date, "month") - days(1)

# Unire i dataframe sulla base della data
combined_ratios <- full_join(ps_ratio_clean, pb_ratio_clean, by = "Date", suffix = c("_PS", "_PB")) %>%
  full_join(pe_ratio_clean, by = "Date") %>%
  drop_na() %>%
  rename (Value_PE = Value) %>%
  mutate (Sales_to_Book = Value_PB / Value_PS,
          Earnings_to_Book = Value_PB / Value_PE,
          Earnings_to_Sales = Value_PS / Value_PE) %>%
  pivot_longer(cols = c(Value_PB, Value_PS, Sales_to_Book, Earnings_to_Book, Earnings_to_Sales), names_to = "Metric", values_to = "Value")


# Creare il grafico a linee
ggplot(combined_ratios, aes(x = Date, y = Value, color = Metric)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "S&P 500 Price-to-Book & Price-to-Sales Ratios Over Time",
    x = "Date",
    y = "Ratio",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Value_PB" = "#0072B2", "Value_PS" = "#D55E00", "Sales_to_Book" = "#F5A321")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank())


---------

pe_ratio_clean %>%
  ggplot(aes(x = Date, y = Value)) +
  geom_line(color = "#0072B2", size = 1.2, alpha = 0.8) +  # Linea blu elegante
  geom_point(color = "#D55E00", size = 1, alpha = 0.8) +  # Punti arancioni per evidenziare i dati
  labs(
    title = "S&P 500 Price/Earnings Ratio Over Time",
    x = "Date",
    y = "P/E Ratio"
  ) +
  theme_minimal(base_size = 14) +  # Stile minimal per un look professionale
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Titolo centrato e in grassetto
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotazione delle date per leggibilità
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),  # Griglia sottile
    panel.grid.minor = element_blank()
  )


################################################################################
# BACK TESTING SECTION
################################################################################
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(xts)

# Definizione degli ETF nel portafoglio e loro pesi
etf_symbols <- c("XDEW.DE", "EXUS.L", "IWSZ.MI", "AGGH.MI", "XDEV.MI", "IUSN.DE", "EIMI.MI")
etf_weights <- c(0.23, 0.28, 0.08, 0.20, 0.08, 0.05, 0.08)  # Pesi assegnati nel portafoglio
 
# Scarica i dati storici da Yahoo Finance
getSymbols(etf_symbols, src = "yahoo", from = "2000-01-01", to = Sys.Date())

# Creazione di un dataframe con i prezzi di chiusura
portfolio_prices <- do.call(merge, lapply(etf_symbols, function(x) Cl(get(x))))
colnames(portfolio_prices) <- etf_symbols

# Normalizzazione dei prezzi (base 100 all'inizio) gestendo NA
portfolio_prices <- na.omit(portfolio_prices)  # Rimuove valori mancanti
portfolio_returns <- sweep(portfolio_prices, 2, as.numeric(portfolio_prices[1, ]), FUN = "/") * 100

# Calcolo del valore del portafoglio pesato
portfolio_value <- portfolio_returns %*% etf_weights
portfolio_df <- data.frame(Date = index(portfolio_prices), Portfolio_Value = as.vector(portfolio_value))

# Grafico dell'andamento del portafoglio
portfolio_df %>%
  ggplot(aes(x = Date, y = Portfolio_Value)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(title = "Backtest Storico del Portafoglio",
       y = "Valore Normalizzato (Base 100)", x = "Data") +
  theme_minimal()

# Analisi della volatilità e statistiche di rischio
portfolio_returns_daily <- na.omit(Return.calculate(portfolio_prices, method = "log"))

# Metriche chiave
etf_weights <- matrix(etf_weights, ncol = 1)
portfolio_returns_weighted <- as.data.frame(portfolio_returns_daily %*% etf_weights)

# Supponiamo che portfolio_returns_daily abbia già un indice temporale
dates <- index(portfolio_returns_daily)  # Prendiamo le date originali
clean_returns_xts <- xts(clean_returns, order.by = dates)

# Ora table.Stats dovrebbe funzionare
portfolio_stats <- table.Stats(clean_returns_xts)


# Drawdown massimo
chart.Drawdown(clean_returns_xts, main = "Drawdown del Portafoglio")

# Sharpe Ratio
sharpe_ratio <- SharpeRatio(clean_returns_xts, Rf = 0.01/252, FUN = "StdDev")
print(sharpe_ratio)

# Confronto con il benchmark 60/40
benchmark_data <- getSymbols(c("IWDA.AS", "AGGH.AS"), src = "yahoo", from = "2010-01-01", auto.assign = TRUE)
benchmark_prices <- do.call(merge, lapply(c("IWDA.AS", "AGGH.AS"), function(x) Cl(get(x))))
colnames(benchmark_prices) <- c("IWDA", "AGGH")
benchmark_returns <- benchmark_prices / first(benchmark_prices) * 100
benchmark_6040 <- benchmark_returns$IWDA * 0.6 + benchmark_returns$AGGH * 0.4
benchmark_df <- data.frame(Date = index(benchmark_prices), Benchmark_60_40 = as.vector(benchmark_6040))

# Merge e grafico comparativo
comparison_data <- left_join(portfolio_df, benchmark_df, by = "Date")
comparison_data %>%
  pivot_longer(cols = c("Benchmark_60_40", "Portfolio_Value"), names_to = "Series", values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Series)) +
  geom_line(size = 1) +
  labs(title = "Confronto Portafoglio vs Benchmark 60/40",
       y = "Indice Normalizzato (Base 100)", x = "Data") +
  theme_minimal()
