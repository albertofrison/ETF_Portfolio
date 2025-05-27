# ##############################################################################
# Script_2.R - Analisi Avanzata Portafoglio ETF
# ##############################################################################
#
# OBIETTIVO: Caricare i dati di composizione di un portafoglio ETF,
#            pulirli, standardizzarli e condurre analisi visuali
#            per comprendere l'esposizione per area, settore, valuta, ecc.
#
# Versione: 2.0
# Data: 2025-05-27
#
# ##############################################################################


# A. INTRODUCTION --------------------------------------------------------------
# R Project to better understand the composition of my ETF Portfolio
# I download the components of some ETFs listed in Xetra or Borsa Italiana from a number of providers (composing my Portfolio) such as iShares or Vanguard and I
# analyse them from the Market, Area, Sector, Currency, ... point of view and hopefully provide more insights.
# Created - February 2025


# B. LIBRARIES -----------------------------------------------------------------
# Carichiamo tutte le librerie necessarie per l'esecuzione dello script.
# È buona pratica caricarle tutte all'inizio.

library(tidyverse)    # Collezione essenziale per manipolazione dati (dplyr) e grafici (ggplot2)
library(httr)         # Per scaricare file da internet (richieste HTTP)
library(readxl)       # Per leggere file Excel (.xls e .xlsx)
library(rvest)        # Utile per web scraping (anche se non usato nel caricamento base)
library(forcats)      # Per lavorare con i 'factors' (variabili categoriali), utile per grafici
library(stringdist)   # Per calcolare la distanza tra stringhe, utile per normalizzare i nomi
library(treemapify)   # Per creare grafici Treemap con ggplot2
library(scales)       # Per formattare assi e etichette nei grafici (es. percentuali)
library(RColorBrewer) # Fornisce palette di colori professionali per i grafici
library(lubridate)    # Per lavorare con le date (utile se aggiungeremo analisi temporali o salvataggi con data)

# C. INITIALIZATION / CLEANUP --------------------------------------------------
# Pulisce l'ambiente di lavoro da tutti gli oggetti preesistenti.
# Utile per assicurarsi che lo script parta sempre da zero, evitando conflitti.
rm (list = ls())


# D. LOADING THE DATASET -------------------------------------------------------
# Questa sezione definisce le fonti dei dati (URL), i parametri per leggerli
# e implementa un ciclo per scaricare, convertire (se necessario) e
# caricare i dati in un unico dataframe chiamato 'portfolio'.

# URL del file Excel dei datasets ----------------------------------------------
# Lista degli indirizzi web da cui scaricare i file Excel dei componenti ETF.
# NOTA: url8 è un percorso locale, dovrà essere adattato se lo script viene
#       eseguito su un altro computer.

# Azionari
url1 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BLNMYC90/" #XDEW
url2 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/296576/fund/1538022822418.ajax?fileType=xls&fileName=iShares-MSCI-World-Small-Cap-UCITS-ETF-USD-Acc_fund&dataType=fund" #IUSN
url3 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE0006WW1TQ4/" #EXUS
url4 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/264659/ishares-msci-emerging-markets-imi-ucits-etf/1538022822418.ajax?fileType=xls&fileName=iShares-Core-MSCI-EM-IMI-UCITS-ETF-USD-Acc_fund&dataType=fund" #EIMI
url5 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/270057/ishares-msci-world-size-factor-ucits-etf/1538022822418.ajax?fileType=xls&fileName=IWSZ&dataType=fund" # IWSZ
# Obbligazionari
url6 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/291770/fund/1538022822418.ajax?fileType=xls&fileName=iShares-Core-Global-Aggregate-Bond-UCITS-ETF-EUR-Hedged-Acc_fund&dataType=fund" #AGGH
# Azionario
url7 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BL25JM42/" #XDEV
# Locale
url8 <- "/home/alberto/Scaricati/20250527 - Invesco Export.xlsx" # Percorso locale!

#-------------------------------------------------------------------------------
# Preparazione dei parametri per il caricamento automatico.
# Ogni file Excel ha una struttura diversa, quindi definiamo:
# - Quante righe saltare all'inizio (skip_rows)
# - Quali colonne tenere (keep_columns)
# - I nomi degli ETF (etf_names)
# - Il peso di ogni ETF nel portafoglio (etf_weights)
# - I nomi standard da dare alle colonne (columns_name)

urls <- c(url1, url2, url3, url4, url5, url6, url7, url8)
skip_rows <- c(3, 7, 3, 7, 7, 7, 3, 0)
keep_columns <- list(c(2, 4, 5, 7, 10, 11), c(2, 3, 4, 6, 10, 12), c(2, 4, 5, 7, 10, 11), c(2, 3, 4, 6, 10, 12), c(2, 3, 4, 6, 10, 12), c(2, 3, 4, 6, 11, 16), c(2, 4, 5, 7, 10, 11), c(1, 2, 3, 4, 5, 6))
etf_names <- c("XDEW", "IUSN", "EXUS", "EIMI", "IWSZ", "AGGH", "XDEV", "MWEQ")
etf_weights <- c(0.1875, 0.1522, 0.1267, 0.0539, 0.0539, 0.0540, 0.0355, 0.3363)
# ATTENZIONE: La lista 'columns_name' deve avere 9 elementi per ogni ETF,
#             perché aggiungeremo ETF, PTF_Weight e Effective_Weight.
columns_name <- list(c("Name", "Country", "Currency", "Asset_Class", "Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight"),
                     c("Name", "Industry", "Asset_Class", "Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                     c("Name", "Country", "Currency", "Asset_Class", "Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight"),
                     c("Name", "Industry", "Asset_Class", "Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                     c("Name", "Industry", "Asset_Class", "Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                     c("Name", "Industry", "Asset_Class", "Weight", "Country", "Currency", "ETF", "PTF_Weight", "Effective_Weight"),
                     c("Name", "Country", "Currency", "Asset_Class", "Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight"),
                     c("Name", "Country", "Currency", "Industry", "Asset_Class", "Weight", "ETF", "PTF_Weight", "Effective_Weight"))

# Creiamo un dataframe ('df_urls') che contiene tutti i parametri.
# Questo rende più facile gestire il ciclo di download.
df_urls = data.frame (url = urls,
                      skip_row = skip_rows,
                      etf_name = etf_names,
                      etf_weight = etf_weights,
                      stringsAsFactors = FALSE)

# Aggiungiamo le liste come colonne nel dataframe.
df_urls$keep_column <- keep_columns
df_urls$column_name <- columns_name


# Inizializziamo il dataframe 'portfolio' che conterrà tutti i dati consolidati.
# Creiamo una struttura vuota con le colonne finali desiderate.
columns <- c("Name", "Country", "Currency", "Asset_Class", "Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight")
portfolio <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(portfolio) <- columns # Assegniamo i nomi corretti alle colonne

# Ciclo automatico per scaricare i file, convertirli e aggiungerli al dataframe.
# Questo ciclo itera su ogni riga di df_urls (ogni ETF).
for (i in 1:nrow(df_urls)) {
  
  # Crea i file temporanei
  temp_file <- tempfile(fileext = ".xls")
  converted_file <- tempfile(fileext = ".xlsx")  # File finale in formato xlsx
  
  current_url <- df_urls$url[i]
  
  if (i != 8) {
    # Scaricare il file
    res <- GET(current_url, write_disk(temp_file, overwrite = TRUE), add_headers("User-Agent" = "Mozilla/5.0"))
    
    # Convertire il file usando LibreOffice (headless mode)
    system(paste("libreoffice --headless --convert-to xlsx", shQuote(temp_file), "--outdir", shQuote(dirname(converted_file))), wait = TRUE)
    
    # Ottenere il nome effettivo del file convertito
    converted_file <- sub("\\.xls$", ".xlsx", temp_file)
    
    # Leggere il file Excel convertito
    df <- try(read_xlsx(converted_file, skip = df_urls$skip_row[i]), silent = TRUE)
  } else {
    df <- try(read_xlsx(current_url, skip = df_urls$skip_row[i]), silent = TRUE)
  }
  
  df <- df[df_urls$keep_column[[i]]]
  df$ETF <- df_urls$etf_name[i]
  df$PTF_W <- df_urls$etf_weight[i]
  df$EffectiveWeight <- 0
  colnames(df) <- df_urls$column_name[[i]]
  
  
  # Appending each ETF into the global dataframe
  portfolio <- rbind(portfolio,df)
  
}


print("--- Data Loading Complete ---")


# ##############################################################################
# DATA CLEANING ################################################################
# ##############################################################################
# Ora che tutti i dati sono caricati, li puliamo e standardizziamo.

print("--- Starting Data Cleaning ---")

# Standardizziamo i Nomi dei Paesi.
# Usiamo 'ifelse' per cambiare nomi specifici.
# NOTA: Usare 'case_when' (come per Industry) o un join con una tabella
#       di lookup sarebbe più pulito ed espandibile.
portfolio$Country <- ifelse (portfolio$Country == "Stati Uniti d'America", "Stati Uniti", portfolio$Country)
portfolio$Country <- ifelse (portfolio$Country == "Paesi Bassi (Olanda)", "Olanda", ifelse (portfolio$Country == "Paesi Bassi", "Olanda", portfolio$Country))
portfolio$Country <- ifelse (portfolio$Country == "Repubblica di Corea (Corea del Sud)", "Corea", portfolio$Country)
portfolio$Country <- ifelse (portfolio$Country == "Regno Unito", "Regno unito", portfolio$Country)
# Gestiamo eventuali valori mancanti o molto corti (spesso indicano errori)
portfolio$Country <- ifelse (is.na(portfolio$Country) | nchar(as.character(portfolio$Country)) <= 2, "Other", portfolio$Country)

# Creiamo la colonna 'MacroArea' basata sul Paese.
# 'case_when' è ideale per queste mappature multiple.
portfolio <- portfolio %>%
  mutate (MacroArea = case_when(
    Country == "Stati Uniti" ~ "USA",
    Country %in% c("Italia", "Francia", "Germania", "Irlanda", "Spagna", "Danimarca",
                   "Finlandia", "Norvegia", "Belgio", "Lussemburgo", "Polonia", "Austria",
                   "Portogallo", "Grecia", "Ungheria", "Olanda", "Svezia", "Repubblica Ceca",
                   "Unione Europea", "Svizzera", "Regno unito", "Russia") ~ "Europe",
    Country == "Giappone" ~ "Giappone",
    Country %in% c("Canada", "Australia", "Hong Kong", "Singapore", "Corea",
                   "Nuova Zelanda", "Taiwan", "Israele") ~ "Other Developed",
    Country == "Cina" ~ "Cina",
    TRUE ~ "Developing" # 'TRUE' agisce come 'else', cattura tutto il resto.
  )
  )

# Standardizziamo i Nomi dei Settori ('Industry').
# Usiamo 'case_when' per raggruppare i vari nomi usati dai provider.
portfolio$Industry <- case_when(
  portfolio$Industry %in% c("Technology", "IT", "Tecnologia", "Tecnologia dell'informazione") ~ "IT",
  portfolio$Industry %in% c("Financial Services", "Finanza", "Società finanziarie", "Brokerage/Asset Managers/Exchanges",
                            "Financial Other", "Finanziari", "Assicurazioni") ~ "Finanza",
  portfolio$Industry %in% c("Healthcare", "Salute", "Sanità") ~ "Salute",
  portfolio$Industry %in% c("Communication Services", "Comunicazione", "Comunicazioni", "Servizi di comunicazione") ~ "Comunicazione",
  portfolio$Industry %in% c("Consumer Cyclical", "Consumi Discrezionali", "Beni di consumo ciclici", "Beni voluttuari") ~ "Consumi Discrezionali",
  portfolio$Industry %in% c("Consumer Defensive", "Consumer Non-Cyclical", "Generi di largo consumo", "Beni di prima necessità") ~ "Generi di largo consumo",
  portfolio$Industry %in% c("Industrials", "Industriali", "Prodotti industriali", "Industrial Other", "Industrials", "Basic Industry") ~ "Industriali",
  portfolio$Industry %in% c("Basic Materials", "Materiali") ~ "Materiali",
  portfolio$Industry %in% c("Energy", "Energia", "Gas Naturale") ~ "Energia",
  portfolio$Industry %in% c("Utilities", "Utility Other", "Elettrico", "Stranded Cost Utility",
                            "Imprese di servizi di pubblica utilità", "Servizi di pubblica utilità") ~ "Utilities",
  portfolio$Industry %in% c("Real Estate", "Immobiliare", "Immobili", "Certificato Immobiliare") ~ "Immobiliare",
  portfolio$Industry %in% c("Agency Fixed Rate", "Buoni Del Tesoro", "Owned No Guarantee", "Mortgage Collateralized",
                            "Supranational", "Government Guaranteed", "Attività bancarie", "Local Authority",
                            "Sovrani", "Government Sponsored", "Non-Agency CMBS", "Agency CMBS",
                            "Hybrid Collateralized", "Public Sector Collateralized", "Whole Business") ~ "Obbligazioni strutturate",
  portfolio$Industry %in% c("Altro", "Liquidità e/o derivati") ~ "Liquidità e/o derivati",
  is.na(portfolio$Industry) | portfolio$Industry %in% c("<NA>", "unknown", "Non trovato") ~ "Ignoto",
  TRUE ~ portfolio$Industry # Se non corrisponde a nulla, lascia il nome originale.
)

# Standardizziamo le Classi di Attività ('Asset_Class').
# Usiamo 'ifelse' per raggruppare tipi specifici in 'Other' o 'Obbligazionario'.
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Azioni", "Azionario", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class %in% c("Future", "Futures", "Cash", "Contanti", "FX",
                                                              "Money Market", "Cash Collateral and Margins",
                                                              "Real Estate Investment Trust", "Depository Receipts",
                                                              "Fondi comuni", "Forwards",
                                                              "diritti su operazioni societarie"), "Other", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (portfolio$Asset_Class == "Preferred Stock", "Obbligazionario", portfolio$Asset_Class)
portfolio$Asset_Class <- ifelse (is.na(portfolio$Asset_Class), "Other", portfolio$Asset_Class) # Gestisce NA

# Convertiamo le colonne nei tipi di dati corretti.
# 'factor' è utile per le variabili categoriali, specialmente nei grafici.
# 'numeric' è essenziale per i calcoli.
portfolio$Country <- as.factor(portfolio$Country)
portfolio$Currency <- as.factor (portfolio$Currency)
portfolio$Weight <- as.numeric (portfolio$Weight)
portfolio$ETF <- as.factor(portfolio$ETF)
portfolio$Industry <- as.factor(portfolio$Industry)
portfolio$MacroArea <- as.factor(portfolio$MacroArea)
portfolio$Asset_Class <-as.factor(portfolio$Asset_Class)

# Calcoliamo il Peso Effettivo ('Effective_Weight').
# 1. Calcoliamo il peso % di ogni titolo *all'interno* del suo ETF (Weight_A).
#    Questo è importante perché alcuni provider danno pesi che non sommano a 100.
# 2. Moltiplichiamo questo peso normalizzato per il peso dell'ETF nel portafoglio.
portfolio <- portfolio %>%
  # Gestiamo NA nei pesi prima di calcolare la somma
  mutate(Weight = ifelse(is.na(Weight), 0, Weight)) %>%
  group_by(ETF) %>%
  mutate(Weight_A = Weight / sum(Weight, na.rm = TRUE)) %>%
  ungroup() %>%
  # Gestiamo eventuali NA che potrebbero risultare da somme pari a 0
  mutate(Weight_A = ifelse(is.na(Weight_A), 0, Weight_A))

# Calcoliamo il peso finale.
portfolio$Effective_Weight <- portfolio$Weight_A * portfolio$PTF_Weight

# Controlliamo che i pesi sommino a 1 (o quasi) per ogni ETF e nel totale.
portfolio %>%
  group_by(ETF) %>%
  summarize (w_ETF = sum(Weight_A, na.rm = T), w_PTF = sum(Effective_Weight, na.rm = T)) %>%
  print()

print(paste("Total Portfolio Weight:", sum(portfolio$Effective_Weight, na.rm = T)))

# Mostriamo un riassunto del dataframe finale.
summary (portfolio)

print("--- Data Cleaning Complete ---")

# ##############################################################################
# END OF DATA LOAD
# ##############################################################################


# ##############################################################################
# NAME NORMALIZATION ###########################################################
# ##############################################################################
# Per aggregare correttamente i titoli che appaiono in più ETF,
# dobbiamo standardizzare i loro nomi, che possono variare leggermente.
# Usiamo 'stringdist' per trovare nomi simili.
# Questo passaggio è FONDAMENTALE per l'analisi di concentrazione e Top Holdings.

print("--- Starting Name Normalization (can be slow) ---")

# Creiamo una funzione per trovare il nome più simile in una lista data.
# Usa l'algoritmo Jaro-Winkler (method = "jw"), buono per nomi propri.
match_names <- function(name, name_list) {
  if (is.na(name)) return(NA) # Gestisce NA
  distances <- stringdist::stringdist(tolower(name), tolower(name_list), method = "jw")
  name_list[which.min(distances)] # Restituisce il nome più vicino
}

# Definiamo una lista di suffissi/parti da rimuovere dai nomi per pulirli.
# Usiamo \\b per indicare i confini di parola e evitiamo di rimuovere parti interne.
# La lista è lunga e potrebbe richiedere aggiustamenti.
remove_pattern <- "\\b(s\\.a\\.|/s|non voting|non voting\\s+pre|non-v|non-voting|pc|dr|npv| a| b| c| cl a| cl b| cl c| fxd| pcl| as| a\\.s| llc| plc| regs| mtn| fxd-to-flt| fxd-flt| flat| sa| spa| inc| corp| the| co| class a| reg| ag reg| se| ltd| nv| ab| class b| ag| class c| a/s| cls a| cls b| cls c| 144a| \\(fxd-fxn\\)| fxd-frn| bv| ucits| etf| acc| dist|usd[0-9]+\\.[0-9]+)\\b"

# Puliamo i nomi e creiamo una lista di nomi unici 'puliti'.
# Usiamo tolower() per rendere il confronto case-insensitive.
# Usiamo str_trim() per rimuovere spazi bianchi all'inizio/fine.
clean_names <- str_trim(str_remove_all(tolower(portfolio$Name), remove_pattern))
unique_names <- unique(clean_names)
unique_names <- unique_names[!is.na(unique_names)] # Rimuoviamo NA se presenti

# Applichiamo la funzione 'match_names' a ogni nome nel portfolio.
# 'sapply' applica la funzione a ogni elemento e restituisce un vettore.
# ATTENZIONE: Questo può essere LENTO se il dataframe è molto grande.
portfolio$Name_Normalized <- sapply(clean_names, match_names, name_list = unique_names, USE.NAMES = FALSE)

# Mostriamo alcuni nomi originali e normalizzati per controllo.
print("Sample of Normalized Names:")
print(head(portfolio %>% select(Name, Name_Normalized)))

print("--- Name Normalization Complete ---")

# ##############################################################################
# START OF NEW ANALYSIS ########################################################
# ##############################################################################
# Qui inseriamo i nuovi grafici per esplorare il portafoglio.

print("--- Starting New Portfolio Analysis ---")

# 1. TREEMAP GEOGRAFICA (MacroArea / Paese)
# ----------------------------------------
# Mostra l'esposizione geografica in modo gerarchico.
print("  Generating Geographic Treemap...")
geo_data <- portfolio %>%
  filter(Effective_Weight > 0.0001) %>%
  group_by(MacroArea, Country) %>%
  summarise(total_weight = sum(Effective_Weight, na.rm = TRUE), .groups = 'drop') %>%
  mutate(label_text = paste(Country, percent(total_weight, accuracy = 0.1), sep = "\n"))


plot1 <- ggplot(geo_data, aes(area = total_weight, fill = MacroArea, subgroup = MacroArea, label = label_text)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 3) +
  geom_treemap_text(
    colour = "white", place = "centre", size = 10, grow = FALSE, reflow = TRUE
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Esposizione Geografica del Portafoglio",
    subtitle = "Divisione per MacroArea (riquadri grandi) e Paese (riquadri interni)",
    caption = "La dimensione dei riquadri rappresenta il peso % nel portafoglio.",
    fill = "Macro Area"
  ) +
  theme(legend.position = "bottom")

print(plot1) # Mostra il grafico

# alternativa , da un altro risultato
portfolio %>%
  #filter (Asset_Class %in% c("Azionario", "Obbligazionario")) %>%
  group_by (MacroArea, Asset_Class) %>%
  summarise (total = sum(Effective_Weight, na.rm = T), .groups = 'drop' )  %>%
  arrange(desc(total), by_group = TRUE) %>%
  ggplot (aes (x = reorder (MacroArea, -total), y = total, fill = MacroArea)) +
  geom_bar(stat = "identity") +
  geom_text (aes(label = format (round (total*100, digits = 2), digits = 2, scientific = FALSE) ),vjust = -1, size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), legend.position = "none")


# 2. INDUSTRIA per MACROAREA (Stacked Bar %)
# --------------------------------------------
# Mostra la distribuzione % dei settori all'interno di ogni area (solo Azionario).
print("  Generating Industry by MacroArea Stacked Bar Chart...")
industry_area_data <- portfolio %>%
  filter(Asset_Class == "Azionario" & Effective_Weight > 0) %>%
  group_by(MacroArea, Industry) %>%
  summarise(total_weight = sum(Effective_Weight, na.rm = TRUE), .groups = 'drop') %>%
  filter(!Industry %in% c("Ignoto", "Other", "Liquidità e/o derivati", "Obbligazioni strutturate"))

plot2 <- ggplot(industry_area_data, aes(x = MacroArea, y = total_weight, fill = Industry)) +
  geom_bar(stat = "identity", position = "fill", colour = "white", size = 0.2) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Paired") +
  coord_flip() +
  labs(
    title = "Composizione Settoriale per Macro Area (Solo Azionario)",
    subtitle = "Distribuzione % dei settori all'interno di ogni area",
    x = "Macro Area",
    y = "Peso Relativo %",
    fill = "Settore Industriale"
  ) +
  theme_minimal() +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'))

print(plot2) # Mostra il grafico

# 3. TOP 15 TITOLI (Bar Chart Orizzontale)
# ------------------------------------------
# Mostra i 15 titoli con il peso maggiore nel portafoglio, usando i nomi normalizzati.
print("  Generating Top 15 Holdings Bar Chart...")
top_holdings_data <- portfolio %>%
  # Assicurati che Name_Normalized non sia NA
  filter(!is.na(Name_Normalized) & Effective_Weight > 0) %>%
  group_by(Name_Normalized) %>%
  summarise(total_weight = sum(Effective_Weight, na.rm = TRUE)) %>%
  arrange(desc(total_weight)) %>%
  slice_head(n = 15) %>%
  # Converti Name_Normalized in factor per l'ordinamento nel grafico
  mutate(Name_Normalized = fct_reorder(Name_Normalized, total_weight))

plot3 <- ggplot(top_holdings_data, aes(x = Name_Normalized, y = total_weight, fill = total_weight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(total_weight, accuracy = 0.01)), hjust = -0.1, size = 3.5, color = "black") +
  scale_y_continuous(labels = percent, limits = c(0, max(top_holdings_data$total_weight) * 1.15)) +
  scale_fill_viridis_c(option = "magma", direction = -1) + # Usa 'viridis' per i colori
  coord_flip() +
  labs(
    title = "Top 15 Titoli per Peso nel Portafoglio Complessivo",
    subtitle = "Basato sui nomi normalizzati.",
    x = "Titolo",
    y = "Peso Effettivo %"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(plot3) # Mostra il grafico

print("--- Analysis Complete ---")
print("--- Script_2.R Finished ---")

# ##############################################################################
# CURRENCY ANALYSIS ############################################################
# ##############################################################################
# Analizziamo l'esposizione del portafoglio alle diverse valute.
# È importante per capire il rischio di cambio.

print("--- Starting Currency Analysis ---")

# 1. GRAFICO A BARRE - ESPOSIZIONE VALUTARIA
# ---------------------------------------------
# Un classico grafico a barre per vedere il peso di ogni valuta.

# Prepariamo i dati: Aggreghiamo per valuta e filtriamo quelle con peso irrisorio.
currency_data_bar <- portfolio %>%
  group_by(Currency) %>%
  summarise(total = sum(Effective_Weight, na.rm = T)) %>%
  filter(total > 0.001) %>% # Mostriamo solo valute > 0.1%
  # Ordiniamo le valute per peso decrescente per un grafico più leggibile
  mutate(Currency = fct_reorder(as.character(Currency), -total))

# Creiamo il grafico
plot_currency_bar <- ggplot(currency_data_bar, aes(x = Currency, y = total, fill = Currency)) +
  geom_bar(stat = "identity") +
  # Aggiungiamo etichette % sopra le barre
  geom_text(aes(label = percent(total, accuracy = 0.1)), vjust = -0.5, size = 3.5) +
  # Formattiamo l'asse Y come percentuale
  scale_y_continuous(labels = percent, limits = c(0, max(currency_data_bar$total) * 1.1)) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Esposizione Valutaria Complessiva del Portafoglio",
    subtitle = "Peso percentuale di ogni valuta (sopra 0.1%)",
    x = "Valuta",
    y = "Peso % nel Portafoglio"
  ) +
  theme(legend.position = "none", # La legenda non serve, i colori sono indicativi
        axis.text.x = element_text(angle = 45, hjust = 1)) # Ruotiamo le etichette X

print(plot_currency_bar)

# 2. GRAFICO A CIAMBELLA - ESPOSIZIONE VALUTARIA
# -----------------------------------------------
# Un'alternativa al grafico a torta, spesso più leggibile,
# raggruppando le valute minori in "Altre".

# Prepariamo i dati: Raggruppiamo le valute sotto una certa soglia (es. 2%)
currency_data_donut <- portfolio %>%
  group_by(Currency) %>%
  summarise(Weight = sum(Effective_Weight, na.rm = TRUE)) %>%
  mutate(Currency_Group = ifelse(Weight < 0.02, "Altre", as.character(Currency))) %>%
  group_by(Currency_Group) %>%
  summarise(Total_Weight = sum(Weight)) %>%
  # Calcoliamo le posizioni per le etichette
  mutate(fraction = Total_Weight / sum(Total_Weight),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(Currency_Group, "\n", percent(Total_Weight, accuracy = 0.1)))

# Creiamo il grafico a ciambella (Donut Chart)
plot_currency_donut <- ggplot(currency_data_donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Currency_Group)) +
  geom_rect() + # Disegna i rettangoli che formeranno la ciambella
  coord_polar(theta="y") + # Trasforma in coordinate polari (torta)
  xlim(c(2, 4)) + # Crea il "buco" al centro (valori < 3 sono il buco)
  # Aggiunge le etichette
  geom_text(x = 3.5, aes(y = labelPosition, label = label), size=4, color="white") +
  scale_fill_brewer(palette="Set3") + # Palette di colori
  theme_void() + # Tema vuoto, senza assi
  theme(legend.position = "none") + # Nascondiamo la legenda (etichette sul grafico)
  labs(title = "Esposizione Valutaria Raggruppata",
       subtitle = "Valute con peso < 2% raggruppate in 'Altre'")

print(plot_currency_donut)

print("--- Currency Analysis Complete ---")


# ##############################################################################
# ABC ANALYSIS (CONCENTRATION) #################################################
# ##############################################################################
# Analizziamo la concentrazione del portafoglio: quanto peso è detenuto
# dai primi N titoli? La curva ABC (o di Lorenz) è perfetta per questo.
# Ci concentriamo sulla parte Azionaria, come nell'originale.

print("--- Starting ABC (Concentration) Analysis ---")

# Prepariamo i dati per la curva ABC
abc_data <- portfolio %>%
  filter(Asset_Class == "Azionario" & !is.na(Name_Normalized) & Effective_Weight > 0) %>%
  group_by(Name_Normalized) %>%
  summarise(total = sum(Effective_Weight, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  mutate(
    cum_sum = cumsum(total),           # Calcola la somma cumulata del peso
    rank_n = row_number(),            # Calcola il rank (1°, 2°, 3°...)
    rank_pct = row_number() / n()     # Calcola il rank in percentuale (0% a 100%)
  )

# Troviamo il punto in cui si raggiunge l'80% (o il più vicino)
pct_80_point <- abc_data %>%
  filter(cum_sum >= 0.80) %>%
  slice_head(n = 1)

# Creiamo il grafico della curva ABC
plot_abc <- ggplot(abc_data, aes(x = rank_pct, y = cum_sum)) +
  geom_line(size = 1.2, color = "#0073C2") + # Linea principale
  geom_area(fill = "#0073C2", alpha = 0.2) + # Area sottostante
  # Linee di riferimento (es. 80% del peso)
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = pct_80_point$rank_pct, linetype = "dashed", color = "red", size = 0.8) +
  # Etichetta per il punto 80%
  geom_text(aes(x = pct_80_point$rank_pct, y = 0.82,
                label = paste0(percent(pct_80_point$rank_pct, accuracy = 1), " dei titoli \nraggiunge l'80% del peso")),
            color = "red", hjust = -0.05, vjust = 0, size = 3.5) +
  # Formattiamo gli assi come percentuali
  scale_x_continuous(labels = percent, name = "Percentuale Cumulata Titoli (Ordinati per Peso)") +
  scale_y_continuous(labels = percent, name = "Percentuale Cumulata Peso Portafoglio") +
  labs(
    title = "Analisi ABC - Curva di Concentrazione (Azionario)",
    subtitle = "Mostra come si distribuisce il peso tra i titoli.",
    caption = "La linea rossa indica il punto in cui si raggiunge l'80% del peso."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(size = 10))

print(plot_abc)

print("--- ABC Analysis Complete ---")