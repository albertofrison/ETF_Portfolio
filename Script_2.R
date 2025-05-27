# ##############################################################################
# Script_2.R - Analisi Avanzata Portafoglio ETF
# ##############################################################################
#
# Version: 2.0
# Data: 2025-05-27
#
# ##############################################################################


# A. INTRODUCTION --------------------------------------------------------------
# R Project to better understand the composition of my ETF Portfolio
# I download the components of some ETFs listed in Xetra or Borsa Italiana from a number of providers (composing my Portfolio) such as iShares or Vanguard and I
# analyse them from the Market, Area, Sector, Currency, ... point of view and hopefully provide more insights.
# Created - February 2025
# Revised - May 2025


# B. LIBRARIES -----------------------------------------------------------------
# Librerie necessarie per l'esecuzione dello script.

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
rm (list = ls())


# D. LOADING THE DATASET -------------------------------------------------------
# Questa sezione definisce le fonti dei dati (URL), i parametri per leggerli
# e implementa un ciclo per scaricare, convertire (se necessario) e
# caricare i dati in un unico dataframe chiamato 'portfolio'.

# URL del file Excel dei datasets ----------------------------------------------
# Lista degli indirizzi web da cui scaricare i file Excel dei componenti ETF.
# NOTA: url8 è un percorso locale, per un ETF Invesco.

# Azionari
#XDEW
url1 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BLNMYC90/" 
#IUSN
url2 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/296576/fund/1538022822418.ajax?fileType=xls&fileName=iShares-MSCI-World-Small-Cap-UCITS-ETF-USD-Acc_fund&dataType=fund" 
#EXUS
url3 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE0006WW1TQ4/" 
#EIMI
url4 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/264659/ishares-msci-emerging-markets-imi-ucits-etf/1538022822418.ajax?fileType=xls&fileName=iShares-Core-MSCI-EM-IMI-UCITS-ETF-USD-Acc_fund&dataType=fund" 
#IWSZ
url5 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/270057/ishares-msci-world-size-factor-ucits-etf/1538022822418.ajax?fileType=xls&fileName=IWSZ&dataType=fund" 

# Obbligazionari
#AGGH
url6 <- "https://www.ishares.com/it/investitore-privato/it/prodotti/291770/fund/1538022822418.ajax?fileType=xls&fileName=iShares-Core-Global-Aggregate-Bond-UCITS-ETF-EUR-Hedged-Acc_fund&dataType=fund" 

# Azionario
#XDEV
url7 <- "https://etf.dws.com/etfdata/export/ITA/ITA/excel/product/constituent/IE00BL25JM42/" 

# File Locale
#MWEQ
url8 <- "/home/alberto/Scaricati/20250527 - Invesco Export.xlsx" 

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
# ATTENZIONE: La lista 'columns_name' deve avere 9 elementi per ogni ETF, perché aggiungeremo ETF (il nome),
#PTF_Weight (peso dell'ETF nel portafoglio e Effective_Weight (moltiplicazione tra PTF_Weight ed il Weight relativo di ogni asset in ogni ETF).
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
portfolio <- data.frame(matrix(nrow = 0, ncol = length(columns)))

# Creiamo una struttura vuota con le colonne finali desiderate e assegniamo i nomi corretti alle colonne
columns <- c("Name", "Country", "Currency", "Asset_Class", "Industry", "Weight", "ETF", "PTF_Weight", "Effective_Weight")
colnames(portfolio) <- columns

# Ciclo automatico per scaricare i file, convertirli e aggiungerli al dataframe.
# Questo ciclo itera su ogni riga di df_urls (ogni ETF).
for (i in 1:nrow(df_urls)) {
  
  # Messaggio di stato per seguire il progresso
  print(paste("Processing ETF:", df_urls$etf_name[i], "(", i, "/", nrow(df_urls), ")"))
  
  # Creiamo nomi di file temporanei per gestire download e conversione.
  temp_file <- tempfile(fileext = ".xls")
  converted_file_base <- tempfile(fileext = "") # Base per il nome convertito
  
  current_url <- df_urls$url[i]
  
  # Gestiamo separatamente i file online (da scaricare) e il file locale (url8).
  if (i != 8) {
    print(paste("  Downloading from:", current_url))
    # Scarichiamo il file usando GET, salvandolo nel file temporaneo.
    # Aggiungiamo un User-Agent per simulare un browser ed evitare blocchi.
    res <- GET(current_url, write_disk(temp_file, overwrite = TRUE), add_headers("User-Agent" = "Mozilla/5.0"))
    
    # Controlliamo se il download ha avuto successo
    if (http_status(res)$category != "Success") {
      print(paste("  !!! ERROR: Download failed for", current_url))
      next # Salta al prossimo ETF se il download fallisce
    }
    
    # *** CONVERSIONE TRAMITE LIBREOFFICE ***
    # Questo è il passaggio più critico e dipendente dall'ambiente.
    # Usa un comando di sistema per chiamare LibreOffice e convertire
    # il file .xls (spesso i file scaricati sono .xls anche se l'URL non lo dice)
    # in .xlsx, che è più facile da leggere con readxl.
    # ATTENZIONE: Richiede che LibreOffice sia installato sul sistema!
    # ATTENZIONE: Potrebbe non funzionare su tutti i sistemi operativi o configurazioni.
    print("  Converting file with LibreOffice...")
    system_command <- paste("libreoffice --headless --convert-to xlsx", shQuote(temp_file), "--outdir", shQuote(dirname(converted_file_base)))
    system(system_command, wait = TRUE)
    
    # LibreOffice aggiunge .xlsx al nome originale. Ricostruiamo il nome.
    converted_file <- paste0(basename(temp_file), ".xlsx")
    # Ricostruiamo il percorso completo del file convertito
    converted_file_path <- file.path(dirname(converted_file_base), converted_file)
    
    # Leggiamo il file Excel convertito, saltando le righe iniziali.
    # 'try' serve a gestire eventuali errori durante la lettura.
    print(paste("  Reading converted file:", converted_file_path))
    df <- try(read_xlsx(converted_file_path, skip = df_urls$skip_row[i]), silent = TRUE)
    
    # Pulizia dei file temporanei
    try(file.remove(temp_file), silent = TRUE)
    try(file.remove(converted_file_path), silent = TRUE)
    
  } else {
    # Se è l'ETF 8, leggiamo direttamente il file locale.
    print(paste("  Reading local file:", current_url))
    df <- try(read_xlsx(current_url, skip = df_urls$skip_row[i]), silent = TRUE)
  }
  
  # Controlliamo se la lettura è andata a buon fine.
  if (inherits(df, "try-error")) {
    print(paste("  !!! ERROR: Could not read data for", df_urls$etf_name[i]))
    next # Salta al prossimo ETF
  }
  
  # Selezioniamo solo le colonne che ci interessano.
  df <- df[, df_urls$keep_column[[i]]] # Usiamo '[,' per mantenere un dataframe
  
  # Aggiungiamo le informazioni specifiche dell'ETF.
  df$ETF <- df_urls$etf_name[i]
  df$PTF_W <- df_urls$etf_weight[i]
  df$EffectiveWeight <- 0 # Inizializziamo a 0, lo calcoleremo dopo
  
  # Assegniamo i nomi standard alle colonne.
  colnames(df) <- df_urls$column_name[[i]]
  
  # *** APPENDING DATA ***
  # Aggiungiamo i dati di questo ETF al dataframe 'portfolio'.
  # NOTA: Usare rbind in un ciclo può essere lento con molti dati.
  # Un metodo più efficiente sarebbe salvare ogni 'df' in una lista
  # e poi usare dplyr::bind_rows() alla fine. Ma manteniamo l'originale
  # per ora, come richiesto.
  portfolio <- rbind(portfolio, df)
  print(paste("  Added", nrow(df), "rows for", df_urls$etf_name[i], ". Total rows:", nrow(portfolio)))
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
remove_pattern <- "\\b(s\\.a\\.|/s|non voting|non voting\\s+pre|non-v|non-voting|pc|dr| a| b| c| cl a| cl b| cl c| fxd| pcl| as| a\\.s| llc| plc| regs| mtn| fxd-to-flt| fxd-flt| flat| sa| spa| inc| corp| the| co| class a| reg| ag reg| se| ltd| nv| ab| class b| ag| class c| a/s| cls a| cls b| cls c| 144a| \\(fxd-fxn\\)| fxd-frn| bv| ucits| etf| acc| dist)\\b"

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