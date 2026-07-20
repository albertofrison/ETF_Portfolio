# ##############################################################################
# Script_3.R - Analisi Avanzata Portafoglio ETF
# (Parte 1: Librerie, Pulizia e Caricamento Dati)
# ##############################################################################

# A. LIBRERIE ------------------------------------------------------------------
# Carichiamo gli strumenti di base necessari per il nostro lavoro.
library(tidyverse)    # Per manipolare i dati e creare grafici
library(readxl)       # Per leggere i file Excel locali
library(forcats)      # Per gestire le categorie (utile nei grafici)
library(stringdist)   # Per confrontare e pulire i nomi delle azioni
library(scales)       # Per formattare i numeri (es. percentuali)
library(RColorBrewer) # Per avere palette di colori professionali
library(lubridate)    # Per lavorare con le date

# B. PULIZIA DELL'AMBIENTE -----------------------------------------------------
rm(list = ls()) # Cancelliamo tutto dalla memoria di R per partire da zero e non avere vecchi dati.
gc() # Garbage Collection - pulisce la RAM

# C. CARICAMENTO DATI ----------------------------------------------------------
# 1. Definiamo la cartella in cui si trovano i file Excel
folder_path <- "D:/Users/F29332B/Downloads/ETF" 

# 2. Creiamo una lista con tutti i file Excel presenti in quella cartella
file_list <- list.files(path = folder_path, pattern = "\\.xlsx?$", full.names = TRUE)
print(paste(">>> Trovati", length(file_list), "file Excel nella cartella."))

# 3. Prepariamo il contenitore vuoto (dataframe) per il nostro portafoglio
# Inseriamo esattamente le colonne che ci serviranno
columns <- c("Name", "Weight", "Industry", "Currency", "Country", 
             "ETF", "PTF_Weight", "Effective_Weight", "Source_File")
portfolio <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(portfolio) <- columns

# 4. Iniziamo a leggere un file alla volta tramite un ciclo
for (file in file_list) {
  
  # Estraiamo i nomi per tenerne traccia (con e senza l'estensione .xlsx)
  etf_name <- tools::file_path_sans_ext(basename(file))
  file_name <- basename(file)
  
  # Proviamo a leggere SOLO il foglio "Components" del file Excel
  # Usiamo tryCatch per evitare che un file sbagliato blocchi tutto lo script
  df <- tryCatch({
    readxl::read_excel(file, sheet = "Components")
  }, error = function(e) {
    return(e) # Catturiamo l'errore se il foglio non esiste
  })
  
  # Se la lettura ha funzionato (non ha generato errori)
  if (!inherits(df, "error")) {
    
    tryCatch({
      # Estraiamo le colonne per posizione: A(1), B(2), F(6), G(7), H(8)
      df_temp <- df[, c(1, 2, 6, 7, 8)]
      
      # Diamo alle colonne i nostri nomi standard, ignorando quelli originali
      colnames(df_temp) <- c("Name", "Weight", "Industry", "Currency", "Country")
      
      # Aggiungiamo le informazioni mancanti per questo ETF
      df_temp$ETF <- etf_name
      df_temp$PTF_Weight <- 0        # Lo assegneremo successivamente
      df_temp$Effective_Weight <- 0  # Lo calcoleremo successivamente
      df_temp$Source_File <- file_name 
      
      # Incolliamo le righe appena lette nel nostro portafoglio globale
      portfolio <- rbind(portfolio, df_temp)
      print(paste("    ->", file_name, "caricato con successo!"))
      
    }, error = function(e) {
      # Se il file ha meno di 8 colonne, ci avvisa senza bloccare il programma
      print(paste("    -> ERRORE in", file_name, ": Impossibile estrarre le colonne richieste."))
    })
    
  } else {
    # Se manca il foglio "Components" o il file è danneggiato, ci avvisa
    print(paste(">>> ERRORE LETTURA in", file_name, "-> Il foglio 'Components' non esiste o il file è danneggiato."))
  }
}
print(paste("--- Fine caricamento. ETF caricati:", length(unique(portfolio$ETF)), "---"))


# D. ASSEGNAZIONE PESI ETF E CALCOLO PESO EFFETTIVO ----------------------------
print("--- Inizio calcolo pesi effettivi (Base 100 su Azionario) ---")

# 1. Inseriamo i Valori di Mercato REALI in Euro (solo per gli 8 ETF azionari)
etf_values_table <- data.frame(
  ETF_Name = c(
    "XTRACKERS_MSCI_WORLD_EX_USA",
    "INVESCO_MSCI_WORLD_EQUAL_WEIGHT",
    "INVESCO_SP500",
    "ISHARES_CORE_MSCI_EM_IMI",
    "XTRACKERS_MSCI_WORLD_VALUE",
    "ISHARES_MSCI_WORLD_MID_CAP_EW",
    "XTRACKERS_MSCI_WORLD_MOMENTUM",
    "ISHARES_MSCI_WORLD_SMALL_CAP"
  ),
  Valore_Mercato = c(
    5822.32, # ex USA
    4213.66, # Equal Weight
    3893.82, # S&P 500
    2890.72, # EM IMI
    2530.15, # Value
    2234.76, # Mid-Cap
    1892.52, # Momentum
    1489.78  # Small Cap
  ),
  stringsAsFactors = FALSE
)

# 2. Calcoliamo la "Base 100" direttamente in R
etf_values_table <- etf_values_table %>%
  # Chiamiamo la nuova colonna 'Target_Weight' per evitare conflitti di nome
  mutate(Target_Weight = Valore_Mercato / sum(Valore_Mercato))

# 3. Uniamo i pesi al nostro portafoglio globale e sistemiamo le colonne
portfolio <- portfolio %>%
  left_join(etf_values_table, by = c("ETF" = "ETF_Name")) %>%
  # Aggiorniamo la nostra colonna originale con le nuove percentuali
  mutate(PTF_Weight = Target_Weight) %>%
  # Eliminiamo le colonne in più che non ci servono più
  select(-Target_Weight, -Valore_Mercato)

# 4. Calcolo del Peso Effettivo (Effective_Weight)
# Convertiamo la colonna in formato numerico per i calcoli
portfolio$Weight <- as.numeric(portfolio$Weight)

portfolio <- portfolio %>%
  # Sostituiamo eventuali celle vuote con 0
  mutate(Weight = ifelse(is.na(Weight), 0, Weight)) %>%
  group_by(ETF) %>%
  # Normalizziamo il peso interno di ogni azione nel suo foglio Excel
  mutate(Weight_Internal = Weight / sum(Weight, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Weight_Internal = ifelse(is.na(Weight_Internal), 0, Weight_Internal)) %>%
  # Moltiplichiamo per la nostra nuova Base 100
  mutate(Effective_Weight = Weight_Internal * PTF_Weight) %>%
  # Escludiamo dal dataframe finale tutto ciò che non ha trovato un peso
  filter(!is.na(Effective_Weight))

# Stampiamo a schermo il risultato per dimostrare che fa esattamente 1 (100%)
totale_portafoglio <- sum(portfolio$Effective_Weight, na.rm = TRUE)
print(paste("Peso totale calcolato del portafoglio azionario:", totale_portafoglio))





# ##############################################################################
# E. DATA CLEANING E STANDARDIZZAZIONE (IN ITALIANO)
# ##############################################################################

# HOTFIX: Rimuoviamo spazi bianchi nascosti o caratteri speciali all'inizio/fine del testo
portfolio$Country  <- str_trim(as.character(portfolio$Country))
portfolio$Industry <- str_trim(as.character(portfolio$Industry))
portfolio$Currency <- str_trim(as.character(portfolio$Currency))

# Ora puoi far eseguire il tuo case_when dei settori e vedrai che si accorperanno perfettamente!

# ESPLORAZIONE DEI DATI: Troviamo i valori unici
print("--- Valori unici per COUNTRY ---")
print(sort(unique(portfolio$Country)))

print("--- Valori unici per INDUSTRY ---")
print(sort(unique(portfolio$Industry)))

print("--- Valori unici per CURRENCY ---")
print(sort(unique(portfolio$Currency)))


# 1. Standardizzazione dei Paesi (Country)
portfolio <- portfolio %>%
  mutate(Country = case_when(
    # Uniformiamo i duplicati
    Country %in% c("Stati Uniti d'America", "United States", "USA") ~ "Stati Uniti",
    Country %in% c("Regno unito", "UK", "United Kingdom") ~ "Regno Unito",
    Country %in% c("Paesi Bassi (Olanda)", "Netherlands") ~ "Paesi Bassi",
    Country %in% c("Tailandia", "Thailand") ~ "Thailandia",
    Country %in% c("Corea", "South Korea", "Repubblica di Corea (Corea del Sud)") ~ "Corea del Sud",
    # Gestiamo i valori vuoti o anomali
    Country %in% c("-", "--", "Codice: PA") ~ "Ignoto",
    is.na(Country) | nchar(as.character(Country)) <= 2 ~ "Ignoto",
    TRUE ~ Country # Lascia inalterati tutti gli altri
  ))

# 2. Creazione della colonna 'MacroArea'
portfolio <- portfolio %>%
  mutate(MacroArea = case_when(
    Country == "Stati Uniti" ~ "USA",
    Country %in% c("Italia", "Francia", "Germania", "Irlanda", "Spagna", "Danimarca",
                   "Finlandia", "Norvegia", "Belgio", "Lussemburgo", "Polonia", "Austria",
                   "Portogallo", "Grecia", "Ungheria", "Paesi Bassi", "Svezia", "Repubblica Ceca",
                   "Unione Europea", "Svizzera", "Regno Unito", "Russia", "Jersey") ~ "Europa",
    Country == "Giappone" ~ "Giappone",
    Country %in% c("Canada", "Australia", "Hong Kong", "Singapore", "Corea del Sud",
                   "Nuova Zelanda", "Taiwan", "Israele") ~ "Altri Sviluppati",
    Country %in% c("Cina", "India", "Brasile", "Sud Africa", "Messico", "Indonesia", "Filippine",
                   "Turchia", "Malesia", "Thailandia", "Cile", "Colombia", "Peru", "Egitto",
                   "Arabia Saudita", "Emirati Arabi Uniti", "Qatar", "Kuwait") ~ "Emergenti",
    Country == "Ignoto" ~ "Ignoto",
    TRUE ~ "Altre Aree"
  ))

# 3. Standardizzazione dei Settori (Industry) - VERSIONE DEFINITIVA
portfolio <- portfolio %>%
  mutate(Industry = case_when(
    Industry %in% c("Technology", "IT", "Tecnologia", "Tecnologia dell'informazione", "Information Technology") ~ "Tecnologia",
    Industry %in% c("Financial Services", "Finanza", "Società finanziarie", "Brokerage/Asset Managers/Exchanges",
                    "Financial Other", "Finanziari", "Assicurazioni", "Financials") ~ "Finanza",
    Industry %in% c("Healthcare", "Salute", "Sanità", "Health Care") ~ "Salute",
    Industry %in% c("Communication Services", "Comunicazione", "Comunicazioni", "Servizi di comunicazione") ~ "Comunicazione",
    Industry %in% c("Consumer Cyclical", "Consumi Discrezionali", "Beni di consumo ciclici", "Beni voluttuari", "Consumer Discretionary") ~ "Consumi Discrezionali",
    Industry %in% c("Consumer Defensive", "Consumer Non-Cyclical", "Generi di largo consumo", "Beni di prima necessità", "Consumer Staples") ~ "Beni di prima necessità",
    Industry %in% c("Industrials", "Industriali", "Prodotti industriali", "Industrial Other", "Basic Industry") ~ "Industriali",
    Industry %in% c("Basic Materials", "Materiali", "Materials") ~ "Materiali",
    Industry %in% c("Energy", "Energia", "Gas Naturale") ~ "Energia",
    Industry %in% c("Utilities", "Utility Other", "Elettrico", "Stranded Cost Utility",
                    "Imprese di servizi di pubblica utilità", "Servizi di pubblica utilità") ~ "Utilities",
    Industry %in% c("Real Estate", "Immobiliare", "Immobili", "Certificato Immobiliare") ~ "Immobiliare",
    is.na(Industry) | Industry %in% c("<NA>", "unknown", "Non trovato", "-", "--", "N/D", "sconosciuta") ~ "Ignoto",
    TRUE ~ Industry
  ))

# 4. Standardizzazione di base per le Valute (Currency)
portfolio <- portfolio %>%
  mutate(Currency = case_when(
    is.na(Currency) | Currency %in% c("-", "--") ~ "Ignoto",
    TRUE ~ Currency
  ))

# 5. Convertiamo le colonne nel formato corretto per i grafici futuri
portfolio$Country <- as.factor(portfolio$Country)
portfolio$Currency <- as.factor(portfolio$Currency)
portfolio$ETF <- as.factor(portfolio$ETF)
portfolio$Industry <- as.factor(portfolio$Industry)
portfolio$MacroArea <- as.factor(portfolio$MacroArea)




# ##############################################################################
# F. NORMALIZZAZIONE DEI NOMI (FONDAMENTALE PER TOP HOLDINGS)
# ##############################################################################
# Funzione per trovare il nome più simile usando l'algoritmo Jaro-Winkler
match_names <- function(name, name_list) {
  if (is.na(name)) return(NA)
  distances <- stringdist::stringdist(tolower(name), tolower(name_list), method = "jw")
  name_list[which.min(distances)]
}

# Regex per rimuovere suffissi societari (es. Inc, Corp, Spa)
remove_pattern <- "\\b(s\\.a\\.|/s|non voting|non voting\\s+pre|non-v|non-voting|pc|dr|npv| a| b| c| cl a| cl b| cl c| fxd| pcl| as| a\\.s| llc| plc| regs| mtn| fxd-to-flt| fxd-flt| flat| sa| spa| inc| corp| the| co| class a| reg| ag reg| se| ltd| nv| ab| class b| ag| class c| a/s| cls a| cls b| cls c| 144a| \\(fxd-fxn\\)| fxd-frn| bv| ucits| etf| acc| dist|usd[0-9]+\\.[0-9]+)\\b"

# Puliamo i nomi
clean_names <- str_trim(str_remove_all(tolower(portfolio$Name), remove_pattern))
unique_names <- unique(clean_names)
unique_names <- unique_names[!is.na(unique_names)]

# Applichiamo la normalizzazione
portfolio$Name_Normalized <- sapply(clean_names, match_names, name_list = unique_names, USE.NAMES = FALSE)


# ##############################################################################
# G. VISUALIZZAZIONE DEI DATI (I GRAFICI)
# ##############################################################################

# ------------------------------------------------------------------------------
# GRAFICO 1: ESPOSIZIONE GEOGRAFICA (Bar Chart Orizzontale al posto della Treemap)
# ------------------------------------------------------------------------------
geo_data <- portfolio %>%
  filter(Effective_Weight > 0) %>%
  group_by(MacroArea) %>%
  summarise(total_weight = sum(Effective_Weight, na.rm = TRUE), .groups = 'drop') %>%
  # Ordiniamo le aree dalla più grande alla più piccola
  mutate(MacroArea = fct_reorder(MacroArea, total_weight))

plot1 <- ggplot(geo_data, aes(x = MacroArea, y = total_weight, fill = MacroArea)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(total_weight, accuracy = 0.1)), hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent, limits = c(0, max(geo_data$total_weight) * 1.15)) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() + # Rende il grafico a barre orizzontale
  labs(
    title = "Esposizione Geografica per Macro Area",
    #subtitle = "Ripartizione percentuale del portafoglio azionario",
    x = "Macro Area",
    y = "Peso Effettivo %"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Nascondiamo la legenda perché i nomi sono già sull'asse X

print(plot1)

# ------------------------------------------------------------------------------
# GRAFICO 2: INDUSTRIA PER MACROAREA (Stacked Bar %)
# ------------------------------------------------------------------------------
industry_area_data <- portfolio %>%
  filter(Effective_Weight > 0) %>%
  group_by(Industry) %>%
  summarise(total_weight = sum(Effective_Weight, na.rm = TRUE), .groups = 'drop') %>%
  filter(Industry != "Ignoto") %>%
  mutate(Industry = fct_reorder(Industry, total_weight))


plot2 <- ggplot(industry_area_data, aes(x = Industry, y = total_weight, fill = Industry)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(total_weight, accuracy = 0.1)), hjust = -0.1, size = 4, fontface = "bold") +
  scale_y_continuous(labels = percent, limits = c(0, max(industry_area_data$total_weight) * 1.15)) +
  #scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title = "Composizione Settoriale",
    x = "Industry",
    y = "Peso Relativo %",
    fill = "Settore Industriale"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(plot2)

# ------------------------------------------------------------------------------
# GRAFICO 3: ESPOSIZIONE VALUTARIA (Grafico a Ciambella)
# ------------------------------------------------------------------------------
currency_data_donut <- portfolio %>%
  group_by(Currency) %>%
  summarise(Weight = sum(Effective_Weight, na.rm = TRUE)) %>%
  # Raggruppiamo le valute sotto il 2% in "Altre"
  mutate(Currency_Group = ifelse(Weight < 0.02, "Altre", as.character(Currency))) %>%
  group_by(Currency_Group) %>%
  summarise(Total_Weight = sum(Weight)) %>%
  
  # 1. ORDINA: Mettiamo le valute in ordine decrescente di peso
  arrange(desc(Total_Weight)) %>% 
  
  # 2. CALCOLA POSIZIONI: Ora i cumulati seguiranno l'ordine corretto
  mutate(fraction = Total_Weight / sum(Total_Weight),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(Currency_Group, "\n", percent(Total_Weight, accuracy = 0.1)))

plot3 <- ggplot(currency_data_donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=reorder(Currency_Group, -Total_Weight))) +
  geom_rect(colour = "white", size = 0.3) + # Aggiunge un bordino bianco pulito tra le fette
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  # Spostiamo il colore del testo a "black" come avevi modificato tu, per massima leggibilità
  geom_text(x = 3.5, aes(y = labelPosition, label = label), size=4, color="black", fontface = "bold") +
  scale_fill_brewer(palette="Set3") +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Esposizione Valutaria", 
       subtitle = "Valute ordinate per peso (valute minori < 2% in 'Altre')")

print(plot3)



# ##############################################################################
# H. CALCOLO E NORMALIZZAZIONE DEI NOMI
# ##############################################################################

# 1. Definiamo la funzione di corrispondenza Jaro-Winkler
match_names <- function(name, name_list) {
  if (is.na(name)) return(NA)
  distances <- stringdist::stringdist(tolower(name), tolower(name_list), method = "jw")
  name_list[which.min(distances)]
}

# 2. Espressione regolare per ripulire i suffissi societari e text spazzatura
remove_pattern <- "\\b(s\\.a\\.|/s|non voting|non voting\\s+pre|non-v|non-voting|pc|dr|npv| a| b| c| cl a| cl b| cl c| fxd| pcl| as| a\\.s| llc| plc| regs| mtn| fxd-to-flt| fxd-flt| flat| sa| spa| inc| corp| the| co| class a| reg| ag reg| se| ltd| nv| ab| class b| ag| class c| a/s| cls a| cls b| cls c| 144a| \\(fxd-fxn\\)| fxd-frn| bv| ucits| etf| acc| dist|usd[0-9]+\\.[0-9]+)\\b"

# 3. Pulizia e creazione dei nomi unici di confronto
clean_names <- str_trim(str_remove_all(tolower(portfolio$Name), remove_pattern))
unique_names <- unique(clean_names)
unique_names <- unique_names[!is.na(unique_names)]

# 4. Calcoliamo la colonna Name_Normalized applicando la funzione
portfolio$Name_Normalized <- sapply(clean_names, match_names, name_list = unique_names, USE.NAMES = FALSE)


# ------------------------------------------------------------------------------
# GRAFICO 4: Grafico a Concentrazione per Nome 
# ------------------------------------------------------------------------------

top_holdings_data <- portfolio %>%
  # Escludiamo i valori mancanti, scritte vuote o righe di liquidità/derivati
  filter(!is.na(Name_Normalized) & Effective_Weight > 0) %>%
  filter(!Name_Normalized %in% c("-", "--", "unknown", "n/d", "liquidità e/o derivati")) %>%
  group_by(Name_Normalized) %>%
  summarise(total_weight = sum(Effective_Weight, na.rm = TRUE)) %>%
  arrange(desc(total_weight)) %>%
  slice_head(n = 15) %>%
  # Trasformiamo la prima lettera in maiuscola per estetica nel grafico
  mutate(Name_Normalized = str_to_title(Name_Normalized)) %>%
  # Ordiniamo l'asse del grafico in base al peso
  mutate(Name_Normalized = fct_reorder(Name_Normalized, total_weight))

plot4 <- ggplot(top_holdings_data, aes(x = Name_Normalized, y = total_weight, fill = total_weight)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percent(total_weight, accuracy = 0.01)), hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = percent, limits = c(0, max(top_holdings_data$total_weight) * 1.15)) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  coord_flip() +
  labs(
    title = "Top 15 Titoli Azionari per Peso Effettivo",
    #subtitle = "Pesi aggregati e normalizzati su tutti gli ETF in portafoglio",
    x = "Azienda",
    y = "Peso Reale nel Portafoglio %"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 10, face = "bold"))

print(plot4)



# ------------------------------------------------------------------------------
# GRAFICO 5: ANALISI ABC (Curva di Concentrazione)
# ------------------------------------------------------------------------------
# 1. Preparazione dei dati per la curva ABC
abc_data <- portfolio %>%
  filter(!is.na(Name_Normalized) & Effective_Weight > 0) %>%
  group_by(Name_Normalized) %>%
  summarise(total = sum(Effective_Weight, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(total)) %>%
  mutate(
    cum_sum = cumsum(total),
    rank_pct = row_number() / n()
  )

# 2. Calcolo dinamico delle metriche di diversificazione per arricchire il grafico
n_etf <- length(unique(portfolio$ETF))
n_titoli_totali <- nrow(abc_data)
pct_80_point <- abc_data %>% filter(cum_sum >= 0.80) %>% slice_head(n = 1)
titoli_per_80pct <- round(pct_80_point$rank_pct * n_titoli_totali)

# 3. Costruzione del grafico avanzato e accattivante
plot5 <- ggplot(abc_data, aes(x = rank_pct, y = cum_sum)) +
  # Area sotto la curva con un colore azzurro/carta da zucchero semi-trasparente
  geom_area(fill = "#4A90E2", alpha = 0.15) +
  
  # Linea della curva ABC più spessa e definita
  geom_line(size = 1.5, color = "#1F4E79") +
  
  # Linee tratteggiate di intersezione per il punto dell'80%
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "#E74C3C", size = 0.8) +
  geom_vline(xintercept = pct_80_point$rank_pct, linetype = "dashed", color = "#E74C3C", size = 0.8) +
  
  # Testo descrittivo del punto di intersezione (80% del peso)
  annotate("label", 
           x = pct_80_point$rank_pct + 0.02, 
           y = 0.75, 
           label = paste0("80% della concetrazione è fatta con:\n", 
                          percent(pct_80_point$rank_pct, accuracy = 0.1), 
                          " dei titoli (", titoli_per_80pct, " su ", n_titoli_totali, ")"),
           color = "#B22222", fontface = "bold", size = 3.5, hjust = 0, fill = "white", label.size = 0.5) +
  
  # Box informativo in basso a destra per evidenziare la DIVERSIFICAZIONE
  annotate("label", 
           x = 0.55, 
           y = 0.25, 
           label = paste0("📈 METRICHE DI DIVERSIFICAZIONE:\n\n",
                          "▪️ ETF totali in lista: ", n_etf, "\n",
                          "▪️ Aziende uniche : ", n_titoli_totali, "\n",
                          "▪️ Il restante 20% del peso è distribuito\n    su  ", (n_titoli_totali - titoli_per_80pct), " aziende diverse."),
           color = "#2C3E50", fontface = "plain", size = 4, hjust = 0, fill = "#F8F9FA", color = "#BDC3C7", label.padding = unit(0.5, "lines")) +
  
  # Formattazione degli assi in percentuali
  scale_x_continuous(labels = percent, expand = c(0.01, 0.01)) +
  scale_y_continuous(labels = percent, expand = c(0.01, 0.01), limits = c(0, 1.02)) +
  
  # Etichette, Titolo e Sottotitolo dinamico
  labs(
    title = "Analisi ABC & Grado di Diversificazione Globale",
    subtitle = paste("Look-Through su", n_etf, "ETF azionari per un totale di", n_titoli_totali, "titoli unici in portafoglio"),
    x = "Percentuale Cumulata dei Titoli (Ordinati dal più grande al più piccolo)",
    y = "Percentuale Cumulata del Peso del Portafoglio"
  ) +
  
  # Restyling del tema grafico per renderlo moderno e pulito
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#1F4E79"),
    plot.subtitle = element_text(face = "italic", size = 11, color = "#555555", margin = margin(b = 15)),
    axis.title.x = element_text(margin = margin(t = 10), face = "bold", size = 10, color = "#333333"),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold", size = 10, color = "#333333"),
    panel.grid.major = element_line(color = "#EAEAEA"),
    panel.grid.minor = element_blank()
  )

print(plot5)

# ##############################################################################
# I. TABELLA RIASSUNTIVA: COMPOSIZIONE DEL PORTAFOGLIO AZIONARIO
# ##############################################################################
tabella_composizione <- portfolio %>%
  group_by(ETF) %>%
  summarise(
    `Peso Azionario %` = max(PTF_Weight),
    `Numero Aziende` = n()
  ) %>%
  arrange(desc(`Peso Azionario %`)) %>%
  # Formattiamo il peso in una percentuale leggibile per la stampa
  mutate(`Peso Azionario %` = scales::percent(`Peso Azionario %`, accuracy = 0.01))

# Stampiamo la tabella nella console
print(tabella_composizione)



# ##############################################################################
# J. ESPORTAZIONE CAROUSEL PER LINKEDIN (FORMATO PDF QUADRATO 1:1)
# ##############################################################################
# Carichiamo le librerie necessarie per la gestione grafica dei PDF e delle tabelle
library(gridExtra)
library(grid)

# 1. Definiamo dove salvare il PDF finale
pdf_output_path <- "D:/Users/F29332B/Downloads/ETF/carousel_linkedin.pdf"

# 2. Apriamo il "dispositivo" PDF impostando dimensioni quadrate (10x10 pollici)
# Questo creerà il formato 1:1 perfetto per il feed di LinkedIn
pdf(file = pdf_output_path, width = 10, height = 10)


# --- SLIDE 1: COPERTINA (Il "Gancio" per LinkedIn) ---
# Una copertina pulita e professionale aumenta drasticamente i clic sul post
plot_cover <- ggplot() +
  annotate("text", x = 5, y = 6, label = "ANALISI AVANZATA\nPORTAFOGLIO AZIONARIO", 
           size = 8, fontface = "bold", color = "#0073C2", hjust = 0.5) +
  annotate("text", x = 5, y = 4.5, label = "Un'analisi look-through della vera allocazione dei miei ETF", 
           size = 4.5, fontface = "italic", color = "gray30", hjust = 0.5) +
  annotate("text", x = 5, y = 2, label = "Scorri per vedere i dati ➡️", 
           size = 4, fontface = "bold", color = "gray50", hjust = 0.5) +
  xlim(0, 10) + ylim(0, 10) +
  theme_void()

#print(plot_cover)
#no cover

# --- SLIDE 0: LA TABELLA DI COMPOSIZIONE FINALIZZATA (Allineata a destra) ---
grid.newpage()

# Inseriamo il titolo
grid.text("Composizione del Portafoglio Azionario", 
          y = 0.92, gp = gpar(fontsize = 18, fontface = "bold", col = "#0073C2"))

# 1. Prepariamo le matrici di allineamento per le celle (nr righe x nc colonne)
nr <- nrow(tabella_composizione)
nc <- ncol(tabella_composizione)

# Diciamo a R: Colonna 1 a destra (1), Colonna 2 e 3 al centro (0.5)
hjust_core <- matrix(c(1, 0.5, 0.5), nrow = nr, ncol = nc, byrow = TRUE)
x_core     <- matrix(c(0.95, 0.5, 0.5), nrow = nr, ncol = nc, byrow = TRUE)

# 2. ConfiguriAMO lo stile grafico applicando le matrici di allineamento
tema_tabella <- ttheme_default(
  core = list(
    bg_params = list(fill = c("white", "#F2F2F2")), 
    # Applichiamo l'allineamento personalizzato alle celle con i dati
    fg_params = list(fontsize = 12, hjust = hjust_core, x = x_core)
  ),
  colhead = list(
    bg_params = list(fill = "#0073C2"), 
    # Allineiamo a destra anche l'intestazione della prima colonna ("ETF")
    fg_params = list(col = "white", fontface = "bold", fontsize = 13, 
                     hjust = c(1, 0.5, 0.5), x = c(0.95, 0.5, 0.5))
  ),
  rowhead = list(fg_params = list(fontsize = 11))
)

# 3. Trasformiamo la tabella in un oggetto grafico e la disegnamo
oggetto_tabella <- tableGrob(tabella_composizione, rows = NULL, theme = tema_tabella)
grid.draw(oggetto_tabella)





# --- SLIDE 2: GRAFICO 1 (Geografia) ---
print(plot1)


# --- SLIDE 3: GRAFICO 2 (Settori per Area) ---
print(plot2)


# --- SLIDE 4: GRAFICO 3 (Valute Ordinate) ---
print(plot3)


# --- SLIDE 5: GRAFICO 4 (Top 15 Titoli) ---
print(plot4)


# --- SLIDE 6: GRAFICO 5 (Curva ABC) ---
print(plot5)


# 3. CHIUDIAMO E SALVIAMO IL FILE
dev.off()

print(paste("🔥 Carousel creato! Trovi il file pronto da pubblicare qui:", pdf_output_path))
