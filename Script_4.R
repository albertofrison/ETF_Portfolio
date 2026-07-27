# ##############################################################################
# Script_4_Benchmark.R - Confronto Portafoglio vs VWCE (Senza Warning/Errori)
# ##############################################################################

# A. LIBRERIE NECESSARIE -------------------------------------------------------
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

library(tidyverse)
library(readxl)
library(scales)
library(treemapify)
library(patchwork)

# B. IMPOSTAZIONE PERCORSI E CARICAMENTO VWCE ----------------------------------
folder_path <- "D:/Users/F29332B/Downloads/ETF"
vwce_file_name <- "VANGUARD_ALL_WORLD.xlsx" 
vwce_full_path <- file.path(folder_path, vwce_file_name)

# ##############################################################################
# C. CARICAMENTO E STANDARDIZZAZIONE PORTAFOGLIO MERCATO (VWCE)
# ##############################################################################

print(paste(">>> Caricamento file di mercato da:", vwce_full_path))

portfoglio_mercato <- readxl::read_excel(vwce_full_path)
colnames(portfoglio_mercato) <- str_trim(colnames(portfoglio_mercato))

# 1. Conversione del peso numerico (da % testo a numero decimale)
portfoglio_mercato <- portfoglio_mercato %>%
  mutate(
    Effective_Weight_Num = str_remove_all(as.character(Effective_Weight), "%"),
    Effective_Weight_Num = str_replace(Effective_Weight_Num, ",", "."),
    Effective_Weight = as.numeric(Effective_Weight_Num) / 100
  ) %>%
  select(-Effective_Weight_Num)

# 2. Standardizzazione dei Settori (Industry): dall'Inglese all'Italiano
portfoglio_mercato <- portfoglio_mercato %>%
  mutate(
    # Pulisce eventuali spazi bianchi ai bordi del testo
    Industry = str_trim(as.character(Industry)),
    
    # Mappatura dei settori in lingua italiana
    Industry = case_when(
      Industry %in% c("Technology", "IT", "Information Technology") ~ "Tecnologia",
      Industry %in% c("Financials", "Financial Services", "Financial Other", "Finanza") ~ "Finanza",
      Industry %in% c("Health Care", "Healthcare", "Salute") ~ "Salute",
      Industry %in% c("Telecommunications", "Communication Services", "Communication", "Comunicazione") ~ "Comunicazione",
      Industry %in% c("Consumer Discretionary", "Consumer Cyclical", "Consumi Discrezionali") ~ "Consumi Discrezionali",
      Industry %in% c("Consumer Staples", "Consumer Defensive", "Consumer Non-Cyclical", "Generi di largo consumo") ~ "Beni di prima necessità",
      Industry %in% c("Industrials", "Industrial Other", "Basic Industry", "Industriali") ~ "Industriali",
      Industry %in% c("Basic Materials", "Materials", "Materiali") ~ "Materiali",
      Industry %in% c("Energy", "Energia") ~ "Energia",
      Industry %in% c("Utilities", "Utility Other", "Servizi di pubblica utilità") ~ "Utilities",
      Industry %in% c("Real Estate", "Immobiliare") ~ "Immobiliare",
      Industry %in% c("Cash", "Liquidity", "Derivatives", "Liquidità e/o derivati") ~ "Liquidità e/o derivati",
      is.na(Industry) | Industry %in% c("-", "--", "unknown", "N/D", "sconosciuta") ~ "Ignoto",
      TRUE ~ Industry # Mantiene inalterati eventuali settori già in italiano
    )
  )

# 3. Standardizzazione dei Paesi (ISO -> Italiano)
portfoglio_mercato <- portfoglio_mercato %>%
  mutate(Country = case_when(
    Country %in% c("US", "USA", "United States") ~ "Stati Uniti",
    Country %in% c("DE", "Germany") ~ "Germania",
    Country %in% c("GB", "UK", "United Kingdom") ~ "Regno Unito",
    Country %in% c("JP", "Japan") ~ "Giappone",
    Country %in% c("FR", "France") ~ "Francia",
    Country %in% c("CA", "Canada") ~ "Canada",
    Country %in% c("CH", "Switzerland") ~ "Svizzera",
    Country %in% c("AU", "Australia") ~ "Australia",
    Country %in% c("KR", "South Korea", "Korea") ~ "Corea del Sud",
    Country %in% c("TW", "Taiwan") ~ "Taiwan",
    Country %in% c("NL", "Netherlands") ~ "Paesi Bassi",
    Country %in% c("ES", "Spain") ~ "Spagna",
    Country %in% c("HK", "Hong Kong") ~ "Hong Kong",
    Country %in% c("DK", "Denmark") ~ "Danimarca",
    Country %in% c("FI", "Finland") ~ "Finlandia",
    Country %in% c("CN", "China") ~ "Cina",
    Country %in% c("IN", "India") ~ "India",
    Country %in% c("IT", "Italy") ~ "Italia",
    Country %in% c("SG", "Singapore") ~ "Singapore",
    is.na(Country) | Country == "" ~ "Ignoto",
    TRUE ~ Country
  ))

print(">>> Traduzione e pulizia dei Settori (Industry) completata con successo!")

# C. PREPARAZIONE DATASET AGGREGATI (TOP 100) ----------------------------------

ptf_top100 <- portfolio %>%
  filter(!is.na(Name_Normalized) & Effective_Weight > 0) %>%
  filter(!tolower(Name_Normalized) %in% c("-", "--", "unknown", "n/d", "liquidità e/o derivati", "liquidita")) %>%
  group_by(Name_Normalized) %>%
  summarise(
    Industry = first(Industry),
    Country = first(Country),
    Peso_Totale = sum(Effective_Weight, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Peso_Totale)) %>%
  slice_head(n = 100) %>%
  mutate(
    Nome_Pulito = str_to_title(str_replace_all(Name_Normalized, "_", " ")),
    Etichetta = paste0(Nome_Pulito, "\n", scales::percent(Peso_Totale, accuracy = 0.01))
  )

mkt_top100 <- portfoglio_mercato %>%
  filter(!is.na(Name_Normalized) & Effective_Weight > 0) %>%
  filter(!tolower(Name_Normalized) %in% c("-", "--", "unknown", "n/d", "liquidità e/o derivati", "liquidita")) %>%
  group_by(Name_Normalized) %>%
  summarise(
    Industry = first(Industry),
    Country = first(Country),
    Peso_Totale = sum(Effective_Weight, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Peso_Totale)) %>%
  slice_head(n = 100) %>%
  mutate(
    Nome_Pulito = str_to_title(str_replace_all(Name_Normalized, "_", " ")),
    Etichetta = paste0(Nome_Pulito, "\n", scales::percent(Peso_Totale, accuracy = 0.01))
  )


# D. GENERAZIONE PALETTE COLORI DINAMICHE ED ESTESE ----------------------------

# 1. Palette Unificata per i Paesi
tutti_i_paesi <- sort(unique(c(ptf_top100$Country, mkt_top100$Country)))
palette_paesi <- colorRampPalette(RColorBrewer::brewer.pal(min(length(tutti_i_paesi), 12), "Paired"))(length(tutti_i_paesi))
colori_paesi_unificati <- setNames(palette_paesi, tutti_i_paesi)

# 2. Palette Unificata per i Settori
tutti_i_settori <- sort(unique(c(ptf_top100$Industry, mkt_top100$Industry)))
palette_settori <- colorRampPalette(RColorBrewer::brewer.pal(min(length(tutti_i_settori), 12), "Set3"))(length(tutti_i_settori))
colori_settori_unificati <- setNames(palette_settori, tutti_i_settori)


# ##############################################################################
# E. GENERAZIONE DEI GRAFICI AFFIANCATI (3 COPPIE)
# ##############################################################################

# ------------------------------------------------------------------------------
# COPPIA 1: CONFRONTO CURVA ABC (Con evidenza dei numeri assoluti)
# ------------------------------------------------------------------------------

crea_grafico_abc <- function(df, titolo_portafoglio) {
  # 1. Preparazione e aggregazione dei dati ordinati
  data_abc <- df %>%
    filter(!is.na(Name_Normalized) & Effective_Weight > 0) %>%
    filter(!tolower(Name_Normalized) %in% c("-", "--", "unknown", "n/d", "liquidità e/o derivati", "liquidita")) %>%
    group_by(Name_Normalized) %>%
    summarise(Peso_Totale = sum(Effective_Weight, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(Peso_Totale)) %>%
    mutate(
      cum_sum = cumsum(Peso_Totale), 
      rank_num = row_number(),        # Conteggio numerico assoluto (1, 2, 3...)
      rank_pct = rank_num / n()       # Percentuale relativa (0% -> 100%)
    )
  
  # 2. CALCOLO DELLE METRICHE ASSOLUTE
  n_titoli_totali <- nrow(data_abc)
  pct_80 <- data_abc %>% filter(cum_sum >= 0.80) %>% slice_head(n = 1)
  titoli_80_pct <- pct_80$rank_num
  
  # Creiamo la stringa di testo per la casella informativa
  testo_box <- paste0(
    "80% del peso ottenuto con:\n",
    format(titoli_80_pct, big.mark = "."), " titoli su ", format(n_titoli_totali, big.mark = "."), "\n",
    "(pari al ", percent(pct_80$rank_pct, accuracy = 0.1), " dei titoli totali)"
  )
  
  # 3. Costruzione del grafico con le etichette esplicative
  ggplot(data_abc, aes(x = rank_pct, y = cum_sum)) +
    # Area azzurra sotto la curva
    geom_area(fill = "#4A90E2", alpha = 0.2) +
    
    # Linea della curva ABC
    geom_line(linewidth = 1.2, color = "#1F4E79") +
    
    # Retta orizzontale e verticale rossi tratteggiati
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
    geom_vline(xintercept = pct_80$rank_pct, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
    
    # BOX INFORMATIVO CON I NUMERI REALI ASSOLUTI
    annotate(
      "label", 
      x = pct_80$rank_pct + 0.03, 
      y = 0.60, 
      label = testo_box, 
      color = "#B22222", 
      fontface = "bold", 
      size = 3.2, 
      hjust = 0, 
      fill = "white", 
      label.size = 0.4
    ) +
    
    # Formattazione assi e zoom sicuro
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    coord_cartesian(ylim = c(0, 1.02)) +
    
    # Titolo e Sottotitolo che evidenziano la dimensione totale
    labs(
      title = paste("Analisi ABC -", titolo_portafoglio),
      subtitle = paste0("📌 Totale aziende uniche in portafoglio: ", format(n_titoli_totali, big.mark = ".")),
      x = "% Titoli Cumulati", 
      y = "% Peso Cumulato"
    ) +
    
    # Tema grafico pulito
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "#1F4E79"),
      plot.subtitle = element_text(face = "bold", size = 10, color = "#B22222"),
      axis.title = element_text(size = 9, face = "bold")
    )
}

# Generiamo i due grafici con le nuove metriche
abc_mio_ptf <- crea_grafico_abc(portfolio, "Mio Portafoglio")
abc_mercato <- crea_grafico_abc(portfoglio_mercato, "Mercato (VWCE)")

# Affianchiamo i due grafici per il confronto
coppia_1_abc <- abc_mio_ptf + abc_mercato

# Stampiamo a schermo
print(coppia_1_abc)




# ------------------------------------------------------------------------------
# COPPIA 2: TREEMAP PER SETTORE (Industry)
# ------------------------------------------------------------------------------
crea_treemap_industry <- function(df_top100, titolo) {
  tot_weight <- scales::percent(sum(df_top100$Peso_Totale), accuracy = 0.1)
  
  ggplot(df_top100, aes(area = Peso_Totale, fill = Industry, label = Etichetta, subgroup = Industry)) +
    geom_treemap(color = "white") +
    geom_treemap_subgroup_border(color = "black") +
    geom_treemap_text(colour = "white", place = "centre", grow = FALSE, reflow = TRUE, size = 7) +
    scale_fill_manual(values = colori_settori_unificati, name = "Settore") +
    labs(title = paste(titolo, "- Top 100 (Peso:", tot_weight, ")")) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      plot.title = element_text(face = "bold", size = 11)
    )
}

tree_ind_ptf <- crea_treemap_industry(ptf_top100, "Mio Portafoglio")
tree_ind_mkt <- crea_treemap_industry(mkt_top100, "Mercato (VWCE)")

coppia_2_industry <- tree_ind_ptf + tree_ind_mkt
print(coppia_2_industry)


# ------------------------------------------------------------------------------
# COPPIA 3: TREEMAP PER PAESE (Country)
# ------------------------------------------------------------------------------
crea_treemap_country <- function(df_top100, titolo) {
  tot_weight <- scales::percent(sum(df_top100$Peso_Totale), accuracy = 0.1)
  
  ggplot(df_top100, aes(area = Peso_Totale, fill = Country, label = Etichetta, subgroup = Country)) +
    geom_treemap(color = "white") +
    geom_treemap_subgroup_border(color = "black") +
    geom_treemap_text(colour = "white", place = "centre", grow = FALSE, reflow = TRUE, size = 7) +
    scale_fill_manual(values = colori_paesi_unificati, name = "Paese") +
    labs(title = paste(titolo, "- Top 100 (Peso:", tot_weight, ")")) +
    theme_minimal() +
    theme(
      legend.position = "bottom", 
      plot.title = element_text(face = "bold", size = 11)
    )
}

tree_cnt_ptf <- crea_treemap_country(ptf_top100, "Mio Portafoglio")
tree_cnt_mkt <- crea_treemap_country(mkt_top100, "Mercato (VWCE)")

coppia_3_country <- tree_cnt_ptf + tree_cnt_mkt
print(coppia_3_country)

portfolio %>%
  group_by (Industry) %>%
  summarize (num = n())
