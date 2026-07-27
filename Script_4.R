# ##############################################################################
# Script_4_Benchmark.R - Confronto Portafoglio vs VWCE & Esportazione PDF
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

print(paste(">>> Caricamento file di mercato da:", vwce_full_path))

portfoglio_mercato <- readxl::read_excel(vwce_full_path)
colnames(portfoglio_mercato) <- str_trim(colnames(portfoglio_mercato))

# 1. Conversione del peso numerico
portfoglio_mercato <- portfoglio_mercato %>%
  mutate(
    Effective_Weight_Num = str_remove_all(as.character(Effective_Weight), "%"),
    Effective_Weight_Num = str_replace(Effective_Weight_Num, ",", "."),
    Effective_Weight = as.numeric(Effective_Weight_Num) / 100
  ) %>%
  select(-Effective_Weight_Num)

# 2. Standardizzazione Nomi Paesi (ISO -> Italiano)
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

# 3. Standardizzazione dei Settori (Inglese -> Italiano)
portfoglio_mercato <- portfoglio_mercato %>%
  mutate(
    Industry = str_trim(as.character(Industry)),
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
      TRUE ~ Industry
    )
  )


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

# ##############################################################################
# D. PALETTE COLORI VIVACI, DISTINTE E AD ALTO CONTRASTO
# ##############################################################################

# 1. Palette per i Paesi (Colori saturi e ben visibili)
tutti_i_paesi <- sort(unique(c(ptf_top100$Country, mkt_top100$Country)))
base_col_paesi <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", 
                    "#8C564B", "#E377C2", "#17BECF", "#BCBD22", "#31A354")
palette_paesi <- colorRampPalette(base_col_paesi)(length(tutti_i_paesi))
colori_paesi_unificati <- setNames(palette_paesi, tutti_i_paesi)

# 2. Palette per i Settori (Colori vivaci distinti per evitare confusioni)
tutti_i_settori <- sort(unique(c(ptf_top100$Industry, mkt_top100$Industry)))
base_col_settori <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B", 
                      "#48A9A6", "#E63946", "#457B9D", "#2A9D8F", "#E9C46A")
palette_settori <- colorRampPalette(base_col_settori)(length(tutti_i_settori))
colori_settori_unificati <- setNames(palette_settori, tutti_i_settori)


# ##############################################################################
# E. GENERAZIONE DEI GRAFICI AFFIANCATI (CON LEGENDE UNIFICATE E TITOLI CHIARI)
# ##############################################################################

# ------------------------------------------------------------------------------
# 1. COPPIA ABC: CONFRONTO CONCENTRAZIONE TITOLI
# ------------------------------------------------------------------------------
crea_grafico_abc <- function(df, titolo_portafoglio) {
  data_abc <- df %>%
    filter(!is.na(Name_Normalized) & Effective_Weight > 0) %>%
    filter(!tolower(Name_Normalized) %in% c("-", "--", "unknown", "n/d", "liquidità e/o derivati", "liquidita")) %>%
    group_by(Name_Normalized) %>%
    summarise(Peso_Totale = sum(Effective_Weight, na.rm = TRUE), .groups = 'drop') %>%
    arrange(desc(Peso_Totale)) %>%
    mutate(
      cum_sum = cumsum(Peso_Totale), 
      rank_num = row_number(),
      rank_pct = rank_num / n()
    )
  
  n_titoli_totali <- nrow(data_abc)
  pct_80 <- data_abc %>% filter(cum_sum >= 0.80) %>% slice_head(n = 1)
  titoli_80_pct <- pct_80$rank_num
  
  testo_box <- paste0(
    "80% del peso ottenuto con:\n",
    format(titoli_80_pct, big.mark = "."), " titoli su ", format(n_titoli_totali, big.mark = "."), "\n",
    "(pari al ", percent(pct_80$rank_pct, accuracy = 0.1), " dei titoli totali)"
  )
  
  ggplot(data_abc, aes(x = rank_pct, y = cum_sum)) +
    geom_area(fill = "#4A90E2", alpha = 0.2) +
    geom_line(linewidth = 1.2, color = "#1F4E79") +
    geom_hline(yintercept = 0.8, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
    geom_vline(xintercept = pct_80$rank_pct, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
    annotate("label", x = pct_80$rank_pct + 0.03, y = 0.60, label = testo_box, 
             color = "#B22222", fontface = "bold", size = 3, hjust = 0, fill = "white", label.size = 0.4) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    coord_cartesian(ylim = c(0, 1.02)) +
    labs(
      title = titolo_portafoglio,
      subtitle = paste0("📌 Aziende uniche totali: ", format(n_titoli_totali, big.mark = ".")),
      x = "% Titoli Cumulati", y = "% Peso Cumulato"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 11, color = "#1F4E79"),
      plot.subtitle = element_text(face = "bold", size = 9, color = "#B22222"),
      axis.title = element_text(size = 8.5, face = "bold")
    )
}

abc_mio_ptf <- crea_grafico_abc(portfolio, "Mio Portafoglio (Allocazione Personale)")
abc_mercato <- crea_grafico_abc(portfoglio_mercato, "Mercato Benchmark (VWCE)")

coppia_1_abc <- (abc_mio_ptf + abc_mercato) + 
  plot_annotation(
    title = "ANALISI ABC DI CONCENTRAZIONE - CONFRONTO DIVERSIFICAZIONE",
    subtitle = "Mio Portafoglio vs Benchmark di Mercato (VWCE) - Curva di Distribuzione del Peso",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, color = "#1F4E79"),
      plot.subtitle = element_text(face = "italic", size = 10, color = "#555555")
    )
  )


# ------------------------------------------------------------------------------
# 2. COPPIA TREEMAP SETTORI (INDUSTRY) - CON LEGENDA UNIFICATA E COMPATTA
# ------------------------------------------------------------------------------
crea_treemap_industry <- function(df_top100, titolo) {
  tot_weight <- scales::percent(sum(df_top100$Peso_Totale), accuracy = 0.1)
  
  ggplot(df_top100, aes(area = Peso_Totale, fill = Industry, label = Etichetta, subgroup = Industry)) +
    geom_treemap(color = "white") +
    geom_treemap_subgroup_border(color = "black") +
    geom_treemap_text(colour = "white", place = "centre", grow = FALSE, reflow = TRUE, size = 6.5) +
    scale_fill_manual(values = colori_settori_unificati, name = "Settore Industriale:") +
    labs(title = paste(titolo, "- Top 100 Titoli (Peso Complessivo:", tot_weight, ")")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, color = "#1F4E79")
    )
}

tree_ind_ptf <- crea_treemap_industry(ptf_top100, "Mio Portafoglio")
tree_ind_mkt <- crea_treemap_industry(mkt_top100, "Mercato (VWCE)")

# AFFIANCAMENTO + FONDIAMO LE LEGENDE IN UNA SOLA PICCOLA IN BASSO
coppia_2_industry <- (tree_ind_ptf + tree_ind_mkt) + 
  plot_layout(guides = "collect") + 
  plot_annotation(
    title = "RIPARTIZIONE PER SETTORE INDUSTRIALE (INDUSTRY) - TOP 100 TITOLI",
    subtitle = "Confronto dell'allocazione settoriale tra il Mio Portafoglio e il Benchmark di Mercato (VWCE)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, color = "#1F4E79"),
      plot.subtitle = element_text(face = "italic", size = 10, color = "#555555")
    )
  ) & 
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 7.5),             # TESTO LEGENDA RIDOTTO
    legend.key.size = unit(0.35, "cm"),                  # QUADRATINI LEGENDA PICCOLI
    legend.margin = margin(t = 2, b = 2)
  )


# ------------------------------------------------------------------------------
# 3. COPPIA TREEMAP PAESI (COUNTRY) - CON LEGENDA UNIFICATA E COMPATTA
# ------------------------------------------------------------------------------
crea_treemap_country <- function(df_top100, titolo) {
  tot_weight <- scales::percent(sum(df_top100$Peso_Totale), accuracy = 0.1)
  
  ggplot(df_top100, aes(area = Peso_Totale, fill = Country, label = Etichetta, subgroup = Country)) +
    geom_treemap(color = "white") +
    geom_treemap_subgroup_border(color = "black") +
    geom_treemap_text(colour = "white", place = "centre", grow = FALSE, reflow = TRUE, size = 6.5) +
    scale_fill_manual(values = colori_paesi_unificati, name = "Paese Geografico:") +
    labs(title = paste(titolo, "- Top 100 Titoli (Peso Complessivo:", tot_weight, ")")) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, color = "#1F4E79")
    )
}

tree_cnt_ptf <- crea_treemap_country(ptf_top100, "Mio Portafoglio")
tree_cnt_mkt <- crea_treemap_country(mkt_top100, "Mercato (VWCE)")

# AFFIANCAMENTO + FONDIAMO LE LEGENDE IN UNA SOLA PICCOLA IN BASSO
coppia_3_country <- (tree_cnt_ptf + tree_cnt_mkt) + 
  plot_layout(guides = "collect") + 
  plot_annotation(
    title = "ESPOSIZIONE GEOGRAFICA PER PAESE (COUNTRY) - TOP 100 TITOLI",
    subtitle = "Confronto della ripartizione geografica tra il Mio Portafoglio e il Benchmark di Mercato (VWCE)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, color = "#1F4E79"),
      plot.subtitle = element_text(face = "italic", size = 10, color = "#555555")
    )
  ) & 
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8.5),
    legend.text = element_text(size = 7.5),             # TESTO LEGENDA RIDOTTO
    legend.key.size = unit(0.35, "cm"),                  # QUADRATINI LEGENDA PICCOLI
    legend.margin = margin(t = 2, b = 2)
  )


# ##############################################################################
# F. ESPORTAZIONE CAROUSEL BENCHMARK IN PDF (FORMATO ORIZZONTALE 12x7)
# ##############################################################################

pdf_benchmark_path <- "D:/Users/F29332B/Downloads/ETF/carousel_benchmark_linkedin.pdf"

# Apriamo il PDF con proporzioni ampie (12x7 pollici) per ospitare comodamente i due grafici
pdf(file = pdf_benchmark_path, width = 12, height = 7)

# Slide 1: Concentrazione ABC
print(coppia_1_abc)

# Slide 2: Treemap Settori Industriali
print(coppia_2_industry)

# Slide 3: Treemap Paesi Geografici
print(coppia_3_country)

# Chiudiamo e salviamo il file PDF
dev.off()

print(paste("🔥 Carousel Benchmark aggiornato con successo! Trovi il PDF qui:", pdf_benchmark_path))