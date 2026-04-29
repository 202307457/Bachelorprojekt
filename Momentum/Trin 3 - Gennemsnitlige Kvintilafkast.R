# =============================================================================
# Momentum Strategi — Trin 3: Gennemsnitlige Kvintilafkast (Bredt Format)
# Beregner ligevægtet gennemsnit af K3/K6/K9/K12 per Dato × Kvintil,
# og omformer til bredt format.
#
# Input:  J3/6/9/12_Forward_returns.xlsx
# Output: J3/6/9/12_Quintile_Averages.xlsx
# =============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# --- Definer mappe ---
data_folder <- "INDSÆT STI TIL MAPPE"

# --- Hjælpefunktion: fleksibel datoparsing ---
parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  x_num <- suppressWarnings(as.numeric(x))
  if (!all(is.na(x_num))) {
    return(as.Date(x_num, origin = "1899-12-30"))
  }
  result <- as.Date(x, format = "%Y-%m-%d")
  still_na <- is.na(result)
  if (any(still_na)) {
    result[still_na] <- as.Date(x[still_na], format = "%d.%m.%Y")
  }
  return(result)
}

# --- Hjælpefunktion: parsér K-kolonner (fjern % hvis tekst) ---
parse_pct <- function(x) {
  if (is.numeric(x)) return(x)
  as.numeric(gsub("%", "", x))
}

# --- Behandl hver J-fil ---
j_values <- c(3, 6, 9, 12)

for (j in j_values) {
  
  input_file <- file.path(data_folder, paste0("J", j, "_Forward_returns.xlsx"))
  cat(sprintf("Behandler J%d_Forward_returns.xlsx...\n", j))
  
  # Indlæs
  df <- read_excel(input_file)
  
  # Parsér dato
  df$Date <- parse_date(df$Date)
  
  # Parsér K-kolonner
  df$K3  <- parse_pct(df$K3)
  df$K6  <- parse_pct(df$K6)
  df$K9  <- parse_pct(df$K9)
  df$K12 <- parse_pct(df$K12)
  
  # Validering: tjek kvintiler per dato
  quintile_check <- df %>%
    group_by(Date) %>%
    summarise(n_quintiles = length(unique(Quintile)), .groups = "drop")
  
  cat(sprintf("  Datoer med 5 kvintiler: %d / %d\n",
              sum(quintile_check$n_quintiles == 5), nrow(quintile_check)))
  
  # Beregn ligevægtet gennemsnit per Dato × Kvintil
  df_avg <- df %>%
    group_by(Date, Quintile) %>%
    summarise(
      K3  = mean(K3,  na.rm = TRUE),
      K6  = mean(K6,  na.rm = TRUE),
      K9  = mean(K9,  na.rm = TRUE),
      K12 = mean(K12, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Opret kvintil-label: P1, P2, P3, P4, P5
  df_avg <- df_avg %>%
    mutate(Quintile = paste0("P", Quintile))
  
  # Omform til bredt format
  df_wide <- df_avg %>%
    pivot_wider(
      id_cols     = Date,
      names_from  = Quintile,
      values_from = c(K3, K6, K9, K12),
      names_glue  = "{Quintile}_{.value}"
    )
  
  # Sortér kolonner: Date, derefter P1_K3..P5_K3, P1_K6..P5_K6, osv.
  k_order <- c("K3", "K6", "K9", "K12")
  p_order <- paste0("P", 1:5)
  col_order <- c("Date")
  for (k in k_order) {
    for (p in p_order) {
      col_order <- c(col_order, paste0(p, "_", k))
    }
  }
  df_wide <- df_wide %>% select(all_of(col_order))
  
  # Sortér: dato stigende
  df_wide <- df_wide %>% arrange(Date)
  
  # Gem
  output_file <- file.path(data_folder, paste0("J", j, "_Quintile_Averages.xlsx"))
  write_xlsx(df_wide, output_file)
  
  # Validering
  cat(sprintf("  Rækker (datoer): %d\n", nrow(df_wide)))
  cat(sprintf("  Kolonner: %d\n", ncol(df_wide)))
  cat(sprintf("  Gemt: %s\n\n", output_file))
}

cat("Færdig. Alle kvintil-gennemsnitsfiler oprettet.\n")