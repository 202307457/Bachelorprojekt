# =============================================================================
# Momentum Strategi — Trin 2: Fremadrettede holdingsafkast
# Tilføjer K3, K6, K9, K12 til hver eksisterende kvintil-fil
#
# K inkluderer rankingsmåneden:
#   K3  = (1+r_t) * (1+r_{t+1}) * (1+r_{t+2}) - 1
#   K6  = (1+r_t) * (1+r_{t+1}) * ... * (1+r_{t+5}) - 1
#   K9  = (1+r_t) * (1+r_{t+1}) * ... * (1+r_{t+8}) - 1
#   K12 = (1+r_t) * (1+r_{t+1}) * ... * (1+r_{t+11}) - 1
#
# Alle input- og outputfiler i samme mappe
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

# =============================================================================
# 1. Indlæs og omform Return.xlsx
# =============================================================================
cat("Indlæser Return.xlsx...\n")
ret_raw <- read_excel(file.path(data_folder, "Return.xlsx"))

# Fjern underoverskriftsrække
if (is.character(ret_raw[[2]][1]) && grepl("CLOSE", ret_raw[[2]][1], ignore.case = TRUE)) {
  ret_raw <- ret_raw[-1, ]
}

# Omdøb første kolonne til Date
colnames(ret_raw)[1] <- "Date"
ret_raw$Date <- parse_date(ret_raw$Date)

stock_cols <- colnames(ret_raw)[-1]
cat(sprintf("Aktiekolonner fundet: %d\n", length(stock_cols)))
cat(sprintf("Eksempel på aktienavne: %s\n", paste(head(stock_cols, 5), collapse = ", ")))

# Omform bred -> lang
ret_long <- pivot_longer(ret_raw,
                         cols      = -Date,
                         names_to  = "Stock",
                         values_to = "Monthly_Return")

# Konverter til numerisk og fjern NA'er
ret_long$Monthly_Return <- as.numeric(ret_long$Monthly_Return)
ret_long <- ret_long %>%
  filter(!is.na(Monthly_Return)) %>%
  arrange(Stock, Date)

cat(sprintf("Return.xlsx indlæst: %d observationer, %d aktier, %d datoer\n",
            nrow(ret_long), length(unique(ret_long$Stock)), length(unique(ret_long$Date))))
cat("\nEksempel fra Return.xlsx (lang):\n")
print(head(ret_long, 5))
cat("\n")

# =============================================================================
# 2. Beregn fremadrettede kumulative afkast per aktie-dato
# =============================================================================
cat("Beregner fremadrettede kumulative afkast (K3, K6, K9, K12)...\n")

forward_k <- function(monthly_ret, k) {
  n <- length(monthly_ret)
  result <- rep(NA_real_, n)
  if (n < k) return(result)
  for (t in seq_len(n - k + 1)) {
    window <- monthly_ret[t:(t + k - 1)]
    if (any(is.na(window))) {
      result[t] <- NA_real_
    } else {
      result[t] <- prod(1 + window) - 1
    }
  }
  return(result)
}

forward_returns <- ret_long %>%
  arrange(Stock, Date) %>%
  group_by(Stock) %>%
  mutate(
    K3  = forward_k(Monthly_Return, 3),
    K6  = forward_k(Monthly_Return, 6),
    K9  = forward_k(Monthly_Return, 9),
    K12 = forward_k(Monthly_Return, 12)
  ) %>%
  ungroup() %>%
  select(Date, Stock, K3, K6, K9, K12)

cat(sprintf("Fremadrettede afkast beregnet: %d rækker\n", nrow(forward_returns)))
cat(sprintf("K3 ikke-NA: %d\n\n", sum(!is.na(forward_returns$K3))))

# =============================================================================
# 3. Sammenflet med hver kvintil-fil og gem
# =============================================================================

j_values <- c(3, 6, 9, 12)

for (j in j_values) {
  
  quintile_file <- file.path(data_folder, paste0("Quintiles_J", j, ".xlsx"))
  cat(sprintf("Behandler Quintiles_J%d.xlsx...\n", j))
  
  # Indlæs kvintil-fil
  df_quintiles <- read_excel(quintile_file)
  
  # Parsér dato
  df_quintiles$Date <- parse_date(df_quintiles$Date)
  
  cat(sprintf("  Datointerval: %s til %s\n",
              min(df_quintiles$Date, na.rm = TRUE), max(df_quintiles$Date, na.rm = TRUE)))
  cat(sprintf("  Eksempel på aktier: %s\n",
              paste(head(unique(df_quintiles$Stock), 5), collapse = ", ")))
  
  # Tjek overlap
  common_stocks <- intersect(unique(df_quintiles$Stock), unique(forward_returns$Stock))
  common_dates  <- intersect(unique(df_quintiles$Date), unique(forward_returns$Date))
  cat(sprintf("  Matchende aktier: %d, matchende datoer: %d\n", length(common_stocks), length(common_dates)))
  
  # Sammenflet K-kolonner på dato og aktie
  df_merged <- left_join(df_quintiles, forward_returns, by = c("Date", "Stock"))
  
  # Tæl ikke-NA før sortering
  k3_n  <- sum(!is.na(df_merged$K3))
  k6_n  <- sum(!is.na(df_merged$K6))
  k9_n  <- sum(!is.na(df_merged$K9))
  k12_n <- sum(!is.na(df_merged$K12))
  
  # Sortér: dato stigende, derefter kvintil, derefter afkast
  df_merged <- df_merged %>%
    arrange(Date, Quintile, Return)
  
  # Gem til samme mappe
  output_file <- file.path(data_folder, paste0("J", j, "_Forward_returns.xlsx"))
  write_xlsx(df_merged, output_file)
  
  cat(sprintf("  Rækker: %d\n", nrow(df_merged)))
  cat(sprintf("  K3  ikke-NA: %d / %d\n", k3_n,  nrow(df_merged)))
  cat(sprintf("  K6  ikke-NA: %d / %d\n", k6_n,  nrow(df_merged)))
  cat(sprintf("  K9  ikke-NA: %d / %d\n", k9_n,  nrow(df_merged)))
  cat(sprintf("  K12 ikke-NA: %d / %d\n", k12_n, nrow(df_merged)))
  cat(sprintf("  Gemt: %s\n\n", output_file))
}

cat("Færdig. Alle fremadrettede afkastfiler oprettet.\n")