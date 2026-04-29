# =============================================================================
# Momentum Strategi — Trin 5: Overlappende Porteføljeafkast
# Omregner momentum-serier til overlappende porteføljeafkast
# via glidende ligevægtede gennemsnit.
#
# Input:  J3/6/9/12_Momentum (P5 - P1).xlsx
# Output: J3/6/9/12_Overlapping_Momentum.xlsx
# =============================================================================

library(dplyr)
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

# --- Hjælpefunktion: rullende gennemsnit med delvise vinduer ---
rolling_avg <- function(x, k) {
  n <- length(x)
  result <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    start <- max(1, i - k + 1)
    window <- x[start:i]
    window <- window[!is.na(window)]
    if (length(window) > 0) {
      result[i] <- mean(window)
    }
  }
  return(result)
}

# --- Løkke over alle J-værdier ---
j_values <- c(3, 6, 9, 12)

for (j in j_values) {
  
  # Indlæs
  input_file <- file.path(data_folder, paste0("J", j, "_Momentum (P5 - P1).xlsx"))
  df <- read_excel(input_file)
  cat(sprintf("Behandler J%d_Momentum (P5 - P1).xlsx (%d rækker)...\n", j, nrow(df)))
  
  # Parsér dato og sortér stigende
  df$Date <- parse_date(df$Date)
  df <- df %>% arrange(Date)
  
  # Beregn overlappende porteføljeafkast
  df <- df %>%
    mutate(
      Overlap_MOM_K3  = rolling_avg(MOM_K3,  3),
      Overlap_MOM_K6  = rolling_avg(MOM_K6,  6),
      Overlap_MOM_K9  = rolling_avg(MOM_K9,  9),
      Overlap_MOM_K12 = rolling_avg(MOM_K12, 12)
    )
  
  # Vælg outputkolonner
  df_out <- df %>%
    select(
      Date,
      MOM_K3,  Overlap_MOM_K3,
      MOM_K6,  Overlap_MOM_K6,
      MOM_K9,  Overlap_MOM_K9,
      MOM_K12, Overlap_MOM_K12
    )
  
  # Validering: udskriv første 15 rækker
  cat("\n  Første 15 rækker (Date, MOM_K3, Overlap_MOM_K3):\n")
  print(df_out %>% select(Date, MOM_K3, Overlap_MOM_K3) %>% head(15))
  cat("\n")
  
  # Gem
  output_file <- file.path(data_folder, paste0("J", j, "_Overlapping_Momentum.xlsx"))
  write_xlsx(df_out, output_file)
  cat(sprintf("  Rækker: %d\n", nrow(df_out)))
  cat(sprintf("  Gemt: %s\n\n", output_file))
}

cat("Færdig. Alle overlappende momentumfiler oprettet.\n")