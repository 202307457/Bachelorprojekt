# =============================================================================
# Momentum Kvintil Rangering — Trin 1
# Opretter kvintilporteføljer for alle momentum-filer automatisk
#
# Læser alle "Momentum J=*.xlsx" filer fra Momentum-mappen
# Gemmer Quintiles_J*.xlsx filer til Momentum-mappen
# =============================================================================
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# --- Definer mappe ---
data_folder <- "INDSÆT STI TIL MAPPE"

# --- Find alle inputfiler ---
input_files <- list.files(data_folder,
                          pattern    = "Momentum J=.*\\.xlsx$",
                          full.names = TRUE)

cat(sprintf("Fandt %d fil(er) i mappen:\n", length(input_files)))
for (f in input_files) cat(sprintf("  %s\n", basename(f)))
cat("\n")

# --- Løkke over hver fil ---
for (input_file in input_files) {
  
  # Udtræk J-værdi fra filnavn (fx "Momentum J=12.xlsx" -> "12")
  j_value <- sub(".*J=(\\d+).*", "\\1", basename(input_file))
  cat(sprintf("Behandler J=%s: %s\n", j_value, basename(input_file)))
  
  # --- Læs Excel ---
  raw <- read_excel(input_file)
  
  # --- Fjern underoverskriftsrække ---
  raw <- raw[-1, ]
  
  # --- Omdøb første kolonne til Date ---
  colnames(raw)[1] <- "Date"
  raw$Date <- as.Date(as.numeric(raw$Date), origin = "1899-12-30")
  
  # --- Omform: bred -> lang ---
  df_long <- pivot_longer(raw,
                          cols      = -Date,
                          names_to  = "Stock",
                          values_to = "Return")
  
  # --- Konverter Return til numerisk ---
  df_long$Return <- as.numeric(df_long$Return)
  
  # --- Fjern NA'er ---
  df_valid <- filter(df_long, !is.na(Return))
  
  # --- Tildel kvintiler per dato ---
  # Kvintil 1 = laveste afkast (tabere), Kvintil 5 = højeste afkast (vindere)
  df_quintiles <- df_valid %>%
    group_by(Date) %>%
    mutate(Quintile = ntile(Return, 5)) %>%
    ungroup()
  
  # --- Sortér: dato stigende, derefter kvintil, derefter afkast ---
  df_quintiles <- df_quintiles %>%
    arrange(Date, Quintile, Return)
  
  # --- Vælg kolonner ---
  df_quintiles <- df_quintiles %>%
    select(Date, Stock, Return, Quintile)
  
  # --- Opsummering ---
  cat(sprintf("  Observationer: %d\n", nrow(df_quintiles)))
  cat(sprintf("  Datoer: %d\n", length(unique(df_quintiles$Date))))
  cat(sprintf("  Unikke aktier: %d\n", length(unique(df_quintiles$Stock))))
  
  # --- Gem til samme mappe ---
  output_file <- file.path(data_folder, paste0("Quintiles_J", j_value, ".xlsx"))
  write_xlsx(df_quintiles, output_file)
  cat(sprintf("  Gemt: %s\n\n", output_file))
}

cat("Færdig. Alle filer behandlet.\n")