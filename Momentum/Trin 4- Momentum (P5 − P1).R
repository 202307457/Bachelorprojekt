# =============================================================================
# Momentum Strategi — Trin 4: Momentum (P5 − P1)
# Beregner vinder-minus-taber spread for hver holdingperiode
#
# Input:  J3/6/9/12_Quintile_Averages.xlsx
# Output: J3/6/9/12_Momentum (P5 - P1).xlsx
# =============================================================================

library(dplyr)
library(readxl)
library(writexl)

# --- Definer mappe ---
data_folder <- "INDSÆT STI TIL MAPPE"

# --- Løkke over alle J-værdier ---
j_values <- c(3, 6, 9, 12)

for (j in j_values) {
  
  # Indlæs
  input_file <- file.path(data_folder, paste0("J", j, "_Quintile_Averages.xlsx"))
  df <- read_excel(input_file)
  cat(sprintf("Behandler J%d_Quintile_Averages.xlsx (%d rækker)...\n", j, nrow(df)))
  
  # Beregn momentum (P5 - P1)
  df <- df %>%
    mutate(
      MOM_K3  = P5_K3  - P1_K3,
      MOM_K6  = P5_K6  - P1_K6,
      MOM_K9  = P5_K9  - P1_K9,
      MOM_K12 = P5_K12 - P1_K12
    )
  
  # Vælg outputkolonner
  df_out <- df %>%
    select(
      Date,
      P1_K3,  P5_K3,  MOM_K3,
      P1_K6,  P5_K6,  MOM_K6,
      P1_K9,  P5_K9,  MOM_K9,
      P1_K12, P5_K12, MOM_K12
    )
  
  # Gem
  output_file <- file.path(data_folder, paste0("J", j, "_Momentum (P5 - P1).xlsx"))
  write_xlsx(df_out, output_file)
  cat(sprintf("  Rækker: %d\n", nrow(df_out)))
  cat(sprintf("  Gemt: %s\n\n", output_file))
}

cat("Færdig. Alle momentumfiler oprettet.\n")