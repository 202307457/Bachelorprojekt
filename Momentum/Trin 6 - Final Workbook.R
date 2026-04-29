# =============================================================================
# Momentum Strategi — Endelig Arbejdsbog
# Samler alle resultater i én Excel-fil med 5 ark
#
# Output: Final_Momentum_Analysis.xlsx
# =============================================================================

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(sandwich)

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

# --- Hjælpefunktion: indlæs og parsér datoer ---
load_file <- function(filename) {
  path <- file.path(data_folder, filename)
  cat(sprintf("Indlæser: %s\n", filename))
  df <- read_excel(path)
  df$Date <- parse_date(df$Date)
  cat(sprintf("  Dimensioner: %d rækker x %d kolonner\n", nrow(df), ncol(df)))
  return(df)
}

# =============================================================================
# ARK 1: Final_Strategies
# =============================================================================
cat("\n=== ARK 1: Final_Strategies ===\n")

j_values <- c(3, 6, 9, 12)
overlap_list <- list()

for (j in j_values) {
  df <- load_file(paste0("J", j, "_Overlapping_Momentum.xlsx"))
  
  df <- df %>%
    select(Date,
           Overlap_MOM_K3, Overlap_MOM_K6,
           Overlap_MOM_K9, Overlap_MOM_K12)
  
  colnames(df) <- c("Date",
                    paste0("J", j, "_K3"),
                    paste0("J", j, "_K6"),
                    paste0("J", j, "_K9"),
                    paste0("J", j, "_K12"))
  
  overlap_list[[as.character(j)]] <- df
}

sheet1 <- overlap_list[["3"]]
for (j in c("6", "9", "12")) {
  sheet1 <- full_join(sheet1, overlap_list[[j]], by = "Date")
}

col_order <- c("Date")
for (j in j_values) {
  for (k in c("K3", "K6", "K9", "K12")) {
    col_order <- c(col_order, paste0("J", j, "_", k))
  }
}
sheet1 <- sheet1 %>%
  select(all_of(col_order)) %>%
  arrange(Date)

cat(sprintf("Final_Strategies: %d rækker x %d kolonner\n", nrow(sheet1), ncol(sheet1)))
cat(sprintf("Antal strategier: %d\n", ncol(sheet1) - 1))

# =============================================================================
# ARK 3: Winners_Losers  (indlæses før ARK 2, da Summary_Stats bruger sheet3)
# =============================================================================
cat("\n=== ARK 3: Winners_Losers ===\n")

mom_list <- list()

for (j in j_values) {
  df <- load_file(paste0("J", j, "_Momentum (P5 - P1).xlsx"))
  
  new_names <- colnames(df)
  new_names[new_names != "Date"] <- paste0("J", j, "_", new_names[new_names != "Date"])
  colnames(df) <- new_names
  
  mom_list[[as.character(j)]] <- df
}

sheet3 <- mom_list[["3"]]
for (j in c("6", "9", "12")) {
  sheet3 <- full_join(sheet3, mom_list[[j]], by = "Date")
}

sheet3 <- sheet3 %>%
  arrange(Date)

cat(sprintf("Winners_Losers: %d rækker x %d kolonner\n", nrow(sheet3), ncol(sheet3)))

# =============================================================================
# ARK 2: Summary_Stats
# =============================================================================
cat("\n=== ARK 2: Summary_Stats ===\n")

strategy_cols <- col_order[-1]

# --- Byg opslag til P5- og P1-mean fra Winners_Losers (sheet3) ---
# Kolonnenavne i sheet3 følger mønsteret J{j}_{metric}_K{k},
# f.eks. J3_P5_K3 og J3_P1_K3
p5_mean_lookup <- list()
p1_mean_lookup <- list()

for (col in colnames(sheet3)) {
  if (col == "Date") next
  # Matcher f.eks. J3_P5_K3 eller J12_P1_K9
  if (grepl("^J\\d+_P5_K\\d+$", col)) {
    # Strategi-nøgle: J{j}_K{k}
    key <- sub("_P5_", "_", col)   # J3_P5_K3 -> J3_K3
    vals <- sheet3[[col]]
    vals <- vals[!is.na(vals)]
    p5_mean_lookup[[key]] <- mean(vals)
  }
  if (grepl("^J\\d+_P1_K\\d+$", col)) {
    key <- sub("_P1_", "_", col)   # J3_P1_K3 -> J3_K3
    vals <- sheet3[[col]]
    vals <- vals[!is.na(vals)]
    p1_mean_lookup[[key]] <- mean(vals)
  }
}

stats_list <- list()
for (s in strategy_cols) {
  x <- sheet1[[s]]
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  
  # Newey-West standardfejl (automatisk lagvalg via Andrews-metoden)
  df_reg <- data.frame(y = x, const = 1)
  fit <- lm(y ~ 1, data = df_reg)
  nw_vcov <- NeweyWest(fit, prewhite = FALSE, adjust = TRUE)
  nw_se <- sqrt(as.numeric(nw_vcov))
  
  t_stat <- ifelse(nw_se > 0 & n > 1, m / nw_se, NA)
  
  # Standardafvigelse (σ) for W-L
  sd_wl <- sd(x)
  
  # Sharpe ratio uden risk-free rate
  sharpe <- ifelse(sd_wl > 0, m / sd_wl, NA)
  
  # P5- og P1-mean fra Winners_Losers
  p5_m <- if (!is.null(p5_mean_lookup[[s]])) p5_mean_lookup[[s]] else NA_real_
  p1_m <- if (!is.null(p1_mean_lookup[[s]])) p1_mean_lookup[[s]] else NA_real_
  
  stats_list[[s]] <- tibble(
    Strategy            = s,
    Mean_Monthly_Return = round(m, 7),
    Newey_West_SE       = round(nw_se, 7),
    T_Stat              = round(t_stat, 7),
    N_Obs               = n,
    SD_WL               = round(sd_wl, 7),
    Mean_P5             = round(p5_m, 7),
    Mean_P1             = round(p1_m, 7),
    Sharpe_Ratio        = round(sharpe, 7)
  )
}

sheet2 <- bind_rows(stats_list)
cat(sprintf("Summary_Stats: %d rækker\n", nrow(sheet2)))

# =============================================================================
# ARK 4: Quintile_Averages
# =============================================================================
cat("\n=== ARK 4: Quintile_Averages ===\n")

quintile_list <- list()

for (j in j_values) {
  df <- load_file(paste0("J", j, "_Quintile_Averages.xlsx"))
  
  df_long <- df %>%
    pivot_longer(
      cols      = -Date,
      names_to  = "col_name",
      values_to = "value"
    ) %>%
    mutate(
      Quintile = as.integer(gsub("P(\\d+)_K\\d+", "\\1", col_name)),
      K        = gsub("P\\d+_(K\\d+)", "\\1", col_name)
    ) %>%
    select(-col_name) %>%
    pivot_wider(
      id_cols     = c(Date, Quintile),
      names_from  = K,
      values_from = value,
      names_prefix = "Avg_"
    ) %>%
    mutate(J = j) %>%
    select(J, Date, Quintile, Avg_K3, Avg_K6, Avg_K9, Avg_K12)
  
  quintile_list[[as.character(j)]] <- df_long
}

sheet4 <- bind_rows(quintile_list) %>%
  arrange(J, Date, Quintile)

cat(sprintf("Quintile_Averages: %d rækker x %d kolonner\n", nrow(sheet4), ncol(sheet4)))

# =============================================================================
# ARK 5: ReadMe
# =============================================================================
cat("\n=== ARK 5: ReadMe ===\n")

all_dates <- sheet1$Date

sheet5 <- tibble(
  Section = c(
    "Ark: Final_Strategies",
    "Ark: Summary_Stats",
    "Ark: Winners_Losers",
    "Ark: Quintile_Averages",
    "",
    "--- Definitioner ---",
    "J",
    "K",
    "Kvintil 1",
    "Kvintil 5",
    "K-afkast",
    "Endelige strategier",
    "Vægtning",
    "",
    "--- Stikprøveinfo ---",
    "Stikprøvestart",
    "Stikprøveslut",
    "Arbejdsbog oprettet"
  ),
  Description = c(
    "Endelige overlappende månedlige momentumafkast for alle 16 J/K-strategier",
    "Opsummerende statistik (gennemsnit, std.afv., t-stat, N, kumulativt afkast) for de endelige overlappende strategiafkast",
    "Rå vinder- (P5), taber- (P1) og momentum- (P5-P1) serier før overlappende justering",
    "Gennemsnitlige kumulative holdingsafkast for kvintiler 1-5, brugt til validering",
    "",
    "",
    "Formationsperiode i måneder (3, 6, 9 eller 12)",
    "Holdingperiode i måneder (3, 6, 9 eller 12)",
    "Tabere (laveste historiske afkast)",
    "Vindere (højeste historiske afkast)",
    "Kumulative fremadrettede afkast inkl. rankingsmåneden",
    "Anvender overlappende porteføljer/vintager (rullende gennemsnit af K aktive porteføljer)",
    "Ligevægtede porteføljer",
    "",
    "",
    format(min(all_dates, na.rm = TRUE), "%Y-%m-%d"),
    format(max(all_dates, na.rm = TRUE), "%Y-%m-%d"),
    format(Sys.Date(), "%Y-%m-%d")
  )
)

cat(sprintf("ReadMe: %d rækker\n", nrow(sheet5)))

# =============================================================================
# GEM ARBEJDSBOG
# =============================================================================
cat("\n=== Gemmer arbejdsbog ===\n")

output_file <- file.path(data_folder, "Final_Momentum_Analysis.xlsx")

write_xlsx(
  list(
    Final_Strategies  = sheet1,
    Summary_Stats     = sheet2,
    Winners_Losers    = sheet3,
    Quintile_Averages = sheet4,
    ReadMe            = sheet5
  ),
  output_file
)

cat(sprintf("Gemt: %s\n", output_file))
cat("\nFærdig.\n")