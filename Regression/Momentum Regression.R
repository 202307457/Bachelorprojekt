# Momentum regression i EUR med Fama-French 3-faktor modellen
# Output: samlet Excel-fil med regressionsresultater, alfa-matricer og robusthedstest.

# Pakker ------------------------------------------------------------------
# install.packages(c("readxl", "dplyr", "tidyr", "stringr", "lubridate", "lmtest", "sandwich", "openxlsx", "purrr", "broom"))
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(lmtest)
library(sandwich)
library(openxlsx)
library(purrr)
library(broom)

# 1. Filsti og indlæsning --------------------------------------------------
wml_path <- "INDSÆT STI TIL MAPPE"
output_path <- file.path(dirname(wml_path), "momentum_regression_eur_resultater.xlsx")

raw <- read_excel(wml_path, sheet = 1)

# 2. Datofelt og numeriske kolonner ----------------------------------------
# Excel-datoer kan komme ind som tal. origin = "1899-12-30" passer til Excels datosystem.
data <- raw %>%
  mutate(Date = case_when(
    inherits(Date, "Date") ~ as.Date(Date),
    is.numeric(Date) ~ as.Date(Date, origin = "1899-12-30"),
    TRUE ~ as.Date(Date)
  )) %>%
  arrange(Date) %>%
  mutate(across(-Date, ~ suppressWarnings(as.numeric(.))))

wml_cols <- names(data) %>% str_subset("^J\\d+_K\\d+$")
stopifnot(length(wml_cols) > 0)

# 3. Omregn Fama-French faktorer fra USD til EUR ---------------------------
# Antagelse: Kolonnen 'USDEUR=R Middle Price' er EUR pr. 1 USD.
# Derfor er valutakursafkastet: FX_ret = FX_t / FX_{t-1} - 1.
# Fama-French faktorer er fra Kenneth French typisk angivet i procent pr. måned.
# WML-kolonnerne i arket er decimaltal, så faktorerne divideres med 100.
#
# Markedsfaktoren og RF omregnes via totalafkast:
# RF_EUR     = (1 + RF_USD) * (1 + FX_ret) - 1
# MKT_EUR    = (1 + RF_USD + Mkt_RF_USD) * (1 + FX_ret) - 1
# MKT_RF_EUR = MKT_EUR - RF_EUR
#
# SMB og HML er long-short faktorer. Ved fælles USD->EUR omregning bliver:
# SMB_EUR = SMB_USD * (1 + FX_ret), og tilsvarende for HML.
# Dette er en praktisk approksimation/implementering, når kun faktorafkast og FX er til rådighed.

data_eur <- data %>%
  mutate(
    FX_ret = `USDEUR=R Middle Price` / lag(`USDEUR=R Middle Price`) - 1,
    Mkt_RF_USD = `Mkt-RF` / 100,
    SMB_USD    = SMB / 100,
    HML_USD    = HML / 100,
    RF_USD     = RF / 100,
    RF_EUR     = (1 + RF_USD) * (1 + FX_ret) - 1,
    MKT_EUR    = (1 + RF_USD + Mkt_RF_USD) * (1 + FX_ret) - 1,
    MKT_RF_EUR = MKT_EUR - RF_EUR,
    SMB_EUR    = SMB_USD * (1 + FX_ret),
    HML_EUR    = HML_USD * (1 + FX_ret)
  )

# 4. Regressionsfunktion med Newey-West standardfejl -----------------------
# Regressionen svarer til Fama-French tidsserieregressionen:
# WML_t = alpha + beta_m * MKT_RF_EUR_t + beta_s * SMB_EUR_t + beta_h * HML_EUR_t + e_t
# Robusthedstesten bruger i stedet WML_t - RF_EUR_t på venstresiden.
# Newey-West lag sættes til K-1, fordi overlappende K-måneders porteføljer kan skabe autokorrelation.

run_ff_regression <- function(df, strategy, y_type = c("WML", "WML_minus_RF")) {
  y_type <- match.arg(y_type)
  J <- as.integer(str_match(strategy, "J(\\d+)_K(\\d+)")[, 2])
  K <- as.integer(str_match(strategy, "J(\\d+)_K(\\d+)")[, 3])
  nw_lag <- max(K - 1, 0)

  reg_data <- df %>%
    transmute(
      Date,
      y = if (y_type == "WML") .data[[strategy]] else .data[[strategy]] - RF_EUR,
      MKT_RF_EUR,
      SMB_EUR,
      HML_EUR
    ) %>%
    drop_na()

  if (nrow(reg_data) < 12) stop(paste("For få observationer for", strategy, y_type))

  model <- lm(y ~ MKT_RF_EUR + SMB_EUR + HML_EUR, data = reg_data)
  nw_vcov <- NeweyWest(model, lag = nw_lag, prewhite = FALSE, adjust = TRUE)
  ct <- coeftest(model, vcov. = nw_vcov)

  tibble(
    Specifikation = y_type,
    Strategi = strategy,
    J = J,
    K = K,
    N = nrow(reg_data),
    NW_lag = nw_lag,
    Start = min(reg_data$Date),
    Slut = max(reg_data$Date),
    Alpha_pct_pr_måned = unname(coef(model)["(Intercept)"]) * 100,
    Alpha_se_pct = ct["(Intercept)", "Std. Error"] * 100,
    Alpha_t = ct["(Intercept)", "t value"],
    Alpha_p = ct["(Intercept)", "Pr(>|t|)"],
    MKT_RF_beta = unname(coef(model)["MKT_RF_EUR"]),
    MKT_RF_t = ct["MKT_RF_EUR", "t value"],
    SMB_beta = unname(coef(model)["SMB_EUR"]),
    SMB_t = ct["SMB_EUR", "t value"],
    HML_beta = unname(coef(model)["HML_EUR"]),
    HML_t = ct["HML_EUR", "t value"],
    R2 = summary(model)$r.squared,
    Adj_R2 = summary(model)$adj.r.squared,
    Resid_sd_pct = sigma(model) * 100
  )
}

results_wml <- map_dfr(wml_cols, ~ run_ff_regression(data_eur, .x, "WML"))
results_excess <- map_dfr(wml_cols, ~ run_ff_regression(data_eur, .x, "WML_minus_RF"))
results_all <- bind_rows(results_wml, results_excess)

# 5. Deskriptiv statistik og alfa-tabeller ---------------------------------
desc_stats <- data_eur %>%
  summarise(across(all_of(wml_cols), list(
    N = ~ sum(!is.na(.)),
    Mean_pct_pr_måned = ~ mean(., na.rm = TRUE) * 100,
    SD_pct = ~ sd(., na.rm = TRUE) * 100,
    Min_pct = ~ min(., na.rm = TRUE) * 100,
    Max_pct = ~ max(., na.rm = TRUE) * 100
  ))) %>%
  pivot_longer(everything(), names_to = "Navn", values_to = "Værdi") %>%
  separate(Navn, into = c("Strategi", "Mål"), sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Mål, values_from = Værdi)

# Hjælpefunktion til at lave J x K matricer.
# Rækkerne viser formationsperioden J, og kolonnerne viser holdingperioden K.
make_result_matrix <- function(res, value_col) {
  res %>%
    select(J, K, {{ value_col }}) %>%
    mutate(K = paste0("K", K), J = paste0("J", J)) %>%
    pivot_wider(names_from = K, values_from = {{ value_col }}) %>%
    arrange(as.integer(str_remove(J, "J")))
}

# Matricer for hovedspecifikationen: WML på venstresiden.
alpha_matrix_wml   <- make_result_matrix(results_wml, Alpha_pct_pr_måned)
alpha_t_matrix_wml <- make_result_matrix(results_wml, Alpha_t)
alpha_p_matrix_wml <- make_result_matrix(results_wml, Alpha_p)

# Matricer for robusthedstesten: WML - RF_EUR på venstresiden.
alpha_matrix_excess   <- make_result_matrix(results_excess, Alpha_pct_pr_måned)
alpha_t_matrix_excess <- make_result_matrix(results_excess, Alpha_t)
alpha_p_matrix_excess <- make_result_matrix(results_excess, Alpha_p)

factor_loadings <- results_all %>%
  select(Specifikation, Strategi, J, K, MKT_RF_beta, MKT_RF_t, SMB_beta, SMB_t, HML_beta, HML_t)

# 6. Excel-output -----------------------------------------------------------
wb <- createWorkbook()

add_sheet <- function(wb, sheet_name, df, title, note = NULL) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, title, startRow = 1, startCol = 1)
  if (!is.null(note)) writeData(wb, sheet_name, note, startRow = 2, startCol = 1)
  writeDataTable(wb, sheet_name, df, startRow = ifelse(is.null(note), 3, 4), startCol = 1, tableStyle = "TableStyleMedium2")
  freezePane(wb, sheet_name, firstActiveRow = ifelse(is.null(note), 4, 5))
  setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = "auto")
}

readme <- tibble(
  Punkt = c("Afhængig variabel", "Faktorer", "Valutaomregning", "Newey-West", "Alfa-enhed", "Robusthedstest"),
  Forklaring = c(
    "Primær regression bruger WML i decimalform. Output rapporterer alfa i procent pr. måned.",
    "MKT_RF_EUR, SMB_EUR og HML_EUR anvendes som forklarende variable.",
    "Fama-French faktorer er antaget angivet i USD og procent. De omregnes til EUR med USDEUR=R Middle Price.",
    "Standardfejl beregnes med Newey-West og lag = K - 1.",
    "Alle alfaer i resultattabeller og alfa-matricer er i procent pr. måned.",
    "Separat regression med WML - RF_EUR på venstresiden er inkluderet."
  )
)

add_sheet(wb, "README", readme, "Momentum regression i EUR - metode og enheder")
add_sheet(wb, "Results_WML", results_wml, "Regression: WML på Fama-French faktorer i EUR", "Alfa rapporteres i procent pr. måned. Newey-West standardfejl med lag = K - 1.")
add_sheet(wb, "Results_WML_minus_RF", results_excess, "Robusthedstest: WML - RF_EUR på Fama-French faktorer i EUR", "Alfa rapporteres i procent pr. måned. Denne specifikation er medtaget til sammenligning.")
add_sheet(wb, "Alpha_Matrix_WML", alpha_matrix_wml, "Alfa-matrix for WML-regression", "Alle alfaer er i procent pr. måned.")
add_sheet(wb, "Alpha_T_Matrix_WML", alpha_t_matrix_wml, "Alfa t-statistik matrix for WML-regression", "T-statistikker for alfa. Newey-West standardfejl med lag = K - 1.")
add_sheet(wb, "Alpha_P_Matrix_WML", alpha_p_matrix_wml, "Alfa p-værdi matrix for WML-regression", "P-værdier for test af H0: alfa = 0.")

add_sheet(wb, "Alpha_Matrix_Excess", alpha_matrix_excess, "Alfa-matrix for robusthedstest WML - RF_EUR", "Alle alfaer er i procent pr. måned.")
add_sheet(wb, "Alpha_T_Matrix_Excess", alpha_t_matrix_excess, "Alfa t-statistik matrix for robusthedstest WML - RF_EUR", "T-statistikker for alfa. Newey-West standardfejl med lag = K - 1.")
add_sheet(wb, "Alpha_P_Matrix_Excess", alpha_p_matrix_excess, "Alfa p-værdi matrix for robusthedstest WML - RF_EUR", "P-værdier for test af H0: alfa = 0.")

add_sheet(wb, "Factor_Loadings", factor_loadings, "Faktorloadings og t-statistikker", "Betaer viser eksponering mod markeds-, size- og valuefaktoren.")
add_sheet(wb, "Descriptive_Stats", desc_stats, "Deskriptiv statistik for WML-strategier", "Afkastmål er rapporteret i procent pr. måned.")
add_sheet(wb, "Data_Used", data_eur, "Datagrundlag efter valutaomregning", "Faktorer i EUR er beregnet i dette ark.")

saveWorkbook(wb, output_path, overwrite = TRUE)
cat("Excel-fil genereret her:", output_path, "\n")
