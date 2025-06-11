library(tidyverse)
library(scales)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lmtest)
library(nlme)
library(forecast)

# Load datasets
df1 <- read_delim("71488ned_UntypedDataSet_05062025_175816.csv", delim = ";", show_col_types = FALSE)
df2 <- read_delim("83625NED_UntypedDataSet_05062025_101716.csv", delim = ";", show_col_types = FALSE)
df3 <- read_delim("86004NED_UntypedDataSet_07062025_175918.csv", delim = ";", show_col_types = FALSE)

# Pre-merge cleaning: align keys only
df1 <- df1 %>%
  mutate(RegioS = str_trim(tolower(RegioS)), Perioden = str_trim(tolower(Perioden)))

df2 <- df2 %>%
  mutate(RegioS = str_trim(tolower(RegioS)), Perioden = str_trim(tolower(Perioden)))

df3 <- df3 %>%
  mutate(RegioS = str_trim(tolower(RegioS)), Perioden = str_trim(tolower(Perioden)))

# Merge all datasets
merged_df <- list(df1, df2, df3) %>%
  reduce(full_join, by = c("RegioS", "Perioden"))

# Drop not used columns
merged_df <- merged_df %>%
  select(-matches("^ID(\\.x|\\.y)?$"), -starts_with("GestandaardiseerdInkomen"), -starts_with("Partner"), -starts_with("Mediaan"), -GemiddeldBesteedbaarInkomen_5)

# Then define numeric_like_cols based on what's left
numeric_like_cols <- merged_df %>%
  select(where(is.character)) %>%
  summarise(across(everything(), ~mean(str_detect(., "^\\s*[0-9,.]+\\s*$"), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct_numeric") %>%
  filter(pct_numeric > 0.8) %>%
  pull(col)

# Clean character columns by trimming and parse those that are mostly numeric
merged_df <- merged_df %>%
  mutate(across(where(is.character), ~str_trim(.))) %>%
  mutate(across(all_of(numeric_like_cols), ~ parse_number(.) %>% suppressWarnings()))

# Rename RegioS codes to province names
province_map <- c(
  "pv20" = "Groningen",
  "pv21" = "Fryslân",
  "pv22" = "Drenthe",
  "pv23" = "Overijssel",
  "pv24" = "Flevoland",
  "pv25" = "Gelderland",
  "pv26" = "Utrecht",
  "pv27" = "Noord-Holland",
  "pv28" = "Zuid-Holland",
  "pv29" = "Zeeland",
  "pv30" = "Noord-Brabant",
  "pv31" = "Limburg",
  "nl01" = "Nederland"
)

# Rename Leeftijd codes to leeftijdgroepen
age_map <- c(
  "70100" = "0-4",
  "70200" = "5-9",
  "70300" = "10-14",
  "70400" = "15-19",
  "70500" = "20-24",
  "70600" = "25-29",
  "70700" = "30-34",
  "70800" = "35-39",
  "70900" = "40-44",
  "71000" = "45-49",
  "71100" = "50-54",
  "71200" = "55-59",
  "71300" = "60-64",
  "71400" = "65-69",
  "71500" = "70-74",
  "71600" = "75-79",
  "71700" = "80-84",
  "71800" = "85-89",
  "71900" = "90-94",
  "22000" = "95+"
)

merged_df <- merged_df %>%
  mutate(RegioS = recode(RegioS, !!!province_map))

# Multiply income and household columns by 1,000
merged_df <- merged_df %>%
  mutate(
    ParticuliereHuishoudens_1 = ParticuliereHuishoudens_1 * 1000,
    GemiddeldGestandaardiseerdInkomen_3 = GemiddeldGestandaardiseerdInkomen_3 * 1000
  )

cat("\n---- MERGED_DF (Population, Housing Prices and Income) ----\n")
glimpse(merged_df)
print(head(merged_df, 10))

# =========================
# Plot 1: National level (NL01)
# =========================
# === Prepare Data ===
df_nl <- merged_df %>%
  select(Perioden, RegioS, GemiddeldeVerkoopprijs_1) %>%
  filter(RegioS == "Nederland", str_detect(Perioden, "^\\d{4}jj00$")) %>%
  arrange(Perioden) %>%
  mutate(
    Jaar = as.integer(str_sub(Perioden, 1, 4)),
    PrijsK = GemiddeldeVerkoopprijs_1 / 1000,
    Group = "NL",
    TimeIndex = row_number()  # Strictly increasing index for AR(1)
  ) %>%
  drop_na(PrijsK)

# === Breusch-Godfrey test for serial correlation ===
ols_model <- lm(PrijsK ~ Jaar, data = df_nl)
print(bgtest(ols_model, order = 2))  # Should show strong autocorrelation

# === Fit GLS model with AR(1) ===
gls_model <- gls(
  PrijsK ~ Jaar,
  data = df_nl,
  correlation = corAR1(form = ~ TimeIndex | Group),
  method = "REML",  #ML kan ook maar langzamer morgen kijken of newey west standard errors beter werkt aangezien dit lang duurt om te berekenen en lelijk.
  control = glsControl(
    tolerance = 10,     # Convergence tolerance
    msVerbose = TRUE      # Show progress (optional)
  )
)

# === Forecast future years ===
n_future <- 2034 - max(df_nl$Jaar)
future_years <- data.frame(
  Jaar = (max(df_nl$Jaar) + 1):2034,
  TimeIndex = max(df_nl$TimeIndex) + seq_len(n_future),
  Group = "NL"
)

# Predict future values
gls_pred <- predict(gls_model, newdata = future_years)

forecast_df <- future_years %>%
  mutate(PredictedPrijsK = gls_pred)

# === Plot actual + forecast ===
p1 <- ggplot() +
  geom_line(data = df_nl, aes(x = Jaar, y = PrijsK), color = "blue", linewidth = 1.2) +
  geom_line(data = forecast_df, aes(x = Jaar, y = PredictedPrijsK), color = "red", linetype = "dashed", linewidth = 1.1) +
  labs(
    title = "GLS Forecast of Dutch House Prices (AR(1) Corrected)",
    subtitle = "Blue: Actual | Red dashed: GLS Forecast",
    x = "Year", y = "House Price (€ x 1,000)"
  ) +
  scale_x_continuous(breaks = seq(min(df_nl$Jaar), 2034, 2), limits = c(min(df_nl$Jaar), 2034)) +
  theme_minimal()

print(p1)
# =========================
# Plot 2: Provinces PV20–PV31
# =========================
  p2 <- merged_df %>%
  filter(RegioS %in% province_map[1:12], str_detect(Perioden, "^\\d{4}jj00$")) %>%
  mutate(Jaar = as.integer(str_sub(Perioden, 1, 4)), RegioS = toupper(RegioS)) %>%
  drop_na(GemiddeldeVerkoopprijs_1) %>%
    ggplot(aes(x = Jaar, y = GemiddeldeVerkoopprijs_1 / 1000)) +
    geom_line(color = "blue", linewidth = 1.2, na.rm = TRUE) +
    scale_y_continuous(name = "House Price (€ x 1,000)") +
    scale_x_continuous(breaks = seq(1995, 2024, 4)) +
    labs(
      title = "Average House Prices by Province (PV20–PV31)",
      x = "Year"
    ) +
    facet_wrap(~ RegioS) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
print(p2)

  # =========================
  # Plot 3: Bar Chart – Housing Prices in 2024 by Province
  # =========================
  p3 <- merged_df %>%
    filter(RegioS %in% province_map[1:12], Perioden == "2024jj00") %>%
    drop_na(GemiddeldeVerkoopprijs_1) %>%
    distinct(RegioS, .keep_all = TRUE) %>%
    arrange(GemiddeldeVerkoopprijs_1) %>%
    mutate(RegioS = factor(RegioS, levels = RegioS)) %>%
    ggplot(aes(x = RegioS, y = GemiddeldeVerkoopprijs_1 / 1000)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    scale_y_continuous(name = "House Price in 2024 (€ x 1,000)", breaks = seq(0, 600, 100)) +
    labs(
      title = "Average House Price per Province in 2024 (Ascending Order)",
      x = "Province"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p3)
  
  # =========================
  # Plot 4: Income National level (NL01)
  # =========================
  p4 <- merged_df %>%
    filter(RegioS == "Nederland", str_detect(Perioden, "^\\d{4}jj00$"), KenmerkenVanHuishoudens == 1050010) %>%
    mutate(Jaar = as.integer(str_sub(Perioden, 1, 4))) %>%
    drop_na(GemiddeldGestandaardiseerdInkomen_3) %>%
    ggplot(aes(x = Jaar, y = GemiddeldGestandaardiseerdInkomen_3 / 1000)) +
    geom_line(color = "blue", linewidth = 1.2, na.rm = TRUE) +
    scale_y_continuous(name = "Income Level (€ x 1,000)") +
    scale_x_continuous(breaks = seq(2011, 2023, 2)) +
    labs(
      title = "Average Income Level – Netherlands Total (NL01)",
      x = "Year"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p4)
  
  # =========================
  # Plot 5: Income Provinces PV20–PV31
  # =========================
  p5 <- merged_df %>%
    filter(RegioS %in% province_map[1:12], str_detect(Perioden, "^\\d{4}jj00$"), KenmerkenVanHuishoudens == 1050010) %>%
    mutate(Jaar = as.integer(str_sub(Perioden, 1, 4)), RegioS = toupper(RegioS)) %>%
    drop_na(GemiddeldGestandaardiseerdInkomen_3) %>%
    ggplot(aes(x = Jaar, y = GemiddeldGestandaardiseerdInkomen_3 / 1000)) +
    geom_line(color = "blue", linewidth = 1.2, na.rm = TRUE) +
    scale_y_continuous(name = "Income Level (€ x 1,000)") +
    scale_x_continuous(breaks = seq(2011, 2023, 2)) +
    labs(
      title = "Average Income Level by Province (PV20–PV31)",
      x = "Year"
    ) +
    facet_wrap(~ RegioS) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p5)
 
  # =========================
  # Plot 6: Bar Chart – Housing Prices in 2024 by Province
  # =========================
  p6 <- merged_df %>%
    filter(RegioS %in% province_map[1:12], Perioden == "2023jj00", KenmerkenVanHuishoudens == 1050010) %>%
    drop_na(GemiddeldGestandaardiseerdInkomen_3) %>%
    distinct(RegioS, .keep_all = TRUE) %>%
    arrange(GemiddeldGestandaardiseerdInkomen_3) %>%
    mutate(RegioS = factor(RegioS, levels = RegioS)) %>%
    ggplot(aes(x = RegioS, y = GemiddeldGestandaardiseerdInkomen_3 / 1000)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    scale_y_continuous(name = "Income Level in 2023 (€ x 1,000)", breaks = seq(0, 50, 10)) +
    labs(
      title = "Average Income per Province in 2023 (Ascending Order)",
      x = "Province"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p6)
   
  # =========================
  # Plot 7: Indexed Change Since 2011 (2011 = 100)
  # =========================
  index_df <- merged_df %>%
    filter(RegioS == "Nederland", KenmerkenVanHuishoudens == 1050010, str_detect(Perioden, "^\\d{4}jj00$")) %>%
    mutate(Jaar = as.integer(str_sub(Perioden, 1, 4))) %>%
    arrange(Jaar) %>%
    filter(Jaar >= 2011) %>%
    mutate(
      HousePriceIndex = 100 * GemiddeldeVerkoopprijs_1 / GemiddeldeVerkoopprijs_1[Jaar == 2011],
      IncomeLevelIndex = 100 * GemiddeldGestandaardiseerdInkomen_3 / GemiddeldGestandaardiseerdInkomen_3[Jaar == 2011],
      RatioHousingPriceIncomeLevel = HousePriceIndex / IncomeLevelIndex
    )
  
  p7 <- ggplot(index_df, aes(x = Jaar)) +
    geom_line(aes(y = HousePriceIndex, color = "House Price Index"), linewidth = 1.2) +
    geom_line(aes(y = IncomeLevelIndex, color = "Income Level Index"), linewidth = 1.2) +
    scale_color_manual(values = c("House Price Index" = "darkred", "Income Level Index" = "darkgreen")) +
    labs(
      title = "Indexed Change Since 2011: House Prices vs. Income (NL01)",
      x = "Year",
      y = "Index (2011 = 100)",
      color = "Legend"
    ) +
    scale_x_continuous(breaks = seq(2011, max(index_df$Jaar), 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p7)

  # =========================
  # Plot 8: Ratio of House Price Change to Income Change
  # =========================
  p8 <- ggplot(index_df, aes(x = Jaar, y = RatioHousingPriceIncomeLevel)) +
    geom_line(color = "purple", linewidth = 1.2) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
    scale_y_continuous(name = "Price/Income Change Ratio") +
    scale_x_continuous(breaks = seq(2011, max(index_df$Jaar), 1)) +
    labs(
      title = "Ratio of House Price Change to Income Change (NL01)",
      x = "Year"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p8)
  
  years_to_plot <- c("2024jj00", "2014jj00", "2004jj00")
  
  p9 <- merged_df %>%
    filter(
      RegioS == "Nederland",
      Perioden %in% years_to_plot,
      Leeftijd %in% names(age_map),
      Geslacht == "T001038"
    ) %>%
    mutate(
      AgeGroup = age_map[as.character(Leeftijd)],
      Year = substr(Perioden, 1, 4),
      living_on_themselves = 100 * (Alleenstaand_4 + TotaalSamenwonendePersonen_5) / TotaalPersonenInHuishoudens_1
    ) %>%
    filter(!is.na(living_on_themselves), !is.na(AgeGroup)) %>%
    mutate(
      AgeGroup = factor(AgeGroup, levels = age_map),
      Year = factor(Year, levels = sort(unique(substr(years_to_plot, 1, 4))))
    ) %>%
    ggplot(aes(x = AgeGroup, y = living_on_themselves, color = Year, group = Year)) +
    geom_line(linewidth = 0.5, na.rm = TRUE) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_discrete() +
    labs(
      title = "Percentage Living in their own House by Age Group (NL01, Selected Years)",
      x = "Age Group",
      y = "Living in their own house (%)",
      color = "Year"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p9)
  
  # Define the codes and readable labels
  kenmerken_codes <- c("1014800", "1014950", "1014900")
  kenmerken_labels <- c(
    "Owned Home",
    "Rented Home (No Benefit)",
    "Rented Home (With Benefit)"
  )
  
  # Create named vector for mapping
  kenmerken_map <- setNames(kenmerken_labels, kenmerken_codes)
  
  # Plot 10: Income by Housing Type
  p10 <- merged_df %>%
    filter(
      RegioS == "Nederland",
      KenmerkenVanHuishoudens %in% kenmerken_codes,
      Perioden >= "1999JJ00",  # or adjust as needed
      Geslacht == "T001038"     # total gender, or adjust if necessary
    ) %>%
    mutate(
      Year = as.integer(substr(Perioden, 1, 4)),
      HousingType = kenmerken_map[as.character(KenmerkenVanHuishoudens)]
    ) %>%
    group_by(Year, HousingType) %>%
    summarise(
      AverageIncome = mean(GemiddeldGestandaardiseerdInkomen_3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    ggplot(aes(x = Year, y = AverageIncome, color = HousingType, linetype = HousingType)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    geom_point(size = 2, na.rm = TRUE) +
    scale_y_continuous(labels = scales::comma_format(prefix = "€")) +
    scale_color_viridis_d(option = "D", begin = 0.2, end = 0.8) +
    labs(
      title = "Average Income by Housing Type in the Netherlands (1999–2024)",
      x = "Year",
      y = "Average Income (€)",
      color = "Housing Type",
      linetype = "Housing Type"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p10)
  
  #kan nog OLS gooien op de time series voor extrepolatie
  #zelfde voor inkomen OLS kan ook en dan zeggen gat gaat alleen maar groter worden tussen de twee
  #OLS wss niet de beste aangezien de lijnen absoluut niet linear zijn dus wss ramsey reset niet doorkomen dus die ook checken, heteroskedacity, normality, exogeneity. niet aanpassen gewoon zeggen als de test dat aangeeft dat het niet zo is dus de estimate wss niet super accuraat is.
  #wellicht gemeente ook nog inkomen en huizenprijzen voor doen
  #einde alle code even mooi maken en efficient