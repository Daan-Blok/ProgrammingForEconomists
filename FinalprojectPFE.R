library(tidyverse)
library(scales)

# ========== 1. CONSTANTS & MAPPINGS ==========
province_map <- c(
  "pv20" = "Groningen", "pv21" = "Fryslân", "pv22" = "Drenthe", "pv23" = "Overijssel",
  "pv24" = "Flevoland", "pv25" = "Gelderland", "pv26" = "Utrecht", "pv27" = "Noord-Holland",
  "pv28" = "Zuid-Holland", "pv29" = "Zeeland", "pv30" = "Noord-Brabant", "pv31" = "Limburg",
  "nl01" = "Nederland"
)

age_map <- c(
  "70100" = "0-4", "70200" = "5-9", "70300" = "10-14", "70400" = "15-19", "70500" = "20-24",
  "70600" = "25-29", "70700" = "30-34", "70800" = "35-39", "70900" = "40-44", "71000" = "45-49",
  "71100" = "50-54", "71200" = "55-59", "71300" = "60-64", "71400" = "65-69", "71500" = "70-74",
  "71600" = "75-79", "71700" = "80-84", "71800" = "85-89", "71900" = "90-94", "22000" = "95+"
)

kenmerken_map <- c(
  "1014800" = "Owned Home", "1014950" = "Rented (No Benefit)", "1014900" = "Rented (With Benefit)"
)

years_to_plot <- c("2024jj00", "2014jj00", "2004jj00")

# ========== 3. LOAD, MERGE & TRANSFORM ==========

load_and_clean <- function(file) {
  read_delim(file, delim = ";", show_col_types = FALSE) %>%
    mutate(across(c(RegioS, Perioden), ~ str_trim(tolower(.))))
}

numeric_like_cols <- merged_df %>%
  select(where(is.character)) %>%
  summarise(across(everything(), ~ mean(str_detect(., "^\\s*[0-9,.]+\\s*$"), na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "col", values_to = "pct_numeric") %>%
  filter(pct_numeric > 0.8) %>%
  pull(col)

merged_df <- list(
  "71488ned_UntypedDataSet_05062025_175816.csv",
  "83625NED_UntypedDataSet_05062025_101716.csv",
  "86004NED_UntypedDataSet_07062025_175918.csv"
) %>%
  map(load_and_clean) %>%
  reduce(full_join, by = c("RegioS", "Perioden")) %>%
  select(-matches("^ID(\\.x|\\.y)?$"),
         -starts_with("GestandaardiseerdInkomen"),
         -starts_with("Partner"),
         -starts_with("Mediaan"),
         -GemiddeldBesteedbaarInkomen_5) %>%
  mutate(across(where(is.character), str_trim)) %>%
  convert_numeric_like() %>%
  mutate(
    RegioS = recode(RegioS, !!!province_map),
    Jaar = as.integer(str_sub(Perioden, 1, 4)),
    PrijsK = GemiddeldeVerkoopprijs_1 / 1000,
    InkomenK = GemiddeldGestandaardiseerdInkomen_3,
    AlleenPerc = 100 * (Alleenstaand_4 + TotaalSamenwonendePersonen_5) / TotaalPersonenInHuishoudens_1,
    AgeGroup = age_map[as.character(Leeftijd)],
    HousingType = kenmerken_map[as.character(KenmerkenVanHuishoudens)],
    YearStr = substr(Perioden, 1, 4)
  )

index_df <- merged_df %>%
  filter(RegioS == "Nederland", KenmerkenVanHuishoudens == 1050010, !is.na(Jaar)) %>%
  arrange(Jaar) %>%
  filter(Jaar >= 2011) %>%
  mutate(
    HousePriceIndex = 100 * PrijsK / PrijsK[Jaar == 2011],
    IncomeLevelIndex = 100 * InkomenK / InkomenK[Jaar == 2011],
    Ratio = HousePriceIndex / IncomeLevelIndex
  )

# ========== 4. PLOTS ==========

p1 <- merged_df %>%
  filter(RegioS == "Nederland", !is.na(Jaar)) %>%
  ggplot(aes(x = Jaar, y = PrijsK)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Average House Prices – Netherlands", x = "Year", y = "Housing Price (€ x 1,000)")

p2 <- merged_df %>%
  filter(RegioS %in% province_map[1:12], Perioden == "2024jj00") %>%
  distinct(RegioS, .keep_all = TRUE) %>%
  ggplot(aes(x = fct_reorder(RegioS, PrijsK), y = PrijsK)) +
  geom_col(fill = "skyblue") +
  labs(title = "House Prices by Province (2024)", x = "Province", y = "Price (€ x 1,000)")

p3 <- merged_df %>%
  filter(RegioS == "Nederland", KenmerkenVanHuishoudens == 1050010, !is.na(InkomenK)) %>%
  ggplot(aes(x = Jaar, y = InkomenK)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Average Income – Netherlands", x = "Year", y = "Income (€ x 1,000)")

p4 <- merged_df %>%
  filter(RegioS %in% province_map[1:12], Perioden == "2023jj00", KenmerkenVanHuishoudens == 1050010) %>%
  distinct(RegioS, .keep_all = TRUE) %>%
  ggplot(aes(x = fct_reorder(RegioS, InkomenK), y = InkomenK)) +
  geom_col(fill = "skyblue") +
  labs(title = "Income by Province (2023)", x = "Province", y = "Income (€ x 1,000)")

p5 <- ggplot(index_df, aes(x = Jaar)) +
  geom_line(aes(y = HousePriceIndex, color = "House Prices"), linewidth = 1.2) +
  geom_line(aes(y = IncomeLevelIndex, color = "Income"), linewidth = 1.2) +
  scale_color_manual(values = c("House Prices" = "darkred", "Income" = "darkgreen")) +
  labs(title = "Index Since 2011: House Prices vs Income", 
       x = "Year", y = "Index (2011 = 100)", color = "Measure")

p6 <- ggplot(index_df, aes(x = Jaar, y = Ratio)) +
  geom_line(color = "purple", linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  labs(title = "House Price-to-Income Ratio", x = "Year", y = "Ratio")

p7 <- merged_df %>%
  filter(RegioS == "Nederland", Perioden %in% years_to_plot, Geslacht == "T001038") %>%
  filter(!is.na(AlleenPerc), !is.na(AgeGroup)) %>%
  mutate(Year = factor(YearStr, levels = sort(unique(substr(years_to_plot, 1, 4))))) %>%
  ggplot(aes(x = AgeGroup, y = AlleenPerc, color = Year, group = Year)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Living Alone by Age Group", x = "Age Group", y = "Living Alone (%)")

p8 <- merged_df %>%
  filter(RegioS == "Nederland", !is.na(HousingType), Geslacht == "T001038", Jaar >= 1999) %>%
  group_by(Jaar, HousingType) %>%
  summarise(AverageIncome = mean(InkomenK, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Jaar, y = AverageIncome, color = HousingType, linetype = HousingType)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Average Income by Housing Type", 
       x = "Year", y = "Income (€ x 1,000)", color = "Housing Type", linetype = "Housing Type")

# ========== 5. PRINT ==========
list(p1, p2, p3, p4, p5, p6, p7, p8) %>% walk(print)