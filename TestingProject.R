library(tidyverse)
library(scales)
library(cbsodataR)

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

table_ids <- c("83625NED", "86004NED", "71488NED")

table_column_map <- list(
  "83625NED" = c("RegioS", "Perioden", "GemiddeldeVerkoopprijs_1"),
  "86004NED" = c("KenmerkenVanHuishoudens", "RegioS", "Perioden", "GemiddeldGestandaardiseerdInkomen_3", "Populatie"),
  "71488NED" = c("Geslacht", "Leeftijd", "RegioS", "Perioden", "TotaalPersonenInHuishoudens_1", "Alleenstaand_4", "TotaalSamenwonendePersonen_5")
)

# ========== 2. CUSTOM THEMES ==========
my_theme <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ========== 3. LOAD, CLEAN, MERGE & TRANSFORM ==========

load_cbs_raw <- function(table_id) {
  cbs_get_data(table_id, typed = FALSE)
}

raw_tables <- suppressWarnings(map(table_ids, load_cbs_raw))

clean_cbs_data <- function(df, table_id) {
  df <- df %>%
    mutate(across(c(RegioS, Perioden), ~ str_trim(tolower(as.character(.)))))
  
  colnames(df) <- str_trim(colnames(df))
  
  keep_cols <- table_column_map[[table_id]]
  
  keep_cols <- union(keep_cols, c("RegioS", "Perioden"))
  
  df <- df %>% select(any_of(keep_cols)) %>%
    filter(RegioS %in% tolower(names(province_map))) %>%
    {
      if ("Geslacht" %in% names(.)) filter(., Geslacht == "T001038") 
      else if ("Populatie" %in% names(.)) filter(., Populatie == "1050010") else .
    }
  
  return(df)
}

cleaned_tables <- map2(raw_tables, table_ids, clean_cbs_data)

merged_df <- cleaned_tables %>%
  reduce(full_join, by = c("RegioS", "Perioden")) %>%
  mutate(across(where(is.character), str_trim))

numeric_like_cols <- names(merged_df)[
  sapply(merged_df, function(col) mean(str_detect(col, "^\\s*[0-9,.]+\\s*$"), na.rm = TRUE) > 0.8)
]

merged_df <- merged_df %>%
mutate(across(all_of(numeric_like_cols), ~ parse_number(., na = c("", ".", "n.v.t.", "onbekend"))))%>%
mutate(
    RegioS = recode(RegioS, !!!province_map),
    Jaar = as.integer(str_sub(Perioden, 1, 4)),
    HuisPrijs = GemiddeldeVerkoopprijs_1 / 1000,
    GestandaardiseerdInkomen = GemiddeldGestandaardiseerdInkomen_3,
    OpZichzelfWonend = 100 * (Alleenstaand_4 + TotaalSamenwonendePersonen_5) / TotaalPersonenInHuishoudens_1,
    AgeGroup = age_map[as.character(Leeftijd)],
    HousingType = kenmerken_map[as.character(KenmerkenVanHuishoudens)],
    YearStr = substr(Perioden, 1, 4)
  )

index_df <- merged_df %>%
  filter(RegioS == "Nederland", KenmerkenVanHuishoudens == 1050010, !is.na(Jaar)) %>%
  arrange(Jaar) %>%
  filter(Jaar >= 2011) %>%
  mutate(HousePriceIndex = 100 * HuisPrijs / HuisPrijs[Jaar == 2011],
         IncomeLevelIndex = 100 * GestandaardiseerdInkomen / GestandaardiseerdInkomen[Jaar == 2011],
         Ratio = HousePriceIndex / IncomeLevelIndex)

# ========== 4. PLOTS ==========

p1 <- merged_df %>%
  filter(RegioS == "Nederland", !is.na(Jaar)) %>%
  ggplot(aes(x = Jaar, y = HuisPrijs)) +
  geom_line(color = "blue", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(min(merged_df$Jaar, na.rm = TRUE), max(merged_df$Jaar, na.rm = TRUE), by = 2)) +
  labs(title = "Average House Price (Netherlands)", x = "Year", y = "House Price (€ x 1,000)")

p2 <- merged_df %>%
  filter(RegioS %in% province_map[1:12], Perioden == "2024jj00") %>%
  distinct(RegioS, .keep_all = TRUE) %>%
  ggplot(aes(x = fct_reorder(RegioS, HuisPrijs), y = 1, fill = HuisPrijs)) +
  geom_tile(color = "white", height = 0.8) +
  scale_fill_gradient(low = "skyblue", high = "darkblue", name = "House Price (€ x 1,000)") +
  scale_y_continuous(expand = c(0, 0), breaks = NULL, labels = NULL) +
  labs(title = "      Average House Prices by Province (2024)", x = "Province", y = NULL) +
  coord_cartesian(xlim = c(0.3, NA))

p3 <- merged_df %>%
  filter(RegioS == "Nederland", KenmerkenVanHuishoudens == 1050010, !is.na(GestandaardiseerdInkomen)) %>%
  ggplot(aes(x = Jaar, y = GestandaardiseerdInkomen)) +
  geom_line(color = "blue", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(min(merged_df$Jaar, na.rm = TRUE), max(merged_df$Jaar, na.rm = TRUE), by = 2)) +
  labs(title = "Average Disposable Income (Netherlands)", x = "Year", y = "Disposable Income (€ x 1,000)")

p4 <- merged_df %>%
  filter(RegioS %in% province_map[1:12], Perioden == "2023jj00", KenmerkenVanHuishoudens == 1050010) %>%
  distinct(RegioS, .keep_all = TRUE) %>%
  ggplot(aes(x = fct_reorder(RegioS, GestandaardiseerdInkomen), y = 1, fill = GestandaardiseerdInkomen)) +
  geom_tile(color = "white", height = 0.8) +
  scale_fill_gradient(low = "skyblue", high = "darkblue", name = "Disposable Income (€ x 1,000)") +
  scale_y_continuous(expand = c(0, 0), breaks = NULL, labels = NULL) +
  labs(title = "      Average Disposable Income by Province (2023)", x = "Province", y = NULL) +
  coord_cartesian(xlim = c(0.3, NA))

p5 <- ggplot(index_df, aes(x = Jaar)) +
  geom_line(aes(y = HousePriceIndex, color = "House Prices"), linewidth = 1.2) +
  geom_line(aes(y = IncomeLevelIndex, color = "Disposable Income"), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(min(merged_df$Jaar, na.rm = TRUE), max(merged_df$Jaar, na.rm = TRUE), by = 2)) +
  scale_color_manual(values = c("House Prices" = "darkred", "Disposable Income" = "darkgreen")) +
  labs(title = "Average House Price vs Average Disposable Income (Index, Netherlands)", x = "Year", y = "Index (2011 = 100)", color = "Measure")

p6 <- ggplot(index_df, aes(x = Jaar, y = Ratio)) +
  geom_line(color = "purple", linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  scale_x_continuous(breaks = seq(min(merged_df$Jaar, na.rm = TRUE), max(merged_df$Jaar, na.rm = TRUE), by = 2)) +
  labs(title = "Ratio of Average House Prices to Average Disposable Income", x = "Year", y = "Ratio")

p7 <- merged_df %>%
  filter(RegioS == "Nederland", Perioden %in% years_to_plot, Geslacht == "T001038") %>%
  filter(!is.na(OpZichzelfWonend), !is.na(AgeGroup)) %>%
  mutate(Year = factor(YearStr, levels = sort(unique(substr(years_to_plot, 1, 4))))) %>%
  ggplot(aes(x = AgeGroup, y = OpZichzelfWonend, color = Year, group = Year)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Share of Individuals Living Alone by Age Group (2004–2024)", x = "Age Group (Years)", y = "Individuals Living Alone (%)")

p8 <- merged_df %>%
  filter(RegioS == "Nederland", !is.na(HousingType), Geslacht == "T001038") %>%
  group_by(Jaar, HousingType) %>%
  summarise(AverageIncome = mean(GestandaardiseerdInkomen, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Jaar, y = AverageIncome, color = HousingType, linetype = HousingType)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(min(merged_df$Jaar, na.rm = TRUE), max(merged_df$Jaar, na.rm = TRUE), by = 2)) +
  labs(title = "Average Disposable Income by Housing Type", x = "Year", y = "Disposable Income (€ x 1,000)", color = "Housing Type", linetype = "Housing Type")

p9 <- merged_df %>%
  filter(RegioS == "Nederland", !is.na(Jaar)) %>%
  ggplot(aes(x = Jaar, y = HuisPrijs, color = RegioS)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "black", linewidth = 1) +
  scale_x_continuous(breaks = seq(min(merged_df$Jaar, na.rm = TRUE), max(merged_df$Jaar, na.rm = TRUE), by = 2)) +
  labs(title = "Average House Price (Netherlands)", x = "Year", y = "Housing Price (€ x 1,000)", color = "Region")

# ========== 5. PRINT ==========
list(p1, p2, p3, p4, p5, p6, p7, p8, p9) %>% walk(~ print(.x + my_theme))