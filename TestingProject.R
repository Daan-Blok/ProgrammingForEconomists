library(tidyverse)
library(scales)
library(cbsodataR)
library(rlang)
library(sf)
library(giscoR)

# ========== 1. CONSTANTS & MAPPINGS ==========
province_map <- c(
  "pv20" = "Groningen", "pv21" = "Friesland", "pv22" = "Drenthe", "pv23" = "Overijssel",
  "pv24" = "Flevoland", "pv25" = "Gelderland", "pv26" = "Utrecht", "pv27" = "Noord-Holland",
  "pv28" = "Zuid-Holland", "pv29" = "Zeeland", "pv30" = "Noord-Brabant", "pv31" = "Limburg",
  "nl01" = "Nederland"
)

age_map <- c(
  "70100" = "0-19", "70200" = "0-19", "70300" = "0-19", "70400" = "0-19", "70500" = "20-39",
  "70600" = "20-39", "70700" = "20-39", "70800" = "20-39", "70900" = "40-59", "71000" = "40-59",
  "71100" = "40-59", "71200" = "40-59", "71300" = "60-79", "71400" = "60-79", "71500" = "60-79",
  "71600" = "60-79", "71700" = "80+", "71800" = "80+", "71900" = "80+", "22000" = "80+"
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

plot_filters <- list(
  p1 = "RegioS == 'Nederland' & !is.na(Jaar) & !is.na(HuisPrijs)",
  p2 = "RegioS %in% province_map[1:12] & Perioden == '2023jj00' & !is.na(HuisPrijs) & !is.na(RegioS)",
  p3 = "RegioS == 'Nederland' & KenmerkenVanHuishoudens == 1050010 & !is.na(GestandaardiseerdInkomen) & !is.na(Jaar)",
  p4 = "RegioS %in% province_map[1:12] & Perioden == '2023jj00' & KenmerkenVanHuishoudens == 1050010 & !is.na(GestandaardiseerdInkomen) & !is.na(RegioS)",
  p5 = "RegioS == 'Nederland' & KenmerkenVanHuishoudens == 1050010 & !is.na(Jaar) & Jaar >= 2011 & !is.na(HousePriceIndex) & !is.na(IncomeLevelIndex)",
  p6 = "RegioS == 'Nederland' & KenmerkenVanHuishoudens == 1050010 & !is.na(Jaar) & Jaar >= 2011 & !is.na(Ratio)",
  p7 = "RegioS == 'Nederland' & Perioden %in% years_to_plot & Geslacht == 'T001038' & !is.na(OpZichzelfWonend) & !is.na(AgeGroup)",
  p8 = "RegioS == 'Nederland' & !is.na(HousingType) & Geslacht == 'T001038' & !is.na(GestandaardiseerdInkomen) & !is.na(Jaar)",
  p9 = "RegioS == 'Nederland' & !is.na(Jaar) & !is.na(HuisPrijs)"
)

gis_to_regio <- c(
  "Groningen" = "Groningen", "Friesland (NL)" = "Friesland", "Drenthe" = "Drenthe", "Overijssel" = "Overijssel",
  "Flevoland" = "Flevoland", "Gelderland" = "Gelderland", "Utrecht" = "Utrecht", "Noord-Holland" = "Noord-Holland",
  "Zuid-Holland" = "Zuid-Holland", "Zeeland" = "Zeeland", "Noord-Brabant" = "Noord-Brabant","Limburg (NL)" = "Limburg"
)

# ========== 2. CUSTOM THEMES ==========
my_theme <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0), 
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

theme_map_clean <- theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10))
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
  mutate(across(all_of(numeric_like_cols), ~ parse_number(., na = c("", ".", "n.v.t.", "onbekend"))))

merged_df <- merged_df %>%
  mutate(
    RegioS = recode(RegioS, !!!province_map),
    Jaar = as.integer(str_sub(Perioden, 1, 4)),
    HuisPrijs = GemiddeldeVerkoopprijs_1 / 1000,
    GestandaardiseerdInkomen = GemiddeldGestandaardiseerdInkomen_3,
    AgeGroup = age_map[as.character(Leeftijd)],
    HousingType = kenmerken_map[as.character(KenmerkenVanHuishoudens)],
    YearStr = substr(Perioden, 1, 4),
  )

reference_price_2011 <- merged_df %>%
  filter(RegioS == "Nederland", Jaar == 2011) %>%
  pull(HuisPrijs) %>%
  first()

reference_income_2011 <- merged_df %>%
  filter(RegioS == "Nederland", Jaar == 2011) %>%
  pull(GestandaardiseerdInkomen) %>%
  first()

merged_df <- merged_df %>%
  mutate(    OpZichzelfWonend = 100 * (Alleenstaand_4 + TotaalSamenwonendePersonen_5) / TotaalPersonenInHuishoudens_1,
             HousePriceIndex = 100 * HuisPrijs / reference_price_2011,
             IncomeLevelIndex = 100 * GestandaardiseerdInkomen / reference_income_2011,
             Ratio = HousePriceIndex / IncomeLevelIndex)

merged_df <- map(plot_filters, ~ merged_df %>% 
                   filter(!!parse_expr(.x)))

print(head(merged_df))
# ========== 4. SPATIAL MAP ==========
nl_map <- gisco_get_nuts(year = "2021",nuts_level = 2,country = "NL",resolution = "01",spatialtype = "RG") %>%
  st_transform(crs = 28992) %>%
  mutate(RegioS = recode(NAME_LATN, !!!gis_to_regio)) %>%
  filter(!is.na(RegioS))

nl_map_p2 <- left_join(nl_map, distinct(merged_df$p2, RegioS, .keep_all = TRUE), by = "RegioS")
nl_map_p4 <- left_join(nl_map, distinct(merged_df$p4, RegioS, .keep_all = TRUE), by = "RegioS")

# ========== 4. PLOTS ==========
p1 <- merged_df$p1 %>%
  ggplot(aes(x = Jaar, y = HuisPrijs)) +
  geom_line(color = "blue", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1995, 2024, by = 2)) +
  labs(title = "Average House Price (Netherlands)", x = "Year", y = "Price (€ x 1,000)")

p2 <- ggplot(nl_map_p2) +
  geom_sf(aes(fill = HuisPrijs), color = "white", size = 0.2) +
  scale_fill_gradient(low = "skyblue", high = "darkblue", name = "Price (€ x 1,000)") +
  labs(title = "Average House Prices by Province (2023)") 

p3 <- merged_df$p3 %>%
  ggplot(aes(x = Jaar, y = GestandaardiseerdInkomen)) +
  geom_line(color = "blue", linewidth = 1.2) +
  scale_x_continuous(breaks = seq(2011, 2023, by = 2)) +
  labs(title = "Average Standardised Income (Netherlands)", x = "Year", y = "Income (€ x 1,000)")

p4 <- ggplot(nl_map_p4) +
  geom_sf(aes(fill = GestandaardiseerdInkomen), color = "white", size = 0.2) +
  scale_fill_gradient(low = "skyblue", high = "darkblue", name = "Income (€ x 1,000)") +
  labs(title = "Average Standardised Income by Province (2023)")

p5 <- merged_df$p5 %>%
  ggplot(aes(x = Jaar)) +
  geom_line(aes(y = HousePriceIndex, color = "House Prices"), linewidth = 1.2) +
  geom_line(aes(y = IncomeLevelIndex, color = "Standardised Income"), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(2011, 2023, by = 2)) +
  scale_color_manual(values = c("House Prices" = "darkred", "Standardised Income" = "darkgreen")) +
  labs(title = "House Price vs Standardised Income (Netherlands)", x = "Year", y = "Index (2011 = 100)", color = "Measure")

p6 <- merged_df$p6 %>%
  ggplot(aes(x = Jaar, y = Ratio)) +
  geom_line(color = "purple", linewidth = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  scale_x_continuous(breaks = seq(2011, 2023, by = 2)) +
  labs(title = "Ratio of House Prices to Standardised Income", x = "Year", y = "Ratio")

p7 <- merged_df$p7 %>%
  mutate(Year = factor(YearStr, levels = sort(unique(substr(years_to_plot, 1, 4))))) %>%
  ggplot(aes(x = AgeGroup, y = OpZichzelfWonend, color = Year, group = Year)) +
  geom_line(linewidth = 0.5) +
  labs(title = "Share of Individuals Living Indepent by Age Group", x = "Age Group (Years)", y = "Individuals Living Indepent (%)")

p8 <- merged_df$p8 %>%
  group_by(Jaar, HousingType) %>%
  summarise(AverageIncome = mean(GestandaardiseerdInkomen, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Jaar, y = AverageIncome, color = HousingType, linetype = HousingType)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(2011, 2023, by = 2)) +
  labs(title = "Average Standardised Income by Housing Type", x = "Year", y = "Income (€ x 1,000)", color = "Housing Type", linetype = "Housing Type")

p9 <- merged_df$p9 %>%
  ggplot(aes(x = Jaar, y = HuisPrijs, color = RegioS)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "black", linewidth = 1) +
  scale_x_continuous(breaks = seq(1995, 2024, by = 2)) +
  labs(title = "Average House Price (Netherlands)", x = "Year", y = "Price (€ x 1,000)", color = "Region")

# ========== 5. PRINT ==========
list(p1, p3, p5, p6, p7, p8, p9) %>% walk(~ print(.x + my_theme))
list(p2, p4) %>% walk(~ print(.x + theme_map_clean))