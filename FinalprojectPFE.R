library(tidyverse)
library(scales)

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
p1 <- merged_df %>%
  filter(RegioS == "Nederland", str_detect(Perioden, "^\\d{4}jj00$")) %>%
  mutate(Jaar = as.integer(str_sub(Perioden, 1, 4))) %>%
  drop_na(GemiddeldeVerkoopprijs_1) %>%
  ggplot(aes(x = Jaar, y = GemiddeldeVerkoopprijs_1 / 1000)) +
  geom_line(color = "blue", linewidth = 1.2, na.rm = TRUE) +
  scale_y_continuous(name = "House Price (€ x 1,000)") +
  scale_x_continuous(breaks = seq(1995, 2024, 2)) +
  labs(
    title = "Average House Prices – Netherlands Total (NL01)",
    x = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

#Variable house prijs increase vs inkomen increase
#Variable living on themselves 
#plot mensen die opzich wonen per leeftijdsgroep
#plot mensen met eigen huis inkomen vs mensen huurwoning zonder toeslag vs mensen huurwoning met toeslag
#plot iets die population splitsing doen maar lastig te bedenken
#kan nog OLS gooien op de time series voor extrepolatie
#zelfde voor inkomen OLS kan ook en dan zeggen gat gaat alleen maar groter worden tussen de twee
#OLS wss niet de beste aangezien de lijnen absoluut niet linear zijn dus wss ramsey reset niet doorkomen dus die ook checken, heteroskedacity, normality, exogeneity. niet aanpassen gewoon zeggen als de test dat aangeeft dat het niet zo is dus de estimate wss niet super accuraat is.
#wellicht gemeente ook nog inkomen en huizenprijzen voor doen



