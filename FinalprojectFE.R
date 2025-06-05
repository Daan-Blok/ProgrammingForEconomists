library(tidyverse)

# Load datasets
df1 <- read_delim("71488ned_UntypedDataSet_03062025_132547.csv", delim = ";", show_col_types = FALSE)
df2 <- read_delim("83625NED_UntypedDataSet_03062025_133923.csv", delim = ";", show_col_types = FALSE)
df3 <- read_delim("83625NED_UntypedDataSet_03062025_133937.csv", delim = ";", show_col_types = FALSE)

# Clean whitespace
df1 <- df1 %>% mutate(RegioS = str_trim(RegioS))
df2 <- df2 %>% mutate(RegioS = str_trim(RegioS))
df3 <- df3 %>% mutate(RegioS = str_trim(RegioS))

# Convert columns to numeric
df1 <- df1 %>%
  mutate(across(c(
    TotaalPersonenInHuishoudens_1,
    Alleenstaand_4,
    TotaalSamenwonendePersonen_5
  ), ~ as.numeric(str_remove_all(., " "))))

df2 <- df2 %>%
  mutate(GemiddeldeVerkoopprijs_1 = as.numeric(str_remove_all(GemiddeldeVerkoopprijs_1, " ")))

df3 <- df3 %>%
  mutate(GemiddeldeVerkoopprijs_1 = as.numeric(str_remove_all(GemiddeldeVerkoopprijs_1, " ")))

# Plot 1: Percentage of people living alone by age
df1_ageplot <- df1 %>%
  filter(RegioS == "NL01", Perioden == "2024JJ00") %>%
  mutate(living_on_themselves = 100 * (Alleenstaand_4 + TotaalSamenwonendePersonen_5) / TotaalPersonenInHuishoudens_1) %>%
  filter(!is.na(living_on_themselves)) %>%
  mutate(age = case_when(
    Leeftijd == 70100 ~ 0,
    Leeftijd == 70200 ~ 5,
    Leeftijd == 70300 ~ 10,
    Leeftijd == 70400 ~ 15,
    Leeftijd == 70500 ~ 20,
    Leeftijd == 70600 ~ 25,
    Leeftijd == 70700 ~ 30,
    Leeftijd == 70800 ~ 35,
    Leeftijd == 70900 ~ 40,
    Leeftijd == 71000 ~ 45,
    Leeftijd == 71100 ~ 50,
    Leeftijd == 71200 ~ 55,
    Leeftijd == 71300 ~ 60,
    Leeftijd == 71400 ~ 65,
    Leeftijd == 71500 ~ 70,
    Leeftijd == 71600 ~ 75,
    Leeftijd == 71700 ~ 80,
    Leeftijd == 71800 ~ 85,
    Leeftijd == 71900 ~ 90,
    Leeftijd == 22000 ~ 95
  ))

p1 <- ggplot(df1_ageplot, aes(x = age, y = living_on_themselves)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "black") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) +
  labs(title = "Percentage Living in their own house by Age Group (NL01, 2024)",
       x = "Age", y = "Living in their own house (%)") +
  theme_minimal()

print(p1)

# Plot 2: Time series of house prices (df2)
ts_data <- df2 %>%
  filter(RegioS == "NL01", str_detect(Perioden, "^\\d{4}JJ00$")) %>%
  mutate(Jaar = as.integer(str_sub(Perioden, 1, 4)))  # Extract year

ts_plot <- ggplot(ts_data, aes(x = Jaar, y = GemiddeldeVerkoopprijs_1)) +
  geom_line(color = "blue", linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1995, 2024, by = 1)) +
  labs(title = "House Prices in the Netherlands (1995–2024)",
       x = "Year", y = "Average Sale Price (€)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(ts_plot)

# Plot 3: Boxplot of house prices by province (df3)
province_names <- c(
  PV20 = "Groningen", PV21 = "Fryslân", PV22 = "Drenthe", PV23 = "Overijssel",
  PV24 = "Flevoland", PV25 = "Gelderland", PV26 = "Utrecht", PV27 = "Noord-Holland",
  PV28 = "Zuid-Holland", PV29 = "Zeeland", PV30 = "Noord-Brabant", PV31 = "Limburg"
)

box_data <- df3 %>%
  filter(RegioS %in% names(province_names), Perioden == "2024JJ00") %>%
  mutate(Province = province_names[RegioS])

p3 <- ggplot(box_data, aes(x = Province, y = GemiddeldeVerkoopprijs_1)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "House Prices by Province (2024)",
       x = "Province", y = "Average Sale Price (€)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)
