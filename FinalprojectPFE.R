install.packages("tidyverse")
install.packages("scales")

library(tidyverse)
library(scales)

# Load datasets
df1 <- read_delim("71488ned_UntypedDataSet_05062025_175816.csv", delim = ";", show_col_types = FALSE)
df2 <- read_delim("83625NED_UntypedDataSet_05062025_101716.csv", delim = ";", show_col_types = FALSE)

# Preview structure and sample rows
cat("\n---- DF1 (Population) ----\n")
glimpse(df1)
print(head(df1, 10))

cat("\n---- DF2 (House Prices) ----\n")
glimpse(df2)
print(head(df2, 10))

# Pre-merge cleaning: align keys only
df1 <- df1 %>%
  mutate(
    RegioS = str_trim(tolower(RegioS)),
    Perioden = str_trim(tolower(Perioden))
  )

df2 <- df2 %>%
  mutate(
    RegioS = str_trim(tolower(RegioS)),
    Perioden = str_trim(tolower(Perioden))
  )

# Merge on common keys
merged_df <- inner_join(df1, df2, by = c("RegioS", "Perioden"))

# Post-merge cleaning: parse numbers
merged_df <- merged_df %>%
  mutate(
    TotaalPersonenInHuishoudens_1 = parse_number(TotaalPersonenInHuishoudens_1),
    Alleenstaand_4 = parse_number(Alleenstaand_4),
    TotaalSamenwonendePersonen_5 = parse_number(TotaalSamenwonendePersonen_5),
    GemiddeldeVerkoopprijs_1 = parse_number(GemiddeldeVerkoopprijs_1)
  )

# Print merged data preview
cat("\n---- MERGED_DF (Population And Housing Prices) ----\n")
glimpse(merged_df)
print(head(merged_df, 10))

# =========================
# Plot 1: National level (NL01)
# =========================
p1 <- merged_df %>%
  filter(RegioS == "nl01", str_detect(Perioden, "^\\d{4}jj00$")) %>%
  mutate(Jaar = as.integer(str_sub(Perioden, 1, 4))) %>%
  ggplot(aes(x = Jaar, y = GemiddeldeVerkoopprijs_1 / 1000)) +
  geom_line(color = "blue", linewidth = 1.2) +
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
  filter(str_detect(RegioS, "^pv[2-3][0-9]$"), str_detect(Perioden, "^\\d{4}jj00$")) %>%
  mutate(
    Jaar = as.integer(str_sub(Perioden, 1, 4)),
    RegioS = toupper(RegioS)
  ) %>%
  ggplot(aes(x = Jaar, y = GemiddeldeVerkoopprijs_1 / 1000)) +
  geom_line(color = "blue", linewidth = 1.2) +
  scale_y_continuous(name = "House Price (€ x 1,000)") +
  scale_x_continuous(breaks = seq(1995, 2024, 2)) +
  labs(
    title = "Average House Prices by Province (PV20–PV31)",
    x = "Year"
  ) +
  facet_wrap(~ RegioS) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)
