
# Paper: "Partisan and Nonpartisan Models of Constitution-Making: A Comparative Case Study of Brazil and Chile"
# Authors: Rodolfo de Camargo Lima e Danilo Medeiros
# Journal of Politics in Latin America
# https://doi.org/10.1177/1866802X251397954

# Script to replicate tables 3 and 4

#Load packages
library(tidyverse)
library(tidylog)
library(readxl)
library(wnominate)
library(emIRT)
library(ggthemes)
library(scales)
library(flextable)
library(officer)
library(foreign)

#Define the working directory
setwd("C:\\Users\\yourname\\yourdirectory\\") #change to your own

### Brazil ANC
# Read data, pivot and Rename columns for clarity
anc <- read.dta("anc.dta")

data <- anc %>%
  pivot_longer(
    cols = starts_with("voto"),         
    names_to = "rollcall_id",           # New column name for vote IDs
    names_prefix = "voto",              # Remove "voto" from the column names
    values_to = "vote"                  # Column name for vote values
  )

data$centrao <- ifelse(data$centrao=="S","Centrão",
                       "Others")

# Change Small parties
data$party <- ifelse(data$party=="Dir", "SRight",
                     ifelse(data$party=="Esq", "SLeft", data$party))

# Convert votes from "S"/"N" to 1/0
data$vote <- ifelse(data$vote==1, 1,
                     ifelse(data$vote==6, 0, NA))

# Compute chamber outcome for each roll call
chamber_majority <- data %>%
  group_by(rollcall_id) %>%
  summarise(chamber_result = mean(vote, na.rm = TRUE) > 0.5, .groups = "drop")

# Compute party majority per roll call (accounting for party-switching)
party_majority <- data %>%
  group_by(rollcall_id, party) %>%
  summarise(
    party_result = mean(vote, na.rm = TRUE) > 0.5,
    .groups = "drop"
  )

# Merge to identify when a party was rolled
roll_data <- left_join(party_majority, chamber_majority, by = "rollcall_id") %>%
  mutate(rolled = party_result == FALSE & chamber_result == TRUE)

# Compute roll rates by party
roll_rates <- roll_data %>%
  group_by(party) %>%
  summarise(
    rolls = sum(rolled, na.rm = TRUE),
    total_votes = n(),  # Total roll calls where the party had votes
    roll_rate = rolls / total_votes,
    .groups = "drop"
  ) %>%
  filter(total_votes >= 500)  # Keep only parties with at least 500 roll calls

# 7️⃣ Plot the roll rates in grayscale
ggplot(roll_rates, aes(x = reorder(party, roll_rate), y = roll_rate)) +
  geom_col(fill = "grey30", color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    x = "Party",
    y = "Roll Rate (%)"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_blank()
  )

#
### Rice Index

# Filter only 'S' and 'N' votes
rice_data <- data %>%
  filter(vote %in% c(0, 1))

# Calculate Rice Index per party
rice_index <- rice_data %>%
  group_by(party, rollcall_id) %>%
  summarise(
    yay = sum(vote == 1),
    nay = sum(vote == 0),
    total = yay + nay,
    rice = ifelse(total > 0, abs(yay - nay) / total, NA_real_),
    .groups = "drop"
  ) %>%
  group_by(party) %>%
  summarise(
    avg_rice_index = mean(rice, na.rm = TRUE),
    roll_calls = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_rice_index))


# Merge the Rice Index and Roll Rate results
party_stats <- left_join(rice_index, roll_rates, by = "party") %>%
  select(party, avg_rice_index, roll_rate) %>%
  mutate(
    avg_rice_index = round(avg_rice_index, 3),
    roll_rate = round(roll_rate, 3)) 

# Filter to keep only parties that have both indexes (i.e., no NA)
party_stats_clean <- party_stats %>%
  filter(!is.na(avg_rice_index) & !is.na(roll_rate)) %>%
  arrange(roll_rate)  # Order from lowest to highest roll rate

# Create formatted flextable with roll rate first
my_table <- party_stats %>%
  filter(!is.na(avg_rice_index) & !is.na(roll_rate)) %>%
  arrange(roll_rate) %>%
  rename(
    "Party" = party,
    "Roll Rate (Rolled %)" = roll_rate,
    "Rice Index (Cohesion)" = avg_rice_index
  ) %>%
  select(Party, `Roll Rate (Rolled %)`, `Rice Index (Cohesion)`) %>%
  mutate(
    `Roll Rate (Rolled %)` = percent(`Roll Rate (Rolled %)`, accuracy = 0.1),
    `Rice Index (Cohesion)` = percent(`Rice Index (Cohesion)`, accuracy = 0.1)
  ) %>%
  flextable() %>%
  autofit() %>%
  set_caption("Party Cohesion and Roll Rates in the Brazilian Constituent Assembly")

# Export to Word
read_docx() %>%
  body_add_flextable(my_table) %>%
  print(target = "party_roll_rates_table.docx")

## By coalition (Centrão)
#
# Compute party majority per roll call (accounting for party-switching)
party_majority <- data %>%
  group_by(rollcall_id, centrao) %>%
  summarise(
    party_result = mean(vote, na.rm = TRUE) > 0.5,
    .groups = "drop"
  )

# Merge to identify when a party was rolled
roll_data <- left_join(party_majority, chamber_majority, by = "rollcall_id") %>%
  mutate(rolled = party_result == FALSE & chamber_result == TRUE)

# Compute roll rates by party
roll_rates <- roll_data %>%
  group_by(centrao) %>%
  summarise(
    rolls = sum(rolled, na.rm = TRUE),
    total_votes = n(),  # Total roll calls where the party had votes
    roll_rate = rolls / total_votes,
    .groups = "drop"
  ) %>%
  filter(total_votes >= 500)  # Keep only parties with at least 500 roll calls

## Rice Index
# Calculate Rice Index per group
rice_index <- rice_data %>%
  group_by(centrao, rollcall_id) %>%
  summarise(
    yay = sum(vote == 1),
    nay = sum(vote == 0),
    total = yay + nay,
    rice = ifelse(total > 0, abs(yay - nay) / total, NA_real_),
    .groups = "drop"
  ) %>%
  group_by(centrao) %>%
  summarise(
    avg_rice_index = mean(rice, na.rm = TRUE),
    roll_calls = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_rice_index))


# Merge the Rice Index and Roll Rate results
party_stats <- left_join(rice_index, roll_rates, by = "centrao") %>%
  select(centrao, avg_rice_index, roll_rate) %>%
  mutate(
    avg_rice_index = round(avg_rice_index, 3),
    roll_rate = round(roll_rate, 3)) 



###############
### Chile CC
# Roll Calls in Chile's floor
vot <- read_excel("Votaciones_CC_.xlsx", 
                  sheet = "Sesion Plenaria")

vot <- vot %>%
  pivot_longer(cols = "Núñez, Nicolás":"Tepper, María Angélica",
               names_to = "apellido_nombre",
               values_to = "voto")

vot <- select(vot,votacion_id, apellido_nombre, voto)

# Change variable vote
vot$voto <- ifelse(vot$voto %in% c("A Favor"), 1,
                   ifelse(vot$voto %in% c("En Contra"), 0, 
                          ifelse(vot$voto %in% c("Abstencion"), NA, NA)))

vot <- vot[!duplicated(vot), ]


# Bring in data from legislators
perfil <- read_excel("perfiles_id_clean_novo.xlsx")
perfil$id <- perfil$id...1
perfil$id...1 <- NULL
perfil$id...3 <- NULL

# Merge votes and legislators info
partido <- read_excel("perfiles_id_clean_novo.xlsx", sheet = "partidos_indep")

partido$`...2` <- NULL
partido$`...3`<- NULL
partido$`...4`<- NULL
partido$`...5`<- NULL
partido$`...6`<- NULL

partido$`tipo partido o independ` <- NULL

partido <- unique(partido)

perfil <- left_join(perfil, partido)

pacto <- read_excel("perfiles_id_clean_novo.xlsx", sheet = "pacto")

perfil <- left_join(perfil, pacto)

#

nominais <- left_join(vot, perfil)

data <- select(nominais, votacion_id, id, tag_partido, nome_pactos, voto)
names(data) <- c("rollcall_id", "legislator_id", "party", "pact","vote")

# Compute chamber outcome for each roll call
chamber_majority <- data %>%
  group_by(rollcall_id) %>%
  summarise(chamber_result = mean(vote, na.rm = TRUE) > 0.5, .groups = "drop")

# Compute party majority per roll call (accounting for party-switching)
party_majority <- data %>%
  group_by(rollcall_id, party) %>%
  summarise(
    party_result = mean(vote, na.rm = TRUE) > 0.5,
    .groups = "drop"
  )

# Merge to identify when a party was rolled
roll_data <- left_join(party_majority, chamber_majority, by = "rollcall_id") %>%
  mutate(rolled = party_result == FALSE & chamber_result == TRUE)

# Compute roll rates by party
roll_rates <- roll_data %>%
  group_by(party) %>%
  summarise(
    rolls = sum(rolled, na.rm = TRUE),
    total_votes = n(),  # Total roll calls where the party had votes
    roll_rate = rolls / total_votes,
    .groups = "drop"
  ) %>%
  filter(total_votes >= 500)  # Keep only parties with at least 500 roll calls

# Plot the roll rates in grayscale
ggplot(roll_rates, aes(x = reorder(party, roll_rate), y = roll_rate)) +
  geom_col(fill = "grey30", color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    x = "Party",
    y = "Roll Rate (%)"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_blank()
  )

#
### Rice Index

# Filter only 'S' and 'N' votes
rice_data <- data %>%
  filter(vote %in% c(0, 1))

# Calculate Rice Index per party
rice_index <- rice_data %>%
  group_by(party, rollcall_id) %>%
  summarise(
    yay = sum(vote == 1),
    nay = sum(vote == 0),
    total = yay + nay,
    rice = ifelse(total > 0, abs(yay - nay) / total, NA_real_),
    .groups = "drop"
  ) %>%
  group_by(party) %>%
  summarise(
    avg_rice_index = mean(rice, na.rm = TRUE),
    roll_calls = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_rice_index))


# Merge the Rice Index and Roll Rate results
party_stats <- left_join(rice_index, roll_rates, by = "party") %>%
  select(party, avg_rice_index, roll_rate) %>%
  mutate(
    avg_rice_index = round(avg_rice_index, 3),
    roll_rate = round(roll_rate, 3)) 

# Filter to keep only parties that have both indexes (i.e., no NA)
party_stats_clean <- party_stats %>%
  filter(!is.na(avg_rice_index) & !is.na(roll_rate)) %>%
  arrange(roll_rate)  # Order from lowest to highest roll rate

# Create formatted flextable with roll rate first
my_table <- party_stats %>%
  filter(!is.na(avg_rice_index) & !is.na(roll_rate)) %>%
  arrange(roll_rate) %>%
  rename(
    "Party" = party,
    "Roll Rate (Rolled %)" = roll_rate,
    "Rice Index (Cohesion)" = avg_rice_index
  ) %>%
  select(Party, `Roll Rate (Rolled %)`, `Rice Index (Cohesion)`) %>%
  mutate(
    `Roll Rate (Rolled %)` = percent(`Roll Rate (Rolled %)`, accuracy = 0.1),
    `Rice Index (Cohesion)` = percent(`Rice Index (Cohesion)`, accuracy = 0.1)
  ) %>%
  flextable() %>%
  autofit() %>%
  set_caption("Party Cohesion and Roll Rates in the Chilean CC")

# Export to Word
read_docx() %>%
  body_add_flextable(my_table) %>%
  print(target = "party_roll_rates_table_cc.docx")

## For pacts
# Compute party majority per roll call (accounting for party-switching)
party_majority <- data %>%
  group_by(rollcall_id, pact) %>%
  summarise(
    party_result = mean(vote, na.rm = TRUE) > 0.5,
    .groups = "drop"
  )

# Merge to identify when a party was rolled
roll_data <- left_join(party_majority, chamber_majority, by = "rollcall_id") %>%
  mutate(rolled = party_result == FALSE & chamber_result == TRUE)

# Compute roll rates by party
roll_rates <- roll_data %>%
  group_by(pact) %>%
  summarise(
    rolls = sum(rolled, na.rm = TRUE),
    total_votes = n(),  # Total roll calls where the party had votes
    roll_rate = rolls / total_votes,
    .groups = "drop"
  ) %>%
  filter(total_votes >= 500)  # Keep only parties with at least 500 roll calls

# Plot the roll rates in grayscale
ggplot(roll_rates, aes(x = reorder(pact, roll_rate), y = roll_rate)) +
  geom_col(fill = "grey30", color = "black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Pact",
    y = "Roll Rate (%)"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_blank()
  )

## Rice Index
# Calculate Rice Index per group
rice_index <- rice_data %>%
  group_by(pact, rollcall_id) %>%
  summarise(
    yay = sum(vote == 1),
    nay = sum(vote == 0),
    total = yay + nay,
    rice = ifelse(total > 0, abs(yay - nay) / total, NA_real_),
    .groups = "drop"
  ) %>%
  group_by(pact) %>%
  summarise(
    avg_rice_index = mean(rice, na.rm = TRUE),
    roll_calls = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_rice_index))


# Merge the Rice Index and Roll Rate results
party_stats <- left_join(rice_index, roll_rates, by = "pact") %>%
  select(pact, avg_rice_index, roll_rate) %>%
  mutate(
    avg_rice_index = round(avg_rice_index, 3),
    roll_rate = round(roll_rate, 3)) 

# Create formatted flextable with roll rate first
my_table <- party_stats %>%
  filter(!is.na(avg_rice_index) & !is.na(roll_rate)) %>%
  arrange(roll_rate) %>%
  rename(
    "Pact" = pact,
    "Roll Rate" = roll_rate,
    "Rice Index" = avg_rice_index
  ) %>%
  select(Pact, `Roll Rate`, `Rice Index`) %>%
  mutate(
    `Roll Rate` = percent(`Roll Rate`, accuracy = 0.1),
    `Rice Index` = percent(`Rice Index`, accuracy = 0.1)
  ) %>%
  flextable() %>%
  autofit() %>%
  set_caption("Party Cohesion and Roll Rates in the Chilean CC")

# Export to Word
read_docx() %>%
  body_add_flextable(my_table) %>%
  print(target = "party_roll_rates_table_cc2.docx")
