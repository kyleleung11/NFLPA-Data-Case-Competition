library(nflreadr)
library(dplyr)
library(ggplot2)

# EPA Calculations

## Passing EPA
pbp <- nflreadr::load_pbp(2024)
passer_epa <- pbp %>%
  filter(!is.na(passer_player_id)) %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarise(
    plays = n(),
    total_epa = sum(epa, na.rm = TRUE),
    epa_per_play = mean(epa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(plays > 50) %>%
  arrange(desc(total_epa))
head(passer_epa, 10)

## Receiving EPA
receiver_epa <- pbp %>%
  filter(!is.na(receiver_player_id)) %>%
  group_by(receiver_player_id, receiver_player_name) %>%
  summarise(
    plays = n(),
    total_epa = sum(epa, na.rm = TRUE),
    epa_per_target = mean(epa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(plays > 50) %>%
  arrange(desc(total_epa))
head(receiver_epa, 10)

## Rushing EPA
rusher_epa <- pbp %>%
  filter(!is.na(rusher_player_id)) %>%
  group_by(rusher_player_id, rusher_player_name) %>%
  summarise(
    plays = n(),
    total_epa = sum(epa, na.rm = TRUE),
    epa_per_rush = mean(epa, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(plays > 50)%>%
  arrange(desc(total_epa))
head(rusher_epa, 10)

## Overall EPA, sorted by Total EPA and EPA/play
player_epa <- bind_rows(
  passer_epa %>% rename(player_id = passer_player_id, player_name = passer_player_name),
  receiver_epa %>% rename(player_id = receiver_player_id, player_name = receiver_player_name),
  rusher_epa %>% rename(player_id = rusher_player_id, player_name = rusher_player_name)
) %>%
  group_by(player_id, player_name) %>%
  summarise(
    total_plays = sum(plays),
    total_epa = sum(total_epa, na.rm = TRUE),
    epa_per_play = total_epa / total_plays,
    .groups = "drop"
  )
top_player_epa <- player_epa %>%
  arrange(desc(total_epa))
head(top_player_epa, 10)

top_player_epa_per_play <- player_epa %>%
  arrange(desc(epa_per_play))
head(top_player_epa_per_play, 10)

## EPA Visualizations
top_10_total_epa <- player_epa %>%
  arrange(desc(total_epa)) %>%
  slice_head(n = 10)

# Create the ggplot for Total EPA
ggplot(top_10_total_epa, aes(x = total_epa, y = reorder(player_name, total_epa), fill = total_epa)) +
  geom_col(color = "black", linewidth = 0.5) +
  # Add labels to the bars
  geom_text(aes(label = sprintf("%.1f", total_epa)),
            hjust = -0.1, size = 4) +
  labs(
    title = "Top 10 NFL Players by Total EPA (2024 Season)",
    x = "Total Expected Points Added (EPA)",
    y = "Player Name"
  )
top_10_epa_per_play <- player_epa %>%
  arrange(desc(epa_per_play)) %>%
  slice_head(n = 10)

# Create the ggplot for EPA per Play
ggplot(top_10_epa_per_play, aes(x = epa_per_play, y = reorder(player_name, epa_per_play), fill = epa_per_play)) +
  geom_col(color = "black", linewidth = 0.5) +
  # Add labels to the bars, formatted to 3 decimal places
  geom_text(aes(label = sprintf("%.3f", epa_per_play)),
            hjust = -0.1, size = 4) +
  labs(
    title = "Top 10 NFL Players by EPA per Play (2024 Season)",
    x = "Expected Points Added per Play (EPA/Play)",
    y = "Player Name"
  )

## Air and YAC EPA Analysis
library(dplyr)

alpha_qb_air <- 1.0

pbp_base <- pbp_clean

# Create a robust dropback flag
if ("qb_dropback" %in% names(pbp_base)) {
  pbp_base <- pbp_base %>% mutate(is_dropback = qb_dropback == 1)
} else {
  pbp_base <- pbp_base %>% mutate(
    is_dropback =
      (pass == 1) |
      (("sack" %in% names(pbp_base)) & sack == 1) |
      (("qb_scramble" %in% names(pbp_base)) & qb_scramble == 1)
  )
}

# Create an ID/name to use for all dropbacks (passer on passes/sacks, rusher on scrambles if needed)
pbp_base <- pbp_base %>%
  mutate(
    qb_player_id = if ("rusher_player_id" %in% names(pbp_base)) coalesce(passer_player_id, rusher_player_id) else passer_player_id,
    qb_player_name = if ("rusher_player_name" %in% names(pbp_base)) coalesce(passer_player_name, rusher_player_name) else passer_player_name
  )

pbp_alloc_depth_qb <- pbp_base %>%
  filter(is_dropback, !is.na(epa)) %>%
  mutate(
    # QB share of YAC on SHORT completions only
    qb_yac_share = case_when(
      pass == 1 & complete_pass == 1 & !is.na(air_yards) & air_yards <= 5 ~ 0.30,
      TRUE ~ 0.00
    ),
    
    # Pass component: use air_epa when available, otherwise fall back to epa
    qb_air_value = case_when(
      pass == 1 & !is.na(air_epa) ~ alpha_qb_air * air_epa,
      pass == 1 &  is.na(air_epa) ~ epa,
      TRUE ~ 0
    ),
    
    # YAC component (only on completed passes)
    qb_yac_value = if_else(pass == 1 & complete_pass == 1 & !is.na(yac_epa), qb_yac_share * yac_epa, 0),
    
    # Sacks/scrambles (non-pass dropbacks): give QB full EPA
    qb_other_dropback_value = if_else(pass != 1, epa, 0),
    
    qb_total_value = qb_air_value + qb_yac_value + qb_other_dropback_value
  )

qb_value_depth_dropbacks <- pbp_alloc_depth_qb %>%
  filter(!is.na(qb_player_id), !is.na(qb_player_name)) %>%
  group_by(season, qb_player_name, qb_player_id) %>%
  summarise(
    dropbacks = n(),
    total_value = sum(qb_total_value, na.rm = TRUE),
    value_per_dropback = total_value / dropbacks,
    .groups = "drop"
  ) %>%
  arrange(desc(total_value))

qb_value_depth_dropbacks_filter <- qb_value_depth_dropbacks %>% filter(dropbacks > 350)


pbp_alloc_depth %>%
  summarise(
    completions = sum(complete_pass == 1, na.rm = TRUE),
    short_completions = sum(complete_pass == 1 & !is.na(air_yards) & air_yards <= 5, na.rm = TRUE),
    short_share = short_completions / completions,
    qb_yac_share_mean_on_completions = mean(qb_yac_share[complete_pass == 1], na.rm = TRUE)
  )

# Receiver leaderboard with TARGETS (includes incompletions)
recv_value_depth_targets <- pbp_alloc_depth %>%
  filter(
    !is.na(receiver_player_id),
    !is.na(receiver_player_name)
  ) %>%
  group_by(season, receiver_player_name, receiver_player_id) %>%
  summarise(
    targets = n(),  # every row here is a target
    catches = sum(complete_pass == 1, na.rm = TRUE),
    total_value = sum(recv_total_value, na.rm = TRUE),  # 0 on incompletions by construction
    yac_value = sum(recv_yac_value, na.rm = TRUE),
    value_per_target = total_value / targets,
    .groups = "drop"
  ) %>%
  arrange(desc(total_value))

# Example filter
recv_value_depth_targets_filter <- recv_value_depth_targets %>% filter(targets > 100)


qb_cut_epa <- quantile(qb_value_depth_dropbacks_filter$total_value, 0.75, na.rm = TRUE)

qb_top_quartile_epa <- qb_value_depth_dropbacks %>%
  filter(total_value >= qb_cut_epa)

wr_cut_epa <- quantile(recv_value_depth_targets_filter$total_value, 0.75, na.rm = TRUE)

wr_top_quartile_epa <- recv_value_depth_targets %>%
  filter(total_value >= wr_cut_epa)

t.test(
  qb_top_quartile_epa$value_per_dropback,
  wr_top_quartile_epa$value_per_target,
  alternative = "two.sided",
  var.equal = FALSE   # Welch t-test (recommended)
)

# Contract analysis, contact analysis, and exploitation scores

## Replaceability Visualizations
positional_data <- total_season_summary %>%
  select(player_id, position, games_played)

player_epa_merged <- player_epa %>%
  left_join(positional_data, by = "player_id")

epa_data_filtered <- player_epa_merged %>%
  filter(position %in% c('QB', 'RB', 'WR', 'TE')) %>%
  filter(games_played > 8)

# Calculate median and 90th Percentile of EPA/play for each position to analyze replacability

replaceability_analysis <- epa_data_filtered %>%
  group_by(position) %>%
  summarize(
    P50_epa_per_play = quantile(epa_per_play, 0.5, na.rm = TRUE),
    P90_epa_per_play = quantile(epa_per_play, 0.9, na.rm = TRUE),
    N_players = n(),
    .groups = "drop"
  ) %>%
  # Calculate the Replaceability Gap (P90 - P50)
  mutate(
    replaceability_gap = P90_epa_per_play - P50_epa_per_play
  ) %>%
  arrange(desc(replaceability_gap))

print("Replaceability Analysis Results:")
print(replaceability_analysis)

library(ggplot2)
library(scales)

replaceability_gap_plot_data <- replaceability_analysis %>%
  select(position, replaceability_gap) %>%
  rename(`Replaceability Gap (P90 - P50 EPA/Play)` = replaceability_gap)

ggplot(replaceability_gap_plot_data, aes(x = position, y = `Replaceability Gap (P90 - P50 EPA/Play)`)) +
  geom_bar(stat = "identity", fill = "darkred", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", `Replaceability Gap (P90 - P50 EPA/Play)`)),
            vjust = -0.5, size = 5) +
  labs(
    title = "Replaceability Gap (P90 - P50 EPA/Play)",
    subtitle = "Higher Gap = Harder to Replace Elite Talent (2024 NFL Data, >8 Games)",
    x = "Position",
    y = "Replaceability Gap (P90 EPA/Play - P50 EPA/Play)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text.y = element_text(size = 10),
    legend.position = "right"
  )



library(dplyr)
library(readr)
library(stringr)
library(nflreadr)
library(ggplot2)
library(scales)
library(ggrepel)
library(tidyr)






# Load contracts dataset
contracts <- read_csv("contracts.csv", show_col_types = FALSE)

# Load cashflow dataset
cashflow <- read_csv("cashflow.csv", show_col_types = FALSE)

cat("✓ Contract data loaded!\n")
cat("  Contracts rows:", nrow(contracts), "\n")
cat("  Cashflow rows:", nrow(cashflow), "\n\n")


cat("Cleaning contract data...\n")

# Clean column names
contracts <- contracts %>%
  rename(
    player = Player,
    position = `Pos.`,
    team = Team,
    total_value = `Total Value`,
    apy = APY,
    total_guaranteed = `Total Guaranteed`,
    avg_guarantee_per_year = `Avg. Guarantee/Year`,
    pct_guaranteed = `% Guaranteed`
  )

cashflow <- cashflow %>%
  rename(
    player = Player,
    team = Team,
    position = Position,
    total = Total,
    apy = APY,
    year_0 = `Year 0`,
    year_1 = `Year 1`,
    year_2 = `Year 2`,
    year_3 = `Year 3`,
    year_4 = `Year 4`
  )

# Function to clean money strings
clean_money <- function(x) {
  x %>%
    str_remove_all("\\$") %>%
    str_remove_all(",") %>%
    str_trim() %>%
    parse_number()
}

# Convert to numeric
contracts <- contracts %>%
  mutate(
    total_value_num = clean_money(total_value),
    apy_num = clean_money(apy),
    total_guaranteed_num = clean_money(total_guaranteed),
    avg_guarantee_per_year_num = clean_money(avg_guarantee_per_year),
    pct_guaranteed_num = parse_number(pct_guaranteed)
  )

cashflow <- cashflow %>%
  mutate(
    total_num = clean_money(total),
    apy_num = clean_money(apy),
    year_0_num = ifelse(year_0 == "--", NA, clean_money(year_0)),
    year_1_num = ifelse(year_1 == "--", NA, clean_money(year_1)),
    year_2_num = ifelse(year_2 == "--", NA, clean_money(year_2)),
    year_3_num = ifelse(year_3 == "--", NA, clean_money(year_3)),
    year_4_num = ifelse(year_4 == "--", NA, clean_money(year_4))
  )

# Join contracts with cashflow
contract_data_combined <- contracts %>%
  left_join(
    cashflow %>% select(player, team, year_0_num, year_1_num, year_2_num, year_3_num, year_4_num),
    by = c("player", "team")
  )

# Calculate contract metrics
contract_data_combined <- contract_data_combined %>%
  mutate(
    # Front-loading metric
    safe_money = coalesce(year_0_num, 0) + coalesce(year_1_num, 0),
    front_load_pct = safe_money / total_value_num,
    
    # Contract vulnerability (higher = more at risk)
    contract_vulnerability = (1 - front_load_pct) * (1 - pct_guaranteed_num / 100),
    
    # Contract type
    contract_type = ifelse(pct_guaranteed_num >= 95, "Rookie", "Veteran")
  )

cat("Loading snap counts...\n")
snaps_all <- bind_rows(
  load_snap_counts(2022) %>% mutate(season = 2022),
  load_snap_counts(2023) %>% mutate(season = 2023),
  load_snap_counts(2024) %>% mutate(season = 2024)
)

cat("Loading player stats...\n")
stats_all <- bind_rows(
  load_player_stats(2022) %>% mutate(season = 2022),
  load_player_stats(2023) %>% mutate(season = 2023),
  load_player_stats(2024) %>% mutate(season = 2024)
)

cat("Loading rosters...\n")
rosters_all <- bind_rows(
  load_rosters(2022) %>% mutate(season = 2022),
  load_rosters(2023) %>% mutate(season = 2023),
  load_rosters(2024) %>% mutate(season = 2024)
) %>%
  select(season, gsis_id, full_name, position) %>%
  distinct()

cat("Loading injury data...\n")
injuries_all <- bind_rows(
  load_injuries(2022) %>% mutate(season = 2022),
  load_injuries(2023) %>% mutate(season = 2023),
  load_injuries(2024) %>% mutate(season = 2024)
)


# Aggregate snaps
player_snaps <- snaps_all %>%
  group_by(player, season, position) %>%
  summarise(
    total_offense_snaps = sum(offense_snaps, na.rm = TRUE),
    total_defense_snaps = sum(defense_snaps, na.rm = TRUE),
    total_st_snaps = sum(st_snaps, na.rm = TRUE),
    games = n_distinct(week),
    .groups = "drop"
  ) %>%
  mutate(total_snaps = total_offense_snaps + total_defense_snaps + total_st_snaps)

# Aggregate touches
player_touches <- stats_all %>%
  group_by(player_id, season) %>%
  summarise(
    total_carries = sum(carries, na.rm = TRUE),
    total_targets = sum(targets, na.rm = TRUE),
    total_receptions = sum(receptions, na.rm = TRUE),
    .groups = "drop"
  )

# Join using roster as bridge
snaps_with_gsis <- player_snaps %>%
  left_join(
    rosters_all %>% select(season, full_name, gsis_id),
    by = c("player" = "full_name", "season"),
    relationship = "many-to-many"
  )

player_workload <- snaps_with_gsis %>%
  left_join(
    player_touches,
    by = c("gsis_id" = "player_id", "season")
  ) %>%
  mutate(
    carries = coalesce(total_carries, 0),
    targets = coalesce(total_targets, 0),
    receptions = coalesce(total_receptions, 0)
  )


# Standardize position function
standardize_position <- function(pos) {
  case_when(
    pos %in% c("T", "LT", "RT") ~ "OT",
    pos %in% c("G", "LG", "RG") ~ "OG",
    pos == "C" ~ "C",
    pos %in% c("DE", "EDGE") ~ "EDGE",
    pos %in% c("DT", "IDL", "NT") ~ "IDL",
    pos %in% c("LB", "ILB", "OLB", "MLB") ~ "LB",
    pos %in% c("CB", "DB") ~ "CB",
    pos %in% c("S", "SS", "FS") ~ "S",
    pos == "QB" ~ "QB",
    pos %in% c("RB", "HB") ~ "RB",
    pos == "FB" ~ "FB",
    pos == "WR" ~ "WR",
    pos == "TE" ~ "TE",
    TRUE ~ pos
  )
}

player_contact_scores <- player_workload %>%
  filter(total_snaps >= 500) %>%
  mutate(
    # Position weights
    position_weight = case_when(
      position %in% c("RB", "FB") ~ 3,
      position %in% c("T", "G", "C", "DT", "NT") ~ 2,
      position %in% c("LB", "TE", "DE") ~ 1.5,
      position %in% c("WR", "CB", "S", "DB", "SS", "FS") ~ 1,
      position == "QB" ~ 0.5,
      TRUE ~ 1
    ),
    
    # Contact score components
    snap_contact_points = total_snaps * position_weight,
    carry_contact_points = carries * 3,
    target_contact_points = targets * 1.5,
    
    # Total
    total_contact_score = snap_contact_points + carry_contact_points + target_contact_points,
    contact_score_per_game = total_contact_score / games,
    
    # Standardize positions
    position_std = standardize_position(position)
  )

# Aggregate by position
contact_by_position <- player_contact_scores %>%
  group_by(position_std) %>%
  summarise(
    n_players = n(),
    avg_snaps = mean(total_snaps),
    avg_carries = mean(carries),
    avg_targets = mean(targets),
    avg_contact_score = mean(total_contact_score),
    avg_contact_per_game = mean(contact_score_per_game),
    .groups = "drop"
  ) %>%
  rename(position = position_std)


injury_by_position <- injuries_all %>%
  mutate(
    position_std = standardize_position(position),
    severity_score = case_when(
      report_status == "Out" ~ 3,
      report_status == "Doubtful" ~ 2.5,
      report_status == "Questionable" ~ 2,
      report_status == "Probable" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(position_std) %>%
  summarise(
    total_injuries = n(),
    avg_severity = mean(severity_score, na.rm = TRUE),
    injury_burden_score = total_injuries * avg_severity / 1000,
    .groups = "drop"
  ) %>%
  rename(position = position_std)


contract_summary <- contract_data_combined %>%
  filter(
    contract_type == "Veteran",
    position %in% c("QB", "RB", "WR", "TE", "LT", "RT", "LG", "RG", "C",
                    "EDGE", "IDL", "LB", "CB", "S")
  ) %>%
  mutate(position_std = standardize_position(position)) %>%
  group_by(position_std) %>%
  summarise(
    n = n(),
    avg_total_gtd = mean(total_guaranteed_num, na.rm = TRUE),
    avg_vulnerability = mean(contract_vulnerability, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(position = position_std)

cat("Real contract data processed:\n")
print(contract_summary)




integrated_final <- contract_summary %>%
  left_join(injury_by_position, by = "position") %>%
  left_join(contact_by_position %>% select(position, avg_contact_score, avg_carries),
            by = "position") %>%
  mutate(
    exploitation_score = (avg_contact_score / 1000) *
      (coalesce(injury_burden_score, 0) + 1) /
      (avg_total_gtd / 1000000),
    risk_adjusted_value = avg_total_gtd /
      ((avg_contact_score / 1000) * (coalesce(injury_burden_score, 0) + 1))
  ) %>%
  arrange(desc(exploitation_score))

cat("Positions Ranked by Exploitation Score:\n")
print(integrated_final %>%
        select(position, n, avg_total_gtd, avg_contact_score, avg_carries,
               injury_burden_score, exploitation_score))


cat("\n\n*** KEY COMPARISONS  ***\n")

rb_final <- integrated_final %>% filter(position == "RB")
qb_final <- integrated_final %>% filter(position == "QB")
lb_final <- integrated_final %>% filter(position == "LB")

cat("\nRUNNING BACKS:\n")
cat("  Number of contracts:", rb_final$n, "\n")
cat("  Avg Guaranteed:", scales::dollar(rb_final$avg_total_gtd), "\n")
cat("  Avg Carries:", round(rb_final$avg_carries, 0), "\n")
cat("  Contact Score:", scales::comma(round(rb_final$avg_contact_score, 0)), "\n")
cat("  Injury Burden:", round(rb_final$injury_burden_score, 2), "\n")
cat("  Exploitation Score:", round(rb_final$exploitation_score, 2), "\n")

cat("\nQUARTERBACKS:\n")
cat("  Number of contracts:", qb_final$n, "\n")
cat("  Avg Guaranteed:", scales::dollar(qb_final$avg_total_gtd), "\n")
cat("  Contact Score:", scales::comma(round(qb_final$avg_contact_score, 0)), "\n")
cat("  Injury Burden:", round(qb_final$injury_burden_score, 2), "\n")
cat("  Exploitation Score:", round(qb_final$exploitation_score, 2), "\n")

cat("\nLINEBACKERS:\n")
cat("  Number of contracts:", lb_final$n, "\n")
cat("  Avg Guaranteed:", scales::dollar(lb_final$avg_total_gtd), "\n")
cat("  Contact Score:", scales::comma(round(lb_final$avg_contact_score, 0)), "\n")
cat("  Injury Burden:", round(lb_final$injury_burden_score, 2), "\n")
cat("  Exploitation Score:", round(lb_final$exploitation_score, 2), "\n")

cat("\n\n*** THE HEADLINE NUMBERS***\n")
cat("QB/RB Pay Ratio:", round(qb_final$avg_total_gtd / rb_final$avg_total_gtd, 1), "x\n")
cat("RB/QB Contact Ratio:", round(rb_final$avg_contact_score / qb_final$avg_contact_score, 1), "x\n")
cat("RB/QB Exploitation Ratio:", round(rb_final$exploitation_score / qb_final$exploitation_score, 0), "x\n")

## Visualizations

# Load all required packages
library(ggplot2)
library(ggrepel)
library(scales)
library(dplyr)
library(tidyr)

cat("✓ All visualization packages loaded\n\n")


exploitation_data <- integrated_final %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "OT", "OG", "C", "EDGE", "IDL", "LB", "CB", "S")) %>%
  arrange(desc(exploitation_score)) %>%
  mutate(
    position = factor(position, levels = position),
    highlight = ifelse(position %in% c("RB", "QB", "LB"), "yes", "no")
  )

chart1 <- ggplot(exploitation_data, aes(x = position, y = exploitation_score, fill = highlight)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("yes" = "#DC143C", "no" = "#4682B4"), guide = "none") +
  geom_text(aes(label = round(exploitation_score, 2)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(
    title = "Player Exploitation by Position",
    subtitle = "High Contact + High Injury + Low Pay = Exploitation\nRBs are 55x more exploited than QBs",
    x = "Position",
    y = "Exploitation Score",
    caption = "Source: NFL data (2022-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(chart1)
ggsave("chart1_exploitation_by_position.png", chart1, width = 10, height = 6, dpi = 300)
cat("✓ Saved: chart1_exploitation_by_position.png\n\n")



cat("Creating Chart 2: Contact Score vs Guaranteed Money...\n")

scatter_data <- integrated_final %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "OT", "OG", "C", "EDGE", "IDL", "LB", "CB", "S")) %>%
  mutate(
    label = ifelse(position %in% c("QB", "RB", "LB", "OT", "EDGE"), position, ""),
    color_group = case_when(
      position == "QB" ~ "QB (High Pay, Low Contact)",
      position == "RB" ~ "RB (Low Pay, High Contact)",
      position == "LB" ~ "LB (Low Pay, High Contact)",
      TRUE ~ "Other Positions"
    )
  )

chart2 <- ggplot(scatter_data, aes(x = avg_contact_score, y = avg_total_gtd / 1e6)) +
  geom_point(aes(color = color_group, size = exploitation_score), alpha = 0.7) +
  geom_text_repel(aes(label = label), size = 4, fontface = "bold",
                  max.overlaps = 20, box.padding = 0.5) +
  scale_color_manual(
    values = c(
      "QB (High Pay, Low Contact)" = "#2E8B57",
      "RB (Low Pay, High Contact)" = "#DC143C",
      "LB (Low Pay, High Contact)" = "#FF6347",
      "Other Positions" = "#4682B4"
    )
  ) +
  scale_size_continuous(range = c(3, 12), guide = "none") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "The Exploitation Gap: Contact Score vs. Guaranteed Money",
    subtitle = "Bubble size = Exploitation Score | RBs have high contact but low pay",
    x = "Average Contact Score (Higher = More Physical Workload)",
    y = "Average Guaranteed Money",
    color = "Position Group",
    caption = "Source: NFL data (2022-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(chart2)
ggsave("chart2_contact_vs_pay.png", chart2, width = 12, height = 8, dpi = 300)
cat("✓ Saved: chart2_contact_vs_pay.png\n\n")



rb_qb_comparison <- integrated_final %>%
  filter(position %in% c("RB", "QB")) %>%
  select(position, avg_total_gtd, avg_contact_score, injury_burden_score, exploitation_score) %>%
  pivot_longer(cols = -position, names_to = "metric", values_to = "value") %>%
  mutate(
    metric_label = case_when(
      metric == "avg_total_gtd" ~ "Avg Guaranteed\n(Millions)",
      metric == "avg_contact_score" ~ "Contact Score",
      metric == "injury_burden_score" ~ "Injury Burden",
      metric == "exploitation_score" ~ "Exploitation Score",
      TRUE ~ metric
    ),
    display_value = case_when(
      metric == "avg_total_gtd" ~ value / 1e6,
      TRUE ~ value
    )
  )

chart3 <- ggplot(rb_qb_comparison, aes(x = metric_label, y = display_value, fill = position)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = round(display_value, 1)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("RB" = "#DC143C", "QB" = "#2E8B57")) +
  labs(
    title = "Running Backs vs Quarterbacks: The Exploitation Story",
    subtitle = "QBs earn 8.8x more despite 4.2x less contact and lower injury burden",
    x = "",
    y = "Value",
    fill = "Position",
    caption = "Source: NFL data (2022-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(chart3)
ggsave("chart3_rb_vs_qb.png", chart3, width = 10, height = 7, dpi = 300)
cat("✓ Saved: chart3_rb_vs_qb.png\n\n")

cat("Creating Chart 4: Injury Burden by Position...\n")

injury_data <- integrated_final %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "OT", "OG", "C", "EDGE", "IDL", "LB", "CB", "S")) %>%
  filter(!is.na(injury_burden_score)) %>%
  arrange(desc(injury_burden_score)) %>%
  mutate(
    position = factor(position, levels = position),
    highlight = ifelse(position %in% c("RB", "LB", "QB"), "yes", "no")
  )

chart4 <- ggplot(injury_data, aes(x = position, y = injury_burden_score, fill = highlight)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("yes" = "#DC143C", "no" = "#4682B4"), guide = "none") +
  geom_text(aes(label = round(injury_burden_score, 2)),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  labs(
    title = "Injury Burden by Position",
    subtitle = "Injury Burden = Severity × Frequency (normalized)",
    x = "Position",
    y = "Injury Burden Score",
    caption = "Source: NFL injury reports (2022-2024)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    axis.text.x = element_text(size = 11, face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(chart4)
ggsave("chart4_injury_burden.png", chart4, width = 10, height = 6, dpi = 300)
cat("✓ Saved: chart4_injury_burden.png\n\n")


cat("Creating Chart 5: Exploitation Quadrant Analysis...\n")

quadrant_data <- integrated_final %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "OT", "OG", "C", "EDGE", "IDL", "LB", "CB", "S")) %>%
  mutate(
    label = position,
    color_group = case_when(
      position == "QB" ~ "Protected (Low Contact, High Pay)",
      position %in% c("RB", "LB") ~ "EXPLOITED (High Contact, Low Pay)",
      avg_contact_score > 1500 & avg_total_gtd > 5e6 ~ "Fair Value (High Contact, High Pay)",
      TRUE ~ "Other"
    )
  )

median_contact <- median(quadrant_data$avg_contact_score, na.rm = TRUE)
median_pay <- median(quadrant_data$avg_total_gtd, na.rm = TRUE)

chart5 <- ggplot(quadrant_data, aes(x = avg_contact_score, y = avg_total_gtd / 1e6)) +
  geom_vline(xintercept = median_contact, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = median_pay / 1e6, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 700, y = 30, label = "Low Contact\nHigh Pay",
           color = "gray40", size = 3.5, fontface = "italic") +
  annotate("text", x = 2200, y = 30, label = "High Contact\nHigh Pay",
           color = "gray40", size = 3.5, fontface = "italic") +
  annotate("text", x = 700, y = 5, label = "Low Contact\nLow Pay",
           color = "gray40", size = 3.5, fontface = "italic") +
  annotate("text", x = 2200, y = 5, label = "EXPLOITED\nHigh Contact, Low Pay",
           color = "#DC143C", size = 4, fontface = "bold") +
  geom_point(aes(color = color_group, size = exploitation_score), alpha = 0.7) +
  geom_text_repel(aes(label = label, color = color_group),
                  size = 4, fontface = "bold", max.overlaps = 20) +
  scale_color_manual(
    values = c(
      "Protected (Low Contact, High Pay)" = "#2E8B57",
      "EXPLOITED (High Contact, Low Pay)" = "#DC143C",
      "Fair Value (High Contact, High Pay)" = "#4682B4",
      "Other" = "gray60"
    )
  ) +
  scale_size_continuous(range = c(3, 12), guide = "none") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "The Exploitation Quadrant: Who Bears the Risk?",
    subtitle = "Bubble size = Exploitation Score | Bottom-right = Exploited positions",
    x = "Average Contact Score",
    y = "Average Guaranteed Money (Millions)",
    color = "",
    caption = "Quadrant lines = median values"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

print(chart5)
ggsave("chart5_exploitation_quadrant.png", chart5, width = 12, height = 8, dpi = 300)
cat("✓ Saved: chart5_exploitation_quadrant.png\n\n")



cat("Creating Chart 6: RB Contact Score Breakdown...\n")

# Calculate average RB contact components
rb_contact_detail <- player_contact_scores %>%
  filter(position_std == "RB") %>%
  summarise(
    snap_points = mean(snap_contact_points),
    carry_points = mean(carry_contact_points),
    target_points = mean(target_contact_points)
  ) %>%
  pivot_longer(everything(), names_to = "component", values_to = "points") %>%
  mutate(
    component_label = case_when(
      component == "snap_points" ~ "Base Snaps",
      component == "carry_points" ~ "Carries",
      component == "target_points" ~ "Targets",
      TRUE ~ component
    ),
    percentage = points / sum(points) * 100
  )

chart6 <- ggplot(rb_contact_detail, aes(x = "", y = points, fill = component_label)) +
  geom_col(width = 1, color = "white", linewidth = 2) +
  geom_text(aes(label = paste0(round(points, 0), " pts\n", round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), color = "white",
            size = 4.5, fontface = "bold") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Base Snaps" = "#8B0000", "Carries" = "#DC143C", "Targets" = "#FF6347")) +
  labs(
    title = "Running Back Contact Score Breakdown",
    subtitle = "Average RB accumulates 2,674 contact points per season",
    fill = "Contact Type",
    caption = "Based on 108 RBs with 500+ snaps (2022-2024)"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30", margin = margin(b = 20)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10)
  )

print(chart6)
ggsave("chart6_rb_contact_breakdown.png", chart6, width = 9, height = 9, dpi = 300)
cat("✓ Saved: chart6_rb_contact_breakdown.png\n\n")

# Salary Cap Regression and Prediction

library(nflreadr)
contracts <- nflreadr::load_contracts()
cap_data <- data.frame(year = c(1994:2009, 2011:2025), cap = c(34.6, 37.1, 40.8, 41.5, 52.4, 57.3, 63.2, 67.4, 71.1, 75, 80.6, 85.5, 102, 109, 116, 128, 120, 120.6, 123, 133, 143.28, 155.27, 167, 177, 188.2, 198.2, 182.5, 208.2, 224.8, 255.4, 279.2))
cap_data_new <- cap_data %>% filter(year > 2010)
fit <- lm(log(cap) ~ year, data = cap_data_new)
cap_data_new$cap_hat <- exp(predict(fit, newdata = cap_data_new))
future <- data.frame(year = 2026:2028) %>%
  mutate(
    cap = exp(predict(fit, newdata = .)),
    type = "Predicted"
  )

obs <- cap_data_new %>%
  mutate(type = "Observed")
line_obs <- data.frame(year = sort(unique(cap_data_new$year))) %>%
  mutate(cap_hat = exp(predict(fit, newdata = .)))
plot_df <- bind_rows(obs, future)
ggplot() +
  geom_point(data = cap_data_new, aes(year, cap), color = "black", size = 3) +
  geom_line(data = line_obs, aes(year, cap_hat), color = "black", linewidth = 1) +
  geom_point(data = future, aes(year, cap), color = "red", size = 3) +
  geom_line(data = future, aes(year, cap), color = "red", linetype = "dashed", linewidth = 1) +
  labs(x = "Year", y = "Salary Cap (Millions)", title = "NFL Salary Cap (2011–2025): Actual Values and Model Fit", subtitle = "Points in red include our predictions for the next three years based on our model") +
  theme_minimal()
install.packages("janitor")
library(rvest)
library(dplyr)
library(purrr)
library(janitor)
library(stringr)

url <- "https://overthecap.com/salary-cap-space"
pg  <- read_html(url)

money_to_num <- function(x) {
  x <- str_trim(as.character(x))
  neg <- str_detect(x, "^\\(")
  x <- str_replace_all(x, "[\\$,\\(\\),]", "")
  val <- suppressWarnings(as.numeric(x))
  ifelse(neg, -val, val)
}

tables <- pg %>%
  html_elements("table") %>%
  map(~ html_table(.x, fill = TRUE) %>% clean_names())

cap_tabs <- tables %>%
  keep(~ all(c("team", "cap_space", "active_cap_spending", "dead_money") %in% names(.x)))

years <- 2025:(2025 + length(cap_tabs) - 1)

cap_all_teams <- map2_dfr(cap_tabs, years, \(df, yr) {
  
  out <- df %>%
    transmute(
      year = yr,
      team = team,
      cap_space = money_to_num(cap_space),
      active_cap_spending = money_to_num(active_cap_spending),
      dead_money = money_to_num(dead_money),
      all_expenses = active_cap_spending + dead_money,
      team_salary_cap = cap_space + active_cap_spending + dead_money
    )
  
  # add optional columns if present
  if ("effective_cap_space" %in% names(df)) {
    out <- out %>% mutate(effective_cap_space = money_to_num(df$effective_cap_space))
  }
  if ("n_active" %in% names(df)) {
    out <- out %>% mutate(n_active = suppressWarnings(as.integer(df$n_active)))
  }
  
  out
})

cap_all_teams_predict <- cap_all_teams %>% filter(year < 2027)
cap_predictions <- left_join(cap_all_teams_predict, future, by = "year")
cap_predictions_pie <- data.frame(cap_space = cap_all_teams_predict$cap_space/cap_all_teams_predict$team_salary_cap,
                                  active_cap_spending = cap_all_teams_predict$active_cap_spending/cap_all_teams_predict$team_salary_cap,
                                  dead_money = cap_all_teams_predict$dead_money/cap_all_teams_predict$team_salary_cap,
                                  year = cap_all_teams_predict$year)
cap_predictions_pie <- cap_predictions_pie %>% group_by(year) %>% summarize(cap_space = mean(cap_space), active_cap_spending = mean(active_cap_spending), dead_money = mean(dead_money))
library(dplyr)
library(tidyr)
library(ggplot2)

df <- tibble::tribble(
  ~year, ~cap_space, ~active_cap_spending, ~dead_money,
  2025, 0.04255064, 0.7872719, 0.17017747,
  2026, 0.10058164, 0.8511230, 0.04829533
)

df_long <- df %>%
  pivot_longer(
    cols = c(cap_space, active_cap_spending, dead_money),
    names_to = "category",
    values_to = "percent"
  ) %>%
  mutate(
    category = factor(category, levels = c("cap_space","active_cap_spending","dead_money")),
    category = recode(category,
                      cap_space = "Cap space",
                      active_cap_spending = "Active cap spending",
                      dead_money = "Dead money")
  )

cols <- c(
  "Cap space" = "#00205B",          # Rice Blue
  "Active cap spending" = "#BF5700",# Burnt orange
  "Dead money" = "#500000"          # A&M maroon
)

ggplot(df_long, aes(x = "", y = percent, fill = category)) +
  geom_col(width = 1, color = "white", linewidth = 0.25) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = scales::percent(percent, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  facet_wrap(~ year) +
  scale_fill_manual(values = cols) +
  labs(title = "Cap Allocation Breakdown", fill = "") +
  theme_void() +
  theme(legend.position = "right") +
  
  geom_text(
    aes(label = scales::percent(percent, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 4
  )