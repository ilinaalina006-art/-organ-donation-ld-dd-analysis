# Organ Donation Analysis
install.packages(c("readxl", "janitor", "tidyverse", "scales", "ggrepel", "patchwork"))

library(readxl)
library(janitor)
library(tidyr)
library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)

# PART 1 DATA CLEANING

df <- read_excel("~/Downloads/GODT_downloads.xlsx")

# drop unused columns
df <- df[, !names(df) %in% c(
  "Total Utilized DD", "Utilized DBD", "Utilized DCD", "DOMINO Liver Tx",
  "Total Heart TX", "Pancreas Tx", "Kidney Pancreas Tx", "Small Bowel Tx"
)]

# standardize column names
df <- clean_names(df)

# trim whitespace in character columns
char_cols <- c("region", "country")
for (col in char_cols) df[[col]] <- trimws(df[[col]])

# unify all missing-value markers -> NA
missing_values <- c("", " ", "NA", "N/A", "-", "--")
for (col in names(df)) {
  if (is.character(df[[col]])) {
    df[[col]] <- trimws(df[[col]])
    df[[col]][df[[col]] %in% missing_values] <- NA
  }
}

# catch "not applicable" variants -> NA
for (col in names(df)) {
  if (is.character(df[[col]])) {
    temp <- trimws(tolower(df[[col]]))
    df[[col]][temp %in% c("not applicable", "not_applicable", "n.a.")] <- NA
  }
}

# check for duplicate country-year rows
sum(duplicated(df[c("country", "reportyear")]))  # should be 0

# manual inspection of country/region names
sort(unique(df$country))
sort(unique(df$region))

# fix known typo
df$country <- dplyr::recode(df$country, "Untied Kingdom" = "United Kingdom")

# coerce numeric columns, strip commas from thousands separators
num_cols <- c(
  "reportyear", "population",
  "total_actual_dd", "actual_dbd", "actual_dcd",
  "dd_kidney_tx", "ld_kidney_tx", "total_kidney_tx",
  "dd_liver_tx",  "ld_liver_tx",  "total_liver_tx",
  "dd_lung_tx",   "ld_lung_tx",   "total_lung_tx"
)

for (col in num_cols) {
  df[[col]] <- as.character(df[[col]])
  df[[col]] <- gsub(",", "", df[[col]])
  df[[col]] <- trimws(df[[col]])
  df[[col]][df[[col]] %in% c("", "NA", "N/A", "-", "--")] <- NA
  df[[col]] <- as.numeric(df[[col]])
}

df$reportyear <- as.integer(df$reportyear)

#real zeros vs missing values
zero_vs_na <- data.frame(
  column   = num_cols,
  n_zero   = sapply(df[num_cols], function(x) sum(x == 0,   na.rm = TRUE)),
  n_na     = sapply(df[num_cols], function(x) sum(is.na(x))),
  n_nonzero = sapply(df[num_cols], function(x) sum(!is.na(x) & x != 0))
)
write.csv(zero_vs_na, "zero_vs_na_summary.csv", row.names = FALSE)

# total should equal dd + ld for each organ, flag mismatches

df$kidney_check <- with(df, total_kidney_tx == dd_kidney_tx + ld_kidney_tx)
df$liver_check  <- with(df, total_liver_tx  == dd_liver_tx  + ld_liver_tx)
df$lung_check   <- with(df, total_lung_tx   == dd_lung_tx   + ld_lung_tx)
df$dd_check     <- with(df, total_actual_dd == actual_dbd   + actual_dcd)

df$kidney_diff <- df$total_kidney_tx - (df$dd_kidney_tx + df$ld_kidney_tx)
df$liver_diff  <- df$total_liver_tx  - (df$dd_liver_tx  + df$ld_liver_tx)
df$lung_diff   <- df$total_lung_tx   - (df$dd_lung_tx   + df$ld_lung_tx)
df$dd_diff     <- df$total_actual_dd - (df$actual_dbd   + df$actual_dcd)

#extract mismatch rows per organ, sorted by size of discrepancy
kidney_mismatch <- df[!is.na(df$kidney_check) & !df$kidney_check,
                      c("region","country","reportyear","dd_kidney_tx","ld_kidney_tx","total_kidney_tx","kidney_diff")]
liver_mismatch  <- df[!is.na(df$liver_check)  & !df$liver_check,
                      c("region","country","reportyear","dd_liver_tx","ld_liver_tx","total_liver_tx","liver_diff")]
lung_mismatch   <- df[!is.na(df$lung_check)   & !df$lung_check,
                      c("region","country","reportyear","dd_lung_tx","ld_lung_tx","total_lung_tx","lung_diff")]
dd_mismatch     <- df[!is.na(df$dd_check)     & !df$dd_check,
                      c("region","country","reportyear","total_actual_dd","actual_dbd","actual_dcd","dd_diff")]

kidney_mismatch <- kidney_mismatch[order(abs(kidney_mismatch$kidney_diff), decreasing = TRUE), ]
liver_mismatch  <- liver_mismatch[order(abs(liver_mismatch$liver_diff),   decreasing = TRUE), ]
lung_mismatch   <- lung_mismatch[order(abs(lung_mismatch$lung_diff),     decreasing = TRUE), ]
dd_mismatch     <- dd_mismatch[order(abs(dd_mismatch$dd_diff),           decreasing = TRUE), ]

write.csv(kidney_mismatch, "kidney_mismatch.csv", row.names = FALSE)
write.csv(liver_mismatch,  "liver_mismatch.csv",  row.names = FALSE)
write.csv(lung_mismatch,   "lung_mismatch.csv",   row.names = FALSE)
write.csv(dd_mismatch,     "dd_mismatch.csv",     row.names = FALSE)

#mismatch rates
kidney_error_rate <- round(sum(!df$kidney_check, na.rm=T) / sum(!is.na(df$kidney_check)) * 100, 2)
liver_error_rate  <- round(sum(!df$liver_check,  na.rm=T) / sum(!is.na(df$liver_check))  * 100, 2)
lung_error_rate   <- round(sum(!df$lung_check,   na.rm=T) / sum(!is.na(df$lung_check))   * 100, 2)
dd_error_rate     <- round(sum(!df$dd_check,     na.rm=T) / sum(!is.na(df$dd_check))     * 100, 2)

cat(sprintf("Mismatch rates — kidney: %s%%, liver: %s%%, lung: %s%%, dd: %s%%\n",
            kidney_error_rate, liver_error_rate, lung_error_rate, dd_error_rate))

# per-country liver mismatch summary
liver_checked_by_country  <- table(df$country[!is.na(df$liver_check)])
liver_mismatch_by_country <- table(liver_mismatch$country)

liver_country_summary <- data.frame(
  country    = names(liver_checked_by_country),
  n_checked  = as.numeric(liver_checked_by_country),
  n_mismatch = as.numeric(liver_mismatch_by_country[names(liver_checked_by_country)])
)
liver_country_summary$n_mismatch[is.na(liver_country_summary$n_mismatch)] <- 0
liver_country_summary$error_rate <- liver_country_summary$n_mismatch / liver_country_summary$n_checked
liver_country_summary <- liver_country_summary[order(liver_country_summary$error_rate, decreasing = TRUE), ]
write.csv(liver_country_summary, "liver_country_summary.csv", row.names = FALSE)

overall_country_table <- df[, c(
  "region", "country", "reportyear", "population",
  "total_actual_dd", "actual_dbd", "actual_dcd",
  "total_kidney_tx", "total_liver_tx", "total_lung_tx"
)]
overall_country_table$na_count <- rowSums(is.na(overall_country_table))

overall_missing_by_country <- aggregate(na_count ~ country, data = overall_country_table, FUN = sum)
overall_rows_by_country    <- aggregate(reportyear ~ country, data = overall_country_table, FUN = length)
names(overall_rows_by_country)[2] <- "n_rows"

overall_country_summary <- merge(overall_missing_by_country, overall_rows_by_country, by = "country")
overall_country_summary$avg_na_per_row <- overall_country_summary$na_count / overall_country_summary$n_rows
overall_country_summary <- overall_country_summary[order(overall_country_summary$avg_na_per_row), ]

write.csv(overall_country_table,    "country_year_overview.csv",  row.names = FALSE)
write.csv(overall_country_summary,  "country_completeness.csv",   row.names = FALSE)


donor_type_table <- df[, c(
  "region", "country", "reportyear",
  "dd_kidney_tx", "ld_kidney_tx",
  "dd_liver_tx",  "ld_liver_tx",
  "dd_lung_tx",   "ld_lung_tx"
)]

donor_type_table <- tidyr::pivot_longer(
  donor_type_table,
  cols         = c(dd_kidney_tx, ld_kidney_tx, dd_liver_tx, ld_liver_tx, dd_lung_tx, ld_lung_tx),
  names_to     = c("donor_type", "organ"),
  names_pattern = "(dd|ld)_(kidney|liver|lung)_tx",
  values_to    = "tx_count"
)
donor_type_table$donor_type <- toupper(donor_type_table$donor_type)
write.csv(donor_type_table, "country_year_organ_tx.csv", row.names = FALSE)

# donor-type missingness by country
donor_missing_by_country <- aggregate(is.na(tx_count) ~ country, data = donor_type_table, FUN = sum)
names(donor_missing_by_country)[2] <- "na_count"
donor_rows_by_country <- aggregate(tx_count ~ country, data = donor_type_table, FUN = length)
names(donor_rows_by_country)[2] <- "n_rows"

donor_country_summary <- merge(donor_missing_by_country, donor_rows_by_country, by = "country")
donor_country_summary$avg_na_per_row <- donor_country_summary$na_count / donor_country_summary$n_rows
donor_country_summary <- donor_country_summary[order(donor_country_summary$avg_na_per_row), ]

# combined completeness table
country_completeness_summary <- merge(
  overall_country_summary[, c("country","na_count","n_rows","avg_na_per_row")],
  donor_country_summary[,   c("country","na_count","n_rows","avg_na_per_row")],
  by = "country", suffixes = c("_overall", "_donor")
)
country_completeness_summary <- country_completeness_summary[
  order(country_completeness_summary$avg_na_per_row_overall +
          country_completeness_summary$avg_na_per_row_donor), ]

best_list <- country_completeness_summary$country[
  (country_completeness_summary$avg_na_per_row_overall +
     country_completeness_summary$avg_na_per_row_donor) < 0.3
]

best_countries <- country_completeness_summary[country_completeness_summary$country %in% best_list, ]
write.csv(best_countries, "best_countries_for_analysis.csv", row.names = FALSE)

# LD share summary for the shortlisted countries
best_df <- df[df$country %in% best_list, ]
ld_summary <- aggregate(
  cbind(ld_kidney_tx, ld_liver_tx, ld_lung_tx, total_kidney_tx, total_liver_tx, total_lung_tx) ~ country,
  data = best_df, FUN = function(x) sum(x, na.rm = TRUE)
)
ld_summary$total_ld       <- with(ld_summary, ld_kidney_tx + ld_liver_tx + ld_lung_tx)
ld_summary$total_tx       <- with(ld_summary, total_kidney_tx + total_liver_tx + total_lung_tx)
ld_summary$ld_share       <- ld_summary$total_ld / ld_summary$total_tx
ld_summary$kidney_ld_share <- ld_summary$ld_kidney_tx / ld_summary$total_kidney_tx
ld_summary <- ld_summary[order(ld_summary$kidney_ld_share, decreasing = TRUE), ]
write.csv(ld_summary, "ld_summary_best_countries.csv", row.names = FALSE)

# kidney LD share across all countries
kidney_ld_all <- aggregate(
  cbind(ld_kidney_tx, total_kidney_tx) ~ country,
  data = df, FUN = function(x) sum(x, na.rm = TRUE)
)
kidney_ld_all$kidney_ld_share <- kidney_ld_all$ld_kidney_tx / kidney_ld_all$total_kidney_tx
kidney_ld_all <- kidney_ld_all[!is.na(kidney_ld_all$kidney_ld_share) &
                                 is.finite(kidney_ld_all$kidney_ld_share), ]

country_region <- unique(df[, c("country", "region")])
kidney_ld_all  <- merge(kidney_ld_all, country_region, by = "country", all.x = TRUE)

country_rows   <- aggregate(reportyear ~ country, data = df, FUN = length)
names(country_rows)[2] <- "n_rows"
kidney_ld_all  <- merge(kidney_ld_all, country_rows, by = "country", all.x = TRUE)
kidney_ld_all_filtered <- kidney_ld_all[kidney_ld_all$n_rows >= 20, ]
kidney_ld_all_filtered <- kidney_ld_all_filtered[order(kidney_ld_all_filtered$kidney_ld_share, decreasing = TRUE), ]
write.csv(kidney_ld_all_filtered, "kidney_ld_all_countries.csv", row.names = FALSE)


# PART 2 ANALYSIS & VISUALISATION


DATA_DIR <- "./"

overview  <- read_csv(file.path(DATA_DIR, "country_year_overview.csv"))
organ_tx  <- read_csv(file.path(DATA_DIR, "country_year_organ_tx.csv"))
kidney_iss <- read_csv(file.path(DATA_DIR, "kidney_mismatch.csv"))
liver_iss  <- read_csv(file.path(DATA_DIR, "liver_mismatch.csv"))
dd_iss     <- read_csv(file.path(DATA_DIR, "dd_mismatch.csv"))

# back to wide so each row is country × year
organ_wide <- organ_tx |>
  pivot_wider(
    id_cols    = c(region, country, reportyear),
    names_from = c(donor_type, organ),
    values_from = tx_count,
    names_glue = "{tolower(donor_type)}_{organ}"
  )

#main analytical dataset
df2 <- overview |>
  left_join(
    organ_wide |> select(country, reportyear,
                         dd_kidney, ld_kidney, dd_liver, ld_liver, dd_lung, ld_lung),
    by = c("country", "reportyear")
  ) |>
  mutate(
    actual_dd_pmp   = total_actual_dd / population,   # population already in millions
    actual_dbd_pmp  = actual_dbd / population,
    actual_dcd_pmp  = actual_dcd / population,
    ld_share_kidney = ld_kidney / total_kidney_tx,
    ld_share_liver  = ld_liver  / total_liver_tx
  )

# flag rows with arithmetic issues
kidney_issue_keys <- kidney_iss |> select(country, reportyear)
liver_issue_keys  <- liver_iss  |> select(country, reportyear)
dd_issue_keys     <- dd_iss     |> select(country, reportyear)

df2 <- df2 |>
  mutate(
    has_kidney_issue = paste(country, reportyear) %in%
      paste(kidney_issue_keys$country, kidney_issue_keys$reportyear),
    has_liver_issue  = paste(country, reportyear) %in%
      paste(liver_issue_keys$country,  liver_issue_keys$reportyear),
    has_dd_issue     = paste(country, reportyear) %in%
      paste(dd_issue_keys$country,     dd_issue_keys$reportyear)
  )

write_csv(df2, "analytical_dataset.csv")

# Clean
kidney_clean <- df2 |>
  filter(!has_kidney_issue,
         !is.na(total_kidney_tx), !is.na(ld_kidney),
         ld_share_kidney >= 0, ld_share_kidney <= 1) |>
  filter(!(country == "France" & reportyear <= 2002))  # France 2000-2002: DD not yet reported, LD share = 100% is an artefact

liver_clean <- df2 |>
  filter(!has_liver_issue,
         !is.na(total_liver_tx), !is.na(ld_liver),
         ld_share_liver >= 0, ld_share_liver <= 1)

#
# Thresholds:>=60% = structurally dependent on LD; <30% = DD-dominant; between = mixed
kidney_stats <- kidney_clean |>
  group_by(country, region) |>
  summarise(
    years_kidney        = n(),
    avg_ld_share_kidney = mean(ld_share_kidney, na.rm = TRUE),
    avg_dd_pmp          = mean(actual_dd_pmp,   na.rm = TRUE),
    avg_total_kidney_tx = mean(total_kidney_tx, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(years_kidney >= 10) |>   # min 10 yrs for stable average across 2000-2024
  mutate(
    typology = case_when(
      avg_ld_share_kidney >= 0.6 ~ "Living-Dominant",
      avg_ld_share_kidney >= 0.3 ~ "Mixed",
      TRUE                       ~ "Deceased-Dominant"
    ),
    typology = factor(typology, levels = c("Living-Dominant", "Mixed", "Deceased-Dominant"))
  ) |>
  arrange(desc(avg_ld_share_kidney))

liver_stats <- liver_clean |>
  group_by(country, region) |>
  summarise(
    years_liver        = n(),
    avg_ld_share_liver = mean(ld_share_liver, na.rm = TRUE),
    avg_total_liver_tx = mean(total_liver_tx, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(years_liver >= 8) |>
  arrange(desc(avg_ld_share_liver))

# LD share kidney ~ DD PMP
corr_data <- kidney_stats |> filter(!is.na(avg_dd_pmp), !is.na(avg_ld_share_kidney))

corr_test <- cor.test(corr_data$avg_ld_share_kidney, corr_data$avg_dd_pmp, method = "pearson")
cat(sprintf("Correlation: r = %.3f, p = %.4f, n = %d\n",
            corr_test$estimate, corr_test$p.value, nrow(corr_data)))

# LD share kidney ~ LD share liver
both_organs <- kidney_stats |>
  inner_join(liver_stats |> select(country, avg_ld_share_liver, years_liver), by = "country") |>
  filter(!is.na(avg_ld_share_kidney), !is.na(avg_ld_share_liver))

kl_cor <- cor.test(both_organs$avg_ld_share_kidney, both_organs$avg_ld_share_liver, method = "pearson")
cat(sprintf("Kidney-liver correlation: r = %.3f, p = %.4f, n = %d\n",
            kl_cor$estimate, kl_cor$p.value, nrow(both_organs)))

# regression: LD share ~ DD PMP
lm_fit <- lm(avg_ld_share_kidney ~ avg_dd_pmp, data = corr_data)
lm_s   <- summary(lm_fit)
beta   <- coef(lm_fit)[["avg_dd_pmp"]]
ci     <- confint(lm_fit, "avg_dd_pmp", level = 0.95)
cat(sprintf(
  "OLS: beta = %.4f (SE %.4f), 95%% CI [%.4f, %.4f], R2 = %.3f\n",
  beta, coef(lm_s)["avg_dd_pmp","Std. Error"], ci[1], ci[2], lm_s$r.squared
))
cat(sprintf(
  "Interpretation: each additional DD/pmp is associated with %.2f pp lower LD kidney share.\n",
  abs(beta) * 100
))

# Iceland outlier note
kidney_stats |> filter(country == "Iceland") |>
  select(country, avg_ld_share_kidney, avg_dd_pmp, avg_total_kidney_tx, years_kidney) |>
  print()


cat("\nTop 20 — avg LD share kidney:\n")
kidney_stats |> head(20) |>
  select(country, region, avg_ld_share_kidney, avg_dd_pmp, typology, years_kidney) |>
  mutate(across(c(avg_ld_share_kidney, avg_dd_pmp), function(x) round(x, 3))) |> print(n = 20)

cat("\nTop 20 — avg DD PMP:\n")
kidney_stats |> arrange(desc(avg_dd_pmp)) |> head(20) |>
  select(country, region, avg_dd_pmp, avg_ld_share_kidney, typology) |>
  mutate(across(c(avg_dd_pmp, avg_ld_share_kidney), function(x) round(x, 3))) |> print(n = 20)

cat("\nTop 20 — avg LD share liver:\n")
liver_stats |> head(20) |>
  select(country, region, avg_ld_share_liver, avg_total_liver_tx, years_liver) |>
  mutate(avg_ld_share_liver = round(avg_ld_share_liver, 3)) |> print(n = 20)

cat("\nTypology breakdown:\n")
print(table(kidney_stats$typology))

cat("\nRegional summary:\n")
kidney_stats |>
  group_by(region) |>
  summarise(
    n_countries          = n(),
    avg_ld_share_kidney  = mean(avg_ld_share_kidney, na.rm = TRUE),
    avg_dd_pmp           = mean(avg_dd_pmp, na.rm = TRUE),
    pct_living_dominant  = mean(typology == "Living-Dominant") * 100
  ) |>
  arrange(desc(avg_ld_share_kidney)) |>
  mutate(across(where(is.numeric), function(x) round(x, 2))) |> print(n = Inf)

write_csv(kidney_stats, "kidney_country_stats_R.csv")
write_csv(liver_stats,  "liver_country_stats_R.csv")

typology_colors <- c(
  "Living-Dominant"   = "#C0504D",
  "Mixed"             = "#BF8F00",
  "Deceased-Dominant" = "#375623"
)
typology_fills <- c(
  "Living-Dominant"   = "#FCE4D6",
  "Mixed"             = "#FFF2CC",
  "Deceased-Dominant" = "#E2EFDA"
)

label_countries <- c(
  "Spain", "United States of America", "Portugal", "France", "Croatia",
  "Türkiye", "Iran (Islamic Republic of)", "Republic of Korea", "Japan",
  "India", "Pakistan", "Georgia", "Jordan", "Israel", "Norway"
)

# scatter LD share ~ DD PMP with OLS line
p_corr <- corr_data |>
  mutate(label = if_else(country %in% label_countries, country, NA_character_)) |>
  ggplot(aes(x = avg_dd_pmp, y = avg_ld_share_kidney, color = typology, fill = typology)) +
  geom_point(aes(size = avg_total_kidney_tx), alpha = 0.75, shape = 21, stroke = 0.5) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE,
              color = "#1F3864", fill = "#D6E4F7", linewidth = 0.8, alpha = 0.2) +
  geom_text_repel(aes(label = label), size = 2.8, max.overlaps = 20,
                  color = "grey20", segment.color = "grey60", segment.size = 0.3,
                  fontface = "italic") +
  annotate("text",
           x = max(corr_data$avg_dd_pmp, na.rm = TRUE) * 0.95, y = 0.98, hjust = 1,
           label = sprintf("r = %.3f  |  beta = %.3f  |  R² = %.2f  |  p < 0.001  |  n = %d",
                           corr_test$estimate, beta, lm_s$r.squared, nrow(corr_data)),
           size = 2.8, color = "#1F3864", fontface = "italic") +
  scale_color_manual(values = typology_colors, name = "Typology") +
  scale_fill_manual(values  = typology_colors, name = "Typology") +
  scale_size_continuous(range = c(2, 12), name = "Avg kidney tx/yr", labels = comma_format()) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  scale_x_continuous(labels = comma_format()) +
  labs(title    = "Living donation dependence vs. deceased donor capacity",
       subtitle = "Each bubble = one country (avg over ≥10 years). Bubble size = avg annual kidney transplants.",
       x = "Avg actual deceased donors per million population (DD PMP)",
       y = "Avg LD share of kidney transplants") +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey40", size = 9),
        legend.position = "right", panel.grid.minor = element_blank())

print(p_corr)

# top-30 kidney LD share
top30_kidney <- kidney_stats |> head(30) |>
  mutate(country = fct_reorder(country, avg_ld_share_kidney))

p_lollipop_kidney <- top30_kidney |>
  ggplot(aes(x = avg_ld_share_kidney, y = country, color = typology)) +
  geom_segment(aes(x = 0, xend = avg_ld_share_kidney, y = country, yend = country),
               linewidth = 0.7, alpha = 0.6) +
  geom_point(aes(size = avg_total_kidney_tx), alpha = 0.85) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  scale_color_manual(values = typology_colors, name = "Typology") +
  scale_size_continuous(range = c(2, 9), name = "Avg tx/yr", labels = comma_format()) +
  labs(title    = "Top 30 countries by average LD share — Kidney",
       subtitle = "Countries with ≥ 10 clean years of data",
       x = "Avg LD share of kidney transplants", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey40", size = 9),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8))

print(p_lollipop_kidney)

#top-25 DD PMP
top25_dd <- kidney_stats |> arrange(desc(avg_dd_pmp)) |> head(25) |>
  mutate(country = fct_reorder(country, avg_dd_pmp))

p_lollipop_dd <- top25_dd |>
  ggplot(aes(x = avg_dd_pmp, y = country, color = typology)) +
  geom_segment(aes(x = 0, xend = avg_dd_pmp, y = country, yend = country),
               linewidth = 0.7, alpha = 0.6) +
  geom_point(size = 3, alpha = 0.9) +
  scale_x_continuous(labels = comma_format()) +
  scale_color_manual(values = typology_colors, name = "Typology") +
  labs(title    = "Top 25 countries by avg deceased donors per million (DD PMP)",
       subtitle = "Countries with ≥ 10 clean years of data",
       x = "Avg DD PMP", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey40", size = 9),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8))

print(p_lollipop_dd)

#lollipop top-25 liver LD share
top25_liver <- liver_stats |> head(25) |>
  mutate(
    country       = fct_reorder(country, avg_ld_share_liver),
    liver_typology = case_when(
      avg_ld_share_liver >= 0.6 ~ "Living-Dominant",
      avg_ld_share_liver >= 0.3 ~ "Mixed",
      TRUE                      ~ "Deceased-Dominant"
    )
  )

p_lollipop_liver <- top25_liver |>
  ggplot(aes(x = avg_ld_share_liver, y = country, color = liver_typology)) +
  geom_segment(aes(x = 0, xend = avg_ld_share_liver, y = country, yend = country),
               linewidth = 0.7, alpha = 0.6) +
  geom_point(aes(size = avg_total_liver_tx), alpha = 0.85) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  scale_color_manual(values = typology_colors, name = "Typology") +
  scale_size_continuous(range = c(2, 9), name = "Avg liver tx/yr", labels = comma_format()) +
  labs(title    = "Top 25 countries by average LD share — Liver",
       subtitle = "Countries with ≥ 8 clean years of data",
       x = "Avg LD share of liver transplants", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey40", size = 9),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 8))

print(p_lollipop_liver)

# time trends for selected countries
highlight_countries <- c(
  "Türkiye", "Iran (Islamic Republic of)", "Republic of Korea",
  "Japan", "India", "Pakistan", "Georgia", "Spain",
  "United States of America", "France", "Norway", "Israel"
)

country_labels <- c(
  "Türkiye"                    = "Türkiye",
  "Iran (Islamic Republic of)" = "Iran",
  "Republic of Korea"          = "Korea",
  "Japan"                      = "Japan",
  "India"                      = "India",
  "Pakistan"                   = "Pakistan",
  "Georgia"                    = "Georgia",
  "Spain"                      = "Spain",
  "United States of America"   = "USA",
  "France"                     = "France",
  "Norway"                     = "Norway",
  "Israel"                     = "Israel"
)

ts_data   <- kidney_clean |>
  filter(country %in% highlight_countries) |>
  mutate(country_short = recode(country, !!!country_labels))

ts_labels <- ts_data |> group_by(country_short) |> filter(reportyear == max(reportyear))

line_colors <- c(
  "Türkiye" = "#C0504D", "Iran"    = "#E6A020", "Korea"  = "#375623",
  "Japan"   = "#7030A0", "India"   = "#F4A460", "Pakistan" = "#8B0000",
  "Georgia" = "#D64D4D", "Spain"   = "#2E75B6", "USA"    = "#1F3864",
  "France"  = "#70AD47", "Norway"  = "#00B0F0", "Israel" = "#FF9900"
)

p_trends <- ts_data |>
  ggplot(aes(x = reportyear, y = ld_share_kidney, color = country_short, group = country_short)) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 1.2, alpha = 0.6) +
  geom_text_repel(data = ts_labels, aes(label = country_short),
                  size = 2.8, nudge_x = 0.5, hjust = 0,
                  direction = "y", segment.size = 0.2,
                  segment.color = "grey70", max.overlaps = 20) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  scale_x_continuous(breaks = seq(2000, 2024, 4), limits = c(2000, 2028)) +
  scale_color_manual(values = line_colors, guide = "none") +
  labs(title    = "LD share of kidney transplants over time — selected countries",
       subtitle = "Only years without documented data inconsistencies",
       x = "Year", y = "LD share of kidney transplants") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey40", size = 9),
        panel.grid.minor = element_blank())

print(p_trends)

#region_typology
region_typology <- kidney_stats |>
  count(region, typology) |>
  group_by(region) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

region_order <- region_typology |>
  filter(typology == "Living-Dominant") |>
  arrange(desc(pct)) |>
  pull(region)

p_region <- region_typology |>
  mutate(region = factor(region, levels = region_order)) |>
  ggplot(aes(x = pct, y = region, fill = typology)) +
  geom_col(position = "stack", width = 0.7) +
  scale_x_continuous(labels = percent_format()) +
  scale_fill_manual(values = typology_colors, name = "Typology") +
  labs(
    title    = "Kidney donation typology by WHO region",
    subtitle = "Share of countries in each typology group (≥ 10 yrs data)",
    x        = "Share of countries",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "grey40", size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

print(p_region)

#kidney vs liver LD share
label_both <- c("Japan", "Republic of Korea", "Türkiye", "Iran (Islamic Republic of)",
                "India", "Spain", "United States of America", "France", "Norway",
                "Georgia", "Pakistan", "Saudi Arabia", "Israel", "Kazakhstan")

p_bivariate <- both_organs |>
  mutate(label = if_else(country %in% label_both, country, NA_character_)) |>
  ggplot(aes(x = avg_ld_share_kidney, y = avg_ld_share_liver, color = typology)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(aes(size = avg_total_kidney_tx), alpha = 0.75, shape = 16) +
  geom_text_repel(aes(label = label), size = 2.8, max.overlaps = 18,
                  color = "grey20", segment.color = "grey60", segment.size = 0.3,
                  fontface = "italic") +
  annotate("text", x = 0.5, y = 0.53, label = "Diagonal = kidney ≈ liver",
           color = "grey50", size = 2.8, fontface = "italic", angle = 42) +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1.05)) +
  scale_color_manual(values = typology_colors, name = "Kidney typology") +
  scale_size_continuous(range = c(2, 10), name = "Avg kidney tx/yr", labels = comma_format()) +
  labs(title    = "LD share: kidney vs liver",
       subtitle = "Countries with clean data for both organs",
       x = "Avg LD share — Kidney", y = "Avg LD share — Liver") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey40", size = 9),
        panel.grid.minor = element_blank())

print(p_bivariate)

#Combined
combined <- (p_corr | p_trends) /
  (p_lollipop_kidney | p_lollipop_liver) +
  plot_annotation(
    title    = "Living vs Deceased Donation: Global Patterns in Organ Transplantation",
    subtitle = "WHO Global Observatory on Donation and Transplantation · 2000–2024",
    theme    = theme(plot.title    = element_text(face = "bold", size = 15),
                     plot.subtitle = element_text(color = "grey40", size = 10))
  )

print(combined)
ggsave("living_donation_combined.png", plot = combined, width = 18, height = 12, dpi = 200, bg = "white")
cat("Saved: living_donation_combined.png\n")

# PART 3 COVID NATURAL EXPERIMENT & EXTENDED OLS

# compare 2018-19 vs 2020-21 across countries
covid_analysis <- df2 |>
  filter(!has_kidney_issue,
         !is.na(ld_share_kidney), !is.na(actual_dd_pmp),
         ld_share_kidney >= 0, ld_share_kidney <= 1) |>
  mutate(period = case_when(
    reportyear %in% 2018:2019 ~ "pre_covid",
    reportyear %in% 2020:2021 ~ "covid",
    reportyear %in% 2022:2023 ~ "post_covid",
    TRUE                      ~ NA_character_
  )) |>
  filter(!is.na(period)) |>
  group_by(country, region, period) |>
  summarise(
    avg_dd_pmp   = mean(actual_dd_pmp,   na.rm = TRUE),
    avg_ld_share = mean(ld_share_kidney, na.rm = TRUE),
    .groups = "drop"
  )

# keep only countries present in all 3 periods
complete_covid_countries <- covid_analysis |>
  group_by(country) |>
  filter(n_distinct(period) == 3) |>
  pull(country) |>
  unique()

covid_wide <- covid_analysis |>
  filter(country %in% complete_covid_countries) |>
  pivot_wider(id_cols = c(country, region),
              names_from  = period,
              values_from = c(avg_dd_pmp, avg_ld_share)) |>
  mutate(
    dd_pmp_drop     = avg_dd_pmp_covid   - avg_dd_pmp_pre_covid,
    ld_share_change = avg_ld_share_covid - avg_ld_share_pre_covid,
    covid_response  = case_when(
      dd_pmp_drop < -1 & ld_share_change >  0.03 ~ "DD drop -> LD compensated",
      dd_pmp_drop < -1 & ld_share_change <= 0.03 ~ "DD drop -> LD did NOT compensate",
      dd_pmp_drop >= -1 & ld_share_change > 0.03 ~ "DD stable -> LD still rose",
      TRUE                                       ~ "DD stable -> LD stable"
    )
  )

cat("\nCOVID response breakdown:\n")
print(table(covid_wide$covid_response))
write_csv(covid_wide, "covid_analysis.csv")

# IRAN vs TURKEY CASE STUDY
cat("\n══ IRAN vs TURKEY CASE STUDY ══\n")

case_countries <- c("Iran (Islamic Republic of)", "Türkiye")
# use kidney_clean (cleaned data) instead of the raw panel
case_data <- kidney_clean |>
  filter(country %in% case_countries) |>
  mutate(country_short = recode(country,
                                "Iran (Islamic Republic of)" = "Iran",
                                "Türkiye" = "Turkey"
  ))

# pre/post 2010 statistics
stats_2010 <- case_data |>
  mutate(period = ifelse(reportyear < 2010, "Pre-2010", "Post-2010")) |>
  group_by(country_short, period) |>
  summarise(
    avg_ld  = mean(ld_share_kidney, na.rm=TRUE),
    avg_dd  = mean(actual_dd_pmp, na.rm=TRUE),
    .groups = "drop"
  )
print(stats_2010)

# within-country correlation
iran_cor   <- cor.test(
  case_data$ld_share_kidney[case_data$country_short=="Iran"],
  case_data$actual_dd_pmp[case_data$country_short=="Iran"], na.action=na.omit)
turkey_cor <- cor.test(
  case_data$ld_share_kidney[case_data$country_short=="Turkey"],
  case_data$actual_dd_pmp[case_data$country_short=="Turkey"], na.action=na.omit)

cat(sprintf("Iran within-country r(LD~DD): %.3f (p=%.4f)\n", iran_cor$estimate, iran_cor$p.value))
cat(sprintf("Turkey within-country r(LD~DD): %.3f (p=%.4f)\n", turkey_cor$estimate, turkey_cor$p.value))

# Dual-panel plot
p_iran <- case_data |>
  filter(country_short == "Iran") |>
  ggplot(aes(x = reportyear)) +
  geom_rect(aes(xmin=2009.5, xmax=2010.5, ymin=-Inf, ymax=Inf), fill="#FFF2CC", alpha=0.5) +
  geom_line(aes(y = ld_share_kidney * 100), color="#C0504D", linewidth=1) +
  geom_line(aes(y = actual_dd_pmp * 5), color="#375623", linewidth=1, linetype="dashed") +
  annotate("text", x=2010, y=95, label="2010\nbreakpoint", size=2.5, color="grey30", hjust=0.5) +
  annotate("text", x=2005, y=10, label=sprintf("r = %.2f, p < .001", iran_cor$estimate),
           size=2.8, color="#375623", fontface="italic") +
  scale_y_continuous(
    name = "LD share (%)",
    sec.axis = sec_axis(~./5, name = "DD PMP")
  ) +
  scale_x_continuous(breaks = seq(2003,2022,4)) +
  labs(title = "Iran: Completed Structural Transition",
       subtitle = "Red = LD share | Green dashed = DD PMP (scaled)",
       x = NULL) +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(face="bold", color="#C0504D"),
        plot.subtitle=element_text(color="grey40", size=8))

p_turkey <- case_data |>
  filter(country_short == "Turkey") |>
  ggplot(aes(x = reportyear)) +
  geom_rect(aes(xmin=2019.5, xmax=2021.5, ymin=-Inf, ymax=Inf), fill="#FCE4D6", alpha=0.5) +
  geom_line(aes(y = ld_share_kidney * 100), color="#C0504D", linewidth=1) +
  geom_line(aes(y = actual_dd_pmp * 10), color="#375623", linewidth=1, linetype="dashed") +
  annotate("text", x=2020.5, y=50, label="COVID\nshock", size=2.5, color="grey30", hjust=0.5) +
  annotate("text", x=2005, y=10, label=sprintf("r = %.2f, p = %.3f", turkey_cor$estimate, turkey_cor$p.value),
           size=2.8, color="#375623", fontface="italic") +
  scale_y_continuous(
    name = "LD share (%)",
    sec.axis = sec_axis(~./10, name = "DD PMP")
  ) +
  scale_x_continuous(breaks = seq(2000,2024,4)) +
  labs(title = "Turkey: Stalled Transition + COVID Reversal",
       subtitle = "Red = LD share | Green dashed = DD PMP (scaled)",
       x = "Year") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(face="bold", color="#C0504D"),
        plot.subtitle=element_text(color="grey40", size=8))

p_case_study <- p_iran / p_turkey +
  plot_annotation(
    title = "Iran vs Turkey: Two Responses to Expanding Deceased Donation",
    subtitle = "Same starting point (~80% LD share, ~2 DD pmp) — divergent outcomes 15 years later",
    theme = theme(plot.title=element_text(face="bold", size=13),
                  plot.subtitle=element_text(color="grey40", size=9))
  )

print(p_case_study)
ggsave("iran_turkey_case_study.png", plot = p_case_study, width = 10, height = 9, dpi = 200, bg = "white")
cat("Saved: iran_turkey_case_study.png\n")
</content>
