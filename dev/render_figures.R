library(tidyverse)
library(readr)
library(janitor)
library(scales)
library(rvest)
library(lubridate)
library(ggcorrplot)
library(patchwork)
library(ggridges)
library(viridis)

theme_set(theme_minimal(base_size = 12))

out_dir <- "../images"
dir.create(out_dir, showWarnings = FALSE)

# ---- data pipeline (mirrors index.qmd) ----

game_films <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv",
  show_col_types = FALSE
) |>
  janitor::clean_names()

fx_rates <- tibble::tribble(
  ~currency, ~to_usd,
  "$",        1,
  "¥",        0.0091,
  "£",        1.30,
  "HK$",      0.128,
  "NT$",      0.033
)

game_films <- game_films |>
  left_join(fx_rates, by = c("worldwide_box_office_currency" = "currency")) |>
  mutate(worldwide_box_office_usd = worldwide_box_office * to_usd) |>
  select(-to_usd) |>
  left_join(fx_rates, by = c("budget_currency" = "currency")) |>
  mutate(
    budget_low_usd  = budget_low  * to_usd,
    budget_high_usd = budget_high * to_usd
  ) |>
  select(-to_usd)

cpi <- tibble::tribble(
  ~year, ~cpi,
  1986, 109.6, 1987, 113.6, 1988, 118.3, 1989, 124.0,
  1990, 130.7, 1991, 136.2, 1992, 140.3, 1993, 144.5,
  1994, 148.2, 1995, 152.4, 1996, 156.9, 1997, 160.5,
  1998, 163.0, 1999, 166.6, 2000, 172.2, 2001, 177.1,
  2002, 179.9, 2003, 184.0, 2004, 188.9, 2005, 195.3,
  2006, 201.6, 2007, 207.3, 2008, 215.3, 2009, 214.5,
  2010, 218.1, 2011, 224.9, 2012, 229.6, 2013, 233.0,
  2014, 236.7, 2015, 237.0, 2016, 240.0, 2017, 245.1,
  2018, 251.1, 2019, 255.7, 2020, 258.8, 2021, 271.0,
  2022, 292.7, 2023, 304.7, 2024, 313.7
)

cpi_2024 <- cpi$cpi[cpi$year == 2024]

game_films <- game_films |>
  mutate(release_year = lubridate::year(release_date)) |>
  left_join(cpi, by = c("release_year" = "year")) |>
  mutate(
    inflation_factor = cpi_2024 / cpi,
    worldwide_box_office_usd_adj = worldwide_box_office_usd * inflation_factor,
    budget_low_usd_adj  = budget_low_usd  * inflation_factor,
    budget_high_usd_adj = budget_high_usd * inflation_factor
  ) |>
  select(-cpi, -inflation_factor)

game_films <- game_films %>%
  filter(category == "Theatrical releases")

cinema_score_conversion <- tibble::tribble(
  ~cinemascore_letter, ~to_num,
  "N/A",        NA,
  "A+",        100,   "A",   91.6666,  "A-",  83.3333,
  "B+",        74.9999, "B", 66.6666,  "B-",  58.3333,
  "C+",        49.9999, "C", 41.6666,  "C-",  33.3333,
  "D+",        24.9999, "D", 16.6666,  "D-",   8.3333,
  "F",          0
)

game_films <- game_films %>%
  left_join(cinema_score_conversion, join_by("cinema_score" == "cinemascore_letter"))

game_films <- game_films %>%
  mutate(cinema_score = to_num) %>%
  select(-starts_with("to_num"))

cat("Data pipeline loaded:", nrow(game_films), "films\n")

# ---- Descriptive stats boxplot ----

desc_long <- game_films %>%
  select(
    `Box Office (adj. USD, M)` = worldwide_box_office_usd_adj,
    `Budget High (adj. USD, M)` = budget_high_usd_adj,
    `Rotten Tomatoes` = rotten_tomatoes,
    `Metacritic` = metacritic,
    `CinemaScore` = cinema_score
  ) %>%
  mutate(
    `Box Office (adj. USD, M)` = `Box Office (adj. USD, M)` / 1e6,
    `Budget High (adj. USD, M)` = `Budget High (adj. USD, M)` / 1e6
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value))

p_box <- desc_long %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free_y", nrow = 1) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "fig-desc-boxplot.png"), p_box, width = 9, height = 4.5, dpi = 300)
cat("saved fig-desc-boxplot.png\n")

# ---- Q1: Critic-Audience Agreement ----

aud_v_cri <- game_films %>%
  filter(!is.na(rotten_tomatoes) | !is.na(metacritic), !is.na(cinema_score)) %>%
  pivot_longer(cols = rotten_tomatoes:cinema_score, names_to = "critic_type", values_to = "score") %>%
  select(title, critic_type, score, release_date)

critic_colors <- c(
  "rotten_tomatoes" = "#E63946",
  "metacritic"      = "#457B9D",
  "cinema_score"    = "#2A9D8F"
)

critic_labels <- c(
  "rotten_tomatoes" = "Rotten Tomatoes (Critic)",
  "metacritic"      = "Metacritic (Critic)",
  "cinema_score"    = "CinemaScore (Audience)"
)

p_q1_scatter <- aud_v_cri %>%
  ggplot(aes(x = release_date, y = score, color = critic_type)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(se = TRUE, method = "loess", span = 0.6, alpha = 0.15) +
  scale_color_manual(values = critic_colors, labels = critic_labels) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(x = "Release Date", y = "Score", color = "Score Type") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "fig-q1-scatter.png"), p_q1_scatter, width = 9, height = 5, dpi = 300)
cat("saved fig-q1-scatter.png\n")

aud_v_cri_gap <- game_films %>%
  filter(!is.na(rotten_tomatoes) | !is.na(metacritic), !is.na(cinema_score)) %>%
  mutate(
    avg_critic = rowMeans(cbind(rotten_tomatoes, metacritic), na.rm = TRUE),
    gap = avg_critic - cinema_score
  ) %>%
  filter(!is.na(gap))

p_q1_gap <- aud_v_cri_gap %>%
  ggplot(aes(x = release_date, y = gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_point(aes(fill = gap > 0), size = 2.5, alpha = 0.7, shape = 21, color = "white") +
  geom_smooth(method = "loess", se = TRUE, color = "#E63946", fill = "#E63946", alpha = 0.15) +
  scale_fill_manual(
    values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51"),
    labels = c("TRUE" = "Critics Higher", "FALSE" = "Audience Higher")
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(x = "Release Date", y = "Gap (Critic-Audience)", fill = "Direction") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "fig-q1-gap.png"), p_q1_gap, width = 9, height = 5, dpi = 300)
cat("saved fig-q1-gap.png\n")

# ---- Q2: Publisher Size ----

company_url <- "https://en.wikipedia.org/wiki/List_of_largest_video_game_companies_by_revenue"
TablesRedun <- company_url |>
  read_html() |>
  html_elements("table") |>
  html_table()
top_50 <- TablesRedun[[2]]

top_50_clean <- clean_names(top_50) %>%
  mutate(revenue_usd_billions = parse_number(revenue_usd)) %>%
  select(!c(ref, revenue_usd))

pub_joined <- as.data.frame(top_50_clean) %>%
  right_join(game_films, join_by("company" == "original_game_publisher"), copy)

p_q2_multi <- pub_joined %>%
  filter(!is.na(worldwide_box_office_usd_adj), !is.na(revenue_usd_billions), !is.na(budget_high_usd_adj)) %>%
  ggplot(aes(x = revenue_usd_billions,
             y = worldwide_box_office_usd_adj / 1e6,
             color = company,
             size  = budget_high_usd_adj / 1e6)) +
  geom_point(alpha = 0.75) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "gray30",
              linewidth = 0.8, linetype = "dashed", alpha = 0.2) +
  scale_color_viridis_d(option = "turbo", guide = guide_legend(ncol = 2)) +
  scale_size_continuous(name = "Budget (M USD)", labels = dollar_format(), range = c(2, 10)) +
  scale_x_log10(labels = dollar_format(suffix = "B")) +
  scale_y_log10(labels = dollar_format(suffix = "M")) +
  labs(
    x = "Yearly Company Revenue (USD Billions, log scale)",
    y = "Inflation-Adjusted Box Office (M USD, log scale)",
    color = "Publisher"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "fig-q2-multivariate.png"), p_q2_multi, width = 9, height = 6, dpi = 300)
cat("saved fig-q2-multivariate.png\n")

ridge_eligible <- pub_joined %>%
  filter(!is.na(worldwide_box_office_usd_adj), !is.na(revenue_usd_billions)) %>%
  count(company) %>%
  filter(n >= 3) %>%
  pull(company)

p_q2_ridge <- pub_joined %>%
  filter(!is.na(worldwide_box_office_usd_adj), !is.na(revenue_usd_billions), company %in% ridge_eligible) %>%
  mutate(company = fct_reorder(company, revenue_usd_billions, .desc = TRUE)) %>%
  ggplot(aes(x = worldwide_box_office_usd_adj / 1e6, y = company, fill = company)) +
  geom_density_ridges(alpha = 0.8, scale = 1.2) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  scale_x_log10(labels = dollar_format(suffix = "M")) +
  labs(x = "Inflation-Adjusted Box Office (M USD, log scale)", y = "Publisher") +
  theme_minimal(base_size = 13)

ggsave(file.path(out_dir, "fig-q2-ridge.png"), p_q2_ridge, width = 9, height = 5, dpi = 300)
cat("saved fig-q2-ridge.png\n")

# ---- Q3: Franchise Fatigue ----

MovieCount <- game_films %>%
  mutate(
    title_clean = str_remove(title, "^(The|A|An) "),
    start_word  = str_extract(title_clean, "^[^ ]+")
  ) %>%
  select(title, start_word, release_date, worldwide_box_office_usd_adj, rotten_tomatoes) %>%
  filter(if_all(everything(), \(x) !is.na(x)))

MovieCountSummary <- MovieCount %>%
  group_by(start_word) %>%
  count() %>%
  filter(n >= 3) %>%
  arrange(desc(n))

MovieCountFinal <- semi_join(MovieCount, MovieCountSummary, join_by(start_word)) %>%
  group_by(start_word) %>%
  mutate(installment = rank(release_date)) %>%
  ungroup()

# Reworked: bar chart sorted chronologically by release date, no SE/trend band
p_q3_bar <- MovieCountFinal %>%
  ggplot(aes(x = release_date, y = worldwide_box_office_usd_adj / 1e6, fill = start_word)) +
  geom_col(width = 200) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  scale_x_date(date_labels = "%Y") +
  facet_wrap(~ reorder(start_word, release_date), scales = "free", ncol = 3) +
  labs(x = "Release Date", y = "Inflation-Adjusted Box Office (M USD)") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "fig-q3-bar.png"), p_q3_bar, width = 10, height = 7, dpi = 300)
cat("saved fig-q3-bar.png\n")

p_q3_rt <- MovieCountFinal %>%
  ggplot(aes(x = installment, y = rotten_tomatoes)) +
  geom_line(aes(group = start_word, color = start_word), linewidth = 0.6, alpha = 0.6) +
  geom_point(aes(color = start_word), size = 3, alpha = 0.9) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ reorder(start_word, -rotten_tomatoes), scales = "free_x") +
  labs(x = "Installment Number", y = "Rotten Tomatoes Score") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"), panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "fig-q3-rt.png"), p_q3_rt, width = 10, height = 7, dpi = 300)
cat("saved fig-q3-rt.png\n")

# Original color gradient, kept as-is. Only the label color is fixed: it is computed
# from the actual luminance of each tile's interpolated fill color so text stays
# legible whether the tile lands on the dark navy end or the light/bright end.
heatmap_palette <- c("#023047", "#219EBC", "#8ECAE6", "#FFB703", "#FB8500")

heatmap_data <- MovieCountFinal %>%
  mutate(
    start_word = fct_reorder(start_word, worldwide_box_office_usd_adj, .fun = max, .desc = TRUE),
    box_office_m = worldwide_box_office_usd_adj / 1e6
  )

log_box <- log10(heatmap_data$box_office_m)
norm_pos <- (log_box - min(log_box)) / (max(log_box) - min(log_box))
ramp_rgb <- colorRamp(heatmap_palette)(norm_pos) / 255
luminance <- 0.299 * ramp_rgb[, 1] + 0.587 * ramp_rgb[, 2] + 0.114 * ramp_rgb[, 3]
heatmap_data$label_color <- ifelse(luminance < 0.55, "white", "gray10")

p_q3_heatmap <- heatmap_data %>%
  ggplot(aes(x = factor(installment), y = start_word, fill = box_office_m)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::dollar(box_office_m, suffix = "M"), color = label_color),
            size = 2.5) +
  scale_color_identity() +
  scale_fill_gradientn(
    colors  = heatmap_palette,
    labels  = dollar_format(suffix = "M"),
    na.value = "gray90",
    trans   = "log10"
  ) +
  labs(x = "Installment Number", y = "Franchise", fill = "Box Office") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(), axis.ticks = element_blank())

ggsave(file.path(out_dir, "fig-q3-heatmap.png"), p_q3_heatmap, width = 10, height = 5, dpi = 300)
cat("saved fig-q3-heatmap.png\n")

# Charlie Bupp's original preliminary Q3 visualization (commit 997e63c, TestingForProject.R),
# adapted to the cleaned/filtered franchise data: actual release date on the x-axis,
# single combined panel, no SE band on the per-franchise trend lines.
p_q3_charlie <- MovieCountFinal %>%
  ggplot(aes(x = release_date, y = worldwide_box_office_usd_adj / 1e6, color = start_word)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(se = FALSE, method = "lm", linewidth = 0.7) +
  scale_color_viridis_d(option = "turbo", name = "Franchise") +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    x = "Release Date",
    y = "Inflation-Adjusted Box Office (M USD)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "fig-q3-charlie.png"), p_q3_charlie, width = 9, height = 5.5, dpi = 300)
cat("saved fig-q3-charlie.png\n")

# ---- Correlation matrix ----

cor_vars <- game_films %>%
  select(
    worldwide_box_office_usd_adj,
    budget_low_usd_adj,
    budget_high_usd_adj,
    rotten_tomatoes,
    metacritic,
    cinema_score
  ) %>%
  rename(
    `Box Office (adj)`  = worldwide_box_office_usd_adj,
    `Budget Low (adj)`  = budget_low_usd_adj,
    `Budget High (adj)` = budget_high_usd_adj,
    `Rotten Tomatoes`   = rotten_tomatoes,
    `Metacritic`        = metacritic,
    `CinemaScore`       = cinema_score
  )

cor_matrix <- cor(cor_vars, use = "pairwise.complete.obs")

p_corr <- ggcorrplot(
  cor_matrix,
  method        = "square",
  type          = "lower",
  lab           = TRUE,
  lab_size      = 4,
  colors        = c("#E63946", "white", "#2A9D8F"),
  title         = "Correlation Matrix: Box Office, Budget & Scores",
  ggtheme       = theme_minimal(base_size = 12),
  legend.title  = "r",
  outline.color = "white"
) +
  theme(panel.grid.major = element_blank())

ggsave(file.path(out_dir, "fig-correlation.png"), p_corr, width = 8, height = 6, dpi = 300)
cat("saved fig-correlation.png\n")

# ---- best/worst films for descriptive stats callout ----

best_worst <- tibble::tribble(
  ~Category, ~Film, ~Value,
  "Highest box office (adj.)",  game_films$title[which.max(game_films$worldwide_box_office_usd_adj)],  scales::dollar(max(game_films$worldwide_box_office_usd_adj, na.rm = TRUE)),
  "Lowest box office (adj.)",   game_films$title[which.min(game_films$worldwide_box_office_usd_adj)],  scales::dollar(min(game_films$worldwide_box_office_usd_adj, na.rm = TRUE)),
  "Highest Rotten Tomatoes",    game_films$title[which.max(game_films$rotten_tomatoes)],                paste0(max(game_films$rotten_tomatoes, na.rm = TRUE), "/100"),
  "Lowest Rotten Tomatoes",     game_films$title[which.min(game_films$rotten_tomatoes)],                paste0(min(game_films$rotten_tomatoes, na.rm = TRUE), "/100")
)

write_csv(best_worst, file.path(out_dir, "best_worst.csv"))
print(best_worst)

cat("\nAll figures saved to", normalizePath(out_dir), "\n")
