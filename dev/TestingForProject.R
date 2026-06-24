# This is a file for testing code related to the various aspects of our project.
# Better to have the documentation for it, right?

library(tidyverse)
library(readr)
library(janitor)
library(scales)
library(rvest)
library(lubridate)
library(ggcorrplot)
library(patchwork)
library(ggridges)
library(kableExtra)
library(viridis)
library(gghighlight)
library(plotly)

# CONVERSION TABLE
fx_rates <- tibble::tribble(
  ~currency, ~to_usd,
  "$",        1,
  "¥",        0.0091,
  "£",        1.30,
  "HK$",      0.128,
  "NT$",      0.033
)

# INFLATION TABLE
cpi <- tibble::tribble(
  ~year, ~cpi,
  1986, 109.6,
  1987, 113.6,
  1988, 118.3,
  1989, 124.0,
  1990, 130.7,
  1991, 136.2,
  1992, 140.3,
  1993, 144.5,
  1994, 148.2,
  1995, 152.4,
  1996, 156.9,
  1997, 160.5,
  1998, 163.0,
  1999, 166.6,
  2000, 172.2,
  2001, 177.1,
  2002, 179.9,
  2003, 184.0,
  2004, 188.9,
  2005, 195.3,
  2006, 201.6,
  2007, 207.3,
  2008, 215.3,
  2009, 214.5,
  2010, 218.1,
  2011, 224.9,
  2012, 229.6,
  2013, 233.0,
  2014, 236.7,
  2015, 237.0,
  2016, 240.0,
  2017, 245.1,
  2018, 251.1,
  2019, 255.7,
  2020, 258.8,
  2021, 271.0,
  2022, 292.7,
  2023, 304.7,
  2024, 313.7
)
cpi_2024 <- cpi$cpi[cpi$year == 2024]

# CINEMASCORE TABLE
cinema_score_conversion <- tibble::tribble(
  ~cinemascore_letter, ~to_num,
  "N/A",        NA,
  "A+",        100,
  "A",        91.6666,
  "A-",      83.3333,
  "B+",      74.9999,
  "B",        66.6666,
  "B-",        58.3333,
  "C+",      49.9999,
  "C",      41.6666,
  "C-",        33.3333,
  "D+",      24.9999,
  "D",      16.6666,
  "D-",        8.3333,
  "F",        0,
)

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# IMPORTING GAMEFILMS
game_films <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-09/game_films.csv",
  show_col_types = FALSE
) |>
  janitor::clean_names()

# ADJUSTING TO USD
game_films <- game_films |>
  left_join(fx_rates, by = c("worldwide_box_office_currency" = "currency")) |>
  mutate(
    worldwide_box_office_usd = worldwide_box_office * to_usd
  ) |>
  select(-to_usd) |>
  left_join(fx_rates, by = c("budget_currency" = "currency")) |>
  mutate(
    budget_low_usd  = budget_low  * to_usd,
    budget_high_usd = budget_high * to_usd
  ) |>
  select(-to_usd)

# ADJUSTING FOR INFLATION
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

# MOVIE FILTERING
game_films <- game_films %>% 
  filter(category == "Theatrical releases")

# CINEMA SCORE CONVERSION
game_films <- game_films %>% 
  left_join(cinema_score_conversion, join_by("cinema_score" == "cinemascore_letter"))

game_films <- game_films %>%
  mutate(cinema_score = to_num) %>% 
  select(-starts_with("to_num"))

# SUMMARY DATA 1
game_films |>
  filter(!is.na(worldwide_box_office_usd)) |>
  summarise(
    n                    = n(),
    raw_median           = median(worldwide_box_office, na.rm = TRUE),
    usd_median           = median(worldwide_box_office_usd, na.rm = TRUE),
    adj_median           = median(worldwide_box_office_usd_adj, na.rm = TRUE)
  )

# SUMMARY DATA 2
game_films |>
  summarise(
    total_films        = n(),
    n_with_box_office  = sum(!is.na(worldwide_box_office)),
    n_with_rt          = sum(!is.na(rotten_tomatoes)),
    n_with_cinema      = sum(!is.na(cinema_score) & cinema_score != "N/A"),
    date_range          = paste(range(release_date, na.rm = TRUE), collapse = " to ")
  )

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# PRIMARY QUESTION 1: AUDIENCE V. CRITICS
# Prep data: pivot longer for all three review types, keep only complete cases
aud_v_cri <- game_films %>%
  filter(!is.na(rotten_tomatoes) | !is.na(metacritic), !is.na(cinema_score)) %>%
  pivot_longer(cols = rotten_tomatoes:cinema_score, names_to = "critic_type", values_to = "score") %>%
  select(title, critic_type, score, release_date) %>%
  arrange(release_date)

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

# FIGURE 1: Fancy scatter + smooth — critic vs audience over time
p1_scatter <- aud_v_cri %>%
  ggplot(aes(x = release_date, y = score, color = critic_type)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_smooth(se = TRUE, method = "loess", span = 0.6, alpha = 0.15) +
  scale_color_manual(values = critic_colors, labels = critic_labels) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "Critic and Audience Scores Over Time",
    subtitle = "Loess-smoothed trends with 95% confidence bands",
    x = "Release Date",
    y = "Score",
    color = "Score Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title   = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p1_scatter

# FIGURE 2: Critic-Audience GAP over time (dumbbell-style)
# Compute per-film gap: average critic score minus audience score
aud_v_cri_gap <- game_films %>%
  filter(!is.na(rotten_tomatoes) | !is.na(metacritic), !is.na(cinema_score)) %>%
  mutate(
    avg_critic = rowMeans(cbind(rotten_tomatoes, metacritic), na.rm = TRUE),
    gap = avg_critic - cinema_score
  ) %>%
  filter(!is.na(gap)) %>%
  select(title, release_date, avg_critic, cinema_score, gap)

p2_gap <- aud_v_cri_gap %>%
  ggplot(aes(x = release_date, y = gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_point(aes(fill = gap > 0), size = 2.5, alpha = 0.7, shape = 21, color = "white") +
  geom_smooth(method = "loess", se = TRUE, color = "#E63946", fill = "#E63946", alpha = 0.15) +
  scale_fill_manual(
    values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51"),
    labels = c("TRUE" = "Critics Higher", "FALSE" = "Audience Higher")
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(
    title = "Critic–Audience Score Gap Over Time",
    subtitle = "Positive = critics rate higher; Negative = audiences rate higher",
    x = "Release Date",
    y = "Gap (Critic – Audience)",
    fill = "Direction"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title   = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p2_gap

# FIGURE 3: Combined patchwork — scatter + gap
p1_scatter / p2_gap +
  plot_annotation(
    title    = "Q1: Do Critics and Audiences Agree on Video Game Films?",
    subtitle = "Loess-smoothed trends reveal a persistent but narrowing critic–audience gap",
    theme    = theme(plot.title = element_text(face = "bold", size = 16))
  )

# PRIMARY QUESTION 2: PUBLISHER V. BOX OFFICE
# Scrape publisher revenue data from Wikipedia
company_url = "https://en.wikipedia.org/wiki/List_of_largest_video_game_companies_by_revenue"
TablesRedun <- company_url |>
  read_html() |>
  html_elements("table") |>
  html_table()
top_50 <- TablesRedun[[2]]

# Clean and join with game_films
top_50 <- clean_names(top_50) %>%
  mutate(revenue_usd_billions = parse_number(revenue_usd)) %>%
  select(!c(ref, revenue_usd))
top_50 <- as.data.frame(top_50) %>%
  right_join(game_films, join_by("company" == "original_game_publisher"), copy) %>%
  select(title, worldwide_box_office_usd_adj, company, revenue_usd_billions,
         budget_high_usd_adj, rotten_tomatoes, release_date) %>%
  filter(!is.na(worldwide_box_office_usd_adj), !is.na(revenue_usd_billions)) %>%
  arrange(revenue_usd_billions)

# FIGURE 4: 3+ VARIABLE SCATTER -- revenue vs box office, colored by company,
#           sized by budget, with Rotten Tomatoes encoded as alpha
p3_multivariate <- top_50 %>%
  ggplot(aes(x = revenue_usd_billions,
             y = worldwide_box_office_usd_adj / 1e6,
             color = company,
             size  = budget_high_usd_adj / 1e6)) +
  geom_point(alpha = 0.75) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "gray30",
              linewidth = 0.8, linetype = "dashed", alpha = 0.2) +
  scale_color_viridis_d(option = "turbo", guide = guide_legend(ncol = 2)) +
  scale_size_continuous(
    name   = "Budget (M USD)",
    labels = dollar_format(),
    range  = c(2, 10)
  ) +
  scale_x_log10(labels = dollar_format(suffix = "B")) +
  scale_y_log10(labels = dollar_format(suffix = "M")) +
  labs(
    title    = "Publisher Revenue vs. Box Office Performance",
    subtitle = "3+ variables: color = company, size = budget, x/y = revenue & box office",
    x        = "Yearly Company Revenue (USD Billions, log scale)",
    y        = "Inflation-Adjusted Box Office (M USD, log scale)",
    color    = "Publisher"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p3_multivariate

# FIGURE 5: Ridgeline plot — box office distribution by publisher
p4_ridge <- top_50 %>%
  mutate(company = fct_reorder(company, revenue_usd_billions, .desc = TRUE)) %>%
  ggplot(aes(x = worldwide_box_office_usd_adj / 1e6, y = company, fill = company)) +
  geom_density_ridges(alpha = 0.8, scale = 1.2) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  scale_x_log10(labels = dollar_format(suffix = "M")) +
  labs(
    title    = "Box Office Distribution by Publisher",
    subtitle = "Ridgeline density plot showing spread of earnings per publisher",
    x        = "Inflation-Adjusted Box Office (M USD, log scale)",
    y        = "Publisher"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

p4_ridge

# Combined Q2 view
p3_multivariate / p4_ridge +
  plot_annotation(
    title    = "Q2: Does Publisher Size Predict Box Office Success?",
    subtitle = "Larger publishers tend to produce higher-grossing films, with wide variance",
    theme    = theme(plot.title = element_text(face = "bold", size = 16))
  )

# PRIMARY QUESTION 3: FRANCHISE FATIGUE
# Approach 1: First-word franchise detection (simple heuristic)
MovieCount <- game_films %>%
  mutate(
    title_clean = str_remove(title, "^(The|A|An) "),
    start_word  = str_extract(title_clean, "^[^ ]+")
  ) %>%
  select(title, start_word, release_date, worldwide_box_office_usd_adj, rotten_tomatoes) %>%
  filter(if_all(everything(), \(x) !is.na(x))) %>%
  arrange(start_word)

MovieCountSummary <- MovieCount %>%
  group_by(start_word) %>%
  count() %>%
  filter(n >= 3) %>%
  arrange(desc(n))

MovieCountFinal <- semi_join(MovieCount, MovieCountSummary, join_by(start_word)) %>%
  group_by(start_word) %>%
  mutate(installment = rank(release_date)) %>%
  ungroup()

# FIGURE 6: Faceted franchise-by-installment box office with dual-color trend
p5_franchise_facet <- MovieCountFinal %>%
  ggplot(aes(x = installment, y = worldwide_box_office_usd_adj / 1e6)) +
  geom_point(aes(color = start_word), size = 3, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "gray30",
              linetype = "dashed", linewidth = 0.8, alpha = 0.2) +
  geom_line(aes(group = start_word, color = start_word), linewidth = 0.5, alpha = 0.4) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  scale_y_log10(labels = dollar_format(suffix = "M")) +
  facet_wrap(~ reorder(start_word, -worldwide_box_office_usd_adj), scales = "free_y") +
  labs(
    title    = "Box Office by Installment — Major Franchises (3+ films)",
    subtitle = "Log-scale; individual trend lines per franchise + overall dashed LM",
    x        = "Installment Number",
    y        = "Inflation-Adjusted Box Office (M USD, log scale)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold"),
    strip.text        = element_text(face = "bold"),
    panel.grid.minor  = element_blank(),
    legend.position   = "none"
  )

p5_franchise_facet

# FIGURE 7: Critic score decline by installment for major franchises
p6_franchise_rt <- MovieCountFinal %>%
  ggplot(aes(x = installment, y = rotten_tomatoes)) +
  geom_point(aes(color = start_word), size = 3, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "gray30",
              linetype = "dashed", linewidth = 0.8, alpha = 0.2) +
  geom_line(aes(group = start_word, color = start_word), linewidth = 0.5, alpha = 0.4) +
  scale_color_viridis_d(option = "turbo", guide = "none") +
  facet_wrap(~ reorder(start_word, -rotten_tomatoes), scales = "free_y") +
  labs(
    title    = "Rotten Tomatoes Score by Installment — Major Franchises",
    subtitle = "Do critic scores decline with later installments?",
    x        = "Installment Number",
    y        = "Rotten Tomatoes Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold"),
    strip.text        = element_text(face = "bold"),
    panel.grid.minor  = element_blank(),
    legend.position   = "none"
  )

p6_franchise_rt

# FIGURE 8: Heatmap-style tile plot — franchise x installment = box office
p7_heatmap <- MovieCountFinal %>%
  mutate(start_word = fct_reorder(start_word, worldwide_box_office_usd_adj, .fun = max, .desc = TRUE)) %>%
  ggplot(aes(x = factor(installment), y = start_word, fill = worldwide_box_office_usd_adj / 1e6)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::dollar(worldwide_box_office_usd_adj / 1e6, suffix = "M")),
            size = 2.5, color = "gray10") +
  scale_fill_gradientn(
    colors  = c("#023047", "#219EBC", "#8ECAE6", "#FFB703", "#FB8500"),
    labels  = dollar_format(suffix = "M"),
    na.value = "gray90",
    trans   = "log10"
  ) +
  labs(
    title    = "Franchise × Installment Box Office Heatmap",
    subtitle = "Color intensity = inflation-adjusted box office (log scale)",
    x        = "Installment Number",
    y        = "Franchise",
    fill     = "Box Office"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid       = element_blank(),
    axis.ticks       = element_blank()
  )

p7_heatmap

# Combined Q3 view
(p5_franchise_facet / p7_heatmap) +
  plot_annotation(
    title    = "Q3: Does Franchise Fatigue Exist in Video Game Films?",
    subtitle = "Box office and critic scores generally decline with later installments — franchise fatigue is real",
    theme    = theme(plot.title = element_text(face = "bold", size = 16))
  )

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# SUPPLEMENTARY: CORRELATION HEATMAP
# Diverging dual-color correlation matrix of key numeric variables
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

ggcorrplot(
  cor_matrix,
  method           = "circle",
  type             = "lower",
  lab              = TRUE,
  lab_size         = 3.5,
  colors           = c("#E63946", "white", "#2A9D8F"),
  title            = "Correlation Matrix: Box Office, Budget & Scores",
  ggtheme          = theme_minimal(base_size = 12),
  legend.title     = "r",
  outline.color    = "white"
) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.major = element_blank()
  )

# ----------------------------------------------------------------------

# SUPPLEMENTARY: FANCY SUMMARY TABLE
# Non-trivial aggregated table: publisher performance metrics
table_data <- game_films %>%
  filter(!is.na(rotten_tomatoes), !is.na(worldwide_box_office_usd_adj)) %>%
  group_by(original_game_publisher) %>%
  summarise(
    `# Films`         = n(),
    `Median Box Office (M)` = round(median(worldwide_box_office_usd_adj) / 1e6, 1),
    `Mean Box Office (M)`   = round(mean(worldwide_box_office_usd_adj) / 1e6, 1),
    `Median RT Score`       = round(median(rotten_tomatoes, na.rm = TRUE), 1),
    `Mean RT Score`         = round(mean(rotten_tomatoes, na.rm = TRUE), 1),
    `Median CinemaScore`    = round(median(cinema_score, na.rm = TRUE), 1),
    `Mean CinemaScore`      = round(mean(cinema_score, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  filter(`# Films` >= 3) %>%
  arrange(desc(`Mean Box Office (M)`))

table_data %>%
  kable(
    caption  = "Table 1: Publisher Performance — Aggregated Box Office & Critic Metrics (3+ films)",
    digits   = 1,
    align    = c("l", rep("r", 7)),
    format   = "pipe"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width        = FALSE,
    position          = "center"
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2A9D8F") %>%
  row_spec(1, bold = FALSE) %>%
  column_spec(1, bold = TRUE, border_right = TRUE)

# ----------------------------------------------------------------------

# SUPPLEMENTARY: INTERACTIVE PLOTLY VERSION OF Q1
p1_interactive <- aud_v_cri %>%
  ggplot(aes(x = release_date, y = score, color = critic_type, text = title)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(se = FALSE, method = "loess", span = 0.6) +
  scale_color_manual(values = c("#E63946", "#457B9D", "#2A9D8F")) +
  labs(title = "Critic vs Audience Scores Over Time (Interactive)",
       x = "Release Date", y = "Score") +
  theme_minimal(base_size = 12)

ggplotly(p1_interactive, tooltip = c("title", "score", "critic_type")) %>%
  layout(legend = list(orientation = "h", y = -0.2))
