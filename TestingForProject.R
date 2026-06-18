# This is a file for testing code related to the various aspects of our project.
# Better to have the documentation for it, right?

library(tidyverse)
library(readr)
library(janitor)
library(scales)
library(rvest)
library(lubridate)

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
aud_v_cri <- game_films %>% 
  filter(!is.na(rotten_tomatoes) | !is.na(metacritic), !is.na(cinema_score)) %>% 
  pivot_longer(cols = rotten_tomatoes:cinema_score, names_to = "critic_type", values_to = "score") %>% 
  select(title, critic_type, score, release_date)

aud_v_cri %>% 
  ggplot(aes(x = release_date, y = score, color = critic_type)) +
  geom_point() +
  geom_smooth(se = F) +
  labs(
    title = "Evolution of Video Game Movies' Audience Score vs. Critic Score over Time",
    x = "Date",
    y = "Score",
    color = "Critic Type"
  )

# PRIMARY QUESTION 2: PUBLISHER V. BOX OFFICE

# First we need to scrape a list of video game publishers and their net worth
# off Wikipedia.
company_url = "https://en.wikipedia.org/wiki/List_of_largest_video_game_companies_by_revenue"
tables_redun <- company_url |>
  read_html() |>
  html_elements("table") |>
  html_table()
top_50 <- tables_redun[[2]]
top_50 <- clean_names(top_50) %>% 
  select(!ref)



# PRIMARY QUESTION 3: FRANCHISE FATIGUE