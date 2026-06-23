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
# First we need to pare the data table down to the name, release date, and
# review score variables. Then, we pivot longer to get all three review
# variables under one column.
aud_v_cri <- game_films %>% 
  filter(!is.na(rotten_tomatoes) | !is.na(metacritic), !is.na(cinema_score)) %>% 
  pivot_longer(cols = rotten_tomatoes:cinema_score, names_to = "critic_type", values_to = "score") %>% 
  select(title, critic_type, score, release_date) %>% 
  arrange(release_date)

# Finally, we graph release date vs score, colored by critic type!
aud_v_cri %>% 
  ggplot(aes(x = release_date, y = score, color = critic_type)) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  labs(
    title = "Evolution of Video Game Movies' Audience Score vs. Critic Score over Time",
    x = "Date",
    y = "Score",
    color = "Critic Type"
  ) +
  theme(legend.position = "bottom")

# PRIMARY QUESTION 2: PUBLISHER V. BOX OFFICE

# First we need to scrape a list of video game publishers and their net worth
# off Wikipedia.
company_url = "https://en.wikipedia.org/wiki/List_of_largest_video_game_companies_by_revenue"
TablesRedun <- company_url |>
  read_html() |>
  html_elements("table") |>
  html_table()
top_50 <- TablesRedun[[2]]

# Then we take that list and join it with the game_films list, removing all nas.
top_50 <- clean_names(top_50) %>% 
  mutate(revenue_usd_billions = parse_number(revenue_usd)) %>% 
  select(!c(ref, revenue_usd))
top_50 <- as.data.frame(top_50) %>% 
  right_join(game_films, join_by("company" == "original_game_publisher"), copy) %>% 
  select(title, worldwide_box_office, company, revenue_usd_billions) %>% 
  filter(!is.na(worldwide_box_office), !is.na(revenue_usd_billions)) %>% 
  arrange(revenue_usd_billions)

# Finally, we graph company revenue against box office earnings!
top_50 %>% 
  ggplot(aes(x = revenue_usd_billions, y = worldwide_box_office)) +
  geom_point(aes(color = company), alpha = .6, size = 4, ) +
  geom_smooth(se = F, method = "lm") +
  labs(
    title = "Company Revenue vs. Box Office Performance of Video Game Movies",
    x = "Yearly Company Revenue (Billions)",
    y = "Box Office Earnings (USD)",
    color = "Company"
  ) +
  scale_x_log10() +
  theme_minimal() +
  theme(legend.position = "bottom")

# PRIMARY QUESTION 3: FRANCHISE FATIGUE
# First, we create a new variable that consists of the first word of the movie.
# For movies that start with "The," use second word.
MovieCount <- game_films %>% 
  mutate(start_word = sub(" .*$", "", title)) %>% 
  mutate(start_word = sub(":", "", start_word)) %>% 
  # WHY DOES THIS TURN ALL THE MOVIES THAT START WITH "THE" INTO MARIO????
  # mutate(start_word = sub("The$", str_extract(title, " .* "), start_word)) %>% 
  select(title, release_date, worldwide_box_office_usd_adj, start_word) %>% 
  filter(if_all(title:start_word, \(x) !is.na(x))) %>% 
  filter(start_word != "The") %>% 
  arrange(start_word)

# Then, summarize the list to only contain the count of movies that have more than 1 entry
MovieCountSummary <- MovieCount %>% 
  group_by(start_word) %>% 
  count() %>% 
  filter(n > 1) %>% 
  arrange(n)
  
# Next, utilize semi_join to only keep movies in MovieCount that start with words
# in MovieCountSummary!
MovieCountFinal <- semi_join(MovieCount, MovieCountSummary, join_by(start_word))

# Finally, graph release date against money earned, grouping by start word!
MovieCountFinal %>% 
  ggplot(aes(x = release_date, y = worldwide_box_office_usd_adj, color = start_word)) +
  geom_point(alpha = .6, size = 3) +
  labs(
    title = "Tracking Franchise Fatigue by First Word in Movie Title",
    x = "Release Date (Year)",
    y = "Box Office Earnings (USD)",
    color = "First Word"
  ) +
  theme_minimal() +
  geom_smooth(se = F, method = "lm")# + scale_y_log10()
  
  
