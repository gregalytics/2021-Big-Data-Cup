library(rvest)
library(RSelenium)
library(tidyverse)
library(elite) # https://github.com/eoppe1022/elite/tree/master/R
library(purrr)

## Use Evan Oppenheimer's elite package to get basic stats and player positions (I used these in my paper)
ohl_2020 <- elite::get_teams("OHL", "2020") %>%
  elite::get_player_stats_team()

## Initialize RSelenium session in Chrome ---
rD <- rsDriver(port = 1026L,browser = c("chrome"),chromever = "87.0.4280.20", verbose = F)
remDr <- rD[["client"]]

#remDr$open()

## Navigate to login page and enter keys ----

remDr$navigate("https://www.eliteprospects.com/login?previous=https://www.eliteprospects.com")
remDr$findElement(using = 'css selector', "#email")$sendKeysToElement(list("<ENTER YOUR ELITEPROSPECTS.COM USERNAME"))
remDr$findElement(using = 'css selector', "#password")$sendKeysToElement(list("<PASSWORD>"))
remDr$findElement(using = 'css selector', "body > section.main_content.clearfix > div > div > div > div > div > div > form > div > div.premium-form-field.button-area > button")$clickElement()

# Extract unique player urls
player_urls <- ohl_2020 %>%
  select(player_url) %>%
  distinct()

# Start on player 1 page
url <- player_urls$player_url[1]
remDr$navigate(player_urls$player_url[1])

html <- remDr$getPageSource()[[1]] %>% read_html()

## Function to scrape all grades from an eliteprospects.com page ----

scrape_fhm_ratings <- function(row) {
  URL <- missing$url[row]
  remDr$navigate(URL)
  Sys.sleep(2)
  html <- remDr$getPageSource()[[1]] %>% read_html()
  noder <- function(page, node) {
    node_content <- html_text(html_nodes(page, node))
    ifelse(length(node_content) == 0, NA, trimws(node_content))
  }
  
  tibble(url = URL,
         player_name = noder(html, "body > div.ep-entity-header > div > div > div.col-md-18.col-24 > div:nth-child(1) > div.col-md-12.col-24.pt-md-4.ep-entity-header__info-container > div.ep-entity-header__name"),
         rating_1 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(1) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_1 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(1) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_2 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(2) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_2 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(2) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_3 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(3) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_3 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(3) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_4 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(4) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_4 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(4) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_5 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(5) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_5 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(5) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_6 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(6) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_6 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(6) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_7 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(7) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_7 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(7) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_8 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(8) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_8 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(8) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_9 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(9) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_9 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(9) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_10 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(10) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_10 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(10) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_11 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(11) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_11 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(11) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_12 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(12) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_12 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(12) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_13 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(13) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_13 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(13) > div > div.ep-rating__label > div.ep-rating__name"),
         rating_14 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(14) > div > div.ep-rating__label > div.ep-rating__value"),
         rating_label_14 = noder(html, "body > div.ep-container.mt-4 > div > div.col > div:nth-child(15) > div.ep-card__body.ep-card__body--unpadded > div > div:nth-child(14) > div > div.ep-rating__label > div.ep-rating__name"),
  )
}

## Clean scraped data ----

# Scraped form with labels and ratings - need to clena 
fhm_player_ratings <- fhm_player_ratings %>% filter(!is.na(rating_1)) %>% 
  bind_rows(filter(fhm_player_ratings_2, !is.na(rating_1)))

# Unique label-url combinations 

labels <- fhm_player_ratings %>%
  gather("key", "lab", -url, -player_name) %>%
  filter(str_detect(key, "label")) %>%
  mutate(key = parse_number(key))

# Unique rank-url combinations 

ranks <- fhm_player_ratings %>%
  gather("key", "value", -url, -player_name) %>%
  filter(!str_detect(key, "label")) %>%
  mutate(key = parse_number(key))

# Create a dataframe of all players and their skillsets
ratings <- labels %>%
  inner_join(ranks, by = c("url", "player_name", "key"))  %>%
  filter(!is.na(value)) %>%
  select(-key) %>%
  mutate(value = as.numeric(value)) %>%
  spread(key = lab, value = value)

# Extract Skater Ratings only (remove goalies)
skater_ratings <- ratings %>%
  select(-Blocker, -Glove, -`Low Shots`, -Mental, -Positioning, -`Rebound Control`, -`Recovery`) %>%
  filter(!is.na(`Shooting Accuracy`)) %>%
  inner_join(ohl_2020  %>%
               filter(player_url != "https://www.eliteprospects.com/player/564485/dylan-robinson") %>%
               select(name, position) %>%
               distinct(),
             by = c("player_name" = "name"))

# Extract Goalie Ratings Only (remove skaters)
goalie_ratings <- ratings %>% 
  select(url, player_name, Blocker, Glove, `Low Shots`, Mental, Positioning, `Rebound Control`, `Recovery`) %>% 
  filter(!is.na(Blocker)) %>% 
  inner_join(select(ohl_2020, name, position), by = c("player_name" = "name")) %>% 
  distinct()

## Analyze Skater Rating Data ---

ohl_stats_and_ranks <- ohl_2020 %>% 
  mutate(team = case_when(
    team == "Niagara IceDogs" ~ "Niagara Ice Dogs",
    team == "Soo Greyhounds" ~ "Sault Ste. Marie Greyhounds",
    TRUE ~ as.character(team)
  )) %>% 
  inner_join(skater_ratings, by = c("player_url" = "url")) %>% 
  select(-contains("playoffs"), -goals_against_average, -save_percentage, -player_name) 

plot_team_offensive_ratings <- function(TEAM) {
  ohl_stats_and_ranks %>% 
    filter(team == TEAM) %>% 
    select(name, position = position.x, points, Offensive, Speed, `Shot Accuracy` = `Shooting Accuracy`, Passing, Puckhandling) %>% 
    gather(key = "label", value = "Rating", -name, -points, -position) %>% 
    ggplot(aes(y = reorder(paste(name, "|", position, "|", points, "points"), points), x = label, fill = Rating)) +
    geom_tile() +
    scale_fill_gradientn(colors = wes_palette('Zissou1', 3, 'continuous')) +
    labs(x = "Skill", y = "Player", title = paste0("2019-20 ", TEAM, " Offensive Skill Ratings"), 
         subtitle = "Stats and Ratings from Elite Prospects and Franchise Hockey Manager")
}

skater_ratings %>% 
  gather(key = "skill", value = "rating", -url, -player_name, -position) %>% 
  mutate(position = ifelse(str_detect(position, "D"), "D", "F")) %>% 
  ggplot(aes(x = skill, y = rating)) +
  geom_boxplot() +
  facet_wrap(~position) +
  coord_flip() + 
  labs(x = "Rating", y = "Skill", title = "Distributions of 2019-20 OHL FHM Ratings, Grouped By Position")

map(opponents$team, plot_team_offensive_ratings)
plot_team_offensive_ratings("Erie Otters")
