library(tidyverse)

# Turn off scientific notation
options(scipen = 999)

## Data load - Filter Data set for 5v5 data and dump-ins/outs and faceoffs ----
clean_xg <- ohl_with_xg %>% 
  filter(!event %in% c("Dump In/Out", "Faceoff Win"),
         situation == "5v5") %>% 
  mutate(absorption = case_when(
    event == "Goal" ~ 1,
    team != lead(team) & period == lead(period) ~ 1,
    lead(event) == "Faceoff Win" ~ 1,
    lead(game_date) != game_date ~ 1,
    TRUE ~ 0),
    # ADD CREASE
    playsection = case_when(
      playsection == "innerSlot" & y >= 38 & y < 47 & x > 182 ~ "crease",
      TRUE ~ as.character(playsection)
    ),
    # CREATE ABSORPTION STATES
    absorption_state = case_when(
      event == "Goal" ~ "goal",
      team != lead(team) & period == lead(period) ~ "possession_change",
      TRUE ~ as.character(NA)),
    last_play_absorption = ifelse(lag(absorption) == 1 | row_number() == 1, "Y", "N")
    )  %>% 
  select(game_date, situation, playsection, score, event, player, team,  absorption, absorption_state, last_play_absorption, row, x, y, x_plot, y_plot) %>% 
  group_by(last_play_absorption) %>% 
  mutate(seq = ifelse(last_play_absorption == "Y", row_number(), NA)) %>% 
  ungroup() %>% 
  fill(seq) %>% 
  group_by(seq) %>% 
  mutate(play_of_seq = row_number()) %>% 
  ungroup() %>% 
  mutate(plays_in_seq = ifelse(lead(last_play_absorption) == "Y", play_of_seq, NA)) %>% 
  fill(plays_in_seq, .direction = "up") %>% 
  unite(play_state, situation, playsection, 
        sep = "--", remove = FALSE) %>%
  mutate(next_state = ifelse(absorption == 1, absorption_state, lead(play_state)))

## Markov Chain Prep ----
# Calculated frequency of each zone "state"
transient_state_df <- clean_xg %>%
  group_by(play_state) %>%
  count() %>%
  ungroup() %>%
  mutate(state_prop = n / sum(n)) %>% 
  arrange(desc(state_prop))

# View the distribution of the frequencies:
ggplot(transient_state_df, aes(x = n)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Frequency of state",
       y = "Count")
  
# Get absorption states 
absorption_states <- unique(clean_xg$absorption_state)[2:3]

# Calculate transition probabilities of each state combination
transitions <- clean_xg %>% 
  group_by(play_state, next_state) %>%
  count() %>%
  ungroup() %>%
  group_by(play_state) %>%
  mutate(total_plays = sum(n)) %>%
  ungroup() %>%
  mutate(transition_prob = n / total_plays) %>%
  # Append rows that are just the absorptions for ease in making the 
  # complete transition matrix:
  bind_rows(data.frame(play_state = absorption_states,
                       next_state = absorption_states,
                       transition_prob = rep(1, length(absorption_states)))) %>% 
  filter(!is.na(next_state))

# Create transition matrix 
transition_matrix <- transitions %>%
  select(play_state, next_state, transition_prob) %>%
  arrange(desc(play_state), desc(next_state)) %>% 
  spread(next_state, transition_prob) 

transition_matrix[is.na(transition_matrix)] <- 0


## Fundamental Matrix Calculation ----

# Find the indices of absorption states:
row_absorption_i <- which(transition_matrix$play_state %in% absorption_states)
col_absorption_i <- which(colnames(transition_matrix) %in% absorption_states)

# Grab the Q matrix - n x n transition matrix for transient states:
q_matrix <- as.matrix(transition_matrix[1:(row_absorption_i[1] - 1),
                                           2:(col_absorption_i[1] - 1)])
# Grab the R matrix - n x r transition matrix to the absorption states:
r_matrix <- as.matrix(transition_matrix[1:(row_absorption_i[1] - 1),
                                           col_absorption_i])

# Calculate the fundamental matrix - (I-Q)**(-1)
fundamental_matrix <- solve(diag(nrow = nrow(q_matrix),
                                 ncol = nrow(q_matrix)) - q_matrix)

## Results ----

# Calculate expected number of plays by zone 
expected_n_plays <- rowSums(fundamental_matrix)

# Calculate probability of absorption for each zone
prob_absorption <- fundamental_matrix %*% r_matrix

# Make transition probabilities into a cleaner data frame
absorption_df <- as.data.frame(prob_absorption) %>%
  mutate(play_state = rownames(prob_absorption),
         expected_n_plays = expected_n_plays,
         playsection = substr(play_state, 6, nchar(play_state))) %>% 
  as_tibble() 

# Record maximum absorption probabilities for goals and possession changes
absorption_df[colnames(absorption_df) %in% absorption_states] %>% 
  gather(absorbing_state, absorbing_prob) %>%
  group_by(absorbing_state) %>%
  summarise(max_absorbing_prob = max(absorbing_prob))

# Record values by each individual 
goal_contribution_values <- absorption_df %>% 
  select(playsection, goal) %>% 
  filter(playsection != "NA") %>% 
  arrange(desc(goal))

# Append goal contributions to each play
play_values <- clean_xg %>%
  left_join(goal_contribution_values) %>% 
  select(game_date, event, player, team, goal, absorption, absorption_state, playsection, x, y, x_plot, y_plot) %>% 
  mutate(contr = case_when(
    is.na(absorption_state) ~ lead(goal) - goal,
    absorption_state == "goal" ~ 1 - goal,
    absorption_state == "possession_change" ~ 0 - goal)
    ) 

# Record goal contributions and play counts for each player
pv <- play_values %>% 
  filter(team == "Erie Otters",
         !event %in% c("Faceoff Win", "Penalty Taken")) %>% 
  group_by(player) %>% 
  summarise(contr = sum(contr, na.rm = TRUE),
            n = n()) 

# Best players
pv %>% 
  arrange(desc(contr))

# Worst players
pv %>% 
  arrange(contr)
