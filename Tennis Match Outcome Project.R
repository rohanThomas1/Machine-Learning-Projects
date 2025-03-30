################################
# 1 DATA LOADING
# Loading libraries
pacman::p_load(tidyverse, ggplot2, dplyr, caret, ROCR, Metrics, purrr, readr, 
               knitr, lubridate, readxl)

# Set the directory where your files are located
directory <- "C:/Users/rohan/OneDrive/Documents/MDS Research project/BettingFiles"

# Get a list of files in the directory
file_list <- list.files(directory)

i <- 0

# Loop through each file in the list
for (file in file_list) {
  # Read the file (you may need to specify other arguments depending on the file format)
  raw_data <- read_excel(file.path(directory, file), na = c('N/A',''))
  
  raw_data <- raw_data %>% 
    mutate(WRank = if_else(WRank == "NR", NA, WRank),
           LRank = if_else(LRank == "NR", NA, LRank))
  
  raw_data$WRank <- as.numeric(raw_data$WRank)
  raw_data$LRank <- as.numeric(raw_data$LRank)
  
  if ("WPts" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(WPts = as.numeric(WPts),
             LPts = as.numeric(LPts))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(WPts = 0, LPts = 0)
  }
  
  if ("B365W" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_B365_w = (B365L/(B365W+B365L)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_B365_w = NA)
  }
  
  if ("CBW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_CB_w = (CBL/(CBW+CBL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_CB_w = NA)
  }
  
  if ("GBW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_GB_w = (GBL/(GBW+GBL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_GB_w = NA)
  }
  
  if ("EXW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_EX_w = (EXL/(EXW+EXL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_EX_w = NA)
  }
  
  if ("IWW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_IW_w = (IWL/(IWW+IWL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_IW_w = NA)
  }
  
  if ("LBW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_LB_w = (LBL/(LBW+LBL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_LB_w = NA)
  }
  
  if ("SBW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_SB_w = (SBL/(SBW+SBL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_SB_w = NA)
  }
  
  if ("B&WW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_BW_w = (`B&WL`/(`B&WW`+`B&WL`)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_BW_w = NA)
  }
  
  if ("PSW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_PS_w = (PSL/(PSW+PSL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_PS_w = NA)
  }
  
  if ("SJW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_SJ_w = (SJL/(SJW+SJL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_SJ_w = NA)
  }
  
  if ("UBW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_UB_w = (UBL/(UBW+UBL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_UB_w = NA)
  }
  
  if ("AvgW" %in% colnames(raw_data)) {
    raw_data <- raw_data %>% 
      mutate(p_AVG_w = (AvgL/(AvgW+AvgL)))
    
  } else {
    raw_data <- raw_data %>% 
      mutate(p_AVG_w = NA)
  }
  
  if(i == 0) {
    betting_data <- raw_data %>% 
      select(ATP, Location, Tournament, Date, Series, Court, Surface, 
             Round, Winner, Loser, WRank, LRank, WPts, LPts, Comment, p_B365_w, 
             p_CB_w, p_GB_w, p_EX_w, p_IW_w, p_LB_w, p_SB_w, p_BW_w, 
             p_PS_w, p_SJ_w, p_UB_w, p_AVG_w)
  } else {
    tmp_data <- raw_data %>% 
      select(ATP, Location, Tournament, Date, Series, Court, Surface, 
             Round, Winner, Loser, WRank, LRank, WPts, LPts, Comment, p_B365_w, 
             p_CB_w, p_GB_w, p_EX_w, p_IW_w, p_LB_w, p_SB_w, p_BW_w, 
             p_PS_w, p_SJ_w, p_UB_w, p_AVG_w)
    
    betting_data <- bind_rows(betting_data, tmp_data)
  }
  
  betting_data <- betting_data %>% 
    filter(is.na(p_B365_w)==FALSE | is.na(p_CB_w)==FALSE | 
           is.na(p_GB_w)==FALSE | is.na(p_EX_w)==FALSE | 
           is.na(p_IW_w)==FALSE | is.na(p_LB_w)==FALSE | 
           is.na(p_SB_w)==FALSE | is.na(p_BW_w)==FALSE | 
           is.na(p_PS_w)==FALSE | is.na(p_SJ_w)==FALSE |  
           is.na(p_UB_w)==FALSE | is.na(p_AVG_w)==FALSE
           )
  i <- i + 1
}



betting_data <- betting_data %>% 
  mutate(Date = as.Date(as.character(Date), "%Y-%m-%d"),
         Location = as.factor(Location),
         Tournament = as.factor(Tournament),
         Series = as.factor(Series),
         Court = as.factor(Court),
         Surface = as.factor(Surface),
         Comment = as.factor(Comment))


##################################
# Plot the time series
betting_data <- as.data.frame(betting_data)
matches_count <- table(betting_data$Date)
matches_count_df <- as.data.frame(matches_count)
matches_count_df$Date <- as.Date(names(matches_count))


# Plot the time series
matches_count_df %>% filter(Date >= dmy("01-01-2020") & Date <= dmy("01-02-2022")) %>% 
ggplot(aes(x = Date, y = Freq)) +
  geom_line() +
  labs(x = "Date", y = "Number of Matches", 
       title = "2020-2022 Time Series Plot: Number of Matches played(Day-wise)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggplot(betting_data, aes(x = Surface, fill = Surface)) +
  geom_bar(width = 0.5) +
  labs(x = "Surface Type", y = "Number of Matches", 
       title = "Bar Plot: Number of Matches vs Surface") +
  theme(plot.title = element_text(hjust = 0.5),
        #axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))+
  guides(fill = FALSE) # To remove the legend


######################################

betting_data$Comment <- recode_factor(betting_data$Comment, "Rrtired" = "Retired")

betting_data <- betting_data %>% 
  mutate(WRank = replace_na(WRank, 9999),
         LRank = replace_na(LRank, 9999),
         WPts = replace_na(WPts, 0),
         LPts = replace_na(LPts, 0))

betting_data <- betting_data %>% 
  mutate(higher_rank_win_prob = ifelse(WRank < LRank, 1, 0),
         rank_difference = abs(WRank - LRank))


betting_data$na_count <- 12 - rowSums(is.na(betting_data[, 16:27]))

betting_data <- betting_data %>%
  mutate_at(vars(16:27), ~ replace_na(., 0.5))

betting_data <- betting_data %>% 
    mutate(winner_probability = (log(p_B365_w/(1-p_B365_w)) + 
             log(p_CB_w/(1-p_CB_w)) + log(p_GB_w/(1-p_GB_w)) + 
             log(p_EX_w/(1-p_EX_w)) + log(p_IW_w/(1-p_IW_w)) + 
             log(p_LB_w/(1-p_LB_w)) + log(p_SB_w/(1-p_SB_w)) + 
             log(p_BW_w/(1-p_BW_w)) + log(p_PS_w/(1-p_PS_w)) +
             log(p_SJ_w/(1-p_SJ_w)) + log(p_UB_w/(1-p_UB_w)) +
             log(p_AVG_w/(1-p_AVG_w)))/(na_count)
           )

betting_data <- betting_data %>%
  mutate(final_winner_probability = exp(winner_probability)/(1 + exp(winner_probability)))

betting_data$tmp_prob <- betting_data$final_winner_probability

betting_data <- betting_data %>% 
  filter(!is.na(final_winner_probability)) %>% 
  mutate(final_winner_probability = if_else(final_winner_probability > 0.5, 1, 0))



### Player List

betting_data_players <- tibble(
  player_name = unique(union(unique(betting_data$Winner), unique(betting_data$Loser)))
  )

betting_data_players <- betting_data_players %>% 
  mutate(player_id = as.numeric(row_number()))

final_betting_data <- betting_data %>%
  left_join(betting_data_players %>% select(player_id, player_name),
            by = c("Winner" = "player_name")) %>% 
  rename(winner_player_id = player_id) %>% 
  left_join(betting_data_players %>% select(player_id, player_name),
            by = c("Loser" = "player_name")) %>% 
  rename(loser_player_id = player_id)


final_betting_data <- as.data.frame(final_betting_data)

betting_data_players <- as.data.frame(betting_data_players)


##########
### Elo model

# k constant

# Creating 2 new columns for elo rating
final_betting_data$winner_elo_rating_k <- 0
final_betting_data$loser_elo_rating_k <- 0

# Setting the default elo rating to 1500 for all players
betting_data_players$elo_rating_k <- 1500
betting_data_players$num_games <- 0

# Elo rating calculation
#K_Factor
k <- 25

elo_predictions_k <- tibble(tourney_date = dmy(NA), elo_rating_diff = double(), elo_prob = double())

for (index in 1:nrow(final_betting_data)) {
  w_id <- final_betting_data[index, 'winner_player_id']
  l_id <- final_betting_data[index, 'loser_player_id']
  t_date <- final_betting_data[index, 'Date']
  
  current_winner_elo <- betting_data_players[betting_data_players$player_id == w_id, 'elo_rating_k']
  current_loser_elo <- betting_data_players[betting_data_players$player_id == l_id, 'elo_rating_k']
  
  log_func <- (1 + 10^((current_loser_elo - current_winner_elo) / 400))^-1
  
  new_winner_elo <- current_winner_elo + k * (1 - log_func)  # update with win condition
  new_loser_elo <- current_loser_elo + k * (-log_func)  # update with loss condition
  
  final_betting_data[index, 'winner_elo_rating_k'] <- new_winner_elo
  final_betting_data[index, 'loser_elo_rating_k'] <- new_loser_elo
  
  betting_data_players[betting_data_players$player_id == w_id, 'elo_rating_k'] <- new_winner_elo
  betting_data_players[betting_data_players$player_id == l_id, 'elo_rating_k'] <- new_loser_elo
  
  elo_predictions_k <- elo_predictions_k %>% 
    add_row(tourney_date = t_date,
            elo_rating_diff = abs(new_winner_elo - new_loser_elo),
            elo_prob = log_func)
}


# 538
delta <- 150
nu <- 100
sigma <- 0.5

final_betting_data$winner_elo_rating <- 0
final_betting_data$loser_elo_rating <- 0

betting_data_players$elo_rating <- 1500
betting_data_players$num_games <- 0
betting_data_players$games_won <- 0
  
elo_predictions <- tibble(tourney_date = dmy(NA), elo_rating_diff = double(), elo_prob = double())
      
for (index in 1:nrow(final_betting_data)) {
  
  w_id <- final_betting_data[index, 'winner_player_id']
  l_id <- final_betting_data[index, 'loser_player_id']
  t_date <- final_betting_data[index, 'Date']
  
  current_winner_elo <- betting_data_players[betting_data_players$player_id == w_id, 'elo_rating']
  current_loser_elo <- betting_data_players[betting_data_players$player_id == l_id, 'elo_rating']
  
  log_func <- (1 + 10^((current_loser_elo - current_winner_elo) / 400))^-1
  
  m_winner <- betting_data_players[betting_data_players$player_id == w_id, 'num_games']
  m_loser <- betting_data_players[betting_data_players$player_id == l_id, 'num_games']
  
  games_won <- betting_data_players[betting_data_players$player_id == w_id, 'games_won']
  
  winner_k <- delta / (m_winner + nu)^sigma
  loser_k <- delta / (m_loser + nu)^sigma
  
  if(final_betting_data[index, 'Series'] == "Grand Slam") {
    if(final_betting_data[index, 'Round'] == "The Final") {
      winner_k <- winner_k * 1.4
      loser_k <- loser_k * 1.4
      } else if(final_betting_data[index, 'Round'] == "Semifinals") {
        winner_k <- winner_k * 1.3
        loser_k <- loser_k * 1.3
        } else if(final_betting_data[index, 'Round'] == "Quarterfinals") {
          winner_k <- winner_k * 1.2
          loser_k <- loser_k * 1.2
          } else {
            winner_k <- winner_k * 1.1
            loser_k <- loser_k * 1.1
          }
  }
  
  new_winner_elo <- current_winner_elo + winner_k * (1 - log_func)  # update with win condition
  new_loser_elo <- current_loser_elo + loser_k * (-log_func)  # update with loss condition
  
  final_betting_data[index, 'winner_elo_rating'] <- new_winner_elo
  final_betting_data[index, 'loser_elo_rating'] <- new_loser_elo
  
  betting_data_players[betting_data_players$player_id == w_id, 'elo_rating'] <- new_winner_elo
  betting_data_players[betting_data_players$player_id == l_id, 'elo_rating'] <- new_loser_elo
  
  betting_data_players[betting_data_players$player_id == w_id, 'num_games'] <- m_winner + 1
  betting_data_players[betting_data_players$player_id == l_id, 'num_games'] <- m_loser + 1
  
  betting_data_players[betting_data_players$player_id == w_id, 'games_won'] <- games_won + 1
  
  elo_predictions <- elo_predictions %>% 
    add_row(tourney_date = t_date,
            elo_rating_diff = abs(new_winner_elo - new_loser_elo),
            elo_prob = log_func)
}


split_time <- dmy("01-01-2019")

test_final_betting_data <- filter(final_betting_data, Date >= dmy("01-01-2019"))

test_elo_predictions <- filter(elo_predictions, tourney_date >= dmy("01-01-2019"))

elo_accuracy <- mean(ifelse(test_elo_predictions$elo_prob > 0.5, 1, 0) == test_final_betting_data$final_winner_probability)

w <- test_final_betting_data$final_winner_probability
N <- nrow(test_elo_predictions)
elo_log_loss <- -1 / N * sum(w * log(test_elo_predictions$elo_prob) + 
                               (1 - w) * log(1 - test_elo_predictions$elo_prob), na.rm = T)

elo_calibration <- sum(test_elo_predictions$elo_prob) / sum(w)


ggplot(aes(x = elo_rating_diff, y = elo_prob, color = factor(elo_prob > 0.5)), data = test_elo_predictions) +
  geom_point() +
  labs(title = "Elo K-Factor model(k=25)",
       x = "Difference in Elo rating between the winner and loser player", 
       y = "Probability of higher Elo rated player winning") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c("<= 0.5", "> 0.5"),
                     name = "Probability") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))


###################################################

### Splitting data

split_time <- dmy("01-01-2019")

train_final_betting_data <- filter(final_betting_data, Date < split_time)

test_final_betting_data <- filter(final_betting_data, Date >= split_time)


### Naive model
N <- nrow(test_final_betting_data)
w <- test_final_betting_data$higher_rank_win_prob

# Accuracy
naive_accuracy <- mean(test_final_betting_data$higher_rank_win_prob)

# Log-loss
naive_pi <- naive_accuracy
naive_log_loss <- -1 / N * sum(w * log(naive_pi) + (1 - w) * log(1 - naive_pi))

# Calibration
naive_calibration <- naive_pi * N / sum(w)

test_final_betting_data %>% 
  ggplot(aes(x = rank_difference, y = higher_rank_win_prob, color = factor(higher_rank_win_prob > 0.5))) +
  geom_point() +
  labs(title = "Naive Model: Predictor vs Response variable",
       x = "Difference in ranking between players", 
       y = "Probability of higher rank winning") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c("0", "1"),
                     name = "Probability") +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))

#####################################################
### Logistic Regression

log_data_fit <- glm(final_winner_probability ~ rank_difference+0, 
                    data = train_final_betting_data, 
                    family = binomial(link = 'logit'))
      
summary(log_data_fit)
      
# Model prediction using test data
log_prob_diff <- tibble(prob = predict(log_data_fit, test_final_betting_data, type = 'response'))

# Table creation
log_tmp_df <- tibble(rank_difference = test_final_betting_data$rank_difference, prob = log_prob_diff$prob)

# Accuracy calculation
accuracy <- mean(ifelse(log_tmp_df$prob > 0.5, 1, 0) == test_final_betting_data$final_winner_probability)

# Log-loss calculation
w <- test_final_betting_data$final_winner_probability
N <- nrow(test_final_betting_data)
log_loss <- -1 / N * sum(w * log(log_tmp_df$prob) + (1 - w) * log(1 - log_tmp_df$prob), na.rm = T)
      
# Calibration calculation
calibration <- sum(log_tmp_df$prob) / sum(w)

min(log_tmp_df$prob)

ggplot(aes(x = rank_difference, y = prob, color = factor(prob > 0.5)), data = log_tmp_df) +
  geom_point() +
  labs(title = "Logistic Model",
       x = "Rank difference between the winner and loser player", 
       y = "Probability of the higher ranked player winning") +
  scale_color_manual(values = c("#00BFC4"),
                     labels = c(">0.5"),
                     name = "Probability") +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))


#==========================================

final_betting_data %>% filter(Date >= dmy("01-01-2024"), WRank %in% c(1,2,3,4,5,6)) %>% 
  select(Winner, WRank, winner_player_id) %>% unique()

plot_data <- final_betting_data %>% 
  filter(Date >= dmy("01-01-2008") & Date < dmy("01-01-2018") &
           winner_player_id %in% c(44, 366, 279, 390)) %>% 
  select(Date, winner_player_id, Winner, WPts) %>% 
  as_tibble()

plot_data2 <- final_betting_data %>% 
  filter(Date >= dmy("01-01-2008") & Date < dmy("01-01-2018") &
           loser_player_id %in% c(44, 366, 279, 390)) %>%
  select(Date, loser_player_id, Loser, LPts) %>% 
  as_tibble()

plot_data <-  plot_data %>% 
  add_row(Date = plot_data2$Date, 
          winner_player_id = plot_data2$loser_player_id,
          Winner = plot_data2$Loser, 
          WPts = plot_data2$LPts)

ggplot(plot_data, aes(x = Date, y = WPts, color=Winner)) +
  geom_line(size=0.8) +
  labs(title = "Big Four players' ranking points over time", x = "Tournament Date", 
       y = "Ranking points", color = "Player") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))  # Center the title

filter(final_betting_data, Surface=="Clay") %>% count()

nrow(final_betting_data)
#=================================================

betting_data_players %>% 
  filter(player_name %in% c("Murray A.", "Djokovic N.", "Nadal R.", "Federer R."))

betting_data_players[grep("Ferrero", betting_data_players$player_name), ]

elo_plot_data <- final_betting_data %>% 
  filter(Date >= dmy("01-01-2008") & Date < dmy("01-01-2019") &
           winner_player_id %in% c(44, 366, 279, 390)) %>% 
  select(Date, Winner, WPts, winner_elo_rating_k, winner_elo_rating) %>% 
  as_tibble()

elo_plot_data2 <- final_betting_data %>% 
  filter(Date >= dmy("01-01-2008") & Date < dmy("01-01-2019") &
           loser_player_id %in% c(44, 366, 279, 390)) %>%
  select(Date, Loser, LPts, loser_elo_rating_k, loser_elo_rating) %>% 
  as_tibble()

elo_plot_data <-  elo_plot_data %>% 
  add_row(Date = elo_plot_data2$Date, 
          Winner = elo_plot_data2$Loser, 
          WPts = elo_plot_data2$LPts,
          winner_elo_rating_k = elo_plot_data2$loser_elo_rating_k,
          winner_elo_rating = elo_plot_data2$loser_elo_rating)

ggplot(elo_plot_data, aes(x = Date)) +
  geom_line(aes(y = winner_elo_rating_k, color = "K factor"), size = 1) +
  geom_line(aes(y = winner_elo_rating, color = "K FTE"), size = 1) +
  #geom_line(aes(y = WPts, color = "normal"), size = 1) +
  facet_wrap(~Winner, scales = "free_y", ncol = 2) +
  labs(title = "Elo Rating variation of Big Four players based on K-Factor and FiveThirtyEight",
       x = "Tournament Date",
       y = "Elo Rating",
       color = "Metric") +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        legend.position = "bottom",
        legend.justification = "right",
        axis.text = element_text(size = 11))





#================================================
betting_data_players

top_10_elo <- betting_data_players %>%
  arrange(desc(elo_rating)) %>%  # Arrange the dataframe in descending order of elo_rating
  slice(1:10)  # Select the first 10 rows (top 10 players based on elo_rating)

# Create a bar plot for top 10 players based on elo_rating
ggplot(top_10_elo, aes(x = reorder(player_name, -elo_rating), y = elo_rating)) +
  geom_bar(stat = "identity", fill = "#F8766D") +
  labs(title = "Top 10 Players Based on Elo 538 Rating with best parameters",
       x = "Player Name",
       y = "Elo Rating") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))

# Select the top 10 players based on num_games
top_10_num_games <- betting_data_players %>%
  arrange(desc(num_games)) %>%  # Arrange the dataframe in descending order of num_games
  slice(1:10)  # Select the first 10 rows (top 10 players based on num_games)

# Create a bar plot for top 10 players based on num_games
ggplot(top_10_num_games, aes(x = reorder(player_name, -win_percent), y = win_percent)) +
  geom_bar(stat = "identity", fill = "#00BFC4") +
  labs(title = "Top 10 Players Based on Win probability",
       x = "Player Name",
       y = "Win probability") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11))

top_10_num_games$win_percent <- round(top_10_num_games$games_won/top_10_num_games$num_games,3)
  
ggplot(top_10_num_games, aes(x = reorder(player_name, -games_won), y = num_games)) +
  geom_bar(aes(fill = "Total Games Played"), stat = "identity") +  # Total games played
  geom_bar(aes(y = games_won, fill = "Games Won"), stat = "identity") +  # Games won
  labs(title = "Top 10 Players Based on Number of Games Won",
       x = "Player Name",
       y = "Number of Games Played") +
  scale_fill_manual(values = c("Total Games Played" = "#F8766D", "Games Won" = "#00BFC4")) +  # Define colors
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 11),
        legend.title = element_blank(),  # Remove legend title
        legend.position = "bottom")

