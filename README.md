# Board-Game-Geek-Stats

```R
library(jsonlite)
library(tidyverse)
library(scales)

data <- as.list(fromJSON(txt = "C:/jmidkiff/Personal Work/BGStatsExport.json"))

plays <- as_tibble(data$plays)
games <- as_tibble(data$games)
players <- as_tibble(data$players)

games_joined <- left_join(plays, games, by = c("gameRefId" = "id")) #%>% 

###### Scythe ######  
(scythe <- games_joined %>% 
  filter(bggName == "Scythe") %>% 
  select(bggId.y, bggName, playerScores, playDate, locationRefId) %>% 
  rename(bggId = bggId.y))

scythe_plays <- vector("list", length = nrow(scythe))
for (session in 1:nrow(scythe)) {
  scythe_plays[[session]] <- scythe$playerScores[[session]] %>% 
    mutate(score = as.integer(score), 
           winning_score = as.integer(max(scythe$playerScores[[session]]$score)), 
           percent_of_victory = score/winning_score, 
           game = "Scythe", 
           date = scythe$playDate[session])
}

scythe_plays <- as_tibble(bind_rows(scythe_plays)) %>% 
  filter(!is.na(score))

(final_scythe_plays <- scythe_plays %>% 
  group_by(role) %>% 
  summarise(avg_faction_score = mean(percent_of_victory), 
            avg_score = mean(score), 
            games_played = n(), 
            median_faction_score = median(percent_of_victory), 
            median_score = median(score), 
            wins = sum(winner), 
            win_perc = wins / games_played) %>% 
  arrange(desc(median_faction_score)))

# Scythe No Automa
scythe_plays_no_automa <- vector("list", length = nrow(scythe))
for (session in 1:nrow(scythe)) {
  #Get the max score for the session
  session_no_automa <- scythe$playerScores[[session]] %>% 
    filter(playerRefId != 4)
  session_max_score_no_automa <- max(as.integer(session_no_automa$score))
  
  scythe_plays_no_automa[[session]] <- scythe$playerScores[[session]] %>% 
    filter(playerRefId != 4) %>% 
    mutate(score = as.integer(score), 
           winning_score = as.integer(session_max_score_no_automa), 
           percent_of_victory = score/winning_score, 
           game = "Scythe", 
           date = scythe$playDate[session], 
           winner = if_else(score == winning_score, TRUE, FALSE))
}

scythe_plays_no_automa <- as_tibble(bind_rows(scythe_plays_no_automa)) %>% 
  filter(!is.na(score))

(final_scythe_plays_no_automa <- scythe_plays_no_automa %>% 
    group_by(role) %>% 
    summarise(avg_faction_score = mean(percent_of_victory), 
              avg_score = mean(score), 
              games_played = n(), 
              median_faction_score = median(percent_of_victory), 
              median_score = median(score), 
              wins = sum(winner), 
              win_perc = wins / games_played) %>% 
    arrange(desc(median_faction_score)))

# Initial Graph - No Automa
ggplot(final_scythe_plays_no_automa, aes(x = role, y = median_faction_score, fill = role)) +
  geom_col(color = "black", size = 1) +
  scale_fill_manual(values = c("Albion" = "#4A7A36", 
                           "Togawa" = "#6A3897", 
                           "Crimea" = "#F3AA38", 
                           "Nordic" = "#6ABDE6", 
                           "Polonia" = "#FFFFFF", 
                           "Rusviet" = "#EA162C", 
                           "Saxony" = "#222222")) +
  labs(title = "Median Faction Score as a Percentage of Winning Score (Automa Excluded)", 
       caption = str_c("Instances in which the automa controlled a faction are excluded.\n", 
                       "In those games, the highest score by a player-controlled faction was declared the winning score.")) + 
  scale_y_continuous(labels = percent) +
  theme(legend.position = "none", 
        axis.title = element_blank())
        
```
