library(tidyverse)
library(ggthemes)
library(scales)
library(plotly)

raw_scores <- read_csv(".//data//scores.csv")
scores <- raw_scores %>% 
  mutate(modified_date = paste(date, game_number)) %>% 
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"))) %>% 
  gather("player", "score", -date, -max_score, -game_number, -modified_date)

ggplot(scores, aes(x = modified_date, y = score, group = player, colour = player)) +
  geom_line()
ggplot(scores, aes(x = modified_date, y = score, group = player, colour = player)) +
  geom_point() 

ggplot(scores, aes(x = modified_date, y = score, group = player, colour = player)) +
  geom_bar(stat = "identity", position = "identity") 

test_scores <- scores %>% 
  filter(date > "2018/08/01")

ggplot(test_scores, aes(x = modified_date, y = score, group = player, colour = player)) +
  geom_line() +
  geom_point()

differences <- raw_scores %>% 
  mutate(diff = andrew - yuri) %>% 
  mutate(winner = case_when(
    diff > 0 ~ "Andrew",
    TRUE ~ "Yuri"
  )) %>% 
  mutate(yuri_score = yuri * -1,
         andrew_score = andrew) %>% 
  mutate(modified_date = paste(date, game_number)) %>% 
  mutate(date = as.Date(date, tryFormats = c("%m/%d/%Y"))) 


test <- differences %>% 
  dplyr::arrange(game_number) %>% 
  dplyr::arrange(date) %>% 
  mutate(modified_date = as.factor(modified_date)) 
factor(test$modified_date)

differences %>% 
  dplyr::arrange(game_number) %>% 
  dplyr::arrange(date) %>% 
  mutate(modified_date = as.factor(modified_date)) %>% 
  ggplot(aes(x = modified_date, y = diff, fill = winner)) +
  # fivethirtyeight theme
  theme_fivethirtyeight() +
  # Custom coloring for barchart
  scale_fill_manual("legend", values = c("Andrew" = "#497999", "Yuri" = "#FFA500")) +
  geom_bar(stat = "identity") +
  # Points for andrew
  geom_point(data = differences %>% filter(winner == "Andrew"), aes(x = modified_date, y = andrew_score), color = "#497999") +
  geom_point(data = differences %>% filter(!winner == "Andrew"), aes(x = modified_date, y = andrew_score), color = "#497999", alpha = .15) +
  # points for yuri
  geom_point(data = differences %>% filter(winner == "Yuri"), aes(x = modified_date, y = yuri_score), color = "#FFA500") +
  geom_point(data = differences %>% filter(!winner == "Yuri"), aes(x = modified_date, y = yuri_score), color = "#FFA500", alpha = .15) +
  # adjusting the theme
  theme(
    axis.text.x = element_text(size = 5, angle = 45),
    # reinstituting xlab and ylab because the themefivethirtyeight removes it.
    axis.title = element_text()
  ) +
  labs(x = "Date and Game",
       y = "Score Difference") 
  
  p + theme(axis.text.x = element_text(face="bold", color="#993333", 
                                       size=14, angle=45),
            axis.text.y = element_text(face="bold", color="#993333", 
                                       size=14, angle=45))

  
  

# Total record ------------------------------------------------------------

differences
differences %>% 
  select(date, winner) %>%
  group_by(winner) %>% 
  summarise(
    Wins = n()
  ) %>% 
  ggplot(aes(x = winner, y = Wins, fill = winner)) +
  theme_fivethirtyeight() +
  geom_bar(stat = "identity") +
  geom_label(aes(label = Wins), nudge_y = 3, label.size = 0) +
  scale_fill_manual("legend", values = c("Andrew" = "#497999", "Yuri" = "#FFA500")) +
  coord_flip() +
  theme(
    # removing bg color
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "none"
  ) 

re

# running reecord ---------------------------------------------------------

differences %>% 
  dplyr::arrange(game_number) %>% 
  dplyr::arrange(date) %>% 
  tail(n = 15)



# re-spreading ------------------------------------------------------------

differences %>% 
  # arranging to reverse chronological
  dplyr::arrange(desc(game_number)) %>% 
  dplyr::arrange(desc(date)) %>% 
  # only selecting relevant columns
  select(date, winner, andrew, yuri, game_number) %>% 
  # making it pretty
  rename(
    Date = date,
    Winner  = winner,
    `Yuri Score` = yuri,
    `Andrew Score` = andrew,
    `Game Number` = game_number
  ) %>% 
  # For Kableextra, making the winner name the right color
  mutate(Winner = 
    case_when(
      Winner == "Yuri" ~ cell_spec(Winner, background = "#FFA500", color = "white"),
      TRUE ~ cell_spec(Winner, background = "#497999", color = "white")
    )
  ) %>% 
  kable(escape = F) %>% 
  kable_styling("hover")
  


# highest scored game -----------------------------------------------------
differences %>% 
  filter(max_score == 11) %>% 
  select(yuri, andrew, date) %>% 
  rename(Yuri = yuri, 
         Andrew = andrew) %>% 
  gather("key", "value", -date) %>%
  arrange(desc(value)) %>% 
  slice(1)
  

differences %>% 
  # filter(max_score == 11) %>% 
  select(diff, winner, date, yuri, andrew) %>% 
  mutate(diff = abs(diff)) %>% 
  arrange(desc(diff)) %>% 
  slice(1)



# streaks -----------------------------------------------------------------

# https://stackoverflow.com/questions/4655848/calculating-a-consecutive-streak-in-data
# https://stackoverflow.com/questions/31139356/how-do-you-convert-information-from-rle-into-a-data-frame 
differences
rle(differences$winner)

z <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
rle(z)
data.frame(unclass(rle(z)))
rle(as.character(z))
print(rle(z), prefix = "..| ")

streak <- differences %>% 
  dplyr::arrange(game_number) %>% 
  dplyr::arrange(date)
streak <- data.frame(unclass(rle(as.vector(streak$winner)))) %>% 
  arrange(desc(lengths)) %>% 
  slice(1)



# Wins by game number -----------------------------------------------------

differences %>% 
  select(winner, game_number) %>% 
  group_by(winner, game_number) %>% 
  summarise(
    Wins = n()
  ) %>% 
  ggplot(aes(x = game_number, y = Wins, group = winner, fill = winner)) +
  theme_fivethirtyeight() +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual("legend", values = c("Andrew" = "#497999", "Yuri" = "#FFA500")) 


ggplot(aes(x = winner, y = Wins, fill = winner)) +
  theme_fivethirtyeight() +
  geom_bar(stat = "identity") +
  geom_label(aes(label = Wins), nudge_y = 3, label.size = 0) +
  scale_fill_manual("legend", values = c("Andrew" = "#497999", "Yuri" = "#FFA500")) +
  coord_flip() +
  theme(
    # removing bg color
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.position = "none"
  ) 