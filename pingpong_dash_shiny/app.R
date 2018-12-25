################################
##                            ##
##  Ping Pong Shiny Dashboard ##
##                            ##
################################

# creating a Shiny Dashboard for Ping Pong games between Yuri and Andrew


# Shinyapps.io Integration Code -------------------------------------------
# note for this to work, need the file to be app.R
library(rsconnect)
# rsconnect::deployApp('C:\\Users\\yuriki\\OneDrive - Bill & Melinda Gates Foundation\\pingpong_dash\\pingpong_dash_shiny')
# https://github.com/jennybc/googlesheets/issues/272
# declaring the redirct URI
# based on this: https://github.com/jennybc/googlesheets/blob/master/inst/shiny-examples/20_gs-explorer/server.R
options("googlesheets.webapp.redirect_uri" = "https://yurikimbmgf.shinyapps.io/shiny/")

# library -----------------------------------------------------------------
library(googlesheets)
library(tidyverse)
library(lubridate) # oddly, not loading as part of tidyverse
library(DT)
library(shinythemes)
library(ggthemes)
library(plotly)
library(shinydashboard)


# loading data ------------------------------------------------------------

# loading a publicly available google sheet
scores_raw <- gs_url("https://docs.google.com/spreadsheets/d/1EP4jg-3bTn5JoMYo4p0shUn3uFmAIhCHaF-yo8ODq94/edit#gid=525351072") %>%
  gs_read()

scores <-  scores_raw %>%
  # conforming and merging dates
  mutate(Timestamp = lubridate::mdy_hms(Timestamp)) %>%
  # Making timestamp mdy and making simple_time mdy into a single column
  mutate(date = case_when(!is.na(Timestamp) ~ as_date(Timestamp),
                          TRUE ~ mdy(simple_time))) %>%
  # Adding game number based on the timestamp.
  group_by(date) %>%
  mutate(game_number = case_when(is.na(game_number) ~ rank(Timestamp, ties.method = "first"),
                                 TRUE ~ as.integer(game_number))) %>%
  # adding winner
  mutate(diff = Andrew - Yuri) %>%
  # Showing the winner
  mutate(winner = case_when(
    diff > 0 ~ "Andrew",
    TRUE ~ "Yuri")) %>%
  ungroup()



# Processing Data ---------------------------------------------------------
# df for main visual
differences <- scores %>% 
  # Getting scores for the winners. need to make a negative for one of them to match the visual
  mutate(yuri_score = Yuri * -1,
         andrew_score = Andrew) %>% 
  # Adding a modified_date to include game number
  mutate(modified_date = paste(date, game_number)) 

# The plot for the differences
diff_plot <- differences %>% 
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
    axis.text.x = element_text(size = 7, angle = 45),
    # reinstituting xlab and ylab because the themefivethirtyeight removes it.
    axis.title = element_text(),
    # removing bg color
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white")
  ) +
  labs(x = "Date and Game",
       y = "Score Difference") 


# plot for win distribution by game number
win_by_game_number <- ggplotly(differences %>% 
           select(winner, game_number) %>% 
           group_by(winner, game_number) %>% 
           summarise(
             Wins = n()
           ) %>%
           ungroup() %>% 
           ggplot(aes(x = game_number, y = Wins, group = winner, fill = winner)) +theme_fivethirtyeight() +
           geom_bar(stat = "identity", position = "dodge") +
           scale_fill_manual("legend", values = c("Andrew" = "#497999", "Yuri" = "#FFA500")) +
           # adjusting the theme
           theme(
             # reinstituting xlab and ylab because the themefivethirtyeight removes it.
             axis.title = element_text(),
             # removing bg color
             panel.background = element_rect(fill = "white"),
             plot.background = element_rect(fill = "white"),
             legend.background = element_rect(fill = "white")
           ),
         height = 300
)

#### Max Score
max <- differences %>% 
  filter(`Max Score` == 11) %>% 
  select(Yuri, Andrew, date) %>% 
  gather("key", "value", -date) %>%
  dplyr::arrange(desc(value)) %>% 
  dplyr::slice(1) 

### Beat
beat <- differences %>% 
  # filter(max_score == 11) %>% 
  select(diff, winner, date, Yuri, Andrew) %>% 
  mutate(diff = abs(diff)) %>% 
  arrange(desc(diff)) %>% 
  slice(1)

### Streak
streak <- differences %>% 
  dplyr::arrange(game_number) %>% 
  dplyr::arrange(date)
streak <- data.frame(unclass(rle(as.vector(streak$winner)))) %>% 
  arrange(desc(lengths)) %>% 
  slice(1)


# UI ----------------------------------------------------------------------
# Define UI for application that plots features of movies
ui <- fixedPage(
  theme = shinytheme("paper"),
  # App title
  titlePanel("Yuri - Andrew Ping Pong Dashboard", windowTitle = "Ping Pong"),
  fixedRow(column(width = 12),
           tabsetPanel(type = "tabs",
                       id = "tabsetpanel",
                       tabPanel("Main View", 
                                br(),
                                plotlyOutput("main_plot"),

                                 box(title = "Record (All Games)", 
                                     # status = "primary",
                                     tableOutput("record_allgames"),
                                     width = 4),
                                 box(title = "Record (11-point Games)", 
                                     # status = "primary",
                                     tableOutput("record_11pt"),
                                     width = 4),
                                 box(title = "Record (Last 15 Games)", 
                                     # status = "primary",
                                     tableOutput("record_15"),
                                     width = 4)
                                ),
                       
                       tabPanel("Data Table",
                                br(),
                                dataTableOutput(outputId = "scores_table")
                                ),
                       tabPanel("#FunFacts",

                                div(
                                  strong("Highest Score: "), 
                                  paste(max$value, " by ", max$key, " on ", max$date, sep = "")),
                                div(
                                  strong("Biggest Beat: "), 
                                  paste(beat$winner, " won by ", beat$diff, " points on ", beat$date, " (", beat$Yuri, " - ", beat$Andrew, sep = "")),
                                div(
                                  strong("Longest Streat: "), 
                                  paste(streak$values, " has the longest win streak, with a streak of ", streak$lengths, sep = "")),
                                div(
                                  strong("Win Distribution by Game Number: "),
                                  plotlyOutput("wins_by_game_number")
                                )
                       )
           )
  )
)

# Server ------------------------------------------------------------------
# Define server function required to create the scatterplot
server <- function(input, output, session) {
  


  # Scores Table
  output$scores_table <- renderDataTable({
    datatable(data = scores %>%
                select(date, winner, Yuri, Andrew, Notes, `Max Score`, game_number, diff),
              options = list(pageLength = 50, lengthMenu = c(10, 25, 40)),
              rownames = FALSE)
  })
  
  output$main_plot <- renderPlotly({
    plotly::ggplotly(diff_plot, tooltip = c("y"))
  })
  
  output$record_allgames <- renderTable({
    differences %>% 
      select(date, winner) %>%
      group_by(winner) %>% 
      summarise(
        Wins = n()
      )
  })
  
  output$record_11pt <- renderTable({
    differences %>% 
      filter(`Max Score` == 11) %>% 
      select(date, winner) %>%
      group_by(winner) %>% 
      summarise(
        Wins = n()
      )
  })
  
  output$record_15 <- renderTable({
    differences %>% 
      dplyr::arrange(game_number) %>% 
      dplyr::arrange(date) %>%
      tail(n = 15) %>% 
      select(date, winner) %>%
      group_by(winner) %>% 
      summarise(
        Wins = n()
      )
  })
  
  
  output$wins_by_game_number <- renderPlotly({
    plotly::ggplotly(win_by_game_number, tooltip = c("y"))
  })
  
  

}


# shinyapp ----------------------------------------------------------------
# Create Shiny app object
shinyApp(ui = ui, server = server)



