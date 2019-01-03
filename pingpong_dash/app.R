# minimally reproducible example
# https://stackoverflow.com/questions/42176491/shiny-update-googlesheets


library(rsconnect)
# rsconnect::deployApp('C:\\Users\\yuriki\\OneDrive - Bill & Melinda Gates Foundation\\pingpong_dash\\pingpong_dash')
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

# UI ----------------------------------------------------------------------
# Define UI for application that plots features of movies
ui <- fixedPage(
  theme = shinytheme("paper"),
  # App title
  titlePanel("Yuri - Andrew Ping Pong Dashboard", windowTitle = "Ping Pong"),
  fixedRow(column(width = 12),
           tabsetPanel(type = "tabs",
                       id = "tabsetpanel",
                       tabPanel("Dashboard",
                                br(),
                                plotlyOutput("main_plot"),
                                box(title = h4("Record (All Games)"), 
                                    # status = "primary",
                                    tableOutput("record_allgames"),
                                    width = 4),
                                box(title = h4("Record (11-point Games)"), 
                                    # status = "primary",
                                    tableOutput("record_11pt"),
                                    width = 4),
                                box(title = h4("Record (Last 15 Games)"), 
                                    # status = "primary",
                                    tableOutput("record_15"),
                                    width = 4),
                                box(title = h4("Total Wins (All Games)"),
                                    plotlyOutput("running_wins"),
                                    width = 6),
                                box(title = h4("Total Wins (11-point Games)"),
                                    plotlyOutput("running_wins11"),
                                    width = 6)
                                ),
                       tabPanel("Data Table",
                                br(),
                                dataTableOutput(outputId = "scores_table")
                       ),
                       tabPanel("#FunFacts",
                                br(),
                                div(
                                  strong("Highest Score: "),
                                  textOutput("max_max_score_number", inline = TRUE), " by ", textOutput("max_key", inline = TRUE)),
                                div(
                                  strong("Biggest Beat: "),
                                  textOutput("beat_winner", inline = TRUE), " won by ", textOutput("beat_diff", inline = TRUE), " points", " (", textOutput("beat_yuri", inline = TRUE), " - ", textOutput("beat_andrew", inline = TRUE), ")"),
                                div(
                                  strong("Longest Streak: "), 
                                  textOutput("streak_values", inline = TRUE), paste0(" has the longest win streak, with a streak of "), textOutput("streak_lengths", inline = TRUE), " games"),
                                div(
                                  strong("Win Distribution by Game Number: "),
                                  plotlyOutput("wins_by_game_number")
                                )
                       )
                       
           )))




# Server ------------------------------------------------------------------
# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # not sure what this does. https://stackoverflow.com/questions/42176491/shiny-update-googlesheets
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # DF: Main data pull from google form
  scores <- reactive({
    # invalidateLater(60000,session=session)
    scores_raw <- gs_url("https://docs.google.com/spreadsheets/d/1ozaXrwEWN04xiN_c9VO_Zfr_xhMKRtRuYNVxJg_KL6A/edit#gid=525351072") %>%
      gs_read() %>% 
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
    scores_raw
  })
  
  # Main Data Table
  output$scores_table <- renderDataTable({
    datatable(data = scores() %>%
                select(date, winner, Yuri, Andrew, Notes, `Max Score`, game_number, diff),
              options = list(pageLength = 50, lengthMenu = c(10, 25, 40)),
              rownames = FALSE)
  })
  
  # DF: for main visual
  differences <- reactive({differences_df <- scores() %>%
      # Getting scores for the winners. need to make a negative for one of them to match the visual
      mutate(yuri_score = Yuri * -1,
             andrew_score = Andrew) %>%
      # Adding a modified_date to include game number
      mutate(modified_date = paste(date, game_number))
  differences_df
  })
  
  
  # Main Plot: 
  output$main_plot <- renderPlotly(
    ggplotly(
      ggplot(differences(), aes(x = modified_date, y = diff, fill = winner)) +
        theme_fivethirtyeight() +
        scale_fill_manual("legend", values = c("Andrew" = "#497999", "Yuri" = "#FFA500")) +
        geom_bar(stat = "identity") + 
        geom_point(data = differences() %>% filter(winner == "Andrew"), aes(x = modified_date, y = andrew_score), color = "#497999") +
        geom_point(data = differences() %>% filter(!winner == "Andrew"), aes(x = modified_date, y = andrew_score), color = "#497999", alpha = .15) +
        # points for yuri
        geom_point(data = differences() %>% filter(winner == "Yuri"), aes(x = modified_date, y = yuri_score), color = "#FFA500") +
        geom_point(data = differences() %>% filter(!winner == "Yuri"), aes(x = modified_date, y = yuri_score), color = "#FFA500", alpha = .15) +
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
    )
  )
  
  # allgames box
  output$record_allgames <- renderTable({
    differences() %>% 
      select(date, winner) %>%
      group_by(winner) %>% 
      summarise(
        Wins = n()
      )
  })
  
  
  output$record_11pt <- renderTable({
    differences() %>% 
      filter(`Max Score` == 11) %>% 
      select(date, winner) %>%
      group_by(winner) %>% 
      summarise(
        Wins = n()
      )
  })
  
  output$record_15 <- renderTable({
    differences() %>% 
      dplyr::arrange(game_number) %>% 
      dplyr::arrange(date) %>%
      tail(n = 15) %>% 
      select(date, winner) %>%
      group_by(winner) %>% 
      summarise(
        Wins = n()
      )
  })
  
  
  # fun facts section -------------------------------------------------------
  # plot for win distribution by game number
  output$wins_by_game_number <- renderPlotly(
    ggplotly(differences() %>%
               select(winner, game_number) %>%
               group_by(winner, game_number) %>%
               summarise(Wins = n()) %>%
               ungroup() %>%
               ggplot(aes(x = game_number, y = Wins, group = winner, fill = winner)) + 
               theme_fivethirtyeight() +
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
  ))
  
  
  #### Max Score
  max <- reactive({max_df <- differences() %>%
    filter(`Max Score` == 11) %>%
    select(Yuri, Andrew, date) %>%
    gather("key", "max_score_number", -date) %>%
    dplyr::arrange(desc(max_score_number)) %>%
    dplyr::slice(1)
  max_df
  })
  # paste(textOutput(max()$max_score_number, " by ", max()$key, " on ", max$date, sep = ""))),

  output$max_max_score_number <- renderText(max() %>% select(max_score_number) %>% as_vector())
  output$max_key <- renderText(max() %>% select(key) %>% as_vector())
  output$max_date <- renderText(max() %>% select(date) %>% as_vector())
  
  ### Beat
  beat <- reactive({beat_df <- differences() %>%
    # filter(max_score == 11) %>%
    select(diff, winner, date, Yuri, Andrew) %>%
    mutate(diff = abs(diff)) %>%
    arrange(desc(diff)) %>%
    slice(1)
  beat_df
  })
  
  output$beat_winner <- renderText(beat() %>% select(winner) %>% as_vector())
  output$beat_diff <- renderText(beat() %>% select(diff) %>% as_vector())
  output$beat_date <- renderText(beat() %>% select(date) %>% as_vector())
  output$beat_yuri <- renderText(beat() %>% select(Yuri) %>% as_vector())
  output$beat_andrew <- renderText(beat() %>% select(Andrew) %>% as_vector())
  
  # paste(beat$winner, " won by ", beat$diff, " points on ", beat$date, " (", beat$Yuri, " - ", beat$Andrew, ")", sep = "")),

  ### Streak
  
  streak <- reactive({streak_df <- 
    differences() %>%
    dplyr::arrange(game_number) %>%
    dplyr::arrange(date) %>% 
    select(winner) %>% 
    as_vector() %>% 
    # getting streak
    rle() %>% 
    # making it into a tibble
    unclass() %>% 
    as_tibble() %>% 
    # getting top
    arrange(desc(lengths)) %>%
    slice(1)
    streak_df
  })
  
  output$streak_values <- renderText(streak() %>% select(values) %>% as_vector())
  output$streak_lengths <- renderText(streak() %>% select(lengths) %>% as_vector())
  
  
  running_wins_df <- reactive({running_wins_df2 <- scores() %>% 
      dplyr::arrange(game_number) %>%
      dplyr::arrange(date) %>% 
      # getting numbers of games won by person per date
      group_by(date, winner) %>%
      mutate(temp_wins = 1) %>% 
      mutate(temp_wins2 = sum(temp_wins)) %>% 
      select(date, winner, temp_wins2) %>% 
      distinct() %>% 
      group_by(winner) %>% 
      mutate(`Running Wins` = cumsum(temp_wins2)) %>% 
      ungroup()
    running_wins_df2
  })
  
  running_wins_df11 <- reactive({running_wins_df3 <- scores() %>% 
    filter(!`Max Score` == 21) %>% 
    dplyr::arrange(game_number) %>%
    dplyr::arrange(date) %>% 
    # getting numbers of games won by person per date
    group_by(date, winner) %>%
    mutate(temp_wins = 1) %>% 
    mutate(temp_wins2 = sum(temp_wins)) %>% 
    select(date, winner, temp_wins2) %>% 
    distinct() %>% 
    group_by(winner) %>% 
    mutate(`Running Wins` = cumsum(temp_wins2)) %>% 
    ungroup()
  running_wins_df3
  })
  
  ### Beat
  beat <- reactive({beat_df <- differences() %>%
    # filter(max_score == 11) %>%
    select(diff, winner, date, Yuri, Andrew) %>%
    mutate(diff = abs(diff)) %>%
    arrange(desc(diff)) %>%
    slice(1)
  beat_df
  })
  
  
  output$running_wins <- renderPlotly(
    ggplotly(ggplot(running_wins_df(), aes(x = ymd(date), y = `Running Wins`, group = winner, color = winner)) +
               geom_line(size = 1) +
               # fivethirtyeight theme
               theme_fivethirtyeight() +
               scale_colour_manual("legend", values = c("Andrew" = "#497999", "Yuri" = "#FFA500")) +
               theme(
                 axis.title = element_text(),
                 # removing bg color
                 panel.background = element_rect(fill = "white"),
                 plot.background = element_rect(fill = "white"),
                 legend.background = element_rect(fill = "white")
               ) +
               labs(x = "Total Wins",
                    y = "Date")
             ,
             height = 300
    ))  
  output$running_wins11 <- renderPlotly(
    ggplotly(ggplot(running_wins_df11(), aes(x = ymd(date), y = `Running Wins`, group = winner, color = winner)) +
               geom_line(size = 1) +
               # fivethirtyeight theme
               theme_fivethirtyeight() +
               scale_colour_manual("legend", values = c("Andrew" = "#497999", "Yuri" = "#FFA500")) +
               theme(
                 axis.title = element_text(),
                 # removing bg color
                 panel.background = element_rect(fill = "white"),
                 plot.background = element_rect(fill = "white"),
                 legend.background = element_rect(fill = "white")
               ) +
               labs(x = "Total Wins",
                    y = "Date")
             ,
             height = 300
    ))
  
}


# shinyapp ----------------------------------------------------------------
# Create Shiny app object
shinyApp(ui = ui, server = server)



