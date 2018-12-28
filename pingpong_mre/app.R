# minimally reproducible example
# https://stackoverflow.com/questions/42176491/shiny-update-googlesheets


library(rsconnect)
# rsconnect::deployApp('C:\\Users\\yuriki\\OneDrive - Bill & Melinda Gates Foundation\\pingpong_dash\\pingpong_mre')
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


# Server ------------------------------------------------------------------
# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # not sure what this does. https://stackoverflow.com/questions/42176491/shiny-update-googlesheets
  session$onSessionEnded(function() {
    stopApp()
  })
  
  scores <- reactive({
    invalidateLater(60000,session=session)
    scores_raw <- gs_url("https://docs.google.com/spreadsheets/d/1EP4jg-3bTn5JoMYo4p0shUn3uFmAIhCHaF-yo8ODq94/edit#gid=525351072") %>%
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
  
  # Scores Table
  output$scores_table <- renderDataTable({
    datatable(data = scores() %>%
                select(date, winner, Yuri, Andrew, Notes, `Max Score`, game_number, diff),
              options = list(pageLength = 50, lengthMenu = c(10, 25, 40)),
              rownames = FALSE)
  })
}

# UI ----------------------------------------------------------------------
# Define UI for application that plots features of movies
ui <- fixedPage(
  theme = shinytheme("paper"),
  # App title
  titlePanel("Yuri - Andrew Ping Pong Dashboard", windowTitle = "Ping Pong"),
  fixedRow(column(width = 12),
           tabsetPanel(type = "tabs",
                       id = "tabsetpanel",
                       tabPanel("Data Table",
                                br(),
                                dataTableOutput(outputId = "scores_table")
                       ))))



# shinyapp ----------------------------------------------------------------
# Create Shiny app object
shinyApp(ui = ui, server = server)



