rm(list = ls())


library(shiny)
library(shinydashboard)
library(leaflet)
library(jsonlite)
library(dplyr)
library(DT)
library(httr)
library(tidyverse)
library(ggplot2)



# Define the URL of the raw JSON data
url <- "https://raw.githubusercontent.com/openfootball/euro.json/master/2024/euro.json"

# Fetch and read JSON data from the URL
response <- GET(url)
json_data <- data.frame()
json_data <- content(response, "text")
euro2024_data <- fromJSON(json_data)
str(euro2024_data)

matches <- bind_rows(lapply(1:nrow(euro2024_data$rounds), function(i) {
  round_name <- euro2024_data$rounds$name[i]
  round_matches <- euro2024_data$rounds$matches[[i]]
  
  # Print the round data for debugging
  print(paste("Processing round:", round_name))
  print(round_matches)
  
  # Add the round name to each match
  round_matches$round <- round_name
  round_matches
}))


# TODO:
# - Implement a reactive element (e.g. plot for each nation)
# - Implement a map with match locations

# Prepare data for plotting goals per matchday
goals_per_matchday <- matches %>%
    mutate(goals_team1 = map_int(goals1, ~ if (is.data.frame(.x)) nrow(.x) else 0),
           goals_team2 = map_int(goals2, ~ if (is.data.frame(.x)) nrow(.x) else 0)) %>%
    select(round, team1, team2, goals_team1, goals_team2) %>%
    pivot_longer(cols = c(goals_team1, goals_team2), names_to = "goal_type", values_to = "goals") %>%
    pivot_longer(cols = c(team1, team2), names_to = "team_type", values_to = "team") %>%
    filter((goal_type == "goals_team1" & team_type == "team1") | 
           (goal_type == "goals_team2" & team_type == "team2")) %>%
    select(round, team, goals) %>%
    group_by(round, team) %>%
    summarise(goals = sum(goals)) %>%
    ungroup()

print(goals_per_matchday, n = 200)



ui <- dashboardPage(
  dashboardHeader(title = "Euro 2024 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Matches", tabName = "matches", icon = icon("table")),
      menuItem("Goals Scored", tabName = "goals", icon = icon("futbol")),
      menuItem("Match Map", tabName = "map", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "matches",
              fluidRow(
                box(title = "Match Details", width = 12, 
                    DT::dataTableOutput("matches_table"))
              )),
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Match Locations", width = 12, 
                    leafletOutput("match_map", height = 500))
              )),
      tabItem(tabName = "goals",
              fluidRow(
                box(title = "Goals Scored Per Matchday", width = 12, 
                    plotOutput("goals_plot")),
                box(title = "Select Team", width = 4,
                    selectInput("team", "Team:", choices = c("All Teams", unique(matches$team1), unique(matches$team2))))))
    )
  )
)

server <- function(input, output) {
  matches_df <- matches %>%
    mutate(date = as.Date(date),
            team1 = sapply(team1$name, identity),
            team2 = sapply(team2$name, identity),
            score = sapply(score$ft, function(x) paste(x, collapse = "-")),
            goals1 = sapply(goals1, function(x) paste(sapply(x$name, identity), collapse = ", ")),
            goals2 = sapply(goals2, function(x) paste(sapply(x$name, identity), collapse = ", "))) %>%
    select(round, date, team1, team2, score, goals1, goals2)

  output$matches_table <- DT::renderDataTable({
    DT::datatable(matches_df, options = list(pageLength = 10))
  })


  output$match_map <- renderLeaflet({
    # Sample data for match locations (update with actual data if available)
    match_data <- data.frame(
      lat = c(51.5074, 48.8566, 40.7128),
      lng = c(-0.1278, 2.3522, -74.0060),
      location = c("London", "Paris", "New York")
    )  
    leaflet(match_data) %>%
      addTiles() %>%
      addMarkers(~lng, ~lat, popup = ~location)
  })

  # Reactive expression to filter goals data based on selected team
  filtered_goals <- reactive({
    req(input$team)  # Ensure input$team is available
    
    # Debugging print to check input$team value
    print(paste("Selected Team:", input$team))
    
    if (input$team == "All Teams") {
      goals_per_matchday %>%
        group_by(round) %>%
        summarise(goals = sum(goals)) %>%
        ungroup()
    } else {
      filtered_data <- goals_per_matchday %>%
        filter(team$name == input$team) %>%
        group_by(round) %>%
        summarise(goals = sum(goals)) %>%
        ungroup()
      
      # Debugging print to check filtered data
      print("Filtered Goals Data:")
      print(filtered_data)
      
      filtered_data
    }
  })


  # Render the goals per matchday plot
  output$goals_plot <- renderPlot({
    plot_data <- filtered_goals()
    
    # Check if plot_data is empty
    if (nrow(plot_data) == 0) {
      return(NULL)
    }
    
    ggplot(plot_data, aes(x = round, y = goals)) +
      geom_bar(stat = "identity") +
      labs(title = "Goals Scored Per Matchday",
           x = "Matchday",
           y = "Goals") +
      theme_minimal()
  })
}
# Run the app
shinyApp(ui, server)
