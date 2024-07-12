rm(list = ls())

library(shiny)
library(shinydashboard)
library(bs4Dash)
library(leaflet)
library(jsonlite)
library(dplyr)
library(DT)
library(httr)
library(tidyverse)
library(ggplot2)
library(htmltools)

# Define the URL of the raw JSON data
url <- "https://raw.githubusercontent.com/openfootball/euro.json/master/2024/euro.json"

# Fetch and read JSON data from the URL
response <- GET(url)
json_data <- data.frame()
json_data <- content(response, "text")
euro2024_data <- fromJSON(json_data)
str(euro2024_data)

# Extract the matches data from the JSON
matches <- bind_rows(lapply(1:nrow(euro2024_data$rounds), function(i) {
  round_name <- euro2024_data$rounds$name[i]
  round_matches <- euro2024_data$rounds$matches[[i]]
  round_matches$round <- round_name
  round_matches
}))

# TODO:
# - add other theme
# - cleanup the code
# - cleanup gui (match details order of columns, etc.)

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

# Prepare HTML Elements for the Leaflet Map Popups
berlin <- paste0(sep = "<br/>",
  "<h4>Olympiastadion Berlin</h4>",
  "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/Olympiastadion_Berlin_Sep-2015.jpg/300px-Olympiastadion_Berlin_Sep-2015.jpg' width='150'>",
  "<p>Capacity: 74,475</p>"
)
hamburg <- paste0(sep = "<br/>",
  "<h4>Volksparkstadion Hamburg</h4>",
  "<img src='https://upload.wikimedia.org/wikipedia/commons/d/de/RK_1009_9831_Volksparkstadion.jpg' width='150'>",
  "<p>Capacity: 57,000</p>"
)
munich <- paste0(sep = "<br/>",
  "<h4>Allianz Arena München</h4>",
  "<img src='https://cdn.britannica.com/69/250069-050-5142DE0C/Allianz-Arena-Munich-Germany.jpg' width='150'>",
  "<p>Capacity: 75,000</p>"
)
cologne <- paste0(sep = "<br/>",
  "<h4>RheinEnergieStadion Köln</h4>",
  "<img src='https://www.koeln.de/wp-content/uploads/2024/05/rheinenergie-stadion-imago1030023181-norina-toenges-2000.jpg' width='150'>",
  "<p>Capacity: 50,000</p>"
)
stuttgart <- paste0(sep = "<br/>",
  "<h4>MHPArena Stuttgart</h4>",
  "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/7/7f/Stuttgart_stadium.jpg/1200px-Stuttgart_stadium.jpg' width='150'>",
  "<p>Capacity: 60,058</p>"
)
dortmund <- paste0(sep = "<br/>",
  "<h4>Westfalenstadion Dortmund</h4>",
  "<img src='https://upload.wikimedia.org/wikipedia/commons/0/0e/Signal_iduna_park_stadium_dortmund_2.jpg' width='150'>",
  "<p>Capacity: 81,365</p>"
)
gelsenkirchen <- paste0(sep = "<br/>",
  "<h4>Veltins-Arena Gelsenkirchen</h4>",
  "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/8/8a/Arena_auf_Schalke_-_panoramio_%281%29.jpg/1200px-Arena_auf_Schalke_-_panoramio_%281%29.jpg' width='150'>",
  "<p>Capacity: 62,271</p>"
)
frankfurt <- paste0(sep = "<br/>",
  "<h4>Deutsche Bank Park Frankfurt</h4>",
  "<img src='https://media.eintracht.de/image/upload/ar_3:2,c_fill,dpr_1.0,f_auto,g_xy_center,q_40,w_1100,x_w_mul_0.43,y_h_mul_0.40/v1659555961/deutsche-bank-park-dbp-stadion-rasenverlegung-heimspiel-9-bafe.jpg' width='150'>",
  "<p>Capacity: 58,000</p>"
)

# add all stadiums to a data frame
stadiums <- data.frame(
  name = c("Olympiastadion Berlin", "Volksparkstadion Hamburg", "Allianz Arena München", "RheinEnergieStadion Köln", "MHPArena Stuttgart", "Westfalenstadion Dortmund", "Veltins-Arena Gelsenkirchen", "Deutsche Bank Park Frankfurt"),
  lat = c(52.514723875882744, 53.58726575460459, 48.21888595608705, 50.933579198726086, 48.79228744165338, 51.49275243012903, 51.554681003159295, 50.06868052596206),
  lon = c(13.239645744225324, 9.899268398674218, 11.624621569312966, 6.875530006452548, 9.2325181552588, 7.452393839024149, 7.067577841413291, 8.64552564132521),
  popupText = c(berlin, hamburg, munich, cologne, stuttgart, dortmund, gelsenkirchen, frankfurt)
)

ui <- dashboardPage(
  dashboardHeader(skin = "light", title <- dashboardBrand(title = "Euro 2024 Dashboard", image = "https://cdn.vectorstock.com/i/1000v/09/43/official-uefa-euro-2024-logo-vector-47540943.jpg")),
  dashboardSidebar(
    skin = "light",
    sidebarMenu(
      menuItem("Matches", tabName = "matches", icon = icon("table")),
      menuItem("Goals Scored", tabName = "goals", icon = icon("futbol")),
      menuItem("Stadiums Map", tabName = "map", icon = icon("globe"))
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
                box(title = "EURO 2024 Stadium Locations", width = 12, 
                    leafletOutput("stadiums_map", height = 1000))
              )),
      tabItem(tabName = "goals",
              fluidRow(
                box(title = "Goals Scored Per Matchday", width = 12, 
                    plotOutput("goals_plot")),
                box(title = "Select Team", width = 4,
                    selectInput("team", "Team:", choices = c("All Teams", unique(matches$team1), unique(matches$team2))))))
    )
  ),
  footer = dashboardFooter(
    left = a("Source Code", href = "https://github.com/kane17/shiny-euros", target = "_blank"),
    right = "2024 © Keijo Nierula, BFH"
  ),
)

server <- function(input, output) {
  # Create a data frame for the matches table
  matches_df <- matches %>%
    mutate(date = as.Date(date),
            team1 = sapply(team1$name, identity),
            team2 = sapply(team2$name, identity),
            score = sapply(score$ft, function(x) paste(x, collapse = "-")),
            goals1 = sapply(goals1, function(x) paste(sapply(x$name, identity), collapse = ", ")),
            goals2 = sapply(goals2, function(x) paste(sapply(x$name, identity), collapse = ", "))) %>%
    select(round, date, team1, team2, score, goals1, goals2)

  # Render the matches table
  output$matches_table <- DT::renderDataTable({
    DT::datatable(matches_df, options = list(pageLength = 10))
  })

  # Render the leaflet map with stadium locations
  output$stadiums_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(stadiums$lon, stadiums$lat, popup = stadiums$popupText)
  })

  # Reactive expression to filter goals data based on selected team
  filtered_goals <- reactive({
    req(input$team)
    
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
    }
  })

  # Render the goals per matchday plot
  output$goals_plot <- renderPlot({
    plot_data <- filtered_goals()
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
