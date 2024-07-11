rm(list = ls())


library(shiny)
library(shinydashboard)
library(leaflet)
library(jsonlite)
library(dplyr)
library(DT)
library(httr)
# reset environment



# Define the URL of the raw JSON data
url <- "https://raw.githubusercontent.com/openfootball/euro.json/master/2024/euro.json"

# Fetch and read JSON data from the URL
response <- GET(url)
json_data <- data.frame()
json_data <- content(response, "text")
euro2024_data <- fromJSON(json_data)
str(euro2024_data)
summary(euro2024_data)
euro2024_data$rounds

str(euro2024_data)
seq(nrow(euro2024_data$rounds))

euro2024_data$rounds[["matches"]]


print(euro2024_data$rounds[[1]])
# Accessing a list item using the $ operator
euro2024_data$rounds[[1]]

# Accessing a list item using the double square brackets notation
euro2024_data[["rounds"]][["name"]][[1]]
# THIS WORKS HELL YEAH
# first iterate through the rounds

# define the data frame
# df <- data.frame("Matchday", "Date", "Time", "Team 1", "Team 2", "Score (FT)", "Score (HT)", "Goals Team 1", "Goals Team 2", "Group")

df <- data.frame()
for (i in seq_len(nrow(euro2024_data$rounds))) {

  round <- euro2024_data$rounds$matches[i]
  #print(matches)

  str(round)

  for (j in seq_len(nrow(round))) {
    match <- round[j]
    print(match)
    # loop through each match in the round
    # for (j in seq_len(nrow(matches))) {
    #   # extract match data
    #   match <- matches[j]
    #   print(match)
    #   match_data <- c(
    #     euro2024_data$rounds$name[i],
    #     match$date,
    #     match$time,
    #     match$team1$name,
    #     match$team2$name,
    #     paste(match$score$ft),
    #     paste(match$score$ht),
    #     # paste the goal scorers for each team
    #     paste(
    #       sapply(match$goals1, function(g) paste(g$name, "(", g$minute, ".)", sep = "")),
    #       collapse = ", "
    #     ),
    #     paste(
    #       sapply(match$goals2, function(g) paste(g$name, "(", g$minute, ".)", sep = "")),
    #       collapse = ", "
    #     ),
    #     match$group
    #   )
    #   # add match data to the data frame
    #   df <- rbind(df, match_data)
    #   # clear match_data
    #   match_data <- NULL
    # }

    # make sure that match_data is a unique object for each match


    # print(match_data)
    # add match data to the data frame
    #df <- rbind(df, match_data)
  }


}

head(df)

print(df)


# Function to extract match data from a round
extract_matches <- function(round) {
  matches <- round$matches
  round_name <- round$name
  
  matches_df <- bind_rows(lapply(matches, function(match) {
    # Check for existence of nested fields
    goals1 <- if (!is.null(match$goals1)) sapply(match$goals1, function(g) paste(g$name, collapse = ", ")) else NA
    goals2 <- if (!is.null(match$goals2)) sapply(match$goals2, function(g) paste(g$name, collapse = ", ")) else NA
    
    data.frame(
      round = round_name,
      num = match$num,
      date = match$date,
      time = match$time,
      team1 = match$team1$name,
      code_team1 = match$team1$code,
      team2 = match$team2$name,
      code_team2 = match$team2$code,
      score_ft = paste(match$score$ft, collapse = "-"),
      score_ht = paste(match$score$ht, collapse = "-"),
      goals_team1 = ifelse(is.na(goals1), "", goals1),
      goals_team2 = ifelse(is.na(goals2), "", goals2),
      group = match$group,
      stringsAsFactors = FALSE
    )
  }))
  
  return(matches_df)
}

# Extract matches from all rounds and combine into a single data frame
all_matches_df <- bind_rows(lapply(euro2024_data$rounds, extract_matches))


# View the combined data frame
print(all_matches_df)


# Print the match list
print(match_list)



# Extract match data
matches <- bind_rows(lapply(euro2024_data$rounds, function(round) {
  round_name <- round[["name"]]
  round_matches <- round[["matches"]]
  lapply(round_matches, function(match) {
    match$round <- round_name
    match
  })
}))




ui <- dashboardPage(
  dashboardHeader(title = "Euro 2024 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Matches", tabName = "matches", icon = icon("table")),
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
              ))
    )
  )
)

server <- function(input, output) {

  print(class(matches))
  print(str(matches))
  
  # Ensure matches is a data frame before proceeding
  if (!inherits(matches, "data.frame")) {
    stop("The 'matches' object is not a data frame.")
  }
  # Prepare match data for display
  matches_df <- matches %>%
    mutate(date = as.Date(date),
           team1 = sapply(team1, function(x) x$name),
           team2 = sapply(team2, function(x) x$name),
           score = sapply(score$ft, function(x) paste(x, collapse = "-")),
           goals1 = sapply(goals1, function(x) paste(sapply(x, function(y) y$name), collapse = ", ")),
           goals2 = sapply(goals2, function(x) paste(sapply(x, function(y) y$name), collapse = ", "))) %>%
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
}

# Run the app
shinyApp(ui, server)











  # for (match in matches) {
  #   match_data <- c(
  #     euro2024_data$rounds$name[i],
  #     match$date,
  #     match$time,
  #     match$team1$name,
  #     match$team2$name,
  #     paste(match$score$ft),
  #     paste(match$score$ht),
  #     # paste the goal scorers for each team
  #     paste(
  #       sapply(match$goals1, function(g) paste(g$name, "(", g$minute, ".)", sep = "")),
  #       collapse = ", "
  #     ),
  #     paste(
  #       sapply(match$goals2, function(g) paste(g$name, "(", g$minute, ".)", sep = "")),
  #       collapse = ", "
  #     ),
  #     match$group
  #   )
  #   print(match_data)
  #   # add match data to the data frame
  #   df <- rbind(df, match_data)
  #   # clear match_data
  #   match_data <- NULL
  # }
  # matches$num
  # str(matches)
  # length(matches)
  # for (j in seq_len(nrow(euro2024_data$rounds$matches[i]))) {
  #   match <- euro2024_data$rounds[i]$matches[j]
  #   print(match)
  #   # extract match data
  #   match_data <- c(
  #     euro2024_data$rounds$name[i],
  #     match$date,
  #     match$time,
  #     match$team1$name,
  #     match$team2$name,
  #     paste(match$score$ft),
  #     paste(match$score$ht),
  #     # paste the goal scorers for each team
  #     paste(
  #       sapply(match$goals1, function(g) paste(g$name, "(", g$minute, ".)", sep = "")),
  #       collapse = ", "
  #     ),
  #     paste(
  #       sapply(match$goals2, function(g) paste(g$name, "(", g$minute, ".)", sep = "")),
  #       collapse = ", "
  #     ),
  #     match$group
  #   )
  #   print(match_data)
  #   # add match data to the data frame
  #   df <- rbind(df, match_data)
  #   # clear match_data
  #   match_data <- NULL
  # }