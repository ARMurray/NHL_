library(httr)
library(jsonlite)
library(tidyverse)

# List of Teams with Info
teams <- GET("https://statsapi.web.nhl.com/api/v1/teams")
data <- fromJSON(rawToChar(teams$content))
tbl <- as.data.frame(data$teams)



# List Flyers players and filter to those on roster
players <- GET("https://records.nhl.com/site/api/player/byTeam/4")
data <- fromJSON(rawToChar(players$content))

plyrTbl <- as.data.frame(data$data)%>%
  filter(onRoster == "Y")


# Game data

gameGet <- GET("https://statsapi.web.nhl.com/api/v1/game/2019020050/feed/live")
data <- fromJSON(rawToChar(gameGet$content))
gameTbl <- as.data.frame(data$gameData)

live <- as.data.frame(data$liveData)


plays <- data$liveData$plays$allPlays

result <- plays$result%>%
  select(eventTypeId)

players <- plays%>%
  select(players)

coords <- plays$coordinates

shotPlays <- cbind(result,players,coords)%>%
  filter(eventTypeId == "SHOT")

goals <- cbind(result,players,coords)%>%
  filter(eventTypeId == "GOAL")

# plot

ggplot()+
  geom_point(data = shotPlays, aes(x = x, y = y), col = 'black', fill = 'black')+
  geom_point(data = goals, aes(x = x, y = y), col = 'red', shape = 4, size = 5)
