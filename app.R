#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)


spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

Year = spotify_songs$track_album_release_date <- strtrim(spotify_songs$track_album_release_date,4)

spotify_songs_time<-spotify_songs %>%
  mutate(track_album_release_date <- Year)

spotify_songs_time_n <-  spotify_songs_time%>%
  mutate(Year = as.numeric(Year))

spotify_songs_time_f <- spotify_songs_time_n%>%
  mutate(
    `Genre` = playlist_genre,
    `SubGenre` = playlist_subgenre,
    `Danceability` = danceability,
    `Energy` = energy,
    `Key` = key,
    `Loudness` = loudness,
    `Speechiness` = speechiness,
    `Liveness` = liveness,
    `Valence` = valence,
    `Duration` = duration_ms,
    `Popularity` = track_popularity
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel(h1("Scatter Plot of Different Components of a Song", align = "center")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "genre",
            label = "Genre:",
            choices = c("Genre", "SubGenre"),
            selected = "Genre"
          ),
          # Select variable for x-axis
          selectInput(
            inputId = "x",
            label = "X-axis:",
            choices = c("Energy", "Loudness", "Speechiness", "Liveness",
                        "Valence", "Duration", "Danceability", "Key"),
            selected = "Energy"
          ),
          # Select variable for y-axis
          selectInput(
            inputId = "y",
            label = "Y-axis:",
            choices = c("Energy", "Loudness", "Speechiness", "Liveness",
                        "Valence", "Duration", "Danceability", "Key"),
            selected = "Loudness"
        ),
        sliderInput(
          "pop_filter",
          label = h4("Popularity"),
          min = min(spotify_songs_time_f$Popularity),
          max = max(spotify_songs_time_f$Popularity),
          value = c(min(spotify_songs_time_f$Popularity),
                    max(spotify_songs_time_f$Popularity)),
          sep = ""
        ),
          sliderInput(
            "year_filter",
            label = h4("Year"),
            min = min(spotify_songs_time_f$Year),
            max = max(spotify_songs_time_f$Year),
            value = c(min(spotify_songs_time_f$Year), 
                      max(spotify_songs_time_f$Year)),
            sep = ""
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Scatterplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  output$Scatterplot <- renderPlot({
    spotify_songs_time_f%>%
      filter(Year >= input$year_filter[1], Year <= input$year_filter[2]) %>%
      filter(Popularity >= input$pop_filter[1], Popularity <= input$pop_filter[2]) %>%
      ggplot(aes_string(x = input$x, 
                        y = input$y),
             na.rm = TRUE) +
      #label(x = input$x, y = input$y)
      geom_point(aes_string(color = input$genre ), alpha = 0.1)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
