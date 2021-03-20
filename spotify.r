' Author: Adam de Haro '
' Page for Spotify '

library(shiny)
library(shinydashboard)
library(DSpoty)
library(tinyspotifyr)
library(leaflet)
library(musicbrainz)
library(dplyr)
library(ggmap)
library(htmltools)
library(usethis)
library(DT)
library(stringr)
library(httr)
source("utils.R")
source("credentials.R")

# Global parameters
listen_threshold = 100
map_theme = providers$CartoDB.DarkMatter

# Git options
use_git_config(user.name = "adzcodez", user.email = "adamdeharo@gmail.com")

# Shiny options
options(shiny.maxRequestSize=10*1024^2) # Increases file size upload limit

# Access key for Google Maps API
key <- return_google_key()
register_google(key = key)

# Generate access token for Spotify API
client_id <- return_spotify_client()
client_secret <- return_spotify_secret()
redirect_uri <- ''
Sys.setenv(client_id = client_id)
Sys.setenv(client_secret = client_secret)
token <- DSpoty::get_spotify_access_token(
  client_id = Sys.getenv("client_id"),
  client_secret = Sys.getenv("client_secret")
)

ui <- dashboardPage(
  skin = "purple", 
  dashboardHeader(title = "Music Map"), 
  dashboardSidebar(
    textInput("spotifyPlaylist", "Upload a Spotify playlist URI here"),
    selectInput("map_theme", label = "Map theme", 
                choices = c("Dark"    = providers$CartoDB.DarkMatter, 
                            "Light"   = providers$Esri.WorldGrayCanvas,
                            "Natural" = providers$Esri.WorldTopoMap), 
                size = 5, selected = map_theme, 
                multiple = FALSE, selectize = FALSE)
  ), 
  dashboardBody(
    fluidRow(box(width = 12, style = 'height: 90vh', leafletOutput(outputId = "map", height = '90vh'))),
    fluidRow(box(width = 12, dataTableOutput(outputId = "table")))
  )
)

# Server
server <- function(input, output) {
  spotify_playlist_upload <- reactive({
    ' This will take the playlist upload and process it '
    req(input$spotifyPlaylist)
    # Process input if it is a URI or URL
    # playlist_string <- str_replace(input$spotifyPlaylist, "spotify:playlist:", "") 
    # playlist_string <- gsub("https://open.spotify.com/playlist/", "\\1", playlist_string)
    # playlist_string <- gsub("\\?.*", "\\1", playlist_string)
    
    # NOTE: Consider using tinyspotifyr's is_uri() funtion
    # Check if input is valid
    valid = TRUE
    # playlist_url <- paste0("https://open.spotify.com/playlist/",playlist_string)
    # if (str_length(playlist_string) != 22) {
    #   valid = FALSE
    # }
    # if (http_error(playlist_url)) { 
    #   valid = FALSE}
    
    # If input is valid, we will build a dataframe of the songs
    if (valid == TRUE) {
      # Spotify can only retrieve 100 songs, calculate how many times we need to query
      # print(playlist_uri)
      # print(playlist_url)
      artist_df <- build_artist_dataframe(input$spotifyPlaylist, token)
    }
  })
  
  ' Adding artist information '
  spotify_artist_info <- reactive({
    ' Produces a dataframe of artist information from artists in artist_plays dataframe by querying MusicBrainz ' 
    req(spotify_playlist_upload())
    rows = dim(spotify_playlist_upload())[1]
    cols = 5
    artist_info <- matrix(ncol=cols, nrow=rows)
    for (i in 1:rows) {
      artist_name <- spotify_playlist_upload()$Artist[i]
      print(artist_name)
      artist_id <- pull_artist_id(artist_name)
      artist_df <- lookup_artist_by_id(artist_id, includes="tags")
      artist_info[i,1] <- artist_name # Artist name
      artist_info[i,2] <- pull_country(artist_df) # Artist country
      artist_info[i,3] <- pull_area(artist_df) # Artist area
      artist_info[i,4] <- pull_year(artist_df, artist_id) # StartYear
      artist_info[i,5] <- pull_genre(artist_df) # Genre
    }
    spotify_artist_info <- as.data.frame(artist_info)
    spotify_artist_info <- rename(spotify_artist_info, Artist=1, Country=2, Area=3, StartYear=4, Genre=5)
  })
  spotify_coordinates <- reactive({
    ' Produces a dataframe of area coordinates for artist areas by querying Google Maps '
    req(spotify_artist_info())
    spotify_coordinates <- unique(spotify_artist_info()[c("Country", "Area")])
    spotify_coordinates["Latitude"] = NA; spotify_coordinates["Longitude"] = NA
    spotify_coordinates <- mutate(spotify_coordinates, address=paste(Area, Country))
    addresses <- spotify_coordinates$address 
    geocodes <- geocode(addresses, source = "google")
    spotify_coordinates <- mutate(spotify_coordinates, Latitude=geocodes$lat, Longitude=geocodes$lon)
    spotify_coordinates <- spotify_coordinates[1:4]
  })
  spotify_final <- reactive({
    ' Produces the final dataframe containing all artist info joined to scrobbles, and a HTML label '
    req(spotify_coordinates(), spotify_artist_info())
    spotify_final <- left_join(spotify_artist_info(), spotify_coordinates(), by=c("Country", "Area"))
    spotify_final$Label <-  paste("<p>", "Artist: ", spotify_final$Artist, "</p>",
                                    "<p>", "Area: ", spotify_final$Area, "</p>")
    spotify_final
  })

  ' Outputs '
  output$table <- renderDataTable({
    ' Renders the table (troubleshooting) '
    spotify_final()
  })
  output$map <- renderLeaflet({
    ' Renders the map '
    req(input$map_theme)
    map <- leaflet() %>% 
      addProviderTiles(input$map_theme) %>% 
      # setView(lng=-6.2603097	, lat=53.34981, zoom=5) %>% 
      addCircleMarkers(lng=spotify_final()$Longitude, 
                       lat=spotify_final()$Latitude,
                       stroke = TRUE, color="black",
                       fillColor = "#FFF", opacity = 100,
                       weight=1,
                       # radius = ~scrobbles$Plays/100,
                       label=lapply(spotify_final()$Label, HTML), 
                       clusterOptions=markerClusterOptions(showCoverageOnHover = FALSE)) 
  })
}

shinyApp(ui = ui, server = server)
