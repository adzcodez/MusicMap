' Author: Adam de Haro '
' Main app to be run for MusicMap '

library(shiny)
library(shinydashboard)
library(DSpoty)
library(leaflet)
library(musicbrainz)
library(dplyr)
library(ggmap)
library(htmltools)
library(usethis)
library(DT)
source("utils.R")
source("credentials.R")

# Global parameters
listen_threshold = 2000
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
        fileInput("scrobblescsv", "Upload scrobbles .csv here"),
        sliderInput("artist_dates", label = "Artist Date Range",
                    min = 1950, max = 2030, value = c(1950, 2030),
                    sep = "", step = 1),
        sliderInput("listen_dates", label = "Listen date range", 
                    min = as.Date("2012-01-01"), max = as.Date("2022-01-01"), 
                    value=c(as.Date("2012-01-01"), as.Date("2022-01-01")), timeFormat = "%F"),
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
# selectInput("variable", "Variable:",
#             c("Cylinders" = "cyl",
#               "Transmission" = "am",
#               "Gears" = "gear")),
# tableOutput("data")

# Server
server <- function(input, output) {
    scrobbles_upload <- reactive({
        req(input$scrobblescsv, file.exists(input$scrobblescsv$datapath))
        read.csv(input$scrobblescsv$datapath, header=FALSE, col.names=c("Artist", "Album", "Track", "Time"))
    })
    
    ' Inputs and preprocessing'
    scrobbles_preprocessed <- reactive({ # Handles the preprocessing of csv
        ' This handles the preprocessing of the CSV '
        req(scrobbles_upload())
        scrobbles <- scrobbles_upload()                                                  # Checks file is uploaded
        scrobbles <- data.frame(scrobbles[1:3], apply(scrobbles[4], 2, processTime))     # Preprocesses dates
        scrobbles <- rename(scrobbles, Day=4, Month=5, Year=6, Hour=7, Minute=8, Date=9)
        scrobbles <- scrobbles[scrobbles$Year > 1970,]                                   # Removes incorrect dates
         })
        
    scrobbles_artist_plays <- reactive({ # Produces artist plays dataframe
        ' Produces a dataframe of artists with >n plays '
        req(scrobbles_preprocessed())
        scrobbles_artist_plays <- scrobbles_preprocessed() %>%
                                  count(Artist, sort=TRUE) %>%
                                  filter(n > listen_threshold)
    })
    scrobbles_artist_info <- reactive({
        ' Produces a dataframe of artist information from artists in artist_plays dataframe by querying MusicBrainz ' 
        req(scrobbles_artist_plays())
        rows = dim(scrobbles_artist_plays())[1]
        cols = 5
        artist_info <- matrix(ncol=cols, nrow=rows)
        for (i in 1:rows) {
            artist_name <- scrobbles_artist_plays()$Artist[i]
            print(artist_name)
            artist_id <- pull_artist_id(artist_name)
            artist_df <- lookup_artist_by_id(artist_id, includes="tags")
            artist_info[i,1] <- artist_name # Artist name
            artist_info[i,2] <- pull_country(artist_df) # Artist country
            artist_info[i,3] <- pull_area(artist_df) # Artist area
            artist_info[i,4] <- pull_year(artist_df, artist_id) # StartYear
            artist_info[i,5] <- pull_genre(artist_df) # Genre
        }
        scrobbles_artist_info <- as.data.frame(artist_info)
        scrobbles_artist_info <- rename(scrobbles_artist_info, Artist=1, Country=2, Area=3, StartYear=4, Genre=5)
    })
    scrobbles_coordinates <- reactive({
        ' Produces a dataframe of area coordinates for artist areas by querying Google Maps '
        req(scrobbles_artist_info())
        scrobbles_coordinates <- unique(scrobbles_artist_info()[c("Country", "Area")])
        scrobbles_coordinates["Latitude"] = NA; scrobbles_coordinates["Longitude"] = NA
        scrobbles_coordinates <- mutate(scrobbles_coordinates, address=paste(Area, Country))
        addresses <- scrobbles_coordinates$address 
        geocodes <- geocode(addresses, source = "google")
        scrobbles_coordinates <- mutate(scrobbles_coordinates, Latitude=geocodes$lat, Longitude=geocodes$lon)
        scrobbles_coordinates <- scrobbles_coordinates[1:4]
    })
    scrobbles_dates <- reactive({
        ' Returns a subset dataframe of scrobbles within the input dates '
        req(input$listen_dates, scrobbles_preprocessed())
        scrobbles_dates <- scrobbles_preprocessed() %>%  
                           filter(Date >= input$listen_dates[1]) %>%
                           filter(Date <= input$listen_dates[2])
    })
    scrobbles_date_plays <- reactive({
        ' Counts the plays of each artist within the input dates '
        req(scrobbles_dates(), input$listen_dates)
        scrobbles_date_plays <- scrobbles_dates() %>%  
                                count(Artist, sort=TRUE) %>% 
                                filter(n >= 10)
        names(scrobbles_date_plays) <- c("Artist", "Plays") # Rename columns
        scrobbles_date_plays
    })
    scrobbles_final <- reactive({
        ' Produces the final dataframe containing all artist info joined to scrobbles, and a HTML label '
        req(scrobbles_coordinates(), scrobbles_artist_info(), scrobbles_dates(), input$listen_dates)
        scrobbles_final <- left_join(scrobbles_artist_info(), scrobbles_coordinates(), by=c("Country", "Area"))
        scrobbles_final <- left_join(scrobbles_date_plays(), scrobbles_final, by = "Artist")
        scrobbles_final$Label <-  paste("<p>", "Artist: ", scrobbles_final$Artist, "</p>",
                                        "<p>", "Plays: ", scrobbles_final$Plays, "</p>",
                                        "<p>", "Area: ", scrobbles_final$Area, "</p>")
        scrobbles_final
    })
    
    ' Outputs '
    output$table <- renderDataTable({
        ' Renders the table (troubleshooting) '
        scrobbles_final()
    })
    output$map <- renderLeaflet({
        ' Renders the map '
        req(input$map_theme)
        map <- leaflet() %>% 
            addProviderTiles(input$map_theme) %>% 
            # setView(lng=-6.2603097	, lat=53.34981, zoom=5) %>% 
            addCircleMarkers(lng=scrobbles_final()$Longitude, 
                             lat=scrobbles_final()$Latitude,
                             color="#FFF",
                             weight=1,
                             radius=10, 
                             label=lapply(scrobbles_final()$Label, HTML), 
                             clusterOptions=markerClusterOptions(showCoverageOnHover = FALSE)) 
    })
}

shinyApp(ui = ui, server = server)
