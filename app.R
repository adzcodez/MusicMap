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

# # UI
# ui <- fluidPage(
#     titlePanel("Spotify Map"),
#     sidebarLayout(
#         sidebarPanel(
#             fileInput("scrobblescsv", "Upload scrobbles .csv here")
#         ), 
#         mainPanel(
#            tableOutput("scrobbles_df")
#         )
#     )
# )
    
ui <- dashboardPage(
    skin = "purple", 
    dashboardHeader(title = "Music Map"), 
    dashboardSidebar(
        fileInput("scrobblescsv", "Upload scrobbles .csv here"),
        sliderInput("artist_dates", label = "Artist Date Range",
                    min = 1950, max = 2030, value = c(1950, 2030),
                    sep = "", step = 1),
        sliderInput("listen_dates", label = "Listen Date Range",
                    min = 2010, max = 2022, value = c(2010, 2022), 
                    sep = "", step = 1)
    ), 
    dashboardBody(
        fluidRow(box(width = 12, leafletOutput(outputId = "map"))),
        fluidRow(box(width = 12, tableOutput(outputId = "table")))
    )
)


# Server
server <- function(input, output) {
    scrobbles_upload <- reactive({
        req(input$scrobblescsv, file.exists(input$scrobblescsv$datapath))
        read.csv(input$scrobblescsv$datapath, header=FALSE, col.names=c("Artist", "Album", "Track", "Time"))
    })
    scrobbles_preprocessed <- reactive({ # Produces preprocessed scrobbles dataframe
        req(input$listen_dates, scrobbles_upload())
        scrobbles <- scrobbles_upload() # Checks file is uploaded
        scrobbles <- data.frame(scrobbles[1:3], apply(scrobbles[4], 2, processTime))
        scrobbles <- rename(scrobbles, Day=4, Month=5, Year=6, Hour=7, Minute=8)
        scrobbles <- scrobbles[scrobbles$Year > 1970,] %>% 
                     filter(Year >= input$listen_dates[1]) %>%
                     filter(Year <= input$listen_dates[2])
         })
        
    scrobbles_artist_plays <- reactive({ # Produces artist plays dataframe
        req(scrobbles_preprocessed())
        scrobbles_artist_plays <- scrobbles_preprocessed() %>% 
                                  count(Artist, sort=TRUE) %>% 
                                  filter(n > 200)
    })
    scrobbles_artist_info <- reactive({
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
        req(scrobbles_artist_info())
        scrobbles_coordinates <- unique(scrobbles_artist_info()[c("Country", "Area")])
        scrobbles_coordinates["Latitude"] = NA; scrobbles_coordinates["Longitude"] = NA
        scrobbles_coordinates <- mutate(scrobbles_coordinates, address=paste(Area, Country))
        addresses <- scrobbles_coordinates$address 
        geocodes <- geocode(addresses, source = "google")
        scrobbles_coordinates <- mutate(scrobbles_coordinates, Latitude=geocodes$lat, Longitude=geocodes$lon)
        scrobbles_coordinates <- scrobbles_coordinates[1:4]
    })
    scrobbles_artist_coordinates <- reactive({
        req(scrobbles_coordinates(), scrobbles_artist_info(), scrobbles_artist_plays())
        scrobbles_artist_coordinates <- left_join(scrobbles_artist_info(), scrobbles_coordinates(), by=c("Country", "Area")) 
        scrobbles_artist_coordinates <- left_join(scrobbles_artist_coordinates, scrobbles_artist_plays(), by="Artist")
        scrobbles_artist_coordinates$Label <-  paste("<p>", "Artist: ", scrobbles_artist_coordinates$Artist, "</p>",
                                                     "<p>", "Plays: ", scrobbles_artist_coordinates$n, "</p>",
                                                     "<p>", "Area: ", scrobbles_artist_coordinates$Area, "</p>")
        scrobbles_artist_coordinates
    })
    output$table <- renderTable({
        scrobbles_artist_coordinates()
    })
    output$map <- renderLeaflet({
        map <- leaflet() %>% 
            addProviderTiles(providers$CartoDB.DarkMatter) %>% 
            setView(lng=-6.2603097	, lat=53.34981, zoom=5) %>% 
            addCircleMarkers(lng=scrobbles_artist_coordinates()$Longitude, 
                             lat=scrobbles_artist_coordinates()$Latitude,
                             color="#FFF",
                             weight=1,
                             radius=10, 
                             label=lapply(scrobbles_artist_coordinates()$Label, HTML), 
                             clusterOptions=markerClusterOptions(showCoverageOnHover = FALSE)) 
    })
}

shinyApp(ui = ui, server = server)
