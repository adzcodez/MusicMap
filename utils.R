' Author: Adam de Haro '
' Utility functions for MusicMap '

# Functions for preprocessing scrobbles csv
processTime <- function(input_time){
  ' Returns a list in the format [day, month, year, hour, minute] '
  time <- as.POSIXlt(input_time, format="%d %b %Y %H:%M")
  
  day <- time$mday
  month <- (time$mon + 1) # Months are 0-indexed
  year <- (time$year + 1900)  # Years since 1900
  hour <- time$hour
  min <- time$min
  date <- as.Date(time)
  
  new_time <- list(day, month, year, hour, min, date)
  
  return(new_time)
}

# Functions for populating artist info dataframe 

pull_artist_id <- function(artist) {
  ' Retrieves query artist MBID for use in further queries ' 
  id <- search_artists(artist) %>% 
    select(mbid) %>% 
    slice(1) %>% 
    pull()
  return(id)
}

pull_country <- function(df) {
  country <- df$country
  return(country)
}

pull_area <- function(df) {
  area <- df$begin_area_name
  return(area)
}

get_coords <- function(country, area){
  
  return(lat, long)
}

pull_year <- function(df, id) {
  if (is.null(df$type)){
    year <- df$life_span_begin
  } else if (is.na(df$type)) {
    year <- df$life_span_begin
  } else if (df$type == "Group"){ # If artist is a group, take year of formation
    year <- df$life_span_begin
  } else { # If artist is a person, take year of first release
    releases <- browse_release_groups_by("artist", id)
    year <- sort(releases$first_releas_date)[1]
  } 
  return(year)
}

# I need this function to handle empty tags
# TODO: Use LastFM API to improve genre selection
pull_genre <- function(df) {
  tags_list <- df$tags
  if (is.null(unlist(tags_list))) {
    return(NA)
  }
  tags_values <- as.numeric(unlist(tags_list[[1]][2]))
  tags_genres <- tags_list[[1]][1]
  genre <- tags_genres[which.max(tags_values),]
  genre <- genre[1,1,1]
  return(genre)
}

# These are Spotify helper functiosn to pull artists from playlists

build_artist_dataframe <- function(playlist_id, token) {
  ' Builds a dataframe of artists in a playlist, with each artist being a row '
  playlist <- get_playlist(playlist_id, authorization=token) # Get the playlist
  num_songs <- playlist$tracks$total                         # and total tracks
  
  total_batches <- ceiling(num_songs / 100) # We can only query 100 at a time
  batch_offset <- 0                         # So we will pull 100 and then increment offset by 100
  artist_names <- c()                       # Empty list to append names to
  
  for(batch in 1:total_batches) { # For each batch
    batch_artists <- get_artists_in_batch(playlist_id, batch_offset, token) # Get the artists in the batch
    batch_offset <- batch_offset + 100 # Increment offset by 100
    artist_names <- append(artist_names, batch_artists)# Append the batch artists to the list
  }
  
  artist_names <- as.data.frame(artist_names)    # Turn the list of names into a dataframe
  artist_names <- unique(artist_names)           # Remove duplicate artists
  artist_names <- rename(artist_names, Artist=1) # Rename first column to 'Artist'
  return(artist_names)
}

get_artists_in_batch <- function(playlist_id, batch_offset, token) {
  ' Builds a vector of artists in each batch of songs '
  batch_artists <- c()
  playlist_info <- get_playlist_items(playlist_id=playlist_id, offset=batch_offset, authorization=token)  # Get the playlist items
  artists <- playlist_info$track.album.artists          # This is a dataframe
  for(artist in 1:length(artists)) {                    # For each artist dataframe
    artist_name <- as.data.frame(artists[artist])$name  # Get the artist name
    batch_artists <- append(batch_artists, artist_name) # Append it to the list of names
  }
  return(batch_artists)
}

