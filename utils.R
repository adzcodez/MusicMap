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
  
  new_time <- list(day, month, year, hour, min)
  
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