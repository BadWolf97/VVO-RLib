# Needed Packages
vvo_require <- function() {
  install.packages(c("jsonlite", "httr", "tidyverse", "leaflet", "shiny", "shinydashboard", "anytime"))
  library(jsonlite)  #Handling of JSON-Strings
  library(httr)  #Handling of HTTP-Requests
  library(tidyverse)  #Tidyverse eben
  library(leaflet)  #To present a good Map
  library(shiny)  #Dashboard
  library(shinydashboard)  #Dashboard
  library(anytime)  #Handling of Date/Time-Components
}

#Helper-Method for getting results via the WebAPI
vvo_get <- function(endpoint, body) {
  request_body_json <- toJSON(body, auto_unbox = TRUE)
  print(paste("RequestBody:", request_body_json))
  print(paste("Using Endpoint", paste("https://webapi.vvo-online.de", endpoint, sep = "/"), "..."))
  post_result <- POST(paste("https://webapi.vvo-online.de", endpoint, sep = "/"),
                      body = request_body_json,
                      add_headers(
                        .headers = c(
                          "Content-Type"="application/json",
                          "charset"="utf-8")
                      )
  )
  return (fromJSON(content(post_result, as = "text")))
}

#Helper-Method for getting a Date from the stuff the API sends
vvo_handleDate <- function(date) {
  return (anytime(as.numeric(str_replace(date, fixed("/Date("), "") %>% str_replace(fixed("-0000)/"), "")) / 1000))
}

#Helper-Method for getting the API-Date from an R-Date
vvo_toDate <- function(date) {
  return (paste0("/Date(", round(as.numeric(date) * 1000) , "-0000)/"))
}

#Helper-Method to beautify Platform-Description
vvo_transformPlatformType <- function(Platform) {
  return (paste(Platform$Type, Platform$Name) %>% 
            str_replace(fixed("Platform"), "Steig") %>%
            str_replace(fixed("Railtrack"), "Gleis")
  )
}

#Get Stops by searchquery
vvo_getStops <- function(query) {
  result <- vvo_get("tr/pointfinder", 
                    list(query = query, stopsOnly = TRUE))
  df_result <- read.table(text = unlist(result$Points), sep = "|", header = FALSE) %>%
    as_tibble() %>%
    set_names(c("ID", "Type", "City", "Station", "GK_Right", "GK_Up", "Distance", "Unknown", "ShortCode"))
  # NA in City means it is in Dresden
  df_result$City[is.na(df_result$City)] = "Dresden"
  # Drop NA or otherwise unknown result
  df_result = subset(df_result, select = -c(Type, Distance, Unknown, ShortCode))
  return(df_result)
}

#Get Departures by StopID
vvo_getDeps <- function(stopid, limit = 10){
  result <- vvo_get("dm", list(stopid = stopid, limit = limit, stopsOnly = TRUE))
  df_result = as_tibble(result$Departures) %>%
    mutate(
      RealTime = vvo_handleDate(RealTime),
      ScheduledTime = vvo_handleDate(ScheduledTime),
      .after = "Mot"
    )
  return(df_result)
}

#Get a Trip by StopID, TripID and Time
vvo_getTrip <- function(stopid, tripid, time = Sys.time()) {
  result <- vvo_get("dm/trip", list(tripid = tripid, stopid = stopid, time = vvo_toDate(time)))
  df_result = as_tibble(result$Stops) %>%
    mutate(
      RealTime = vvo_handleDate(RealTime),
      Time = vvo_handleDate(Time),
      .after = "State"
    )
  return(df_result)
}

#Get a Trip by a Departure from vvo_getDeps
vvo_getTripByDep <- function(stopid, departure) {
  return(vvo_getTrip(stopid, departure$Id, departure$ScheduledTime))
}

#Get all RouteChanges in VVO
vvo_getRouteChanges <- function() {
  result <- vvo_get("rc", list(shortterm = TRUE))
  df_result = as_tibble(result$Changes) %>%
    mutate(
      PublishDate = vvo_handleDate(PublishDate),
      .after = "Mot"
    )
  return(df_result)
}

#Get Lines at Stop by StopID
vvo_getLinesAtStop <- function(stopid) {
  result <- vvo_get("stt/lines", list(stopid = stopid))
  df_result = as_tibble(result$Lines)
  return(df_result)
}

#Get all Stops (daily updated)
vvo_getAllStops <- function() {
  df_result = fromJSON("https://www.vvo-online.de/open_data/VVO_STOPS.JSON") %>%
    as_tibble() %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      
      #MOTs = unique(Lines$Vehicle),
    )
  return(df_result)
}