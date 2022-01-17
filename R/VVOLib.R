#Helper-Method for getting results via the WebAPI
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite toJSON fromJSON
vvo_get <- function(endpoint, body) {
  #Format Request to a JSON
  request_body_json <- toJSON(body, auto_unbox = TRUE)
  print(paste("RequestBody:", request_body_json))
  print(paste("Using Endpoint", paste("https://webapi.vvo-online.de", endpoint, sep = "/"), "..."))
  # Do the Request
  post_result <- POST(paste("https://webapi.vvo-online.de", endpoint, sep = "/"),
                      body = request_body_json,
                      add_headers(
                        .headers = c(
                          "Content-Type"="application/json",
                          "charset"="utf-8")
                      )
  )
  # Return the Request
  return (fromJSON(content(post_result, as = "text")))
}

#Helper-Method for getting a Date from the stuff the API sends
#' @importFrom anytime anytime
#' @importFrom GGally %>%
#' @importFrom stringr str_replace fixed
vvo_handleDate <- function(date) {
  return (anytime(as.numeric(str_replace(date, fixed("/Date("), "") %>% str_replace(fixed("-0000)/"), "")) / 1000))
}

#Helper-Method for getting the API-Date from an R-Date
vvo_toDate <- function(date) {
  return (paste0("/Date(", round(as.numeric(date) * 1000) , "-0000)/"))
}

#' @title Helper-Method to beautify Platform-Description
#' @param Platform Platformobject from other Functions
#' @return String with Steig or Gleis depending on Platformtype
#' @export
#' @importFrom GGally %>%
#' @importFrom stringr str_replace fixed
vvo_transformPlatformType <- function(Platform) {
  return (paste(Platform$Type, Platform$Name) %>%
            str_replace(fixed("Platform"), "Steig") %>%
            str_replace(fixed("Railtrack"), "Gleis")
  )
}

#' @title Get Stops in VVO based on a query
#' @param query Query to search with. Supports Abbrevations by DVB
#' @return Tibble with Informations about the Stop
#' @export
#' @importFrom GGally %>%
#' @importFrom dplyr as_tibble
#' @importFrom purrr set_names
vvo_getStops <- function(query) {
  result <- vvo_get("tr/pointfinder",
                    list(query = query, stopsOnly = TRUE))
  df_result <- read.table(text = unlist(result$Points), sep = "|", header = FALSE) %>%
    as_tibble() %>%
    set_names(c("ID", "Type", "City", "Station", "GK_Right", "GK_Up", "Distance", "Unknown", "ShortCode"))
  # NA in City means it is in Dresden
  df_result$City[is.na(df_result$City)] = "Dresden"
  # Drop NA or otherwise unknown result
  df_result = subset(df_result, select = -c("Type", "Distance", "Unknown", "ShortCode"))
  return(df_result)
}

#' @title Get Departures by StopID
#' @param stopid StopID (can be obtained by vvo_getStops)
#' @param limit Resultcount. Default 10, Maximum (due to API-Restrictions) 50
#' @return Tibble with Informations about the Departures
#' @export
#' @importFrom GGally %>%
#' @importFrom dplyr as_tibble mutate
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

#' @title Get a Trip by StopID, TripID and Time
#' @param stopid StopID (can be obtained by vvo_getStops)
#' @param tripid TripID (can be obtained by vvo_getDeps). Note that its not an ID for a specific trip, but for a route (e.g. all Trips on line 1 from Prohlis to Leutewitz have the same ID)
#' @param time Departuretime of the specific trip on the specified station. Default is current time.
#' @return Tibble with Informations about all Stations in the Trip
#' @export
#' @importFrom GGally %>%
#' @importFrom dplyr as_tibble mutate
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

#' @rdname vvo_getTrip
#' @title ... or by StopID and Departure
#' @param stopid StopID (can be obtained by vvo_getStops)
#' @param departure One observation from vvo_getDeps
#' @export
vvo_getTripByDep <- function(stopid, departure) {
  return(vvo_getTrip(stopid, departure$Id, departure$ScheduledTime))
}

#' @title Get all RouteChanges in VVO
#' @return Tibble with all Changes in the VVO.
#' @export
#' @importFrom GGally %>%
#' @importFrom dplyr as_tibble mutate
vvo_getRouteChanges <- function() {
  result <- vvo_get("rc", list(shortterm = TRUE))
  df_result = as_tibble(result$Changes) %>%
    mutate(
      PublishDate = vvo_handleDate(PublishDate),
      .after = "Mot"
    )
  return(df_result)
}

#' @title Get Lines at Stop by StopID
#' @param stopid StopID (can be obtained by vvo_getStops)
#' @return Tibble with all Lines at this stop.
#' @export
#' @importFrom dplyr as_tibble
vvo_getLinesAtStop <- function(stopid) {
  result <- vvo_get("stt/lines", list(stopid = stopid))
  df_result = as_tibble(result$Lines)
  return(df_result)
}

#' @title Get all Stops (daily updated)
#' @return Tibble with all Stops in VVO, fetched from an open_data-JSON. There might be duplicates.
#' @source https://www.vvo-online.de/open_data/VVO_STOPS.JSON
#' @export
#' @importFrom GGally %>%
#' @importFrom dplyr as_tibble mutate
#' @importFrom jsonlite fromJSON
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
