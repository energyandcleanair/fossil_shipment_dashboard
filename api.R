api.get_voyages <- function(date_from=NULL){
  url <- "https://fossil-shipment-tracker.ew.r.appspot.com/v0/voyage?"

  if(!is.null(date_from)){
    url <- paste0(url, sprintf("date_from=%s&", strftime(date_from, "%Y-%m-%d")))
  }

  jsonlite::fromJSON(url)$data %>%
    filter(departure_iso2=="RU",
           (arrival_iso2!="RU" | is.na(arrival_iso2))) %>%
    mutate_at(c("departure_date_utc","arrival_date_utc"), lubridate::as_date) %>%
    mutate(arrival_country = countrycode(arrival_iso2, "iso2c", "country.name"))
}

api.get_voyages_sf <- function(date_from=NULL){
  # Can be quite slow
  url <- sprintf("https://fossil-shipment-tracker.ew.r.appspot.com/v0/voyage?format=geojson&nest_in_data=False")
  tryCatch({geojsonsf::geojson_sf(url(url)) %>%
      filter(departure_iso2=="RU",
             (arrival_iso2!="RU" | is.na(arrival_iso2)))},
           error=function(e){
             return(NULL)
           })
}

api.get_berths_sf <- function(date_from=NULL){
  # Can be quite slow
  url <- sprintf("https://fossil-shipment-tracker.ew.r.appspot.com/v0/berth?format=geojson&nest_in_data=False")
  tryCatch({geojsonsf::geojson_sf(url(url))},
           error=function(e){
             return(NULL)
           })
}


api.get_voyage_line <- function(voyage_id){
  url <- sprintf("https://fossil-shipment-tracker.ew.r.appspot.com/v0/voyage?id=%d&format=geojson&nest_in_data=False", voyage_id)
  tryCatch({geojsonsf::geojson_sf(url(url))},
           error=function(e){
             return(NULL)
           })
}

api.get_voyage_points <- function(voyage_id){
  url <- sprintf("https://fossil-shipment-tracker.ew.r.appspot.com/v0/position?voyage_id=%d", voyage_id)
  positions <- jsonlite::fromJSON(url)$data %>%
    mutate_at(c("date_utc"), lubridate::as_date) %>%
    sf::st_as_sf(coords=c("lon","lat"))
}

api.get_portcalls <- function(date_from=NULL){
  url <- "https://fossil-shipment-tracker.ew.r.appspot.com/v0/portcall?"

  if(!is.null(date_from)){
    url <- paste0(url, sprintf("date_from=%s&", strftime(date_from, "%Y-%m-%d")))
  }

  jsonlite::fromJSON(url)$data
}


api.get_ports <- function(){
  url <- "https://fossil-shipment-tracker.ew.r.appspot.com/v0/port?"
  jsonlite::fromJSON(url)$data
}

api.get_ships <- function(){
  url <- "https://fossil-shipment-tracker.ew.r.appspot.com/v0/ship?"
  jsonlite::fromJSON(url)$data
}
